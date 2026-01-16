#!/bin/bash
set -euo pipefail

# -----------------------------
# Paths
# -----------------------------
SPATIAL_PRESENCE_DIR="data/derived/annual_stacks/binary/spatial_presence"
FOREST_MASK="data/derived/resampled/forest_mask_30m_resampled.tif"
ECO_RASTER="data/derived/ecoregions/ecoregions_level3_30m.tif"
OUT_DIR="data/derived/spatial_metrics"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}" "${TMP_DIR}"

MODES=("any" "extreme")
DIST_TYPES=("wf" "bt" "hd" "pd" "wf_bt" "wf_hd" "bt_hd" "wf_pd" "bt_pd" "wf_bt_hd" "wf_bt_pd")

echo "Step 1: Clip and align ecoregion raster to western US extent..."

ECO_WEST="${TMP_DIR}/ecoregions_west_aligned.tif"

# Extract bounding box from forest mask and enforce correct min/max
read MINX MINY MAXX MAXY <<< $(gdalinfo "${FOREST_MASK}" | awk '
    /Upper Left/ {ulx=$4; uly=$5}
    /Lower Right/ {lrx=$4; lry=$5}
    END {
        gsub(/[(),]/,"",ulx); gsub(/[(),]/,"",uly)
        gsub(/[(),]/,"",lrx); gsub(/[(),]/,"",lry)
        # Ensure min < max
        if(ulx<lrx){minx=ulx; maxx=lrx} else {minx=lrx; maxx=ulx}
        if(uly<lry){miny=uly; maxy=lry} else {miny=lry; maxy=uly}
        print minx, miny, maxx, maxy
    }')

echo "Clipping ecoregion raster to forest mask extent..."
gdalwarp -overwrite \
    -tr 30 30 \
    -r near \
    -dstnodata 0 \
    -te ${MINX} ${MINY} ${MAXX} ${MAXY} \
    "${ECO_RASTER}" \
    "${ECO_WEST}"

echo "✅ Ecoregions clipped & aligned: ${ECO_WEST}"

# -----------------------------
# Step 2: Loop over modes and disturbance types
# -----------------------------
for MODE in "${MODES[@]}"; do
    echo "Processing mode: ${MODE}"
    
    CSV_US="${OUT_DIR}/spatial_metrics_west_${MODE}.csv"
    CSV_ECO="${OUT_DIR}/spatial_metrics_ecoregion_${MODE}.csv"
    
    # Initialize CSVs with headers
    echo "dist_type,total_area_km2,percent_forest" > "${CSV_US}"
    echo "ecoregion_id,dist_type,area_km2,percent_forest_ecoregion" > "${CSV_ECO}"
    
    for DIST in "${DIST_TYPES[@]}"; do
        RASTER="${SPATIAL_PRESENCE_DIR}/${MODE}/${DIST}_${MODE}_presence.tif"
        [[ ! -f "${RASTER}" ]] && echo "Warning: ${RASTER} not found, skipping" && continue
        
        # ---- western US totals ----
        TMP_MASKED="${TMP_DIR}/${DIST}_${MODE}_masked.tif"
        gdal_calc.py -A "${RASTER}" -B "${FOREST_MASK}" \
            --calc="A*B" --NoDataValue=0 --type=Byte \
            --outfile="${TMP_MASKED}" --overwrite
        
        PIXELS=$(python3 -c "import rasterio; import numpy as np; arr=rasterio.open('${TMP_MASKED}').read(1); print(np.count_nonzero(arr))")
        TOTAL_PIXELS=$(python3 -c "import rasterio; import numpy as np; arr=rasterio.open('${FOREST_MASK}').read(1); print(np.count_nonzero(arr))")
        
        AREA_KM2=$(python3 -c "print(round(${PIXELS}*0.0009,2))")  # 30m x 30m = 0.0009 km²
        PERCENT=$(python3 -c "print(round(${PIXELS}/${TOTAL_PIXELS}*100,2))")
        
        echo "${DIST},${AREA_KM2},${PERCENT}" >> "${CSV_US}"
        
        # ---- per-ecoregion metrics ----
        python3 <<EOF
import rasterio
import numpy as np
import pandas as pd

with rasterio.open("${ECO_WEST}") as eco_src, rasterio.open("${TMP_MASKED}") as dist_src:
    eco = eco_src.read(1)
    dist = dist_src.read(1)
    ids = np.unique(eco[eco>0])
    rows = []
    for eid in ids:
        mask = eco==eid
        area_px = np.count_nonzero(dist[mask])
        total_px = np.count_nonzero(mask)
        area_km2 = area_px*0.0009
        pct = (area_px/total_px*100) if total_px>0 else 0
        rows.append((eid,"${DIST}",round(area_km2,2),round(pct,2)))
df = pd.DataFrame(rows, columns=["ecoregion_id","dist_type","area_km2","percent_forest_ecoregion"])
df.to_csv("${CSV_ECO}", mode='a', index=False, header=False)
EOF
        rm -f "${TMP_MASKED}"
    done
done

rm -rf "${TMP_DIR}"
echo "✅ Spatial metrics calculated for western US and ecoregions."
echo "Output CSVs:"
echo "  US totals: ${OUT_DIR}/spatial_metrics_west_<mode>.csv"
echo "  Per-ecoregion: ${OUT_DIR}/spatial_metrics_ecoregion_<mode>.csv"

