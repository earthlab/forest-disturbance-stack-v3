#!/bin/bash
set -euo pipefail

echo "------------------------------------------------------------"
echo " Creating annual disturbance stacks using GDAL (Float32) "
echo "------------------------------------------------------------"

# ------------------------------
# Paths
# ------------------------------
RESAMPLED_DIR="data/derived/resampled"
ANNUAL_STACK_DIR="data/derived/annual_stacks"
TEMPLATE="${RESAMPLED_DIR}/template_30m_singleband.tif"
MASK="${RESAMPLED_DIR}/forest_mask_30m_resampled.tif"

# Input rasters (already subsetted & resampled; each has 21 bands for 2000..2020)
INPUTS=(
    "${RESAMPLED_DIR}/wildfire_cbi_30m_resampled.tif"
    "${RESAMPLED_DIR}/biotic_gridded_1km_all_years_severity_30m_resampled.tif"
    "${RESAMPLED_DIR}/hd_fingerprint_30m_resampled.tif"
    "${RESAMPLED_DIR}/pdsi_annual_30m_resampled.tif"
)

# Raster type for masking: "categorical" or "continuous"
INPUT_TYPES=(
    "continuous"  # wildfire_cbi
    "continuous"   # biotic
    "categorical"  # hd
    "continuous"   # pdsi
)

# create output dir
mkdir -p "$ANNUAL_STACK_DIR"

# ------------------------------
# Sanity checks
# ------------------------------
for cmd in gdalinfo gdalwarp gdal_calc.py gdal_merge.py jq bc; do
    command -v $cmd >/dev/null 2>&1 || { echo "$cmd not found"; exit 1; }
done

[ ! -f "$TEMPLATE" ] && { echo "Template not found: $TEMPLATE"; exit 1; }
[ ! -f "$MASK" ] && { echo "Forest mask not found: $MASK"; exit 1; }

# ------------------------------
# Step 0: Align forest mask to each input raster
# ------------------------------
declare -A MASK_ALIGNED_MAP

for i in "${!INPUTS[@]}"; do
    RASTER="${INPUTS[$i]}"
    BASENAME=$(basename "$RASTER" .tif)
    ALIGNED_MASK="${RESAMPLED_DIR}/${BASENAME}_mask_aligned.tif"
    MASK_ALIGNED_MAP[$RASTER]="$ALIGNED_MASK"

    if [ ! -f "$ALIGNED_MASK" ]; then
        echo "Aligning forest mask to $BASENAME..."
        # Extract raster extent
        bbox=$(gdalinfo -json "$RASTER" | jq -r '.cornerCoordinates | "\(.upperLeft[0]) \(.upperLeft[1]) \(.lowerRight[0]) \(.lowerRight[1])"')
        read -r ULX ULY LRX LRY <<< "$bbox"

        gdalwarp -overwrite -r near -te "$ULX" "$LRY" "$LRX" "$ULY" -tr 30 30 -t_srs EPSG:5070 \
            -co COMPRESS=LZW -co TILED=YES -co BIGTIFF=YES "$MASK" "$ALIGNED_MASK"
        echo "✓ Mask aligned for $BASENAME: $ALIGNED_MASK"
    else
        echo "Aligned mask already exists for $BASENAME: $ALIGNED_MASK"
    fi
done

# ------------------------------
# Step 1: Loop through years and apply mask
# ------------------------------
echo "Starting per-year stacking..."
for YEAR in {2000..2020}; do
    echo "Processing year $YEAR..."
    STACK_FILE="${ANNUAL_STACK_DIR}/annual_stack_${YEAR}.tif"
    [ -f "$STACK_FILE" ] && { echo "  Stack exists, skipping"; continue; }

    MASKED_LIST=()
    BAND_IDX=$(( YEAR - 2000 + 1 ))

    for i in "${!INPUTS[@]}"; do
        RASTER="${INPUTS[$i]}"
        TYPE="${INPUT_TYPES[$i]}"
        BASENAME=$(basename "$RASTER" .tif)
        OUT_MASKED="${RESAMPLED_DIR}/${BASENAME}_masked_${YEAR}.tif"
        MASK_ALIGNED="${MASK_ALIGNED_MAP[$RASTER]}"

        if [ ! -f "$OUT_MASKED" ]; then
            echo "  Masking $BASENAME band $BAND_IDX"

            # -------------------------------
            # Wildfire-specific calc
            # -------------------------------
            if [[ "$BASENAME" == "wildfire_cbi_30m_resampled" ]]; then
                CALC_EXPR="where(B==1, A, 0)"   # unburned forest = 0
            else
                CALC_EXPR="where(B==1, A, -9999)"  # other rasters unchanged
            fi

            # Run masking with gdal_calc.py
            gdal_calc.py --overwrite -A "$RASTER" --A_band="$BAND_IDX" -B "$MASK_ALIGNED" \
                --outfile="$OUT_MASKED" \
                --calc="$CALC_EXPR" \
                --NoDataValue=-9999 \
                --type=Float32 \
                --co="COMPRESS=LZW" --co="TILED=YES" --co="BIGTIFF=YES"
        else
            echo "  Found existing masked file: $OUT_MASKED"
        fi

        MASKED_LIST+=("$OUT_MASKED")
    done

    # Merge masked single-band files into a multi-band stack
    echo "  Creating stack: $STACK_FILE"
    VRT="${STACK_FILE%.tif}.vrt"
    gdalbuildvrt -separate "$VRT" "${MASKED_LIST[@]}"
    gdal_translate -co COMPRESS=LZW -co TILED=YES -co BIGTIFF=YES -ot Float32 "$VRT" "$STACK_FILE"
    rm -f "$VRT"
    echo "  ✅ Written: $STACK_FILE"
done

echo "------------------------------------------------------------"
echo "All annual stacks written to $ANNUAL_STACK_DIR"
echo "------------------------------------------------------------"




