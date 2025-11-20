#!/bin/bash
set -euo pipefail

echo "------------------------------------------------------------"
echo " Creating annual disturbance stacks using GDAL (all Float32 for continuous) "
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
    "${RESAMPLED_DIR}/wildfire_id_30m_resampled.tif"
    "${RESAMPLED_DIR}/biotic_gridded_1km_all_years_severity_30m_resampled.tif"
    "${RESAMPLED_DIR}/hd_fingerprint_30m_resampled.tif"
    "${RESAMPLED_DIR}/pdsi_annual_30m_resampled.tif"
)

# Raster type for masking: "categorical" or "continuous"
INPUT_TYPES=(
    "categorical"  # wildfire_id
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
# Step 0: Align forest mask to template
# ------------------------------
MASK_ALIGNED="${RESAMPLED_DIR}/forest_mask_30m_resampled_aligned.tif"
if [ ! -f "$MASK_ALIGNED" ]; then
    echo "Aligning forest mask to template..."
    bbox=$(gdalinfo -json "$TEMPLATE" | jq -r '.cornerCoordinates | "\(.upperLeft[0]) \(.upperLeft[1]) \(.lowerRight[0]) \(.lowerRight[1])"')
    read -r ULX ULY LRX LRY <<< "$bbox"
    gdalwarp -overwrite -r near -te "$ULX" "$LRY" "$LRX" "$ULY" -tr 30 30 -t_srs EPSG:5070 \
        -co COMPRESS=LZW -co TILED=YES -co BIGTIFF=YES "$MASK" "$MASK_ALIGNED"
    echo "✓ Mask aligned: $MASK_ALIGNED"
else
    echo "Aligned mask already exists: $MASK_ALIGNED"
fi

# ------------------------------
# Step 1: Loop through years
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

        if [ ! -f "$OUT_MASKED" ]; then
            echo "  Masking $BASENAME band $BAND_IDX"
            if [ "$TYPE" = "categorical" ]; then
                # integer multiplication for categorical
                gdal_calc.py --overwrite -A "$RASTER" --A_band="$BAND_IDX" -B "$MASK_ALIGNED" \
                    --outfile="$OUT_MASKED" --calc="A*B" --NoDataValue=0 --type=Int32 \
                    --co="COMPRESS=LZW" --co="TILED=YES" --co="BIGTIFF=YES"
            else
                # continuous: use numpy.where to mask, keep Float32
                gdal_calc.py --overwrite -A "$RASTER" --A_band="$BAND_IDX" -B "$MASK_ALIGNED" \
                    --outfile="$OUT_MASKED" \
                    --calc="numpy.where(B==1, A, numpy.nan)" \
                    --NoDataValue=nan --type=Float32 \
                    --co="COMPRESS=LZW" --co="TILED=YES" --co="BIGTIFF=YES"
            fi
        else
            echo "  Found existing masked file: $OUT_MASKED"
        fi

        MASKED_LIST+=("$OUT_MASKED")
    done

    # Merge masked single-band files into a multi-band stack
    echo "  Creating stack: $STACK_FILE"
    VRT="${STACK_FILE%.tif}.vrt"
    gdalbuildvrt -separate -vrtnodata nan "$VRT" "${MASKED_LIST[@]}"
    gdal_translate -co COMPRESS=LZW -co TILED=YES -co BIGTIFF=YES -a_nodata nan "$VRT" "$STACK_FILE"
    rm -f "$VRT"
    echo "  ✅ Written: $STACK_FILE"
done

echo "------------------------------------------------------------"
echo "All annual stacks written to $ANNUAL_STACK_DIR"
echo "------------------------------------------------------------"

