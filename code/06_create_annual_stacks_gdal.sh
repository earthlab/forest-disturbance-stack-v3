#!/bin/bash
set -e
echo "------------------------------------------------------------"
echo " Creating annual disturbance stacks using GDAL "
echo "------------------------------------------------------------"

# ------------------------------
# Paths
# ------------------------------
RESAMPLED_DIR="data/derived/resampled"
ANNUAL_STACK_DIR="data/derived/annual_stacks"
TEMPLATE="${RESAMPLED_DIR}/template_30m_singleband.tif"
MASK="${RESAMPLED_DIR}/forest_mask_30m_resampled.tif"

# Input rasters (already subsetted & resampled)
INPUTS=(
    "${RESAMPLED_DIR}/wildfire_id_30m_resampled.tif"
    "${RESAMPLED_DIR}/biotic_gridded_1km_all_years_severity_30m_resampled.tif"
    "${RESAMPLED_DIR}/hd_fingerprint_30m_resampled.tif"
    "${RESAMPLED_DIR}/pdsi_annual_30m_resampled.tif"
)

mkdir -p "$ANNUAL_STACK_DIR"

# ------------------------------
# Step 0: Align forest mask to template (force exact origin/extent)
# ------------------------------
MASK_ALIGNED="${RESAMPLED_DIR}/forest_mask_30m_resampled_aligned.tif"
if [ ! -f "$MASK_ALIGNED" ]; then
    echo "Aligning forest mask to template..."

    # Extract template bounding box using gdalinfo → projwin syntax
    read ULX ULY LRX LRY <<< $(gdalinfo "$TEMPLATE" | \
        awk '/Upper Left/ {gsub(/[(),]/,"",$0); ULX=$4; ULY=$5} 
             /Lower Right/ {gsub(/[(),]/,"",$0); LRX=$4; LRY=$5} 
             END {print ULX, ULY, LRX, LRY}')

    # Use gdalwarp with extracted coordinates
    gdalwarp -overwrite \
        -r near \
        -te $ULX $LRY $LRX $ULY \
        -tr 30 30 -t_srs EPSG:5070 \
        "$MASK" "$MASK_ALIGNED"
fi

# ------------------------------
# Step 1: Loop through years and create annual stacks
# ------------------------------
for YEAR in {2000..2020}; do
    echo "Processing year $YEAR..."
    STACK_FILE="${ANNUAL_STACK_DIR}/annual_stack_${YEAR}.tif"

    # Skip if already exists
    if [ -f "$STACK_FILE" ]; then
        echo "  Stack already exists, skipping..."
        continue
    fi

    # Initialize list of masked rasters for this year
    MASKED_LIST=()

    for RASTER in "${INPUTS[@]}"; do
        BASENAME=$(basename "$RASTER" .tif)
        OUT_MASKED="${RESAMPLED_DIR}/${BASENAME}_masked_${YEAR}.tif"

        # Apply forest mask to the band corresponding to the current year
        if [ ! -f "$OUT_MASKED" ]; then
            echo "  Masking $BASENAME for $YEAR..."
            # Assume band index corresponds to YEAR-START_YEAR+1 for each raster
            # Example: wildfire starts 2000 -> band 1
            # Adjust manually if necessary for your rasters
            BAND_IDX=$((YEAR - 2000 + 1))
            gdal_calc.py -A "$RASTER" --A_band=$BAND_IDX -B "$MASK_ALIGNED" \
                --outfile="$OUT_MASKED" --calc="A*B" --NoDataValue=0
        else
            echo "  $BASENAME already masked for $YEAR"
        fi

        MASKED_LIST+=("$OUT_MASKED")
    done

    # Stack all masked rasters for this year
    echo "  Creating stack for $YEAR..."
    gdal_merge.py -separate -o "$STACK_FILE" "${MASKED_LIST[@]}"
    echo "  ✅ Written: $STACK_FILE"

done

echo "------------------------------------------------------------"
echo "All annual stacks written to $ANNUAL_STACK_DIR"
echo "------------------------------------------------------------"

