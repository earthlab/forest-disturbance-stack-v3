#!/bin/bash
set -euo pipefail

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

# Input rasters (already subsetted & resampled; each has 21 bands for 2000..2020)
INPUTS=(
    "${RESAMPLED_DIR}/wildfire_id_30m_resampled.tif"
    "${RESAMPLED_DIR}/biotic_gridded_1km_all_years_severity_30m_resampled.tif"
    "${RESAMPLED_DIR}/hd_fingerprint_30m_resampled.tif"
    "${RESAMPLED_DIR}/pdsi_annual_30m_resampled.tif"
)

# create output dir
mkdir -p "$ANNUAL_STACK_DIR"

# ------------------------------
# Sanity checks for required tools & files
# ------------------------------
command -v gdalinfo >/dev/null 2>&1 || { echo "gdalinfo not found"; exit 1; }
command -v gdalwarp >/dev/null 2>&1 || { echo "gdalwarp not found"; exit 1; }
command -v gdal_calc.py >/dev/null 2>&1 || { echo "gdal_calc.py not found"; exit 1; }
command -v gdal_merge.py >/dev/null 2>&1 || { echo "gdal_merge.py not found"; exit 1; }
command -v jq >/dev/null 2>&1 || { echo "jq not found; please install jq"; exit 1; }
command -v bc >/dev/null 2>&1 || { echo "bc not found; please install bc"; exit 1; }

if [ ! -f "$TEMPLATE" ]; then
  echo "Template not found: $TEMPLATE"
  exit 1
fi
if [ ! -f "$MASK" ]; then
  echo "Forest mask not found: $MASK"
  exit 1
fi

# ------------------------------
# Step 0: Align forest mask to template (force exact origin/extent)
# ------------------------------
MASK_ALIGNED="${RESAMPLED_DIR}/forest_mask_30m_resampled_aligned.tif"
if [ ! -f "$MASK_ALIGNED" ]; then
    echo "Aligning forest mask to template..."

    # Extract numeric bounding box using GDAL JSON (safe & robust)
    bbox=$(gdalinfo -json "$TEMPLATE" | jq -r '
      .cornerCoordinates | "\(.upperLeft[0]) \(.upperLeft[1]) \(.lowerRight[0]) \(.lowerRight[1])"
    ')

    # Read into variables
    read -r ULX ULY LRX LRY <<< "$bbox"

    echo "Template bounding box (numeric):"
    echo " ULX=$ULX"
    echo " ULY=$ULY"
    echo " LRX=$LRX"
    echo " LRY=$LRY"

    # Basic validation
    if [ -z "$ULX" ] || [ -z "$ULY" ] || [ -z "$LRX" ] || [ -z "$LRY" ]; then
        echo "ERROR: Could not parse template bbox (empty). Aborting."
        exit 1
    fi

    # Ensure ULX < LRX and LRY < ULY
    if [ "$(echo "$ULX >= $LRX" | bc -l)" -eq 1 ]; then
        echo "ERROR: Parsed bounding box is flipped horizontally (ULX >= LRX). Aborting."
        exit 1
    fi
    if [ "$(echo "$LRY >= $ULY" | bc -l)" -eq 1 ]; then
        echo "ERROR: Parsed bounding box is flipped vertically (LRY >= ULY). Aborting."
        exit 1
    fi

    # Run gdalwarp to exactly align the mask to the template bounds & resolution
    echo "Running gdalwarp to align mask..."
    gdalwarp -overwrite -r near \
      -te "$ULX" "$LRY" "$LRX" "$ULY" \
      -tr 30 30 \
      -t_srs EPSG:5070 \
      -co COMPRESS=LZW -co TILED=YES -co BIGTIFF=YES \
      "$MASK" "$MASK_ALIGNED"

    echo "✓ Mask aligned to template: $MASK_ALIGNED"
else
    echo "Aligned mask already exists: $MASK_ALIGNED"
fi

# ------------------------------
# Step 1: Loop through years and create annual stacks
# ------------------------------
echo "Starting per-year stacking..."
for YEAR in {2000..2020}; do
    echo "Processing year $YEAR..."
    STACK_FILE="${ANNUAL_STACK_DIR}/annual_stack_${YEAR}.tif"

    # Skip if already exists
    if [ -f "$STACK_FILE" ]; then
        echo "  Stack already exists, skipping: $STACK_FILE"
        continue
    fi

    # Build list of per-variable masked band files for this year
    MASKED_LIST=()

    BAND_IDX=$(( YEAR - 2000 + 1 ))   # band index in the 2000..2020 subset (1..21)
    for RASTER in "${INPUTS[@]}"; do
        BASENAME=$(basename "$RASTER" .tif)
        OUT_MASKED="${RESAMPLED_DIR}/${BASENAME}_masked_${YEAR}.tif"

        if [ ! -f "$OUT_MASKED" ]; then
            echo "  Masking $BASENAME band ${BAND_IDX} -> $(basename "$OUT_MASKED")"
            # Extract the requested band and apply the aligned mask in one step:
            # Use gdal_calc.py with A = band from multi-band file, B = mask
            gdal_calc.py --overwrite \
              -A "$RASTER" --A_band="$BAND_IDX" \
              -B "$MASK_ALIGNED" \
              --outfile="$OUT_MASKED" \
              --calc="A*B" --NoDataValue=0 \
              --co="COMPRESS=LZW" --co="TILED=YES" --co="BIGTIFF=YES"
        else
            echo "  Found existing masked file: $OUT_MASKED"
        fi

        MASKED_LIST+=("$OUT_MASKED")
    done

    # Now merge the per-variable masked single-band files into a 4-band stack
    echo "  Creating stack for $YEAR -> $STACK_FILE"
    gdal_merge.py -separate -o "$STACK_FILE" -co COMPRESS=LZW -co TILED=YES -co BIGTIFF=YES "${MASKED_LIST[@]}"
    echo "  ✅ Written: $STACK_FILE"
done

echo "------------------------------------------------------------"
echo "All annual stacks written to $ANNUAL_STACK_DIR"
echo "------------------------------------------------------------"

