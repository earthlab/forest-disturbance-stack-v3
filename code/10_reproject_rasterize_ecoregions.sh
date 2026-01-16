#!/bin/bash
set -euo pipefail

# -----------------------------
# Paths
# -----------------------------
ECO_SHP="data/raw/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp"
ECO_LAYER="us_eco_l3_state_boundaries"
ECO_FIELD="US_L3CODE"

FOREST_MASK="data/derived/resampled/forest_mask_30m_resampled.tif"
OUT_DIR="data/derived/ecoregions"
OUT_RASTER="${OUT_DIR}/ecoregions_level3_30m.tif"

mkdir -p "${OUT_DIR}"

echo "Creating empty ecoregion raster using forest mask as template..."

# ----------------------------------
# Step 1: Create empty raster template
# ----------------------------------
gdal_calc.py \
  -A "${FOREST_MASK}" \
  --calc="0" \
  --type=Int16 \
  --NoDataValue=0 \
  --overwrite \
  --co="COMPRESS=DEFLATE" \
  --co="TILED=YES" \
  --outfile="${OUT_RASTER}"

echo "Rasterizing Level III ecoregions..."

# ----------------------------------
# Step 2: Burn ecoregion IDs
# ----------------------------------
gdal_rasterize \
  -l "${ECO_LAYER}" \
  -a "${ECO_FIELD}" \
  -at \
  "${ECO_SHP}" \
  "${OUT_RASTER}"

echo "✅ Ecoregion raster created:"
echo "   ${OUT_RASTER}"

