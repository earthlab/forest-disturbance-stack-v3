#!/usr/bin/env bash
set -euo pipefail

echo "=================================================="
echo " Spatial area metrics for cumulative disturbances"
echo " Westwide + Ecoregions + Any vs Extreme"
echo " Project: forest-disturbance-stack-v3"
echo "=================================================="

# --------------------------------------------------
# Paths
# --------------------------------------------------
SPATIAL_ANY_DIR="data/derived/annual_stacks/binary/spatial_presence/any"
SPATIAL_EXT_DIR="data/derived/annual_stacks/binary/spatial_presence/extreme"

FOREST_MASK="data/derived/resampled/forest_mask_30m_resampled.tif"
ECO_RASTER="data/derived/ecoregions/ecoregions_level3_30m.tif"

OUT_DIR="data/derived/spatial_metrics"
mkdir -p "${OUT_DIR}"

PIXEL_AREA_KM2=0.0009

DIST_TYPES=(
  "wf" "bt" "hd" "pd"
  "wf_bt" "wf_hd" "wf_pd"
  "bt_hd" "bt_pd"
  "wf_bt_hd" "wf_bt_pd"
)

# --------------------------------------------------
# GDAL sanity checks
# --------------------------------------------------
echo ">>> GDAL sanity checks"
gdalinfo "${FOREST_MASK}" | grep -E "Pixel Size|PROJCRS"
gdalinfo "${ECO_RASTER}"  | grep -E "Pixel Size|PROJCRS"
echo ">>> GDAL checks complete"
echo ""

# ==================================================
# FUNCTION: run metrics for one mode
# ==================================================
run_mode () {

MODE_NAME="$1"
SPATIAL_DIR="$2"

echo ">>> Processing mode: ${MODE_NAME}"

CSV_US="${OUT_DIR}/spatial_metrics_west_${MODE_NAME}.csv"
CSV_ECO="${OUT_DIR}/spatial_metrics_ecoregion_${MODE_NAME}.csv"

echo "disturbance,area_km2,percent_forest" > "${CSV_US}"
echo "ecoregion_id,disturbance,area_km2,percent_forest_ecoregion,share_of_westwide" > "${CSV_ECO}"

python3 << EOF
import rasterio
import numpy as np
import pandas as pd
import os

SPATIAL_DIR = "${SPATIAL_DIR}"
MODE = "${MODE_NAME}"
FOREST_MASK = "${FOREST_MASK}"
ECO_RASTER = "${ECO_RASTER}"
CSV_US = "${CSV_US}"
CSV_ECO = "${CSV_ECO}"
PIXEL_AREA = ${PIXEL_AREA_KM2}

DIST_TYPES = [
  "wf","bt","hd","pd",
  "wf_bt","wf_hd","wf_pd",
  "bt_hd","bt_pd",
  "wf_bt_hd","wf_bt_pd"
]

# ---- Load masks ----
with rasterio.open(FOREST_MASK) as fsrc:
    forest = fsrc.read(1) == 1

with rasterio.open(ECO_RASTER) as esrc:
    eco = esrc.read(1)
    eco_nodata = esrc.nodata

valid = forest & (eco != eco_nodata)
eco_ids = np.unique(eco[valid])

forest_px_total = np.count_nonzero(forest)

forest_px_by_eco = {
    eid: np.count_nonzero((eco == eid) & forest)
    for eid in eco_ids
}

# ---- Loop disturbances ----
for dist in DIST_TYPES:
    raster = os.path.join(SPATIAL_DIR, f"{dist}_{MODE}_presence.tif")
    if not os.path.exists(raster):
        continue

    with rasterio.open(raster) as src:
        data = src.read(1) == 1

    # Mask to forest
    data = data & forest

    # ---- Westwide ----
    dist_px = np.count_nonzero(data)
    area_km2 = dist_px * PIXEL_AREA
    pct_forest = (dist_px / forest_px_total * 100) if forest_px_total > 0 else 0

    with open(CSV_US, "a") as f:
        f.write(f"{dist},{area_km2:.3f},{pct_forest:.2f}\n")

    # ---- Per-ecoregion ----
    rows = []
    for eid in eco_ids:
        eco_mask = (eco == eid) & forest
        eco_px = forest_px_by_eco[eid]
        if eco_px == 0:
            continue

        dist_px_eco = np.count_nonzero(data & eco_mask)
        area_km2_eco = dist_px_eco * PIXEL_AREA
        pct_eco = (dist_px_eco / eco_px * 100) if eco_px > 0 else 0
        share_west = (area_km2_eco / area_km2) if area_km2 > 0 else 0

        rows.append((eid, dist, area_km2_eco, pct_eco, share_west))

    pd.DataFrame(
        rows,
        columns=[
            "ecoregion_id",
            "disturbance",
            "area_km2",
            "percent_forest_ecoregion",
            "share_of_westwide"
        ]
    ).to_csv(CSV_ECO, mode="a", header=False, index=False)
EOF
}

# --------------------------------------------------
# Run BOTH modes
# --------------------------------------------------
run_mode "any"     "${SPATIAL_ANY_DIR}"
run_mode "extreme" "${SPATIAL_EXT_DIR}"

# --------------------------------------------------
# Any vs Extreme ratio
# --------------------------------------------------
echo ">>> Calculating any vs extreme ratios"

python3 << EOF
import pandas as pd

out = "${OUT_DIR}"

any_us = pd.read_csv(f"{out}/spatial_metrics_west_any.csv")
ext_us = pd.read_csv(f"{out}/spatial_metrics_west_extreme.csv")

any_eco = pd.read_csv(f"{out}/spatial_metrics_ecoregion_any.csv")
ext_eco = pd.read_csv(f"{out}/spatial_metrics_ecoregion_extreme.csv")

# ---- Westwide ratio ----
us_ratio = any_us.merge(ext_us, on="disturbance", suffixes=("_any", "_extreme"))
us_ratio["extreme_to_any_ratio"] = us_ratio["area_km2_extreme"] / us_ratio["area_km2_any"]
us_ratio["ecoregion"] = "western_US"

# ---- Ecoregion ratio ----
eco_ratio = any_eco.merge(
    ext_eco,
    on=["ecoregion_id", "disturbance"],
    suffixes=("_any", "_extreme")
)
eco_ratio["extreme_to_any_ratio"] = eco_ratio["area_km2_extreme"] / eco_ratio["area_km2_any"]
eco_ratio = eco_ratio.rename(columns={"ecoregion_id": "ecoregion"})

ratio = pd.concat([us_ratio, eco_ratio], ignore_index=True)
ratio.to_csv(f"{out}/spatial_metrics_any_vs_extreme_ratio.csv", index=False)
EOF

echo ""
echo "=================================================="
echo " Spatial metrics COMPLETE"
echo "=================================================="

