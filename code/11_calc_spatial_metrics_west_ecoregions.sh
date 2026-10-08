#!/usr/bin/env bash
set -euo pipefail

echo "=================================================="
echo " Spatial area metrics for disturbances"
echo " Project: forest-disturbance-stack-v3"
echo ""
echo " Outputs include:"
echo "   1. Cumulative spatial presence (Westwide)"
echo "   2. Cumulative spatial presence (Ecoregions)"
echo "   3. Annual disturbance area (Westwide)"
echo "   4. Total disturbance area-through-time"
echo "   5. Any vs Extreme cumulative ratios"
echo "=================================================="

# --------------------------------------------------
# Paths
# --------------------------------------------------

# Cumulative spatial-presence rasters
SPATIAL_ANY_DIR="data/derived/annual_stacks/binary/spatial_presence/any"
SPATIAL_EXT_DIR="data/derived/annual_stacks/binary/spatial_presence/extreme"

# Annual binary stacks
BIN_DIR="data/derived/annual_stacks/binary"

# Frequency rasters
FREQ_DIR="data/derived/frequency"

# Masks
FOREST_MASK="data/derived/resampled/forest_mask_30m_resampled.tif"
ECO_RASTER="data/derived/ecoregions/ecoregions_level3_30m.tif"

# Output
OUT_DIR="data/derived/spatial_metrics"
mkdir -p "${OUT_DIR}"

# 30 m x 30 m pixel = 900 m2 = 0.0009 km2
PIXEL_AREA_KM2=0.0009

# Disturbance types
DIST_TYPES=(
  "wf" "bt" "hd" "pd"
  "wf_bt" "wf_hd" "wf_pd"
  "bt_hd" "bt_pd"
  "wf_bt_hd" "wf_bt_pd"
)


# ==================================================
# SANITY CHECKS
# ==================================================

echo ""
echo ">>> GDAL sanity checks"

gdalinfo "${FOREST_MASK}" | grep -E "Pixel Size|PROJCRS"
gdalinfo "${ECO_RASTER}"  | grep -E "Pixel Size|PROJCRS"

echo ">>> GDAL checks complete"


# ==================================================
# PART 1
# CUMULATIVE SPATIAL-PRESENCE METRICS
#
# These retain the ORIGINAL spatial-presence logic.
#
# Singles:
#   Unique forest area experiencing the disturbance
#   at least once during 2000-2020.
#
# Combinations:
#   Unique forest area experiencing each component
#   disturbance at least once during 2000-2020.
#
# For combinations, the component disturbances do
# NOT need to occur in the same year.
# ==================================================


run_mode () {

MODE_NAME="$1"
SPATIAL_DIR="$2"

echo ""
echo ">>> Processing cumulative spatial metrics: ${MODE_NAME}"

CSV_US="${OUT_DIR}/spatial_metrics_west_${MODE_NAME}.csv"
CSV_ECO="${OUT_DIR}/spatial_metrics_ecoregion_${MODE_NAME}.csv"

echo "disturbance,area_km2,percent_forest" > "${CSV_US}"

echo \
"ecoregion_id,disturbance,area_km2,percent_forest_ecoregion,share_of_westwide" \
> "${CSV_ECO}"


python3 << EOF

import rasterio
import numpy as np
import pandas as pd
import os


# --------------------------------------------------
# Configuration
# --------------------------------------------------

SPATIAL_DIR = "${SPATIAL_DIR}"
MODE = "${MODE_NAME}"

FOREST_MASK = "${FOREST_MASK}"
ECO_RASTER = "${ECO_RASTER}"

CSV_US = "${CSV_US}"
CSV_ECO = "${CSV_ECO}"

PIXEL_AREA = ${PIXEL_AREA_KM2}


DIST_TYPES = [
    "wf",
    "bt",
    "hd",
    "pd",

    "wf_bt",
    "wf_hd",
    "wf_pd",
    "bt_hd",
    "bt_pd",

    "wf_bt_hd",
    "wf_bt_pd"
]


# --------------------------------------------------
# Load forest mask
# --------------------------------------------------

with rasterio.open(FOREST_MASK) as src:
    forest = src.read(1) == 1


# --------------------------------------------------
# Load ecoregion raster
# --------------------------------------------------

with rasterio.open(ECO_RASTER) as src:
    eco = src.read(1)
    eco_nodata = src.nodata


# --------------------------------------------------
# Identify valid ecoregions
# --------------------------------------------------

valid = forest & (eco != eco_nodata)

eco_ids = np.unique(eco[valid])


# --------------------------------------------------
# Forest-area denominators
# --------------------------------------------------

forest_px_total = np.count_nonzero(forest)

forest_px_by_eco = {
    eid: np.count_nonzero(
        (eco == eid) & forest
    )
    for eid in eco_ids
}


# --------------------------------------------------
# Loop through disturbance types
# --------------------------------------------------

for dist in DIST_TYPES:

    raster = os.path.join(
        SPATIAL_DIR,
        f"{dist}_{MODE}_presence.tif"
    )

    if not os.path.exists(raster):

        print(
            f"WARNING: Missing {raster}; skipping."
        )

        continue


    print(f"  Processing {dist}")


    # --------------------------------------------------
    # Read spatial-presence raster
    # --------------------------------------------------

    with rasterio.open(raster) as src:
        data = src.read(1) == 1


    # Restrict to forest
    data = data & forest


    # ==================================================
    # WESTWIDE
    # ==================================================

    dist_px = np.count_nonzero(data)

    area_km2 = (
        dist_px * PIXEL_AREA
    )

    pct_forest = (
        dist_px / forest_px_total * 100
        if forest_px_total > 0
        else 0
    )


    with open(CSV_US, "a") as f:

        f.write(
            f"{dist},"
            f"{area_km2:.3f},"
            f"{pct_forest:.2f}\n"
        )


    # ==================================================
    # ECOREGIONS
    # ==================================================

    rows = []


    for eid in eco_ids:

        eco_mask = (
            (eco == eid)
            & forest
        )

        eco_px = forest_px_by_eco[eid]


        if eco_px == 0:
            continue


        dist_px_eco = np.count_nonzero(
            data & eco_mask
        )


        area_km2_eco = (
            dist_px_eco
            * PIXEL_AREA
        )


        pct_eco = (
            dist_px_eco
            / eco_px
            * 100
        )


        share_west = (
            area_km2_eco
            / area_km2
            if area_km2 > 0
            else 0
        )


        rows.append(
            (
                eid,
                dist,
                area_km2_eco,
                pct_eco,
                share_west
            )
        )


    pd.DataFrame(
        rows,
        columns=[
            "ecoregion_id",
            "disturbance",
            "area_km2",
            "percent_forest_ecoregion",
            "share_of_westwide"
        ]
    ).to_csv(
        CSV_ECO,
        mode="a",
        header=False,
        index=False
    )

EOF
}


# --------------------------------------------------
# Run cumulative metrics
# --------------------------------------------------

run_mode \
    "any" \
    "${SPATIAL_ANY_DIR}"

run_mode \
    "extreme" \
    "${SPATIAL_EXT_DIR}"


# ==================================================
# PART 2
# ANNUAL WESTWIDE DISTURBANCE AREA
#
# Annual metrics are calculated directly from the
# annual binary stacks.
#
# Singles:
#   Area experiencing the disturbance in that year.
#
# Doubles:
#   Area experiencing BOTH disturbances in the
#   SAME year.
#
# Triples:
#   Area experiencing ALL THREE disturbances in
#   the SAME year.
#
# Combinations are INCLUSIVE.
#
# Example:
#
# If WF = 1, BT = 1, HD = 1 in a pixel-year,
# that pixel contributes to:
#
#   wf
#   bt
#   hd
#   wf_bt
#   wf_hd
#   bt_hd
#   wf_bt_hd
#
# The Total row is the SUM of annual areas.
#
# Therefore Total represents area-through-time,
# NOT unique cumulative spatial footprint.
# ==================================================

echo ""
echo ">>> Calculating annual westwide disturbance areas"


python3 << EOF

import rasterio
import numpy as np
import pandas as pd
import os


# --------------------------------------------------
# Configuration
# --------------------------------------------------

BIN_DIR = "${BIN_DIR}"
FOREST_MASK = "${FOREST_MASK}"
OUT_DIR = "${OUT_DIR}"

PIXEL_AREA = ${PIXEL_AREA_KM2}

YEARS = range(2000, 2021)

MODES = [
    "any",
    "extreme"
]

DIST_TYPES = [
    "wf",
    "bt",
    "hd",
    "pd",

    "wf_bt",
    "wf_hd",
    "wf_pd",
    "bt_hd",
    "bt_pd",

    "wf_bt_hd",
    "wf_bt_pd"
]


# --------------------------------------------------
# Load forest mask
# --------------------------------------------------

with rasterio.open(FOREST_MASK) as src:
    forest = src.read(1) == 1


# ==================================================
# Process ANY and EXTREME
# ==================================================

for mode in MODES:

    print("")
    print(
        f"Processing annual areas: "
        f"{mode.upper()}"
    )


    rows = []


    # ==================================================
    # Loop through years
    # ==================================================

    for year in YEARS:

        raster = os.path.join(
            BIN_DIR,
            f"annual_stack_{mode}_{year}.tif"
        )


        if not os.path.exists(raster):

            raise FileNotFoundError(
                f"Missing annual raster: "
                f"{raster}"
            )


        print(f"  Year {year}")


        # --------------------------------------------------
        # Read annual binary stack
        # --------------------------------------------------

        with rasterio.open(raster) as src:

            arr = src.read()


        # --------------------------------------------------
        # Extract disturbance bands
        #
        # Band order:
        #
        #   1 = wildfire
        #   2 = biotic
        #   3 = hotter drought
        #   4 = PDSI
        #
        # IMPORTANT:
        # PDSI is stored as "pdsi" here so that "pd"
        # remains the pandas alias.
        # --------------------------------------------------

        wf = (
            (arr[0] > 0)
            & forest
        )

        bt = (
            (arr[1] > 0)
            & forest
        )

        hd = (
            (arr[2] > 0)
            & forest
        )

        pdsi = (
            (arr[3] > 0)
            & forest
        )


        # --------------------------------------------------
        # Same-year disturbance combinations
        # --------------------------------------------------

        disturbances = {

            # Singles
            "wf": wf,
            "bt": bt,
            "hd": hd,
            "pd": pdsi,

            # Doubles
            "wf_bt": (
                wf & bt
            ),

            "wf_hd": (
                wf & hd
            ),

            "wf_pd": (
                wf & pdsi
            ),

            "bt_hd": (
                bt & hd
            ),

            "bt_pd": (
                bt & pdsi
            ),

            # Triples
            "wf_bt_hd": (
                wf & bt & hd
            ),

            "wf_bt_pd": (
                wf & bt & pdsi
            )
        }


        # --------------------------------------------------
        # Calculate annual area
        # --------------------------------------------------

        for dist in DIST_TYPES:

            data = disturbances[dist]

            dist_px = np.count_nonzero(
                data
            )

            area_km2 = (
                dist_px
                * PIXEL_AREA
            )


            rows.append(
                {
                    "period": str(year),
                    "disturbance": dist,
                    "area_km2": area_km2
                }
            )


    # ==================================================
    # Convert annual results to DataFrame
    # ==================================================

    annual = pd.DataFrame(rows)


    # ==================================================
    # Calculate TOTAL
    #
    # Total is the sum of annual areas.
    #
    # Therefore, if the same pixel experiences the
    # disturbance in multiple years, it contributes
    # once for each year.
    # ==================================================

    totals = (
        annual
        .groupby(
            "disturbance",
            as_index=False
        )
        ["area_km2"]
        .sum()
    )


    totals["period"] = "Total"


    # Match column order
    totals = totals[
        [
            "period",
            "disturbance",
            "area_km2"
        ]
    ]


    # ==================================================
    # Combine annual + total
    # ==================================================

    output = pd.concat(
        [
            annual,
            totals
        ],
        ignore_index=True
    )


    # --------------------------------------------------
    # Round area for output
    # --------------------------------------------------

    output["area_km2"] = (
        output["area_km2"]
        .round(3)
    )


    # ==================================================
    # Write CSV
    # ==================================================

    outfile = os.path.join(
        OUT_DIR,
        f"annual_spatial_metrics_west_{mode}.csv"
    )


    output.to_csv(
        outfile,
        index=False
    )


    print("")
    print(
        f"Wrote: {outfile}"
    )

    print(
        f"Rows: {len(output)}"
    )


print("")
print(
    "Annual + total westwide "
    "disturbance metrics complete."
)

EOF


# ==================================================
# PART 3
# VALIDATE TOTALS AGAINST FREQUENCY RASTERS
#
# The frequency rasters count the number of years
# each pixel experienced a disturbance/combination.
#
# Therefore:
#
#   SUM(frequency raster values) * pixel area
#
# should equal:
#
#   SUM(annual disturbed area)
#
# This applies to singles, doubles, and triples.
# ==================================================

echo ""
echo ">>> Validating annual totals against frequency rasters"


python3 << EOF

import rasterio
import numpy as np
import pandas as pd
import os


# --------------------------------------------------
# Configuration
# --------------------------------------------------

FREQ_DIR = "${FREQ_DIR}"
OUT_DIR = "${OUT_DIR}"

PIXEL_AREA = ${PIXEL_AREA_KM2}


MODES = [
    "any",
    "extreme"
]


DIST_TYPES = [
    "wf",
    "bt",
    "hd",
    "pd",

    "wf_bt",
    "wf_hd",
    "wf_pd",
    "bt_hd",
    "bt_pd",

    "wf_bt_hd",
    "wf_bt_pd"
]


# ==================================================
# Validate each mode
# ==================================================

for mode in MODES:

    print("")
    print(
        f"Validation: {mode.upper()}"
    )


    annual_csv = os.path.join(
        OUT_DIR,
        f"annual_spatial_metrics_west_{mode}.csv"
    )


    annual = pd.read_csv(
        annual_csv
    )


    total_rows = (
        annual[
            annual["period"].astype(str)
            == "Total"
        ]
        .set_index("disturbance")
    )


    validation_rows = []


    for dist in DIST_TYPES:

        # --------------------------------------------------
        # Expected frequency raster:
        #
        # data/derived/frequency/any/
        #     wf_any_frequency.tif
        #
        # data/derived/frequency/extreme/
        #     wf_extreme_frequency.tif
        #
        # If the existing frequency rasters use another
        # naming convention, adjust this path only.
        # --------------------------------------------------

        freq_raster = os.path.join(
            FREQ_DIR,
            mode,
            f"{dist}_{mode}_frequency.tif"
        )


        if not os.path.exists(freq_raster):

            print(
                f"  WARNING: Frequency raster not found: "
                f"{freq_raster}"
            )

            continue


        # --------------------------------------------------
        # Read frequency raster
        # --------------------------------------------------

        with rasterio.open(freq_raster) as src:

            freq = src.read(1)
            nodata = src.nodata


        # --------------------------------------------------
        # Exclude NoData
        # --------------------------------------------------

        if nodata is not None:

            valid = (
                freq != nodata
            )

            frequency_sum = np.sum(
                freq[valid],
                dtype=np.int64
            )

        else:

            frequency_sum = np.sum(
                freq,
                dtype=np.int64
            )


        # --------------------------------------------------
        # Convert frequency pixel-years to km2
        # --------------------------------------------------

        frequency_area = (
            frequency_sum
            * PIXEL_AREA
        )


        # --------------------------------------------------
        # Get annual-sum total
        # --------------------------------------------------

        annual_total = float(
            total_rows.loc[
                dist,
                "area_km2"
            ]
        )


        difference = (
            annual_total
            - frequency_area
        )


        matches = np.isclose(
            annual_total,
            frequency_area,
            atol=0.01
        )


        validation_rows.append(
            {
                "disturbance": dist,
                "annual_total_km2": annual_total,
                "frequency_total_km2": frequency_area,
                "difference_km2": difference,
                "matches": matches
            }
        )


        status = (
            "PASS"
            if matches
            else "FAIL"
        )


        print(
            f"  {dist:12s} "
            f"{status} | "
            f"annual = {annual_total:,.3f} km2 | "
            f"frequency = {frequency_area:,.3f} km2"
        )


    # --------------------------------------------------
    # Write validation CSV
    # --------------------------------------------------

    validation = pd.DataFrame(
        validation_rows
    )


    validation_file = os.path.join(
        OUT_DIR,
        f"annual_frequency_validation_{mode}.csv"
    )


    validation.to_csv(
        validation_file,
        index=False
    )


    # --------------------------------------------------
    # Stop if any available comparison fails
    # --------------------------------------------------

    if len(validation) > 0:

        if not validation["matches"].all():

            failed = validation[
                ~validation["matches"]
            ]

            raise ValueError(
                "\nAnnual totals do not match "
                "frequency rasters for:\n"
                + failed.to_string(index=False)
            )


        print(
            f"  All available {mode} "
            f"frequency rasters PASS."
        )


print("")
print(
    "Frequency validation complete."
)

EOF


# ==================================================
# PART 4
# CUMULATIVE ANY VS EXTREME RATIO
#
# This calculation intentionally remains based on
# cumulative spatial presence, NOT annual totals.
# ==================================================

echo ""
echo ">>> Calculating cumulative any vs extreme ratios"


python3 << EOF

import pandas as pd


OUT_DIR = "${OUT_DIR}"


# --------------------------------------------------
# Read cumulative westwide metrics
# --------------------------------------------------

any_us = pd.read_csv(
    f"{OUT_DIR}/spatial_metrics_west_any.csv"
)

ext_us = pd.read_csv(
    f"{OUT_DIR}/spatial_metrics_west_extreme.csv"
)


# --------------------------------------------------
# Read cumulative ecoregion metrics
# --------------------------------------------------

any_eco = pd.read_csv(
    f"{OUT_DIR}/spatial_metrics_ecoregion_any.csv"
)

ext_eco = pd.read_csv(
    f"{OUT_DIR}/spatial_metrics_ecoregion_extreme.csv"
)


# ==================================================
# Westwide ratio
# ==================================================

us_ratio = any_us.merge(
    ext_us,
    on="disturbance",
    suffixes=(
        "_any",
        "_extreme"
    )
)


us_ratio[
    "extreme_to_any_ratio"
] = (
    us_ratio[
        "area_km2_extreme"
    ]
    /
    us_ratio[
        "area_km2_any"
    ]
)


us_ratio[
    "ecoregion"
] = "western_US"


# ==================================================
# Ecoregion ratio
# ==================================================

eco_ratio = any_eco.merge(
    ext_eco,
    on=[
        "ecoregion_id",
        "disturbance"
    ],
    suffixes=(
        "_any",
        "_extreme"
    )
)


eco_ratio[
    "extreme_to_any_ratio"
] = (
    eco_ratio[
        "area_km2_extreme"
    ]
    /
    eco_ratio[
        "area_km2_any"
    ]
)


eco_ratio = eco_ratio.rename(
    columns={
        "ecoregion_id":
        "ecoregion"
    }
)


# ==================================================
# Combine westwide + ecoregion ratios
# ==================================================

ratio = pd.concat(
    [
        us_ratio,
        eco_ratio
    ],
    ignore_index=True
)


ratio.to_csv(
    f"{OUT_DIR}/spatial_metrics_any_vs_extreme_ratio.csv",
    index=False
)

EOF


# ==================================================
# COMPLETE
# ==================================================

echo ""
echo "=================================================="
echo " Spatial metrics COMPLETE"
echo "=================================================="
echo ""
echo "Cumulative spatial-presence outputs:"
echo ""
echo "  ${OUT_DIR}/spatial_metrics_west_any.csv"
echo "  ${OUT_DIR}/spatial_metrics_west_extreme.csv"
echo "  ${OUT_DIR}/spatial_metrics_ecoregion_any.csv"
echo "  ${OUT_DIR}/spatial_metrics_ecoregion_extreme.csv"
echo ""
echo "Annual + total westwide outputs:"
echo ""
echo "  ${OUT_DIR}/annual_spatial_metrics_west_any.csv"
echo "  ${OUT_DIR}/annual_spatial_metrics_west_extreme.csv"
echo ""
echo "Frequency validation outputs:"
echo ""
echo "  ${OUT_DIR}/annual_frequency_validation_any.csv"
echo "  ${OUT_DIR}/annual_frequency_validation_extreme.csv"
echo ""
echo "Cumulative any-vs-extreme ratio:"
echo ""
echo "  ${OUT_DIR}/spatial_metrics_any_vs_extreme_ratio.csv"
echo ""
echo "=================================================="
