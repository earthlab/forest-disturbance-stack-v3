#!/bin/bash
set -euo pipefail

echo "==============================================="
echo " Spatial presence (ANY vs EXTREME, 2000–2020)"
echo " Project: forest-disturbance-stack-v3"
echo "==============================================="

ROOT_DIR="$(pwd)"
BIN_DIR="${ROOT_DIR}/data/derived/annual_stacks/binary"
OUT_DIR="${BIN_DIR}/spatial_presence"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}" "${TMP_DIR}"

YEARS=$(seq 2000 2020)
MODES=("any" "extreme")

# Annual stack band order
declare -A BAND
BAND[wf]=1
BAND[bt]=2
BAND[hd]=3
BAND[pd]=4

echo "Starting spatial presence calculation..."

for MODE in "${MODES[@]}"; do
    echo "-----------------------------------------------"
    echo "Mode: ${MODE}"
    echo "-----------------------------------------------"

    MODE_OUT="${OUT_DIR}/${MODE}"
    mkdir -p "${MODE_OUT}"

    # -----------------------------
    # Singles (wf, bt, hd, pd)
    # -----------------------------
    for DIST in wf bt hd pd; do
        echo "Processing ${DIST} (${MODE})"

        DIST_TMP="${TMP_DIR}/${DIST}_${MODE}"
        mkdir -p "${DIST_TMP}"

        YEAR_BANDS=()

        # Extract the band for each year
        for YR in ${YEARS}; do
            INFILE="${BIN_DIR}/annual_stack_${MODE}_${YR}.tif"
            [[ ! -f "${INFILE}" ]] && continue

            OUTBAND="${DIST_TMP}/${DIST}_${MODE}_${YR}.tif"

            gdal_translate \
                -b "${BAND[$DIST]}" \
                -of GTiff \
                -co COMPRESS=DEFLATE \
                -co TILED=YES \
                "${INFILE}" \
                "${OUTBAND}"

            YEAR_BANDS+=("${OUTBAND}")
        done

        [[ ${#YEAR_BANDS[@]} -eq 0 ]] && continue

        # Build VRT with one band per year
        VRT_FILE="${TMP_DIR}/${DIST}_${MODE}.vrt"
        gdalbuildvrt -separate "${VRT_FILE}" "${YEAR_BANDS[@]}"

        # Collapse across years (ANY occurrence)
        OUTFILE="${MODE_OUT}/${DIST}_${MODE}_presence.tif"

        gdal_calc.py \
            -A "${VRT_FILE}" \
            --calc="numpy.where(numpy.any(A == 1, axis=0), 1, 0).astype(numpy.uint8)" \
            --type=Byte \
            --NoDataValue=-9999 \
            --outfile="${OUTFILE}" \
            --overwrite \
            --co="COMPRESS=DEFLATE" \
            --co="TILED=YES"
    done

    # -----------------------------
    # Double combinations
    # -----------------------------
    PAIRS=("wf,bt" "wf,hd" "wf,pd" "bt,hd" "bt,pd")
    for PAIR in "${PAIRS[@]}"; do
        IFS=',' read -r D1 D2 <<< "${PAIR}"
        OUTFILE="${MODE_OUT}/${D1}_${D2}_${MODE}_presence.tif"

        gdal_calc.py \
            -A "${MODE_OUT}/${D1}_${MODE}_presence.tif" \
            -B "${MODE_OUT}/${D2}_${MODE}_presence.tif" \
            --calc="numpy.where((A == 1) & (B == 1), 1, 0).astype(numpy.uint8)" \
            --type=Byte \
            --NoDataValue=-9999 \
            --outfile="${OUTFILE}" \
            --overwrite \
            --co="COMPRESS=DEFLATE" \
            --co="TILED=YES"
    done

    # -----------------------------
    # Triple combinations
    # -----------------------------
    TRIPLES=("wf,bt,hd" "wf,bt,pd")
    for TRIP in "${TRIPLES[@]}"; do
        IFS=',' read -r D1 D2 D3 <<< "${TRIP}"
        OUTFILE="${MODE_OUT}/${D1}_${D2}_${D3}_${MODE}_presence.tif"

        gdal_calc.py \
            -A "${MODE_OUT}/${D1}_${MODE}_presence.tif" \
            -B "${MODE_OUT}/${D2}_${MODE}_presence.tif" \
            -C "${MODE_OUT}/${D3}_${MODE}_presence.tif" \
            --calc="numpy.where((A == 1) & (B == 1) & (C == 1), 1, 0).astype(numpy.uint8)" \
            --type=Byte \
            --NoDataValue=-9999 \
            --outfile="${OUTFILE}" \
            --overwrite \
            --co="COMPRESS=DEFLATE" \
            --co="TILED=YES"
    done
done

# Cleanup
rm -rf "${TMP_DIR}"

echo "==============================================="
echo " ✅ Spatial presence rasters created correctly"
echo "==============================================="

