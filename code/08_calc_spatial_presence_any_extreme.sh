#!/bin/bash
set -euo pipefail

ROOT_DIR="$(pwd)"
BIN_DIR="${ROOT_DIR}/data/derived/annual_stacks/binary"
OUT_DIR="${BIN_DIR}/spatial_presence"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}" "${TMP_DIR}"

YEARS=$(seq 2000 2020)
MODES=("any" "extreme")

echo "Starting spatial presence calculation..."

# ---- disturbance band indices ----
# wf = wildfire, bt = biotic, hd = hotter drought, pd = pdsi
declare -A BAND
BAND[wf]=1
BAND[bt]=2
BAND[hd]=3
BAND[pd]=4

for MODE in "${MODES[@]}"; do
    echo "Mode: ${MODE}"
    MODE_OUT="${OUT_DIR}/${MODE}"
    mkdir -p "${MODE_OUT}"

    ########################################
    # Create VRTs for each disturbance across all years
    ########################################
    for DIST in wf bt hd pd; do
        FILE_LIST=()
        for YR in ${YEARS}; do
            INFILE="${BIN_DIR}/annual_stack_${MODE}_${YR}.tif"
            [[ ! -f "${INFILE}" ]] && continue
            FILE_LIST+=("${INFILE}")
        done

        if [ ${#FILE_LIST[@]} -eq 0 ]; then
            echo "No files found for ${DIST} ${MODE}, skipping"
            continue
        fi

        VRT_FILE="${TMP_DIR}/${DIST}_${MODE}.vrt"
        echo "Building VRT for ${DIST}..."
        gdalbuildvrt -separate -b ${BAND[$DIST]} "${VRT_FILE}" "${FILE_LIST[@]}"
    done

    ########################################
    # Compute single disturbance presence (any year)
    ########################################
    for DIST in wf bt hd pd; do
        VRT_FILE="${TMP_DIR}/${DIST}_${MODE}.vrt"
        OUTFILE="${MODE_OUT}/${DIST}_${MODE}_presence.tif"
        echo "Computing presence raster: ${DIST}"
        gdal_calc.py \
            -A "${VRT_FILE}" \
            --calc="numpy.max(A, axis=0)" \
            --type=Byte \
            --NoDataValue=0 \
            --outfile="${OUTFILE}" \
            --overwrite \
            --co="COMPRESS=DEFLATE" --co="TILED=YES"
    done

    ########################################
    # Compute double combinations 
    ########################################
    PAIRS=("wf,bt" "wf,hd" "wf,pd" "bt,hd" "bt,pd")
    for PAIR in "${PAIRS[@]}"; do
        IFS=',' read -r D1 D2 <<< "$PAIR"
        OUTFILE="${MODE_OUT}/${D1}_${D2}_${MODE}_presence.tif"
        echo "Computing double combination: ${D1}_${D2}"
        gdal_calc.py \
            -A "${MODE_OUT}/${D1}_${MODE}_presence.tif" \
            -B "${MODE_OUT}/${D2}_${MODE}_presence.tif" \
            --calc="A*B" \
            --type=Byte \
            --NoDataValue=0 \
            --outfile="${OUTFILE}" \
            --overwrite \
            --co="COMPRESS=DEFLATE" --co="TILED=YES"
    done

    ########################################
    # Compute triple combinations 
    ########################################
    TRIPLES=("wf,bt,hd" "wf,bt,pd")
    for TRIP in "${TRIPLES[@]}"; do
        IFS=',' read -r D1 D2 D3 <<< "$TRIP"
        OUTFILE="${MODE_OUT}/${D1}_${D2}_${D3}_${MODE}_presence.tif"
        echo "Computing triple combination: ${D1}_${D2}_${D3}"
        gdal_calc.py \
            -A "${MODE_OUT}/${D1}_${MODE}_presence.tif" \
            -B "${MODE_OUT}/${D2}_${MODE}_presence.tif" \
            -C "${MODE_OUT}/${D3}_${MODE}_presence.tif" \
            --calc="A*B*C" \
            --type=Byte \
            --NoDataValue=0 \
            --outfile="${OUTFILE}" \
            --overwrite \
            --co="COMPRESS=DEFLATE" --co="TILED=YES"
    done

done

rm -rf "${TMP_DIR}"
echo "✅ Spatial presence rasters created successfully."

