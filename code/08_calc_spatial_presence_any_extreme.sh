#!/bin/bash
set -euo pipefail

ROOT_DIR="$(pwd)"
BIN_DIR="${ROOT_DIR}/data/derived/annual_stacks/binary"
OUT_DIR="${BIN_DIR}/spatial_presence"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}"
mkdir -p "${TMP_DIR}"

YEARS=$(seq 2000 2020)
MODES=("any" "extreme")

echo "Starting spatial presence calculation..."

for MODE in "${MODES[@]}"; do
  echo "Mode: ${MODE}"

  # ----------------------------------
  # Initialize empty presence rasters
  # ----------------------------------

  INIT_YEAR=2000
  INIT_FILE="${BIN_DIR}/annual_stack_${MODE}_${INIT_YEAR}.tif"

  # Singles
  for BAND in 1 2 3 4; do
    gdal_calc.py -A "${INIT_FILE}" --A_band=${BAND} \
      --calc="0" \
      --type=Byte --NoDataValue=0 \
      --outfile="${TMP_DIR}/single_${MODE}_${BAND}.tif"
  done

  # Doubles (pairs)
  for PAIR in wf_bt wf_hd bt_hd wf_pd bt_pd; do
    gdal_calc.py -A "${INIT_FILE}" --A_band=1 \
      --calc="0" \
      --type=Byte --NoDataValue=0 \
      --outfile="${TMP_DIR}/${PAIR}_${MODE}.tif"
  done

  # Triples
  for TRIP in wf_bt_hd wf_bt_pd; do
    gdal_calc.py -A "${INIT_FILE}" --A_band=1 \
      --calc="0" \
      --type=Byte --NoDataValue=0 \
      --outfile="${TMP_DIR}/${TRIP}_${MODE}.tif"
  done

  # ----------------------------------
  # Loop through years
  # ----------------------------------

  for YR in ${YEARS}; do
    echo "  Processing ${YR}"

    INFILE="${BIN_DIR}/annual_stack_${MODE}_${YR}.tif"

    # Extract bands
    WF="${TMP_DIR}/wf_${YR}.tif"
    BT="${TMP_DIR}/bt_${YR}.tif"
    HD="${TMP_DIR}/hd_${YR}.tif"
    PD="${TMP_DIR}/pd_${YR}.tif"

    gdal_translate -b 1 "${INFILE}" "${WF}"
    gdal_translate -b 2 "${INFILE}" "${BT}"
    gdal_translate -b 3 "${INFILE}" "${HD}"
    gdal_translate -b 4 "${INFILE}" "${PD}"

    # ---- Singles (OR over years)
    for BAND in wf bt hd pd; do
      gdal_calc.py \
        -A "${TMP_DIR}/single_${MODE}_$(
          [[ ${BAND} == wf ]] && echo 1 ||
          [[ ${BAND} == bt ]] && echo 2 ||
          [[ ${BAND} == hd ]] && echo 3 ||
          echo 4
        ).tif" \
        -B "${TMP_DIR}/${BAND}_${YR}.tif" \
        --calc="maximum(A,B)" \
        --type=Byte --NoDataValue=0 \
        --outfile="${TMP_DIR}/tmp_single.tif"

      mv "${TMP_DIR}/tmp_single.tif" \
         "${TMP_DIR}/single_${MODE}_$(
          [[ ${BAND} == wf ]] && echo 1 ||
          [[ ${BAND} == bt ]] && echo 2 ||
          [[ ${BAND} == hd ]] && echo 3 ||
          echo 4
        ).tif"
    done

    # ---- Doubles (AND per year, OR over years)

    gdal_calc.py -A "${WF}" -B "${BT}" --calc="A*B" --type=Byte \
      --outfile="${TMP_DIR}/tmp_pair.tif"
    gdal_calc.py -A "${TMP_DIR}/wf_bt_${MODE}.tif" -B "${TMP_DIR}/tmp_pair.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/wf_bt_${MODE}.tif"

    gdal_calc.py -A "${WF}" -B "${HD}" --calc="A*B" --type=Byte \
      --outfile="${TMP_DIR}/tmp_pair.tif"
    gdal_calc.py -A "${TMP_DIR}/wf_hd_${MODE}.tif" -B "${TMP_DIR}/tmp_pair.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/wf_hd_${MODE}.tif"

    gdal_calc.py -A "${BT}" -B "${HD}" --calc="A*B" --type=Byte \
      --outfile="${TMP_DIR}/tmp_pair.tif"
    gdal_calc.py -A "${TMP_DIR}/bt_hd_${MODE}.tif" -B "${TMP_DIR}/tmp_pair.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/bt_hd_${MODE}.tif"

    gdal_calc.py -A "${WF}" -B "${PD}" --calc="A*B" --type=Byte \
      --outfile="${TMP_DIR}/tmp_pair.tif"
    gdal_calc.py -A "${TMP_DIR}/wf_pd_${MODE}.tif" -B "${TMP_DIR}/tmp_pair.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/wf_pd_${MODE}.tif"

    gdal_calc.py -A "${BT}" -B "${PD}" --calc="A*B" --type=Byte \
      --outfile="${TMP_DIR}/tmp_pair.tif"
    gdal_calc.py -A "${TMP_DIR}/bt_pd_${MODE}.tif" -B "${TMP_DIR}/tmp_pair.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/bt_pd_${MODE}.tif"

    # ---- Triples

    gdal_calc.py -A "${WF}" -B "${BT}" -C "${HD}" \
      --calc="A*B*C" --type=Byte \
      --outfile="${TMP_DIR}/tmp_trip.tif"
    gdal_calc.py -A "${TMP_DIR}/wf_bt_hd_${MODE}.tif" -B "${TMP_DIR}/tmp_trip.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/wf_bt_hd_${MODE}.tif"

    gdal_calc.py -A "${WF}" -B "${BT}" -C "${PD}" \
      --calc="A*B*C" --type=Byte \
      --outfile="${TMP_DIR}/tmp_trip.tif"
    gdal_calc.py -A "${TMP_DIR}/wf_bt_pd_${MODE}.tif" -B "${TMP_DIR}/tmp_trip.tif" \
      --calc="maximum(A,B)" --type=Byte \
      --outfile="${TMP_DIR}/wf_bt_pd_${MODE}.tif"

    rm -f "${WF}" "${BT}" "${HD}" "${PD}" \
          "${TMP_DIR}/tmp_pair.tif" "${TMP_DIR}/tmp_trip.tif"

  done

  # ----------------------------------
  # Finalize outputs
  # ----------------------------------

  mv "${TMP_DIR}/single_${MODE}_1.tif" "${OUT_DIR}/wildfire_${MODE}_presence.tif"
  mv "${TMP_DIR}/single_${MODE}_2.tif" "${OUT_DIR}/biotic_${MODE}_presence.tif"
  mv "${TMP_DIR}/single_${MODE}_3.tif" "${OUT_DIR}/hd_${MODE}_presence.tif"
  mv "${TMP_DIR}/single_${MODE}_4.tif" "${OUT_DIR}/pdsi_${MODE}_presence.tif"

  mv "${TMP_DIR}/wf_bt_${MODE}.tif"     "${OUT_DIR}/wf_bt_${MODE}_presence.tif"
  mv "${TMP_DIR}/wf_hd_${MODE}.tif"     "${OUT_DIR}/wf_hd_${MODE}_presence.tif"
  mv "${TMP_DIR}/bt_hd_${MODE}.tif"     "${OUT_DIR}/bt_hd_${MODE}_presence.tif"
  mv "${TMP_DIR}/wf_pd_${MODE}.tif"     "${OUT_DIR}/wf_pd_${MODE}_presence.tif"
  mv "${TMP_DIR}/bt_pd_${MODE}.tif"     "${OUT_DIR}/bt_pd_${MODE}_presence.tif"

  mv "${TMP_DIR}/wf_bt_hd_${MODE}.tif"  "${OUT_DIR}/wf_bt_hd_${MODE}_presence.tif"
  mv "${TMP_DIR}/wf_bt_pd_${MODE}.tif"  "${OUT_DIR}/wf_bt_pd_${MODE}_presence.tif"

done

rm -rf "${TMP_DIR}"

echo "✅ Spatial presence rasters created successfully."

