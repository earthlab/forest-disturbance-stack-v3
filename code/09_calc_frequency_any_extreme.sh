#!/bin/bash
set -euo pipefail

ROOT_DIR="$(pwd)"
BIN_DIR="${ROOT_DIR}/data/derived/annual_stacks/binary"
OUT_DIR="${BIN_DIR}/frequency"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}" "${TMP_DIR}"

YEARS=$(seq 2000 2020)
MODES=("any" "extreme")

echo "Starting frequency calculation..."

# ---- single, double, and triple combinations ----
SINGLES=(wf bt hd pd)
DOUBLES=(wf_bt wf_hd bt_hd wf_pd bt_pd)   # remove hd_pd
TRIPLES=(wf_bt_hd wf_bt_pd)               # remove wf_hd_pd and bt_hd_pd

for MODE in "${MODES[@]}"; do
  echo "Mode: ${MODE}"

  # Initialize cumulative frequency rasters with zeros
  for VAR in "${SINGLES[@]}" "${DOUBLES[@]}" "${TRIPLES[@]}"; do
    gdal_calc.py \
      -A "${BIN_DIR}/annual_stack_${MODE}_2000.tif" --A_band=1 \
      --calc="0" \
      --type=Int16 \
      --NoDataValue=0 \
      --overwrite \
      --outfile="${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
      --co="COMPRESS=DEFLATE" --co="TILED=YES"
  done

  # Loop through each year
  for YR in ${YEARS}; do
    echo "  Year ${YR}"
    IN="${BIN_DIR}/annual_stack_${MODE}_${YR}.tif"

    [[ ! -f "$IN" ]] && continue

    # Extract bands for this year
    WF="${TMP_DIR}/wf_${YR}.tif"
    BT="${TMP_DIR}/bt_${YR}.tif"
    HD="${TMP_DIR}/hd_${YR}.tif"
    PD="${TMP_DIR}/pd_${YR}.tif"

    gdal_translate -q -b 1 "$IN" "$WF"
    gdal_translate -q -b 2 "$IN" "$BT"
    gdal_translate -q -b 3 "$IN" "$HD"
    gdal_translate -q -b 4 "$IN" "$PD"

    # ---- singles ----
    for VAR in "${SINGLES[@]}"; do
      gdal_calc.py \
        -A "${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
        -B "${TMP_DIR}/${VAR}_${YR}.tif" \
        --calc="A+B" \
        --type=Int16 \
        --NoDataValue=0 \
        --overwrite \
        --outfile="${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
        --co="COMPRESS=DEFLATE" --co="TILED=YES"
    done

    # ---- doubles (co-occurrence per year) ----
    for KEY in "${DOUBLES[@]}"; do
      case $KEY in
        wf_bt) A="$WF"; B="$BT";;
        wf_hd) A="$WF"; B="$HD";;
        bt_hd) A="$BT"; B="$HD";;
        wf_pd) A="$WF"; B="$PD";;
        bt_pd) A="$BT"; B="$PD";;
      esac

      # co-occurrence in this year
      gdal_calc.py -A "$A" -B "$B" --calc="A*B" \
        --type=Byte --NoDataValue=0 --overwrite \
        --outfile="${TMP_DIR}/pair.tif" \
        --co="COMPRESS=DEFLATE" --co="TILED=YES"

      # add to cumulative frequency
      gdal_calc.py -A "${TMP_DIR}/${KEY}_${MODE}_freq.tif" -B "${TMP_DIR}/pair.tif" \
        --calc="A+B" --type=Int16 --NoDataValue=0 --overwrite \
        --outfile="${TMP_DIR}/${KEY}_${MODE}_freq.tif" \
        --co="COMPRESS=DEFLATE" --co="TILED=YES"
    done

    # ---- triples (co-occurrence per year) ----
    # wf_bt_hd
    gdal_calc.py -A "$WF" -B "$BT" -C "$HD" --calc="A*B*C" --type=Byte --NoDataValue=0 --overwrite \
      --outfile="${TMP_DIR}/trip.tif" --co="COMPRESS=DEFLATE" --co="TILED=YES"
    gdal_calc.py -A "${TMP_DIR}/wf_bt_hd_${MODE}_freq.tif" -B "${TMP_DIR}/trip.tif" \
      --calc="A+B" --type=Int16 --NoDataValue=0 --overwrite \
      --outfile="${TMP_DIR}/wf_bt_hd_${MODE}_freq.tif" \
      --co="COMPRESS=DEFLATE" --co="TILED=YES"

    # wf_bt_pd
    gdal_calc.py -A "$WF" -B "$BT" -C "$PD" --calc="A*B*C" --type=Byte --NoDataValue=0 --overwrite \
      --outfile="${TMP_DIR}/trip.tif" --co="COMPRESS=DEFLATE" --co="TILED=YES"
    gdal_calc.py -A "${TMP_DIR}/wf_bt_pd_${MODE}_freq.tif" -B "${TMP_DIR}/trip.tif" \
      --calc="A+B" --type=Int16 --NoDataValue=0 --overwrite \
      --outfile="${TMP_DIR}/wf_bt_pd_${MODE}_freq.tif" \
      --co="COMPRESS=DEFLATE" --co="TILED=YES"

    # clean up temp files for this year
    rm -f "$WF" "$BT" "$HD" "$PD" "${TMP_DIR}/pair.tif" "${TMP_DIR}/trip.tif"
  done

  # ---- move outputs to final directory ----
  for VAR in "${SINGLES[@]}" "${DOUBLES[@]}" "${TRIPLES[@]}"; do
    mv "${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
       "${OUT_DIR}/${VAR}_${MODE}_frequency.tif"
  done

done

rm -rf "${TMP_DIR}"
echo "✅ Frequency rasters (singles, doubles, triples) created successfully."

