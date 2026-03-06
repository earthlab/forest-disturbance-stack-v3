#!/bin/bash
set -euo pipefail

ROOT_DIR="$(pwd)"
BIN_DIR="${ROOT_DIR}/data/derived/annual_stacks/binary"
OUT_BASE="${BIN_DIR}/frequency"
TMP_DIR="${OUT_BASE}/tmp"

mkdir -p "${TMP_DIR}"

YEARS=$(seq 2000 2020)
MODES=("any" "extreme")

echo "Starting frequency calculation..."

SINGLES=(wf bt hd pd)
DOUBLES=(wf_bt wf_hd bt_hd wf_pd bt_pd)
TRIPLES=(wf_bt_hd wf_bt_pd)

FREQ_NODATA=-9999

for MODE in "${MODES[@]}"; do
  echo "Mode: ${MODE}"

  OUT_DIR="${OUT_BASE}/${MODE}"
  mkdir -p "${OUT_DIR}"

  # Initialize cumulative rasters
  for VAR in "${SINGLES[@]}" "${DOUBLES[@]}" "${TRIPLES[@]}"; do
    gdal_calc.py \
      -A "${BIN_DIR}/annual_stack_${MODE}_2000.tif" --A_band=1 \
      --calc="0" \
      --type=Int16 \
      --NoDataValue="${FREQ_NODATA}" \
      --overwrite \
      --outfile="${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
      --co="COMPRESS=DEFLATE" \
      --co="TILED=YES"
  done

  for YR in ${YEARS}; do
    echo "  Year ${YR}"
    IN="${BIN_DIR}/annual_stack_${MODE}_${YR}.tif"
    [[ ! -f "$IN" ]] && continue

    WF="${TMP_DIR}/wf_${YR}.tif"
    BT="${TMP_DIR}/bt_${YR}.tif"
    HD="${TMP_DIR}/hd_${YR}.tif"
    PD="${TMP_DIR}/pd_${YR}.tif"

    # Extract yearly bands as plain valid 0/1 rasters
    gdal_translate -q -b 1 -ot Byte "$IN" "$WF"
    gdal_translate -q -b 2 -ot Byte "$IN" "$BT"
    gdal_translate -q -b 3 -ot Byte "$IN" "$HD"
    gdal_translate -q -b 4 -ot Byte "$IN" "$PD"

    # ---- singles ----
    for VAR in "${SINGLES[@]}"; do
      NEXT="${TMP_DIR}/${VAR}_${MODE}_next.tif"

      gdal_calc.py \
        -A "${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
        -B "${TMP_DIR}/${VAR}_${YR}.tif" \
        --calc="numpy.where(A==${FREQ_NODATA},0,A) + B" \
        --type=Int16 \
        --NoDataValue="${FREQ_NODATA}" \
        --overwrite \
        --outfile="${NEXT}" \
        --co="COMPRESS=DEFLATE" \
        --co="TILED=YES"

      mv "${NEXT}" "${TMP_DIR}/${VAR}_${MODE}_freq.tif"
    done

    # ---- doubles ----
    for KEY in "${DOUBLES[@]}"; do
      case $KEY in
        wf_bt) A="$WF"; B="$BT" ;;
        wf_hd) A="$WF"; B="$HD" ;;
        bt_hd) A="$BT"; B="$HD" ;;
        wf_pd) A="$WF"; B="$PD" ;;
        bt_pd) A="$BT"; B="$PD" ;;
      esac

      gdal_calc.py \
        -A "$A" -B "$B" \
        --calc="A*B" \
        --type=Byte \
        --NoDataValue=0 \
        --overwrite \
        --outfile="${TMP_DIR}/pair.tif" \
        --co="COMPRESS=DEFLATE" \
        --co="TILED=YES"

      NEXT="${TMP_DIR}/${KEY}_${MODE}_next.tif"

      gdal_calc.py \
        -A "${TMP_DIR}/${KEY}_${MODE}_freq.tif" \
        -B "${TMP_DIR}/pair.tif" \
        --calc="numpy.where(A==${FREQ_NODATA},0,A) + B" \
        --type=Int16 \
        --NoDataValue="${FREQ_NODATA}" \
        --overwrite \
        --outfile="${NEXT}" \
        --co="COMPRESS=DEFLATE" \
        --co="TILED=YES"

      mv "${NEXT}" "${TMP_DIR}/${KEY}_${MODE}_freq.tif"
    done

    # ---- triples ----
    gdal_calc.py \
      -A "$WF" -B "$BT" -C "$HD" \
      --calc="A*B*C" \
      --type=Byte \
      --NoDataValue=0 \
      --overwrite \
      --outfile="${TMP_DIR}/trip.tif" \
      --co="COMPRESS=DEFLATE" \
      --co="TILED=YES"

    NEXT="${TMP_DIR}/wf_bt_hd_${MODE}_next.tif"

    gdal_calc.py \
      -A "${TMP_DIR}/wf_bt_hd_${MODE}_freq.tif" \
      -B "${TMP_DIR}/trip.tif" \
      --calc="numpy.where(A==${FREQ_NODATA},0,A) + B" \
      --type=Int16 \
      --NoDataValue="${FREQ_NODATA}" \
      --overwrite \
      --outfile="${NEXT}" \
      --co="COMPRESS=DEFLATE" \
      --co="TILED=YES"

    mv "${NEXT}" "${TMP_DIR}/wf_bt_hd_${MODE}_freq.tif"

    gdal_calc.py \
      -A "$WF" -B "$BT" -C "$PD" \
      --calc="A*B*C" \
      --type=Byte \
      --NoDataValue=0 \
      --overwrite \
      --outfile="${TMP_DIR}/trip.tif" \
      --co="COMPRESS=DEFLATE" \
      --co="TILED=YES"

    NEXT="${TMP_DIR}/wf_bt_pd_${MODE}_next.tif"

    gdal_calc.py \
      -A "${TMP_DIR}/wf_bt_pd_${MODE}_freq.tif" \
      -B "${TMP_DIR}/trip.tif" \
      --calc="numpy.where(A==${FREQ_NODATA},0,A) + B" \
      --type=Int16 \
      --NoDataValue="${FREQ_NODATA}" \
      --overwrite \
      --outfile="${NEXT}" \
      --co="COMPRESS=DEFLATE" \
      --co="TILED=YES"

    mv "${NEXT}" "${TMP_DIR}/wf_bt_pd_${MODE}_freq.tif"

    rm -f "$WF" "$BT" "$HD" "$PD" "${TMP_DIR}/pair.tif" "${TMP_DIR}/trip.tif"
  done

  # Move outputs to final mode-specific directory
  for VAR in "${SINGLES[@]}" "${DOUBLES[@]}" "${TRIPLES[@]}"; do
    mv "${TMP_DIR}/${VAR}_${MODE}_freq.tif" \
       "${OUT_DIR}/${VAR}_${MODE}_frequency.tif"
  done
done

rm -rf "${TMP_DIR}"

echo "✅ Frequency rasters created successfully."
echo "   any     → data/derived/annual_stacks/binary/frequency/any/"
echo "   extreme → data/derived/annual_stacks/binary/frequency/extreme/"
