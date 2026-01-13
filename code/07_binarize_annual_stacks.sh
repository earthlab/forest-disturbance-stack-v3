#!/bin/bash
set -euo pipefail

ROOT_DIR="$(pwd)"
STACK_DIR="${ROOT_DIR}/data/derived/annual_stacks"
OUT_DIR="${STACK_DIR}/binary"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}"
mkdir -p "${TMP_DIR}"

for yr in {2000..2020}; do

  INFILE="${STACK_DIR}/annual_stack_${yr}.tif"
  OUTFILE="${OUT_DIR}/annual_stack_bin_${yr}.tif"

  if [[ ! -f "${INFILE}" ]]; then
    echo "WARNING: Missing ${INFILE}, skipping"
    continue
  fi

  echo "Processing ${yr}"

  # ---- Temporary band files ----
  WF="${TMP_DIR}/wf_${yr}.tif"
  BT="${TMP_DIR}/bt_${yr}.tif"
  HD="${TMP_DIR}/hd_${yr}.tif"
  PD="${TMP_DIR}/pd_${yr}.tif"
  VRT="${TMP_DIR}/stack_${yr}.vrt"

  # Wildfire (band 1)
  gdal_calc.py \
    -A "${INFILE}" --A_band=1 \
    --calc="A>0.1" \
    --type=Byte \
    --NoDataValue=0 \
    --co="COMPRESS=DEFLATE" \
    --co="TILED=YES" \
    --outfile="${WF}"

  # Biotic (band 2)
  gdal_calc.py \
    -A "${INFILE}" --A_band=2 \
    --calc="A>=10" \
    --type=Byte \
    --NoDataValue=0 \
    --co="COMPRESS=DEFLATE" \
    --co="TILED=YES" \
    --outfile="${BT}"

  # Hotter drought (band 3)
  gdal_calc.py \
    -A "${INFILE}" --A_band=3 \
    --calc="A>=4" \
    --type=Byte \
    --NoDataValue=0 \
    --co="COMPRESS=DEFLATE" \
    --co="TILED=YES" \
    --outfile="${HD}"

  # PDSI (band 4)
  gdal_calc.py \
    -A "${INFILE}" --A_band=4 \
    --calc="A<=-200" \
    --type=Byte \
    --NoDataValue=0 \
    --co="COMPRESS=DEFLATE" \
    --co="TILED=YES" \
    --outfile="${PD}"

  # ---- Stack into 4-band raster ----
  gdalbuildvrt -separate "${VRT}" "${WF}" "${BT}" "${HD}" "${PD}"

  gdal_translate \
    "${VRT}" \
    "${OUTFILE}" \
    -co COMPRESS=DEFLATE \
    -co TILED=YES

  # ---- Cleanup ----
  rm -f "${WF}" "${BT}" "${HD}" "${PD}" "${VRT}"

done

echo "✅ Annual binary stacks created successfully."

