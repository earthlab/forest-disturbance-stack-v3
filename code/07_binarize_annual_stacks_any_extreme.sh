#!/bin/bash
set -euo pipefail

ROOT_DIR="$(pwd)"
STACK_DIR="${ROOT_DIR}/data/derived/annual_stacks"
OUT_DIR="${STACK_DIR}/binary"
TMP_DIR="${OUT_DIR}/tmp"

mkdir -p "${OUT_DIR}"
mkdir -p "${TMP_DIR}"

# -----------------------------
# Threshold definitions
# -----------------------------

# ANY disturbance thresholds
WF_ANY="(A > 0.1)"
BT_ANY="(A > 0.1)"
HD_ANY="(A >= 4)"
PD_ANY="(A <= -200)"

# EXTREME disturbance thresholds
WF_EXT="(A >= 2.25)"
BT_EXT="(A >= 50)"
HD_EXT="(A >= 6)"
PD_EXT="(A <= -400)"

# -----------------------------
# Loop through years
# -----------------------------
for yr in {2000..2020}; do

  INFILE="${STACK_DIR}/annual_stack_${yr}.tif"

  if [[ ! -f "${INFILE}" ]]; then
    echo "⚠️ Missing ${INFILE}, skipping"
    continue
  fi

  echo "Processing ${yr}"

  for MODE in any extreme; do

    echo "  → Mode: ${MODE}"

    OUTFILE="${OUT_DIR}/annual_stack_${MODE}_${yr}.tif"

    # Skip if already exists (restart-safe)
    if [[ -f "${OUTFILE}" ]]; then
      echo "    ✓ Exists, skipping"
      continue
    fi

    # Select thresholds
    if [[ "${MODE}" == "any" ]]; then
      WF_CALC="${WF_ANY}"
      BT_CALC="${BT_ANY}"
      HD_CALC="${HD_ANY}"
      PD_CALC="${PD_ANY}"
    else
      WF_CALC="${WF_EXT}"
      BT_CALC="${BT_EXT}"
      HD_CALC="${HD_EXT}"
      PD_CALC="${PD_EXT}"
    fi

    # Temporary files
    WF="${TMP_DIR}/wf_${MODE}_${yr}.tif"
    BT="${TMP_DIR}/bt_${MODE}_${yr}.tif"
    HD="${TMP_DIR}/hd_${MODE}_${yr}.tif"
    PD="${TMP_DIR}/pd_${MODE}_${yr}.tif"
    VRT="${TMP_DIR}/stack_${MODE}_${yr}.vrt"

    # -----------------------------
    # Binarization (binary domain, no nodata propagation)
    # -----------------------------

    # --- Wildfire (band 1)
    gdal_calc.py -A "${INFILE}" --A_band=1 \
      --calc="numpy.where(${WF_CALC}, 1, 0)" \
      --type=Byte \
      --NoDataValue=0 \
      --co COMPRESS=DEFLATE --co TILED=YES \
      --outfile="${WF}"

    # --- Biotic (band 2)
    gdal_calc.py -A "${INFILE}" --A_band=2 \
      --calc="numpy.where(${BT_CALC}, 1, 0)" \
      --type=Byte \
      --NoDataValue=0 \
      --co COMPRESS=DEFLATE --co TILED=YES \
      --outfile="${BT}"

    # --- Hotter drought (band 3)
    gdal_calc.py -A "${INFILE}" --A_band=3 \
      --calc="numpy.where(${HD_CALC}, 1, 0)" \
      --type=Byte \
      --NoDataValue=0 \
      --co COMPRESS=DEFLATE --co TILED=YES \
      --outfile="${HD}"

    # --- PDSI (band 4)
    gdal_calc.py -A "${INFILE}" --A_band=4 \
      --calc="numpy.where(${PD_CALC}, 1, 0)" \
      --type=Byte \
      --NoDataValue=0 \
      --co COMPRESS=DEFLATE --co TILED=YES \
      --outfile="${PD}"

    # -----------------------------
    # Stack into 4-band raster
    # -----------------------------
    gdalbuildvrt -separate "${VRT}" "${WF}" "${BT}" "${HD}" "${PD}"

    gdal_translate "${VRT}" "${OUTFILE}" \
      -co COMPRESS=DEFLATE \
      -co TILED=YES \
      -co BIGTIFF=YES

    # -----------------------------
    # Cleanup
    # -----------------------------
    rm -f "${WF}" "${BT}" "${HD}" "${PD}" "${VRT}"

  done
done

echo "✅ ANY and EXTREME annual binary stacks created (binary domain, nodata=0, scientifically consistent)."

