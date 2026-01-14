#!/bin/bash
set -e
echo "Starting presence/absence calculation..."

# ----------------------------
# Directories
# ----------------------------
BIN_DIR="data/derived/annual_stacks/binary"
OUT_DIR="data/derived/annual_stacks/spatial_pa"
mkdir -p $OUT_DIR

YEARS=$(seq 2000 2020)

# ----------------------------
# Helper: Build single disturbance PA raster
# ----------------------------
build_pa() {
    local BAND=$1       # 1=wildfire, 2=biotic, 3=hd, 4=pdsi
    local NAME=$2       # output name
    local TEMP=$OUT_DIR/${NAME}_temp.tif
    local TEMP2=$OUT_DIR/${NAME}_temp2.tif

    echo "Processing $NAME..."

    # Step 1: extract correct band from first year
    gdal_translate -b $BAND $BIN_DIR/annual_stack_bin_2000.tif $TEMP

    # Step 2: loop over remaining years and update presence/absence
    for YEAR in $(seq 2001 2020); do
        YEAR_FILE=$BIN_DIR/annual_stack_bin_${YEAR}.tif

        gdal_calc.py \
            -A $TEMP -B $YEAR_FILE \
            --outfile=$TEMP2 \
            --calc="1*((A>0)+(B>0)>0)" \
            --NoDataValue=0 \
            --type=Int16 \
            --co="COMPRESS=DEFLATE" --co="TILED=YES"

        mv $TEMP2 $TEMP
    done

    # Step 3: rename final raster
    mv $TEMP $OUT_DIR/${NAME}.tif
    echo "$NAME done."
}

# ----------------------------
# Singles
# ----------------------------
build_pa 1 wildfire
build_pa 2 biotic
build_pa 3 hd
build_pa 4 pdsi

# ----------------------------
# Pairwise combos (logical AND)
# ----------------------------
echo "Building pairwise combos..."
gdal_calc.py -A $OUT_DIR/wildfire.tif -B $OUT_DIR/biotic.tif \
    --outfile=$OUT_DIR/fire_biotic.tif --calc="A*B" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

gdal_calc.py -A $OUT_DIR/wildfire.tif -B $OUT_DIR/hd.tif \
    --outfile=$OUT_DIR/fire_hd.tif --calc="A*B" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

gdal_calc.py -A $OUT_DIR/wildfire.tif -B $OUT_DIR/pdsi.tif \
    --outfile=$OUT_DIR/fire_pdsi.tif --calc="A*B" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

gdal_calc.py -A $OUT_DIR/biotic.tif -B $OUT_DIR/hd.tif \
    --outfile=$OUT_DIR/biotic_hd.tif --calc="A*B" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

gdal_calc.py -A $OUT_DIR/biotic.tif -B $OUT_DIR/pdsi.tif \
    --outfile=$OUT_DIR/biotic_pdsi.tif --calc="A*B" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

gdal_calc.py -A $OUT_DIR/hd.tif -B $OUT_DIR/pdsi.tif \
    --outfile=$OUT_DIR/hd_pdsi.tif --calc="A*B" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

# ----------------------------
# Triple combos (logical AND)
# ----------------------------
echo "Building triple combos..."
gdal_calc.py -A $OUT_DIR/wildfire.tif -B $OUT_DIR/biotic.tif -C $OUT_DIR/hd.tif \
    --outfile=$OUT_DIR/fire_biotic_hd.tif --calc="A*B*C" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

gdal_calc.py -A $OUT_DIR/wildfire.tif -B $OUT_DIR/biotic.tif -C $OUT_DIR/pdsi.tif \
    --outfile=$OUT_DIR/fire_biotic_pdsi.tif --calc="A*B*C" --NoDataValue=0 --type=Int16 \
    --co="COMPRESS=DEFLATE" --co="TILED=YES"

echo "All presence/absence rasters created in $OUT_DIR"

