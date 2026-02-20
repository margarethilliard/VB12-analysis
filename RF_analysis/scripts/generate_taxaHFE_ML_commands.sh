#!/bin/bash

# Purpose: generate 12 subset- and metabolite-specific command files that follow your pre-determined hyperparameters and use your list of random seeds 
# Use: bash generate_taxaHFE_ML_commands.sh\
# Expected input files: 
# 1. data/*_metadata.csv. These are subset- and metabolite-specific metadata files that follow the naming pattern: <subset>_<metabolite>_metadata.csv. These files can be generated using scripts/subset_analysis_setup.sh
# 2. modified_merged_metaphlan_v4-0-6_GTDB.txt. Shared microbiome input file used for all models. This version was converted to GTDB taxonomy from the merged metaphlan4 table using this utility script: https://github.com/biobakery/MetaPhlAn/blob/master/metaphlan/utils/sgb_to_gtdb_profile.py
# IMPORTANT NOTE: the GTDB taxa table was further modified by changing the delimiter in the header to '|' to be compliant with taxaHFE-ML's expected delimiter 
# 3. random_seeds.txt. One seed per line to generate reproducible random searches. If you need to generate a list of random seeds, you can use scripts/generate_100_random_seeds.sh
#
# Expected directory structure: 
# 
#intake_subset_analyses/ 
#├── data/ 
#│   ├── high_intake_acetate_metadata.csv
#│   ├── high_intake_butyrate_metadata.csv
#│   ├── high_intake_propionate_metadata.csv
#│   ├── low_intake_acetate_metadata.csv
#│   ├── ...
#│   └── modified_merged_metaphlan_v4-0-6_GTDB.txt  # shared microbiome file
#├── high_intake/
#│   └── microbiome/
#│       ├── acetate/
#│       │   └── commands/  # generated command files go here
#│       ├── butyrate/
#│       │   └── commands/
#│       └── propionate/
#│           └── commands/
#├── low_intake/
#│   └── microbiome/
#│       └── ...
#├── no_supp_use/
#│   └── microbiome/
#│       └── ...
#├── supp_use/
#│   └── microbiome/
#│       └── ...
#└── scripts/
#    └── generate_taxaHFE_ML_commands.sh  # this script
#
# =====================================================
# Base HPC Paths
# =====================================================
BASE_DIR="/quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/intake_subset_analyses"
DATA_DIR="${BASE_DIR}/data"
SIF_PATH="/quobyte/dglemaygrp/aoliver/software/taxahfe_ml_dev.sif"
SEED_FILE="${BASE_DIR}/random_seeds.txt"

# Shared microbiome file (now in the data/ directory)
MICROBIOME="${DATA_DIR}/modified_merged_metaphlan_v4-0-6_GTDB.txt"

# =====================================================
# Shared Model Parameters
# =====================================================
NCORES=6

# =====================================================
# Intake subsets to iterate through
# =====================================================
subsets=(high_intake low_intake no_supp_use supp_use)

# =====================================================
# Define metabolite-specific hyperparameters
# =====================================================
# Format: metabolite:metric:prevalence:abundance:cor:train_split:tune_length:tune_time:tune_stop:folds:nperm
metabolite_params=(
  "acetate:mae:0.20:0.01:0.95:0.75:80:8:15:10:10"
  "propionate:mae:0.30:0.05:0.85:0.80:80:8:15:3:10"
  "butyrate:rmse:0.20:0.05:0.95:0.80:80:2:10:5:10"
)

# =====================================================
# Generate commands for each subset × metabolite
# =====================================================
for subset in "${subsets[@]}"; do
  echo "🦭 Processing subset: ${subset}"

  for metab_info in "${metabolite_params[@]}"; do
    # Split colon-delimited string
    IFS=":" read -r LABEL METRIC PREVALENCE ABUNDANCE COR_LEVEL TRAIN_SPLIT \
                   TUNE_LENGTH TUNE_TIME TUNE_STOP FOLDS NPERM <<< "$metab_info"

    # Build metadata path dynamically (subset + metabolite)
    METADATA="${DATA_DIR}/${subset}_${LABEL}_metadata.csv"

    # Define working directory (e.g., high_intake/microbiome/acetate)
    WORK_DIR="${BASE_DIR}/${subset}/microbiome/${LABEL}"

    # Assign superfilter only to propionate and butyrate
    if [[ "$LABEL" == "propionate" || "$LABEL" == "butyrate" ]]; then
      SUPERFILTER="-d"
    else
      SUPERFILTER=""
    fi

    # Create command output directory
    mkdir -p "${WORK_DIR}/commands"
    OUTPUT_FILE="${WORK_DIR}/commands/${subset}_${LABEL}_commands.txt"
    > "${OUTPUT_FILE}"

    echo "   ➤ Generating ${LABEL} commands for ${subset}..."
    echo "     Using metadata: ${METADATA}"
    echo "     Using shared microbiome file: ${MICROBIOME}"

    # -----------------------------------------------------
    # Build commands for each random seed
    # -----------------------------------------------------
    for seed in $(cat "${SEED_FILE}"); do
      OUTDIR="${WORK_DIR}/output_${seed}"
      CONTAINER_OUTDIR="/data/${subset}/microbiome/${LABEL}/output_${seed}"
      
    # Container-visible paths
    META_IN_CONTAINER="/data/data/$(basename ${METADATA})"
    MICROBIOME_IN_CONTAINER="/data/data/$(basename ${MICROBIOME})"
    
    # Determine the label for the -l argument
    if [[ "${LABEL}" == "butyrate" ]]; then
        CMD_LABEL="new_butyrate"
    else
        CMD_LABEL="${LABEL}"
    fi
    
CMD=$(cat <<EOF
mkdir -p ${OUTDIR} && singularity run --pwd /app --no-home --contain \
--workdir ${WORK_DIR} --bind ${BASE_DIR}:/data ${SIF_PATH} \
${META_IN_CONTAINER} ${MICROBIOME_IN_CONTAINER} -o ${CONTAINER_OUTDIR} \
-s subject_id -l ${CMD_LABEL} -t numeric -L 3 -n ${NCORES} \
--metric ${METRIC} --prevalence ${PREVALENCE} --abundance ${ABUNDANCE} \
-c ${COR_LEVEL} --train_split ${TRAIN_SPLIT} --tune_length ${TUNE_LENGTH} \
--tune_time ${TUNE_TIME} --tune_stop ${TUNE_STOP} --folds ${FOLDS} \
--nperm ${NPERM} --seed ${seed} ${SUPERFILTER}
EOF
)
      echo "${CMD}" >> "${OUTPUT_FILE}"
    done

    echo "  🔥 ${OUTPUT_FILE} created with $(wc -l < "${SEED_FILE}") commands."
  done
done

echo "🥹 All subset × metabolite command files generated successfully!"