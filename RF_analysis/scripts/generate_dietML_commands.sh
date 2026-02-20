#!/bin/bash

# Purpose: generate 12 subset- and metabolite-specific command files that follow your pre-determined hyperparameters and use your list of random seeds 
# Use: bash generate_dietML_commands.sh
# Expected input files: 
# 1. data/*_metadata.csv. These are subset- and metabolite-specific metadata files that follow the naming pattern: <subset>_<metabolite>_pathway_metadata.csv. These files can be generated using scripts/shared_subset_analysis_setup.sh
# 2. random_seeds.txt. One seed per line to generate reproducible random searches. If you need to generate a list of random seeds, you can use scripts/generate_100_random_seeds.sh
# Change log: 17Nov25, modified generate_taxaHFE_ML_commands.sh to generate dietML commands which mostly involved changing the shared- and hyper-parameters as well as some arguments

# Expected directory structure: 
# 
#intake_subset_analyses/ 
#├── data/ 
#│   ├── high_intake_acetate_pathway_metadata.csv
#│   ├── high_intake_butyrate_pathway_metadata.csv
#│   ├── high_intake_propionate_pathway_metadata.csv
#│   ├── low_intake_acetate_pathway_metadata.csv
#│   └── ...
#├── high_intake/
#│   └── pathways/
#│       ├── acetate/
#│       │   └── commands/  # commands executed in generate_array_job_scripts.sh go here 
#│       ├── butyrate/
#│       │   └── commands/
#│       └── propionate/
#│           └── commands/
#├── low_intake/
#│   └── pathways/
#│       └── ...
#├── no_supp_use/
#│   └── pathways/
#│       └── ...
#├── supp_use/
#│   └── pathways/
#│       └── ...
#└── scripts/
#    └── generate_dietML_commands.sh  # this script
#
#
# =====================================================
# Base HPC Paths
# =====================================================
BASE_DIR="/quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/intake_subset_analyses"
DATA_DIR="${BASE_DIR}/data"
SIF_PATH="/quobyte/dglemaygrp/aoliver/software/nutrition_tools.sif"
SEED_FILE="${BASE_DIR}/random_seeds.txt"

# =====================================================
# Shared Model Parameters
# =====================================================
NCORES=4
SUBJECT_IDENTIFYER="subject_id"
TYPE="regression"
MODEL="rf"
# =====================================================
# Intake subsets to iterate through
# =====================================================
subsets=(high_intake low_intake no_supp_use supp_use)

# =====================================================
# Define metabolite-specific hyperparameters -- pull these from hyper_param_tune/
# =====================================================
# Format: metabolite:metric:cor:train_split:tune_length:tune_time:tune_stop:folds
metabolite_params=(
  "acetate:rsq:0.95:0.8:160:5:5:10"
  "propionate:rsq:0.85:0.8:240:8:10:5"
  "butyrate:rsq:0.75:0.8:80:8:5:5"
  )
  
# =====================================================
# Generate commands for each subset × metabolite
# =====================================================
for subset in "${subsets[@]}"; do
  echo "🦭 Processing subset: ${subset}"

  for metab_info in "${metabolite_params[@]}"; do
    # Split colon-delimited string
    IFS=":" read -r LABEL METRIC COR_LEVEL TRAIN_SPLIT \
                   TUNE_LENGTH TUNE_TIME TUNE_STOP FOLDS <<< "$metab_info"

    # Build metadata path dynamically (subset + metabolite)
    METADATA="${DATA_DIR}/${subset}_${LABEL}_pathway_metadata.csv"

    # Define working directory (e.g., high_intake/microbiome/acetate)
    WORK_DIR="${BASE_DIR}/${subset}/pathways/${LABEL}"

    # Create command output directory
    mkdir -p "${WORK_DIR}/commands"
    OUTPUT_FILE="${WORK_DIR}/commands/${subset}_${LABEL}_commands.txt"
    > "${OUTPUT_FILE}"

    echo "   ➤ Generating ${LABEL} commands for ${subset}..."
    echo "     Using metadata file: ${METADATA}"

    # -----------------------------------------------------
    # Build commands for each random seed
    # -----------------------------------------------------
    for seed in $(cat "${SEED_FILE}"); do
      OUTDIR="${WORK_DIR}/output_${seed}"
      CONTAINER_OUTDIR="/data/${subset}/pathways/${LABEL}/output_${seed}"
      
    # Container-visible paths
    META_IN_CONTAINER="/data/data/$(basename ${METADATA})"
    
    # Determine the label for the -l argument
    if [[ "${LABEL}" == "butyrate" ]]; then
        CMD_LABEL="new_butyrate"
    else
        CMD_LABEL="${LABEL}"
    fi
    
CMD=$(cat <<EOF
mkdir -p ${OUTDIR} && singularity exec --no-home --bind ${BASE_DIR}:/data ${SIF_PATH} \
dietML --subject_identifier ${SUBJECT_IDENTIFYER} --label ${CMD_LABEL} --type ${TYPE} --model ${MODEL} --ncores ${NCORES} \
--metric ${METRIC} --cor_level ${COR_LEVEL} --train_split ${TRAIN_SPLIT} --tune_length ${TUNE_LENGTH} --tune_time ${TUNE_TIME} \
--tune_stop ${TUNE_STOP} --folds ${FOLDS} --seed ${seed} \
${META_IN_CONTAINER} ${CONTAINER_OUTDIR}/ml_results/
EOF
)
      echo "${CMD}" >> "${OUTPUT_FILE}"
    done

    echo "  🔥 ${OUTPUT_FILE} created with $(wc -l < "${SEED_FILE}") commands."
  done
done

echo "🥹 All subset × metabolite command files generated successfully!"