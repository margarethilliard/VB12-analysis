#!/bin/bash
# =====================================================
# Usage: bash generate_dietML_array_job_scripts.sh
# Purpose: Automatically generate SLURM array scripts for all subsets/metabolite combinations 
# Note: This script assumes you've run 1) scripts/shared_subset_analysis_setup.sh and 2) scripts/generate_dietML_commands.sh already 
# =====================================================

BASE_DIR="/quobyte/dglemaygrp/mhilliard/FL100/taxaHFE/intake_subset_analyses"
SUBSETS=(high_intake low_intake no_supp_use supp_use)
METABOLITES=(acetate propionate butyrate)

# SLURM parameters
PARTITION="low"
ACCOUNT="publicgrp"
NTASKS=1
CPUS_PER_TASK=6
MEM_PER_CPU="4g"
TIME="02:00:00"
ARRAY_JOBS="1-100%5"

for subset in "${SUBSETS[@]}"; do
for metab in "${METABOLITES[@]}"; do

# Where to store each generated array script
SCRIPT_DIR="${BASE_DIR}/${subset}/pathways/${metab}/scripts"
mkdir -p "${SCRIPT_DIR}"

# Commands file for this subset × metabolite
CMD_FILE="${BASE_DIR}/${subset}/pathways/${metab}/commands/${subset}_${metab}_commands.txt"

# Check that commands file exists
if [[ ! -f "${CMD_FILE}" ]]; then
echo "🚨 Commands file not found: ${CMD_FILE}. Skipping..."
continue
fi

# SLURM output directories
STDOUT_DIR="${BASE_DIR}/${subset}/pathways/${metab}/std_out"
mkdir -p "${STDOUT_DIR}"

# Name of SLURM script to generate
ARRAY_SCRIPT="${SCRIPT_DIR}/${subset}_${metab}_array_job.sh"
> "${ARRAY_SCRIPT}"

# Write SLURM header
cat <<EOF >> "${ARRAY_SCRIPT}"
#!/bin/bash
#SBATCH --job-name=${subset}_${metab}_pathways
#SBATCH --output=${STDOUT_DIR}/${subset}_${metab}_pathways_%A_%a.out
#SBATCH --error=${STDOUT_DIR}/${subset}_${metab}_pathways_%A_%a.err
#SBATCH --partition=${PARTITION}
#SBATCH --account=${ACCOUNT}
#SBATCH --ntasks=${NTASKS}
#SBATCH --cpus-per-task=${CPUS_PER_TASK}
#SBATCH --mem-per-cpu=${MEM_PER_CPU}
#SBATCH --time=${TIME}
#SBATCH --array=${ARRAY_JOBS}

# Load modules
module load apptainer/latest

# Pull the command corresponding to this array task
array_cmd=\$( awk "NR==\$SLURM_ARRAY_TASK_ID" ${CMD_FILE} )

# Execute the command
bash -c "\$array_cmd"
EOF

chmod +x "${ARRAY_SCRIPT}"
echo "🦭 Generated SLURM array script: ${ARRAY_SCRIPT}"

done
done

echo "🥹 All SLURM array scripts generated successfully!"