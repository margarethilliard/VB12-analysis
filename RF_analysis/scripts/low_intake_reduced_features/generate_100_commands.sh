#!/bin/bash

for seed in $(cat random_seeds.txt) 
do
echo "mkdir -p /home/hilliard/low_intake_best_model_reduced_features/output_${seed} && singularity exec --no-home --bind /home/hilliard/low_intake_best_model_reduced_features/:/data /quobyte/dglemaygrp/aoliver/software/nutrition_tools.sif dietML --subject_identifier subject_id --label plasma_b12 --type regression --model rf --ncores 6 --metric rmse --cor_level 0.95 --train_split 0.75 --tune_length 80 --tune_time 2 --tune_stop 10 --folds 5 --seed ${seed} /data/dietML_metadata.csv /data/output_${seed}/ml_results/" >> 100_best_commands.txt
done
