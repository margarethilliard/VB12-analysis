#!/bin/bash

total_runs=$(column --table -s',' output_*/ml_analysis/ml_results.csv | grep "^rsq" | awk '{print $3"\t"$6}' | sort -k1,1nr | wc -l)

echo "Total Runs Completed: " ${total_runs}

column --table -s',' output_*/ml_analysis/ml_results.csv | \
grep "^rsq" | awk '{print $3"\t"$6}' | grep -v "e" | sort -k1,1nr | head

