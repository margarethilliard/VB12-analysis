#!/bin/bash

# Define the number of random numbers to generate
num_numbers=100

# Define the minimum and maximum values for a four-digit number
min_num=1000
max_num=9999

# Loop to generate the specified number of random four-digit numbers
for ((i=0; i<num_numbers; i++)); do
  # Generate a random number between min_num and max_num
  random_number=$((min_num + RANDOM % (max_num - min_num + 1)))

  # Print the generated random number
  echo "$random_number"
done