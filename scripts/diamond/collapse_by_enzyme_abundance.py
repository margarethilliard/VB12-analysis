#!/usr/bin/env python

'''
collapse_by_enzyme_abundance.py by Chad Masarweh
Created on 20240831
Modified by Margaret Hilliard 20250115

Python script collapses DIAMOND results by enzyme function

header of input diamond results file:
## Gene,subject1,subject2,subject3
header of output diamond results file:  
## subject,enzyme1,enzyme2,enzyme3

To modify the script for use with other databases
change the split symbol used in line 27

'''
import pandas as pd
import argparse

def process_and_sum_data(file_path, output_path, transpose=False):
    # Load the CSV file
    df = pd.read_csv(file_path)
    
    # Extract the enzyme prefix before the "-" symbol and add it as a new column
    df['Enzyme'] = df['Gene'].apply(lambda x: x.split('-')[0])
    
    # Drop the original 'Gene' column
    df.drop(columns=['Gene'], inplace=True)
    
    # Group by the new 'Enzyme' column and sum the other columns
    grouped_df = df.groupby('Enzyme').sum()
    
    # Check if transposition is required
    if transpose:
        grouped_df = grouped_df.T

    # Save the DataFrame to a new CSV file
    grouped_df.to_csv(output_path)
    
    if transpose:
        print(f'Transposed and summed data saved to {output_path}')
    else:
        print(f'Summed data saved to {output_path}')

def main():
    # Create argument parser
    parser = argparse.ArgumentParser(description="Process and sum data by enzyme names.")
    parser.add_argument("input_file", help="Path to the input CSV file.")
    parser.add_argument("output_file", help="Path to the output CSV file.")
    parser.add_argument("-t", "--transpose", action="store_true",
                        help="Transpose the output table before saving.")
    
    # Parse arguments
    args = parser.parse_args()
    
    # Call the processing function with the transpose option
    process_and_sum_data(args.input_file, args.output_file, args.transpose)

if __name__ == "__main__":
    main()
