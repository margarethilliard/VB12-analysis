
# Purpose: to map contigs (ouput from MEGAHIT-1.2.9) using Bowtie2 for single-sample binning of FL100 metagenomes on HIVE using Metabat2
# I've also done some MAG/bin QC at the end using CheckM2, then an annotation step so the MAGs can be used with the anvio's suite of programs
# See here for pre-processing steps: https://github.com/dglemay/ARG_metagenome/blob/master/master_pipeline/ARG_pipeline_v0.3.sh

# Here are some helpful anvi'o tutorials that I used to get started: 
# https://merenlab.org/tutorials/assembly-based-metagenomics/
# https://merenlab.org/2016/06/22/anvio-tutorial-v2/#take-a-look-at-your-fasta-file
# https://anvio.org/tutorials/fmt-mag-metabolism/#metabolism-estimation-and-enrichment-on-a-real-world-dataset

# A note on resource usage: My lab mates were really chill abou tme using 80 of our dedicated cores for ~1-2 weeks while I put this together. 
# You can (and should) use more or less resources based on what's available to you! 

# make a dedicated work directory and navigate there 
mkdir -p /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping && cd /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping

# list subject ids and the location of their contigs and cleaned reads

cat /quobyte/dglemaygrp/BACKED-UP/FL100/FL100_output_file_paths_hive.tsv | cut -f1,4,8,9 >> FL100_bowtie_input_paths.txt

######## example format (tab delimited, no header):
#subject1  /path/to/contigs.fa  /path/to/cleaned/read1.fastq.gz  /path/to/cleaned/read2.fastq.gz
#subject2  /path/to/contigs.fa  /path/to/cleaned/read1.fastq.gz  /path/to/cleaned/read2.fastq.gz
#subject1  /path/to/contigs.fa  /path/to/cleaned/read1.fastq.gz  /path/to/cleaned/read2.fastq.gz
########################

cat FL100_bowtie_input_paths.txt | cut -f1 >> FL100_subject_ids.txt

######## example format (no header):
#subject1
#subject2
#subject3
########################

# load software on HIVE 
module load conda/latest 
conda activate /quobyte/dglemaygrp/aoliver/software/anvio8 # Thanks, Andrew! :) 
module load bowtie2/2.5.2
module load samtools/1.19.2
module load parallel/20220522 

# create output directories
mkdir tmp_contigs fixed_contigs report_files bowtie2_indexes sams 

# clean contigs so anvi'o likes the deflines -- note that the contigs can't be gzipped during this step! 
while read -r sample contig read1 read2; do
    # clear tmp variable, just in case  
    tmp_contig=""
    
    # if contigs fasta is gzipped, unzip it to a local tmp directory
    if [[ "$contig" == *.gz ]]; then
        tmp_contig="tmp_contigs/${sample}_tmp_contigs.fa"
        gunzip -c "$contig" > "$tmp_contig"
        input_fasta="$tmp_contig"
    else
        input_fasta="$contig"
    fi

    # if contigs aren't gzipped, run as you normally would 
    anvi-script-reformat-fasta "$input_fasta" \
        -o "fixed_contigs/${sample}_contigs_fixed.fa" \
        -l 200 \
        --simplify-names \
        --report-file "report_files/${sample}_contig_rename_report.txt"
        
    # clean up temporary file if it was created
    if [[ -n "$tmp_contig" && -f "$tmp_contig" ]]; then
        rm -f "$tmp_contig"
    fi
done < FL100_bowtie_input_paths.txt

# remove the local tmp directory
rm -r tmp_contigs

# build sample-wise indexes for contigs in parallel: https://merenlab.org/2016/06/22/anvio-tutorial-v2/#anvi-init-bam
cat FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 10 '
  bowtie2-build --threads 8 fixed_contigs/{1}_contigs_fixed.fa bowtie2_indexes/{1}_index 
'

# align sample reads to contigs in parallel
cat FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 10 '
  bowtie2 --threads 8 -x bowtie2_indexes/{1}_index -1 {3} -2 {4} -S sams/{1}_alignment.sam
'

# compress alignment results in parallel
cat FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 10 '
  samtools view --threads 8 -b sams/{1}_alignment.sam > sams/{1}_alignment-RAW.bam
'

# convert the alignments to an anvi'o-friendly format in parallel
cat FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 10 '
  anvi-init-bam sams/{1}_alignment-RAW.bam -o sams/{1}.bam -T 8
'

# remove intermediate files 
rm sams/*_alignment.sam sams/*_alignment-RAW.bam

conda deactivate 

# make a dedicated work directory for MAG reconstruction outputs and navigate there 
mkdir -p /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/ && cd /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/

# make output directories and load software to cluster contigs using metabat2
mkdir depth_files raw_bins

# run once: conda create --name metabat2
conda activate metabat2
# run once: conda install metabat2

# generate read depth file from sorted bam files in parallel
cat /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 20 '
  jgi_summarize_bam_contig_depths --outputDepth /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/depth_files/{1}_depth.txt /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/sams/{1}.bam
'

# gzip your contigs (required input format for metabat2)
module load pigz/2.8

cat /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 10 '
  pigz --fast -p 8 /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/fixed_contigs/{1}_contigs_fixed.fa
'

# bin contigs using metabat2: https://bitbucket.org/berkeleylab/metabat/src/v2.18/README.md
cat /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/FL100_bowtie_input_paths.txt | parallel --colsep '\t' -j 10 '
  metabat2 -i /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/fixed_contigs/{1}_contigs_fixed.fa.gz -a /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/depth_files/{1}_depth.txt -o /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/raw_bins/{1}/{1} --numThreads 8 --seed 8675309
'

conda deactivate
conda activate /quobyte/dglemaygrp/aoliver/software/anvio8

# reformat bin deflines for use with anvi'o
mkdir -p reformatted_bins

while read -r subject_id; do
    bin_info="/quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/raw_bins/${subject_id}/${subject_id}.BinInfo.txt"

    tail -n +2 "$bin_info" \
    | cut -f1,5 \
    | parallel --colsep '\t' -j 64 \
        "anvi-script-reformat-fasta {2} --simplify-names --output-file reformatted_bins/${subject_id}.{1}.fa"

done < /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/bowtie2_mapping/FL100_subject_ids.txt

# get a list of bin names 
cd reformatted_bins 
find . -type f -name "*.fa" >> ../tmp_reformatted_bins.txt 
cd ..
# take off the './'' preceeding the bin names 
sed -i 's|^\./||' tmp_reformatted_bins.txt 

# compress the fasta files because the next set of programs can work with compressed files :)
cat tmp_reformatted_bins.txt | parallel -j 10 'pigz --fast -p 8 /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/reformatted_bins/{}'

rm tmp_reformatted_bins.txt

# archive raw bins from metabat2 
tar czf raw_bins.tar.gz raw_bins
# to un-compress: tar xf metabat2_bins.tar.gz

conda deactivate

# use checkM2 to estimate completion and contamination before running any annotation steps: https://github.com/chklovski/CheckM2
# run once: conda create --name chekm2 -c bioconda -c conda-forge checkm2
conda activate chekm2

mkdir checkm2_database

checkm2 database --download --path checkm_database

# optional, run a testrun to verify everything works: checkm2 testrun 
checkm2 predict --threads 80 -x fa.gz --input reformatted_bins --output-directory checkm2_output --database_path /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/checkm2_database/CheckM2_database/uniref100.KO.1.dmnd

conda deactivate

######## Filter out MAGs with <=50% completion & <10% contamination (locally in R):
#table <- readr::read_delim("/Users/local-margaret/Downloads/quality_report.tsv")
#
#filtered_table <- table %>%
#  dplyr::filter(Completeness >= 50) %>%
#  dplyr::filter(Contamination < 10) %>%
#  dplyr::pull(Name) 

#writeLines(filtered_table, "/Users/local-margaret/Downloads/final_MAG_list.txt")
########################

# move the filtered subset of bins to a new directory 
mkdir final_bins

for i in $(cat final_MAG_list.txt)
do
cp reformatted_bins/${i}.gz final_bins/
done 

# archive the older versions of MAGs/bins you don't plan to use
tar czf reformatted_bins.tar.gz reformatted_bins
# to un-compress: tar xf reformatted_bins.tar.gz

conda activate /quobyte/dglemaygrp/aoliver/software/anvio8

# generate contig_db files for each of the final bins
mkdir contigs_db

find final_bins/ -type f -name "*.fa.gz" | sort | \
parallel -j 10 '
    bin_path="{}"
    bin_base=$(basename "$bin_path" .fa.gz)

    anvi-gen-contigs-database \
        --contigs-fasta "$bin_path" \
        --output-db-path "/quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction/contigs_db/${bin_base}.db" \
        --project-name "$bin_base" \
        --num-threads 8
'

# download a complete anaerobic B12 synthesis module, in leiu of making one yourself (https://anvio.org/help/8/programs/anvi-setup-user-modules/) 
# someone wiser than me has already made the custom module, so I'm using it: https://github.com/ivagljiva/custom_biosynthesis_modules
mkdir /quobyte/dglemaygrp/mhilliard/FL100/modules && cd /quobyte/dglemaygrp/mhilliard/FL100/modules # required directory structure/naming convention by anvi'o
wget -O B12_01 "https://raw.githubusercontent.com/ivagljiva/custom_biosynthesis_modules/main/USER_MODULES/modules/B12_01"
cd .. 

# format the custom user module for use with anvi'o
# this program will look for the "modules/" directory and produce the key output file "USER_MODULES.db"
anvi-setup-user-modules --user-modules /quobyte/dglemaygrp/mhilliard/FL100 --kegg-data-dir /quobyte/dglemaygrp/mhilliard/estimate-metabolism/kegg_data

# generate an external genomes text file: https://anvio.org/help/8/programs/anvi-script-gen-genomes-file/
anvi-script-gen-genomes-file --input-dir /quobyte/dglemaygrp/mhilliard/FL100/MAG_reconstruction/contigs_db \
                             --output-file /quobyte/dglemaygrp/mhilliard/FL100/MAG_reconstruction/external_genomes.txt

# run once to get KEGG data: anvi-setup-kegg-kofams --kegg-data-dir /quobyte/dglemaygrp/mhilliard/estimate-metabolism/kegg_data/ 

# annotate the contigs db files before running anvi-estimate-metabolism: https://anvio.org/help/7/programs/anvi-estimate-metabolism/
# outputs are stored in the contigs db files themselves. note that this operation is computationally intensive and takes awhile. 
cd /quobyte/dglemaygrp/mhilliard/FL100/FL100_MAGs/MAG_reconstruction

find contigs_db/ -type f -name "*.db" | sort | \
parallel --joblog kofams_resume.log -j 10 '
    base=$(basename "{}" .db)
    anvi-run-kegg-kofams -c "{}" -T 8 --kegg-data-dir /quobyte/dglemaygrp/mhilliard/estimate-metabolism/kegg_data/
'

# if the operation times out, you can use this command to resume it:
# find contigs_db/ -type f -name "*.db" | sort | \
# parallel --resume --joblog kofams_resume.log -j 8 '
#    base=$(basename "{}" .db)
#    anvi-run-kegg-kofams -c "{}" -T 8 --kegg-data-dir /quobyte/dglemaygrp/mhilliard/estimate-metabolism/kegg_data/
#'

mkdir /quobyte/dglemaygrp/mhilliard/FL100/estimate-metabolism-output

# edit the "name"" column in the external genomes file -- anvi'o is picky about names starting with numbers... 
# eg: 5001.16 ==> MAG_5001_16, but don't modify the header :) 
awk 'BEGIN{OFS="\t"} 
NR==1 {print; next} 
{ gsub(/\./,"_",$1); $1="MAG_"$1; print }' MAG_reconstruction/external_genomes.txt > MAG_reconstruction/external_genomes_v2.txt

# run anvi-estimate-metabolism using the external genomes file and the custom user module: https://anvio.org/help/7/programs/anvi-estimate-metabolism/
anvi-estimate-metabolism -e MAG_reconstruction/external_genomes_v2.txt \
   -u /quobyte/dglemaygrp/mhilliard/FL100/ \
   --only-user-modules \
   --matrix-format \
   --include-metadata \
   --include-zeros \
   -O /quobyte/dglemaygrp/mhilliard/FL100/estimate-metabolism-output/B12_biosynth 
