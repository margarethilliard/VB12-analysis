#!/bin/bash

workdir=/share/lemaylab/mhilliard/FL100/diamond_B12
db=/share/lemaylab-backedup/mhilliard/B12_database/database/final_db.faa
diamond=/share/lemaylab/mhilliard/software/diamond

cd ${workdir}
mkdir -p database scripts diamond_out

## make diamond formatted database 
${diamond} makedb --in $db -d $workdir/database/diamond_B12

while read subject frags mc; do

## make scripts
echo "#!/bin/bash

cd ${workdir}

## make sure the output file doesn't already exist 
if [ -f diamond_output/${subject}.txt ] 
then 
echo "diamond_output/${subject}.txt already exists and will not be overwritten..."
else
## run diamond using merged reads & convert output to .tsv format

${diamond} blastx --db database/B12_db_diamond.dmnd -q ${frags} -o diamond_output/${subject}.daa --sensitive --evalue 1e-25 -k 1 -f 100 --threads 12 --quiet
${diamond} view --daa diamond_output/${subject}.daa -o diamond_output/${subject}.txt -f 6
fi
" > scripts/script_${subject}.sh

done < path_file.txt

## run using parallel, for example: 
parallel --dryrun -j 4 ::: ./scripts/*.sh # remove --dryrun and be mindful of core usage on shared resources 
