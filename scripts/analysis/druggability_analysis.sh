#!/bin/bash

# Author: Yitan Lu

# Description:
# This script calculates druggability scores for proteins using fpocket, based on their UniprotIDs.
# Requirements:
# - fpocket must be installed and accessible from the command line.
# - This script is designed to run on a Linux system.
# - Input text files (like uniprotID.txt) should be in Linux format (LF line endings). You can use tools like 'dos2unix' to convert if necessary.
# - To execute the script, place it in the same directory as your .txt files, and then run with './druggability_analysis.sh'.

# 1. Get UniprotIDs for all gene symbols
# Assuming all UniprotIDs have been stored in uniprotID.txt

# 2. Fetch structures from AlphaFold database
for i in $(cat uniprotID.txt)
do 
    mkdir ${i}
    wget -q -N -O ./${i}/${i}.pdb https://alphafold.ebi.ac.uk/files/AF-${i}-F1-model_v1.pdb
done

# 3. Calculate druggability using fpocket
for i in $(cat uniprotID.txt)
do 
    cd ./${i}
    fpocket -f ${i}.pdb
    cd ..
done

# 4. Batch extract computation results to the 'info' folder
mkdir info
for i in $(cat uniprotID.txt)
do 
    cp ./${i}/${i}_out/${i}_info.txt ./info 2>/dev/null
done

# 5. Extract Druggability Scores from info files
cd info
for i in $(cat ../uniprotID.txt)
do 
    awk -F: '$1~/D/' ${i}_info.txt > ${i}.txt 2>/dev/null
done

# 6. Extract the maximum druggability score for each protein to ds_score.txt
for i in $(cat ../uniprotID.txt)
do 
    awk 'BEGIN{max = 0} {if ($4 > max) max = $4} END{print max}' ${i}.txt >> ds_score.txt 2>/dev/null
done

echo "Druggability calculation completed"
