#!/bin/bash
#SBATCH --job-name=denv_bioinf_sample
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --partition=interactive
#SBATCH --account=default
#SBATCH --time=02:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --output=/labs/dlabeaud/snyathi_1/analytic_pipeline/Logs/fasttree/fastreebycountry_array_single.%A.%a.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/analytic_pipeline/Logs/fasttree/fastreebycountry_array_single.%A.%a.err

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script processes a single fasta alignment into a tree. 
# ****************************************************************#
 

# ****************************************************************#
# Preliminaries

# Load required libraries/programs (fastqc, trimgalore, and bowtie)
module load Fastree

# Quit if an error occurs. 
set -e

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/Data/

# Set the sample ID and reference. 
# Retrieve the current sample ids and invidiaul read file ids.
# IDs
this_country_fasta=$(awk -v "ArrayTaskID=$1" '{if (NR == ArrayTaskID) print $1}' DENV_global/country_treetime_filenames_align.txt)

# ****************************************************************#

# Complete a single analysis for each pair of reads in the file. 

# Step 0: Find read 1 and read II file names. 
# Note use of the $ to save the output of a command.
echo Sample Start Point
echo $this_country_fasta

FastTree -gtr -nt -seed 2452562 -OMP_NUM_THREADS 3 $this_country_fasta > $this_country_fasta'_tree'

echo Sample_End_Point