#!/bin/bash
#SBATCH --job-name=denv3_align_genbank_sequences
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --account=jandr
#SBATCH --time=01:30:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --array=1-15%5
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/mafft/aligned.%A_%a.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/mafft/aligned.%A_%a.error

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script processes sets of sequnces downloaded from genbank and aligns them. 

# ****************************************************************#
# Created: May 24. 2022 2021.
# Last Edit: May 24, 2022
# Input: Single file with names of files to process. 
 

# ****************************************************************#
# Preliminaries
# Quit if an error occurs. 
set -e

# Load required libraries/programs (fastqc, trimgalore, and bowtie)
module load mafft

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/Data/denv3/genbank_sequences/Original

# Retrieve the name of the sequence group to align in this iteration
LINE=$(sed -n "$SLURM_ARRAY_TASK_ID"p sequences_to_reduce.txt)

echo $LINE

mafft --thread ${SLURM_CPUS_PER_TASK} --auto $LINE > /labs/dlabeaud/snyathi_1/phylo_analyses/Data/denv3/gen_bank_sequences/Aligned/aligned_$LINE

