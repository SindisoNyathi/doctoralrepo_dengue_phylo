#!/bin/bash
#SBATCH --job-name=mafft_align_large_sequence_databases
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --account=jandr
#SBATCH --time=012:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/mafft_alignment.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/mafft_alignment.error

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script processes sets of sequnces and aligns them. 

# ****************************************************************#
# Created: May 24. 2022 2021.
# Last Edit: June 29, 2022
# Added a line in sbatch commands to redirect logs to specific location
# Input: Single file to align
 

# ****************************************************************#
# Preliminaries
# Quit if an error occurs. 
set -e

# Load required libraries/programs (fastqc, trimgalore, and bowtie)
module load mafft

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/Data/denv2/study_sequences/

mafft --ep 0 --genafpair --maxiterate 1000 --thread 32 $1 > $2

