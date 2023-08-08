#!/bin/bash
#SBATCH --job-name=muscle_align_vlarge_sequence_databases
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --account=default
#SBATCH --time=12:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/mafft/muscle_alignment_d2g.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/mafft/muscle_alignment_d2g.error

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
module load muscle

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/Data/

muscle -in Data/denvg/global_sequences_d2.fasta -out Data/denvg/global_sequences_d2_alignment.fasta -maxiters 2

