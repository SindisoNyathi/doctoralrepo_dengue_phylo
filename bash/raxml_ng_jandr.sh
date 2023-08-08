#!/bin/bash
#SBATCH --job-name=raxml_ng_tree
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --account=jandr
#SBATCH --time=012:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/raxmlng_tree.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/raxmlng_tree.error

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script processes sets of sequnces and aligns them. 

# ****************************************************************#
# Created: Oct 19. 2022 2021.
# Last Edit: 

 

# ****************************************************************#
# Preliminaries
# Quit if an error occurs. 
set -e

# Load required libraries/programs (fastqc, trimgalore, and bowtie)
module load rxml-ng

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/Data/

raxml-ng --all --msa $1 --model GTR+I+G4 --prefix raxml_ --seed 12345 --threads 32 > /labs/dlabeaud/snyathi_1/phylo_analyses/Results/$2/raxml_ng

