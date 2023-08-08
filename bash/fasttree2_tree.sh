#!/bin/bash
#SBATCH --job-name=fasttree_tree_dg
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --account=default
#SBATCH --partition=interactive
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/raxmlng/fastree_tree_dg.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/raxmlng/fasttree_tree_dg.error

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
module load fasttree

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/Data/

FastTree -gtr -nt -seed 2452562 $1 > $2

