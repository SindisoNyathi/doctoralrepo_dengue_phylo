#!/bin/bash
#SBATCH --job-name=beast1_run
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --account=jandr
#SBATCH --time=24:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=Logs/beast/%A_%x.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/beast/%A_%x.er

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script takes as input a MSA and then runs modeltest_ng on the file. 

# ****************************************************************#
# Created: May 29. 2022 2021.
# Last Edit: May 29, 2022
# Input: Single xml file 
# Output: Beast output files
 

# ****************************************************************#
# Preliminaries
# Quit if an error occurs. 
set -e

# Load required libraries/programs (fastqc, trimgalore, and bowtie)
module load java
module load BEAGLE

# Set the working directory
cd /labs/dlabeaud/snyathi_1/phylo_analyses/

input_file=$1

java -jar Software/beast/BEASTv1.10.4/lib/beast.jar -threads 32 -seed 12345 Data/$input_file