#!/bin/bash
#SBATCH --job-name=denv_treetime
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=interactive
#SBATCH --array=[1-95]%5
#SBATCH --account=default
#SBATCH --time=00:10:00
#SBATCH --mail-type ALL
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/treetime/treetimebycountry_%a.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/treetime/treetimebycountry_%a.err

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.

# ****************************************************************#
# Created: Sept. 30, 2022
# Input:Text File containing the names of the alignemnt files with the sequences. 

# ****************************************************************#
# Preliminaries
# Progress message
echo Creating timed trees given countries inthe file
echo This is array task number $SLURM_ARRAY_TASK_ID

# Quit if an error occurs. 
set -e

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/ 

echo Working directory loaded

# Complete a single analysis for each pair of reads in the file. 
sbatch Scripts/treetimebycountry_array_single.sh $SLURM_ARRAY_TASK_ID

echo "Done"

# Done with master script.