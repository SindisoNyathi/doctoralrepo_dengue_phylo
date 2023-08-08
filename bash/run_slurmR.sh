#!/bin/bash
#SBATCH --job-name=treestrure_tree
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --account=default
#SBATCH --partition=interactive
#SBATCH --time=12:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/treestruct/treestruct_denv2.log
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/treestruct/treestruct_denv2.err

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script processes sets of sequnces and aligns them. 

# ****************************************************************#
# Quit if an error occurs. 
set -e
module load R

cd /labs/dlabeaud/snyathi_1/phylo_analyses/Scripts

Rscript --vanilla --verbose slurmR_treestructure.R

