#!/bin/bash
#SBATCH --job-name=denv_treetime_country
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --partition=interactive
#SBATCH --account=default
#SBATCH --time=02:00:00
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --output=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/treetime/treetimebycountry.%A.%a.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/phylo_analyses/Logs/treetime/treetimebycountry.%A.%a.err

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# ****************************************************************#
 

# ****************************************************************#
# Preliminaries

# Load required libraries/programs (fastqc, trimgalore, and bowtie)
module load miniconda/3
module load phylo-treetime

# Quit if an error occurs. 
set -e

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/phylo_analyses/

country_tree_file=$(awk -v "ArrayTaskID=$1" '{if (NR == ArrayTaskID) print $1}' DENV_global/country_treetime_filenames_align.txt)
country_date_file=$(awk -v "ArrayTaskID=$1" '{if (NR == ArrayTaskID) print $1}' raw_runsets/country_treetime_filenames_dates.txt)

treetime --tree $country_tree_file'_tree' --dates $country_date_file --outdir $country_tree_file'_timed.nwk'

echo "Sample_End_Point "
