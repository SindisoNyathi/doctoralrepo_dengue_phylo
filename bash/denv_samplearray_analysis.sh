#!/bin/bash
#SBATCH --job-name=denv_bioinformatics
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=interactive
#SBATCH --array=[1-103]%10
#SBATCH --account=default
#SBATCH --time=02:00:00
#SBATCH --mail-type ALL
#SBATCH --mail-user snyathi@stanford.edu
#SBATCH --output=/labs/dlabeaud/snyathi_1/analytic_pipeline/Logs/denv_bioinf_master_%a.out
#SBATCH --error=/labs/dlabeaud/snyathi_1/analytic_pipeline/Logs/denv_bioinf_master_%a.err

# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script processes demultiplexed run out put data generated by base space, using a custom pipeline based on iVar (Grubaugh et al)
# The script reads in Read1.gzip and Read2.gzip files for N samples, and outputs various intermediate files.  
# Raw files are read in from a raw_runsets_x folder, where X is a group of raw runs for the given project of sequencing run. This is ht emaster script
# the uses an array to call each file and run it seperately.

# ****************************************************************#
# Created: Sept. 30, 2022
# Input:Text File containing the sample IDs as well as a second text file containing the serotypes of samples in the same order as the samples. 
 
# ****************************************************************#
# Preliminaries
# Progress message
echo This is array task number $SLURM_ARRAY_TASK_ID

# Quit if an error occurs. 
set -e

# Set the working directory. Working Directory is the main directory for LaBeaud Lab. 
cd /labs/dlabeaud/snyathi_1/analytic_pipeline/ 
echo Working directory loaded

# Complete a single analysis for each pair of reads in the file. 
sbatch Scripts/denv_singlesample_analysis_intrahost.sh $SLURM_ARRAY_TASK_ID

echo "Done"

# Done with master script.