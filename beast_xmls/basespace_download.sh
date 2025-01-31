#!/bin/bash
#SBATCH --job-name=download_from_basespace
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=interactive
#SBATCH --account=default
#SBATCH --time=5:00:00
# Sindiso Nyathi, Desiree LaBeaud, Jason Andrews, Shannon Bennet, Izabella Rezende, Panpim Thongsripon, Katie Walter, Renu Verma
# Dengue Evolution Project Analytic Pipeline.
# This script downloads fastq files corresponding to a given project from basespace, based on supplied id
# ****************************************************************#
# Created: May. 7. 2022
# Most Recent Edit: May. 7. 2022
# Input: Project Name
# ****************************************************************#
# Doawnload the diectory
$HOME/bin/bs download project -i 295144295  -o /labs/dlabeaud/snyathi_1/analytic_pipeline/raw_runsets/oct1_2021/ --extension=fastq.gz
~                                                                                                                                       