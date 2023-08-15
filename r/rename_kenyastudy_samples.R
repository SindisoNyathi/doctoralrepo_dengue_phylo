#******************************************************************************#
#05/24/2022
#Sindiso Nyathi
#Goal: Correct and format the naming of our study sequences. 

# Notes
# This file runs as is; change only the serotype and run line by line. The code 
# Modifies the format of the sequence name to match the genbak format. 
#******************************************************************************#
#*
#*
#*#******************************************************************************#
#* Preliminaries
require(ape)

setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENV2/study_consensus_genomes")
setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/DENV2/study_consensus_genomes_filtersensitivity")

#Read the cohort data. 
cohort_details <- read.csv("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/Data/denv_cohort.csv")
completecohort <- read.FASTA('denv2_study_cohort_filstersa.fasta', type = 'DNA')
no_samples <- length(completecohort)

for (sample in 1:no_samples){
  
  this_sample_ID <- strsplit(names(completecohort)[[sample]], split = '_')[[1]][2]
  this_sample_year <- cohort_details$Year[cohort_details$SampleID == this_sample_ID]
  this_sample_site <- cohort_details$Site[cohort_details$SampleID == this_sample_ID]

  new_sample_name <- paste(this_sample_ID, " |Kenya: ", this_sample_site, "|Kenya|", this_sample_year, sep = '')
  names(completecohort)[[sample]] <- new_sample_name
  
}
  
write.FASTA(completecohort, file = 'denv2_study_cohort_filstersa.fasta')
