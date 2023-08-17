#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: For AsianII clade sensitivty analysis split sequence dataset into little
#chunks to genotype
#* Methods: 
#* 
#******************************************************************************#

#******************************************************************************#
#*Set wd
#*
setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/DENV2/asiantwocladetree")

#*# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
#******************************************************************************#

#******************************************************************************#
#*Read in the complete dataset and split it
denv2_complete <- read.fasta('denv2_global_sequences.fasta')

#Split
denv2_complete_subset1 <- denv2_complete[c(1:1900)]
denv2_complete_subset2 <- denv2_complete[c(1901:3800)]
denv2_complete_subset3 <- denv2_complete[c(3801:5700)]
denv2_complete_subset4 <- denv2_complete[c(5701:7600)]
denv2_complete_subset5 <- denv2_complete[c(7601:9500)]
denv2_complete_subset6 <- denv2_complete[c(9501:11148)]

write.dna(denv2_complete_subset1, 'denv2_global_sequences_subset1.fasta', format = 'fasta')
write.dna(denv2_complete_subset2, 'denv2_global_sequences_subset2.fasta', format = 'fasta')
write.dna(denv2_complete_subset3, 'denv2_global_sequences_subset3.fasta', format = 'fasta')
write.dna(denv2_complete_subset4, 'denv2_global_sequences_subset4.fasta', format = 'fasta')
write.dna(denv2_complete_subset5, 'denv2_global_sequences_subset5.fasta', format = 'fasta')
write.dna(denv2_complete_subset6, 'denv2_global_sequences_subset6.fasta', format = 'fasta')
#******************************************************************************#

#******************************************************************************#