#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: MArge all relevant city level metadata
#Methods: 
# 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
#******************************************************************************#
#*
#******************************************************************************#
nicaragua_seqmetadata <- read.csv('DENV_global/city_metadata/nicaragua.csv')
nicaragua_genotyping <- read.csv('DENV_global/city_metadata/nicaragua_genotyping.csv')

#Merge
nicaragua_seqmetadata_merge <- merge(nicaragua_seqmetadata, nicaragua_genotyping, by.x = 'Accession', by.y = 'name')

#Remove irrelevant columns
nicaragua_seqmetadata_merge <- nicaragua_seqmetadata_merge[,c(1, 4, 5, 6)]
colnames(nicaragua_seqmetadata_merge) <- c('Accession', 'Date', 'Serotype', 'Genotype')

#Save the file. 
write.csv(nicaragua_seqmetadata_merge, 'DENV_global/city_metadata/nicaragua_metadata_merged.csv')
#******************************************************************************#
#*
#******************************************************************************#