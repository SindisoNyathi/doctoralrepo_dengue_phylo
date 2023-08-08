#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Read n an alignment and find HAmming distances between sequences. 
#Methods:
# 
#******************************************************************************#

#******************************************************************************#
#*Prelims
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
require(stats)
require(tidyr)
require(Biostrings)
#******************************************************************************#

#******************************************************************************#
#*Read in the alignment
this_alignment_stringset <- DNAStringSet(readDNAMultipleAlignment('DENV_global/city_sequences/nicaragua_aln2.fasta'))

# Read the genotype data.  
nicaragua_seqmetadata <- read.csv('DENV_global/city_metadata/nicaragua_metadata_merged.csv')

#Get types. 
nicaragua_seqmetadata$Types <- paste(nicaragua_seqmetadata$Serotype, nicaragua_seqmetadata$Genotype, sep = '_')
types <- unique(nicaragua_seqmetadata$Types)

# Get names
type1_seq_names <- nicaragua_seqmetadata$Accession[which(nicaragua_seqmetadata$Type == "2_2III")]
type2_seq_names <- nicaragua_seqmetadata$Accession[which(nicaragua_seqmetadata$Type == "1_1V")]
type3_seq_names <- nicaragua_seqmetadata$Accession[which(nicaragua_seqmetadata$Type == "3_3III")]

#Subset by types.
type1_sequences <- this_alignment_stringset[type1_seq_names]
type2_sequences <- this_alignment_stringset[type2_seq_names]
type3_sequences <- this_alignment_stringset[type3_seq_names]

#Get the hamming distances. 
hamming_type1_seqs <- as.matrix(stringDist(type1_sequences, method = "hamming", diag = F, upper = F))
hamming_type2_seqs <- as.matrix(stringDist(type2_sequences, method = "hamming", diag = F, upper = F))
hamming_type3_seqs <- as.matrix(stringDist(type3_sequences, method = "hamming", diag = F, upper = F))

heatmap(hamming_type1_seqs, scale = 'row')
