#*****************************************************************************#
#05/09/2023
#Sindiso Nyathi
#Goal: Examine sequence data fro global dengue.
#Methods: Reduce files for inclusion. Details. 
#Methods: 
#1. The file will read a group of aligned sequences in a fasta file. 
#3. Create a similarity matrix using the Bioconuctor pacakge, MSA2dist
#4. Based on the similarity matrix, reduce the sequences available to a fraction of 
#of their original number, based on the size, i.e. 0 - 20:no reduction; 20 - 30: 30% , 30 - 100: 1/6, 100 - 500: 1/10: > 500: 1/15 
#5. Include the new N in the file name
#6. Save the reduced files. 

#Edits. 
#June 28
#1. Include the new number of sequences in the file name

# Oct 18. 
# Correct an error in an order of commands that was resulting in some error. 

# Notes 
# Run as is, just change the working direcotory and run the file, amake sure you have all the alingments, and the text file 
# with the sequence sets you need to reduce. 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/DENV13Phylo")

# Packages
require(seqinr)
require(Biostrings)
require(tidyverse)
require(lubridate)
require(ape)
require(rnaturalearth)
library(tmap)
library(tmaptools)
require(msaR)
require(muscle)
#******************************************************************************#

#******************************************************************************#
denv1_countries <- list.files('Sequences/denv1/SCG/')

for (filename in denv1_countries) {
  
  #Read in the file. 
  print(paste('Working on ', filename, sep = ""))
  
  #this_file <- read.FASTA(paste('Sequences/denv1/', filename, sep = ''), type = 'DNA')
  this_file <- readDNAStringSe(paste('Sequences/denv1/', filename, sep = ''), format = "fasta")
  
  #Get the number of sequneces
  old_n <- length(this_file)
  
  #For DENV3 skip if these african groups as they are important. 
  #if (file  == 'Tanzania') next
    
  #Skip if less than n sequences. 
  if (old_n < 20) {   
  
  file_name <- paste('reduced_n', old_n, '_', filename, sep = "")
  
  #Save the files as a fasta file
  this_file_aligned_strset <- DNAStringSet(this_file)
  write.fasta(this_file_aligned_strset, names = names(this_file_aligned_strset), file = file_name) 
  next 
  }
  
  #Align this file in muscle. 
  this_file_aligned <- muscle(this_file)
  
  #Get the reduction factor (10 (if old_n > 100) or 6(if old_n < 100))
  reduction_factor <- ifelse(old_n < 30, 4, 
                             ifelse(31 < old_n & old_n < 100, 8, 
                                    ifelse(100 < old_n & old_n < 500, 15, 
                                           ifelse(500 < old_n, 20))))
  
  
  #Create the distance matrix
  this_file_aligned_dnabin <- as.DNAbin(this_file_aligned)
  this_seq_dist_mat <- as.matrix(dist.dna(this_file_aligned_dnabin, model = 'raw'))
  
  #Create the clusters based on distance matrices.
  this_seq_hc_obj <- hclust(as.dist(this_seq_dist_mat))
  
  #Cut the tree in such a way that you have k groups.
  this_seq_hc_obj_cuttree <- cutree(this_seq_hc_obj,  k = round(old_n/reduction_factor))
  this_seq_hc_obj_cuttree <- as.data.frame(this_seq_hc_obj_cuttree)
  this_seq_hc_obj_cuttree[,2] <- rownames(this_seq_hc_obj_cuttree)
  colnames(this_seq_hc_obj_cuttree) <- c('Group', 'ID')
  
  #Retrieve the k representative sequence names
  representative_sequence_names <- this_seq_hc_obj_cuttree$ID[match(unique(this_seq_hc_obj_cuttree$Group), this_seq_hc_obj_cuttree$Group)]
  
  #Retrieve the representative sequences.
  this_file_aligned_strset <- DNAStringSet(this_file_aligned)
  representative_sequences <- this_file_aligned_strset[names(this_file_aligned_strset) %in% representative_sequence_names]
  
  #Get the number of representative sequences
  new_n <- length(representative_sequences)
  
  file_name <- paste('reduced_n', new_n, '_', filename, sep = "")
  
  #Save the files as a fasta file
  write.fasta(representative_sequences, names = names(representative_sequences), file = file_name)
}
#******************************************************************************#

#******************************************************************************#
