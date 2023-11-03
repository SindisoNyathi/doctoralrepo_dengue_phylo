#******************************************************************************#
#10/19/2022
#Sindiso Nyathi
#Goal: Reduce sequences to a smaller, but
#representative group. 
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
#Set Working Directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/DENV13Phylo")

#Load or install required pacakges. 
#Bioconductor.
require(seqinr)
require(msa)
require(ggplot2)
require(stringr)
require(reshape2)
library(grid)
library(gridExtra)
require(stringi)
require(ape)
#******************************************************************************#

#******************************************************************************#
#Retrieve the list of files to read in.
sequences_to_reduce <- read.table('Sequences/DENV1/SCG/sequences_to_reduce.txt')
no_of_files <- nrow(sequences_to_reduce)

# Keep count of sequences. 
all_reduced_count = 0 

for (i in 1:no_of_files){
  
  #Read in the file of sequences
  filename <- sequences_to_reduce[i,]
  this_aligned_seq_group <- read.FASTA(paste('Sequences/DENV1/SCG/', filename, sep = ""), type = 'DNA')
  
  #Get the number of sequneces
  old_n <- length(this_aligned_seq_group)
  
  #Get the reduction factor (10 (if old_n > 100) or 6(if old_n < 100))
  reduction_factor <- ifelse(old_n < 10, 6,
                             ifelse(old_n < 30, 12, 
                                    ifelse(30 < old_n & old_n < 100, 20, 
                                           ifelse(100 < old_n & old_n < 500, 7, 
                                                  ifelse(500 < old_n & old_n < 1000, 60,
                                                         ifelse(1000 < old_n, 85))))))
  
  #seq group_name
  this_aligned_seq_group_name <- sequences_to_reduce[i,]
  
  #Create the distance matrix
  this_seq_dist_mat <- as.matrix(dist.dna(this_aligned_seq_group, model = 'raw', pairwise.deletion = T))
  
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
  representative_sequences <- this_aligned_seq_group[which(names(this_aligned_seq_group) %in% representative_sequence_names)]
  
  #Get the number of representative sequences
  new_n <- length(representative_sequences)
  
  all_reduced_count <- all_reduced_count + new_n
  
  country <- strsplit(filename, split = '.f')[[1]][1]

  file_name <- paste(country, '_', new_n, '.fasta', sep = '')

  #Save the files as a fasta file
  write.FASTA(representative_sequences, file = paste('Sequences/DENV1/Reduced/', file_name, sep = ''))
}
#Done.  
write.csv(this_seq_dist_mat, 'test.csv')
#******************************************************************************#

#******************************************************************************#
#*"Black is a blind remembering. You listen for pack sounds, for the cries of those 
#*who hunted your ancestors in a past so ancient only your most primitive cells 
#*remember. The ears see. The nostrils see. "
#*The Reverent Mother Jessica Atreides
#*Dune, Paul Herbert. 
#******************************************************************************#

#******************************************************************************#