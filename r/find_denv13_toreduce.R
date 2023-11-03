#******************************************************************************#
#10/19/2022
#Sindiso Nyathi
#Goal: Reduce sequences to a smaller, but
#representative group. 
#Methods: ID groups to reduce. 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
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
require(Biostrings)
#******************************************************************************#

#******************************************************************************#
denv1_countries <- list.files('Sequences/denv3/Original/')


for (filename in denv1_countries) {
  
  #Read in the file. 
  print(paste('Working on ', filename, sep = ""))
  
  this_file <- read.FASTA(paste('Sequences/denv3/Original/', filename, sep = ''), type = "DNA")
  
  #Get the number of sequneces
  old_n <- length(this_file)
  
  #Save the files as a fasta file
  #this_file_aligned_strset <- DNAStringSet(this_file)
  
  country <- strsplit(filename, split = '.f')[[1]][1]
  
  #Skip if less than n sequences. 
  if (old_n <= 10) {   
    
    file_name <- paste('Sequences/denv3/ToReduce0/', country, '_', old_n, '.fasta', sep = "")
    write.FASTA(this_file, file = file_name) 
  } else if (old_n > 20) {
    
    #file_name <- paste('Sequences/denv1/ToReduce/', country, '_', old_n,  '.fasta', sep = "")
    #write.FASTA(this_file, file = file_name) 
  }
  else next
}
#Done. 
#******************************************************************************#

#******************************************************************************#