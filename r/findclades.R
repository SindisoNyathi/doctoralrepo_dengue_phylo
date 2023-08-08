#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Find and assing different lineages to sequences
#Methods:
# 
#******************************************************************************#

#******************************************************************************#
#*Preliminaries
#*Prelims
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
require(stats)
require(tidyr)
require(phyloch)
require(treestructure)
require(adegenet)
require(ips)
#******************************************************************************#

#******************************************************************************#
#*Read in the csv with metadata and the fasta file alingment. 
nicaragua_seqmetadata <- read.csv('DENV_global/city_metadata/nicaragua_metadata_merged.csv')
nicaragua_seqmetadata_clusters <- nicaragua_seqmetadata
this_alignment <- read.fasta('DENV_global/city_sequences/nicaragua_aln2.fasta')
this_alignment_dnabin <- fasta2DNAbin('DENV_global/city_sequences/nicaragua_aln2.fasta', quiet=FALSE)
rownames(this_alignment_dnabin) <- names(this_alignment)

#format dates and genotype information. 
{
nicaragua_seqmetadata$DateFormat <- NA
nicaragua_seqmetadata$DateFormat <- decimal_date(parse_date_time(nicaragua_seqmetadata$Date, orders = c('mdy', 'ymd', 'my', 'ym', 'y')))
nicaragua_seqmetadata$Year <- floor(nicaragua_seqmetadata$DateFormat)
nicaragua_seqmetadata$Types <- paste(nicaragua_seqmetadata$Serotype, nicaragua_seqmetadata$Genotype, sep = '_')
nicaragua_seqmetadata$Serotype <- as.character(nicaragua_seqmetadata$Serotype)
nicaragua_seqmetadata$Genotype <- as.factor(nicaragua_seqmetadata$Genotype)
}

number_of_types <- length(table(nicaragua_seqmetadata$Types))
exec <- '/Users/sindiso/Documents/raxmlHPC-AVX-v8/raxml'
for (i in c(1:number_of_types)){
  
  this_type <- names(table(nicaragua_seqmetadata$Types))[i]
  this_type_sequence_names <- nicaragua_seqmetadata$Accession[nicaragua_seqmetadata$Types == this_type]
  this_type_sequences <- this_alignment_dnabin[this_type_sequence_names,]
  
  #Make the ml tree using raxml and phyloch
  raxml_tree_output <- ips::raxml(this_type_sequences, exec = exec, N = 3, threads = 2)
  raxml_tree <- raxml_tree_output$bestTree
  
  #Run treestructure with the tree.
  raxml_tree_binary <- multi2di(raxml_tree)
  strucctured_tree <- trestruct(raxml_tree_binary) 
  
  this_clustering_data <- strucctured_tree$data
  colnames(this_clustering_data)[1] <- 'Accession'
  
  nicaragua_seqmetadata <- merge(this_clustering_data, nicaragua_seqmetadata, by = 'Accession', all.y = T)
}

write.csv(nicaragua_seqmetadata, 'DENV_global/city_metadata/nicaragua_metadata_merged_clusters.csv')
#******************************************************************************#

#******************************************************************************#
#*
# The stars have not dealt me the worst they could do:
# My pleasures are plenty, my troubles are two.
# But oh, my two troubles they reave me of rest,
# The brains in my head and the heart in my breast.
# 
# Oh, grant me the ease that is granted so free,
# The birthright of multitudes, give it to me,
# That relish their victuals and rest on their bed
# With flint in the bosom and guts in the head.
# 
# A.E. Housman
#******************************************************************************#

#******************************************************************************#