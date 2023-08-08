#******************************************************************************#
#05/24/2022
#Sindiso Nyathi
#Goal: Define transmisssion cluster sequences. 

# Notes
# This file runs as is; chnage only the serotype and run line by line. The code 
# Modifies the format of the sequence name to match the genbak format. 
#******************************************************************************#
#*
#*
#*#******************************************************************************#
# Load directories


setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENVA")
denva <- read.fasta('denva_complete_cohort_n130_formatted.fasta')
denva_metadata <- read.csv("denva_metadata_final.csv")

#******************************************************************************#
#*
#******************************************************************************#
#*
#*Define the clusters
trans_cluster_1 <- denva_metadata[denva_metadata$Cluster == '2.1',]
trans_cluster_2 <- denva_metadata[denva_metadata$Cluster == '2.2',]

# Create the dataset. 
denva_trans_cluster_1 <- denva[which(names(denva) %in% trans_cluster_1$RealID)]
denva_trans_cluster_2 <- denva[which(names(denva) %in% trans_cluster_2$RealID)]

# Save the files. 
#Save the sequences. 
write.fasta(denva_trans_cluster_1, names(denva_trans_cluster_1), 'denva_trans_cluster_1.fasta')
write.fasta(denva_trans_cluster_2, names(denva_trans_cluster_2), 'denva_trans_cluster_2.fasta')

#******************************************************************************#
#*
#******************************************************************************#
