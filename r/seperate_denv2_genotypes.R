#******************************************************************************#
#02/28/2023
#Sindiso Nyathi
#Goal: Seperate Cosmopolitan and non-cosmopolitan datasets
#Methods: 

#Edits. 

#******************************************************************************#

#******************************************************************************#
#*Prelims
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)

#Working directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENV2")
#******************************************************************************#

#******************************************************************************#
#*Get the current cosmopolitan dataset. 
all_denv2_seq <- seqinr::read.fasta('denv2_complete_cohort_n378_formatted_aligned_curated_norecc_realign.fasta')
cosmo_seq <- seqinr::read.fasta('denv2_cosmopolitan_n199_formatted_aligned_curated_norecc.fasta')

#*Get the names of each dataset
all_denv2_seq_names <- names(all_denv2_seq)
cosmo_seq_names <- names(cosmo_seq)

#Get the non-cosmopolitan data. 
non_cosmo_seq_names <- all_denv2_seq_names[which(!(all_denv2_seq_names %in% cosmo_seq_names))]

#Get random cosmo subset
random_cosmosubset_names <- sample(cosmo_seq_names, 10)

#Get the non-cosmo dataset
non_cosmo_seq <- all_denv2_seq[which(names(all_denv2_seq) %in% non_cosmo_seq_names| names(all_denv2_seq) %in% random_cosmosubset_names)]

#Save the non-cosmopolitan dataset. 
write.fasta(non_cosmo_seq, names(non_cosmo_seq), 'denv2_noncosmo_n189_formatted_aligned_curated_norecc.fasta')

#******************************************************************************#

#******************************************************************************#
#*

