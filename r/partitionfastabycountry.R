#*****************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Partition alignment by country. 
#Methods: 
#
#******************************************************************************#

#******************************************************************************#
#*
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
#******************************************************************************#


#******************************************************************************#
denv2_fasta <- read.fasta('DENV_global/global_sequences_alignment_env_n8160.fasta')

#Create a sequence metadata file.
denv2_seq_metadata <- as.data.frame(matrix(nrow = 8160, ncol = 5))
colnames(denv2_seq_metadata) <- c('ID', 'Acc', 'Country', 'Country_city', 'Date')

for (i in 1:8160){
  
  this_seq <- denv2_fasta[[i]]
  this_seq_name <- attributes(this_seq)$name
  this_seq_name_split <- strsplit(this_seq_name, '/')
  
  this_seq_acc <- this_seq_name_split[[1]][1]
  this_seq_cou <- this_seq_name_split[[1]][2]
  this_seq_cci <- this_seq_name_split[[1]][3]
  this_seq_dat <- this_seq_name_split[[1]][4]
  
  denv2_seq_metadata$ID[i] <- this_seq_name
  denv2_seq_metadata$Acc[i] <- this_seq_acc
  denv2_seq_metadata$Country[i] <- this_seq_cou
  denv2_seq_metadata$Country_city[i] <- this_seq_cci
  denv2_seq_metadata$Date[i] <- this_seq_dat
  
}

#Write the basic metada
write.csv(denv2_seq_metadata, 'DENV_global/denv2_globalseq_basemetadata.csv', row.names = F)

#Count the sequences per country
country_table <- as.data.frame(table(denv2_seq_metadata$Country))
colnames(country_table) <- c('Country', 'Freq')
write.csv(country_table, 'DENV_global/denv2_globalseq_basemetadata_countrycounts.csv', row.names = F)
country_treetime_filenames <- country_table
country_treetime_filenames$Dates <- NA
colnames(country_treetime_filenames) <- c('Country', 'name', 'Dates')

colnames(denv2_seq_metadata) <- c('name', 'Acc', 'Country', 'Country_city', 'dates')

#Create the 96 country specific datasets, as well as the csv files with dates
for (i in 1:95){
  
  this_country <- country_table$Country[i]
  this_country_seq_names <- denv2_seq_metadata$name[denv2_seq_metadata$Country == this_country]
  this_country_seq_dates <- denv2_seq_metadata[denv2_seq_metadata$Country == this_country,]
  this_country_seq_dates <- this_country_seq_dates[,c(1,5)]
  n_this_country <- length(this_country_seq_names)
  
  this_country_seqs <- denv2_fasta[names(denv2_fasta) %in% this_country_seq_names]
  
  #Write each sequence dataset. 
  #write.fasta(this_country_seqs, names(this_country_seqs), paste('DENV_global/Countrydatasets/', this_country, '_N', n_this_country, '.fasta', sep = ''))
  write.csv(this_country_seq_dates, paste('DENV_global/Countrydatasets/Dates/', this_country, '_N', n_this_country, '_dates.csv', sep = ''), row.names = F, col.names = T, quote = FALSE)
  
  #country_treetime_filenames$Name[i] <- paste('DENV_global/Countrydatasets/', this_country, '_N', n_this_country, '.fasta', sep = '')
  country_treetime_filenames$Dates[i] <- paste('DENV_global/Countrydatasets/', this_country, '_N', n_this_country, '_dates.csv', sep = '')
}

country_treetime_filenames <- country_treetime_filenames[,-1]

write.csv(country_treetime_filenames, 'DENV_global/country_treetime_filenames.csv', row.names = F)
write.table(country_treetime_filenames[,3], 'DENV_global/country_treetime_filenames_align.txt', row.names = F, col.names = F, quote = FALSE)
write.table(country_treetime_filenames[,2], 'DENV_global/country_treetime_filenames_dates.txt', row.names = F, col.names = F, quote = FALSE)
#******************************************************************************#


#******************************************************************************#
