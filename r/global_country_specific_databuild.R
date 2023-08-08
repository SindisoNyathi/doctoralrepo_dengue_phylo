#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Create a serotype agnositc country specific dataset build.
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

#******************************************************************************#
#*Read in all the country level data. 
denv1_global <- read.csv('DENV_global/d1_genbankglobal_seqmetadata.csv')
denv2_global <- read.csv('DENV_global/d2_genbankglobal_seqmetadata.csv')
denv3_global <- read.csv('DENV_global/d3_genbankglobal_seqmetadata.csv')
denv4_global <- read.csv('DENV_global/d4_genbankglobal_seqmetadata.csv')
dengue_list <- list(denv1_global, denv2_global, denv3_global, denv4_global)
nicaragua <- as.data.frame(matrix(ncol = 11, nrow = 0))
colnames(nicaragua) <- c("Accession"    ,   "Organism_Name" ,  "Length"    ,      "Genotype"    ,    "Segment"    ,    
"Geo_Location"  , "Collection_Date" , "Country"     ,    "City"      ,      "HasSP"    ,      "Serotype")  

for(i in c(1:4)){
  
  denv_global <- dengue_list[[i]]
  #denv_global <- denv2_global
  #colnames(denv1_global)
  #table(denv1_global$Geo_Location)

  #Get city and country locations.
  denv_global$Country <- NA
  denv_global$City <- NA
  for (j in 1:nrow(denv_global)) {
    country_city <- strsplit(denv_global$Geo_Location[j], ':')
    denv_global$City[j] <- country_city[[1]][2]
    denv_global$Country[j] <- country_city[[1]][1]
  }
  
  #Split the Brazil sequences
  denv_global_nic <- denv_global %>% filter(Country == "Nicaragua")
  table(denv_global_nic$City)
  
  #Split by comma to get true city
  # denv_global_bra$City_primary <- NA
  # for (i in 1:nrow(denv_global_bra)) {
  #   primary_city <- strsplit(denv_global_bra$City[i], ',')
  #   denv_global_bra$City_primary[i] <- primary_city[[1]][1]
  # }
  
  #Here find saol paolo specifically instead of anything else. 
  #for (k in 1:nrow(denv_global_bra)) {
  #denv_global_bra$HasSP[k] <- grepl('Sao Paulo', denv_global_bra$Geo_Location[k], ignore.case = TRUE)
  #}
  
  #denv_global_bra$City_primary <- trimws(denv_global_bra$City_primary) 
  #table(denv_global_bra$City_primary)
  #unique(denv1_global_bra$City_primary)
  
  # Retrieve for Sao Paolo
  #denv_global_bra_saopaolo <- denv_global_bra[which(denv_global_bra$City_primary == 'Sao Paulo'),]
  #denv_global_bra_saopaulo <- denv_global_bra[which(denv_global_bra$HasSP),]
  
  denv_global_nic$Serotype <- i
  
  #Save the file in the list
  #if (nrow(denv_global_nic) > 0){
  nicaragua <- rbind(nicaragua, denv_global_nic)
  #}
}

#Now clean the sao Paolo dataset. 
nicaragua_clean <- nicaragua
nicaragua_clean <- nicaragua_clean[,c(1, 3, 7, 10)]
nicaragua_seqids <- nicaragua_clean$Accession

#Download the sequences from genbank
nicaragua_sequences <- read.GenBank(nicaragua_seqids, as.character = T, quiet = F)

# Save the sequences
write.dna(nicaragua_sequences, 'DENV_global/city_sequences/nicaragua.fasta', format = 'fasta')
write.csv(nicaragua_clean, 'DENV_global/city_metadata/nicaragua.csv')
#******************************************************************************#

#******************************************************************************#
