#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Retrieve sequence data and metadata from genbank and download relevant 
#* sequences and store. 
#* Methods: 
#* 
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
#*Read in the global sequence data metadata downloaded from Genbank
denvga_seqmetada <- read.csv("DENVGA/denvga_ncbisequence_metadata.csv", na.strings=c("","NA"))

# Take a peak
tail(denvga_seqmetada, n = 100)
hist(denvga_seqmetada$Length, breaks=20)
summary(denvga_seqmetada$Length)
summary(denvga_seqmetada$Date)
countries <- as.data.frame(table(denvga_seqmetada$Geo_Location))

geolocationna <- denvga_seqmetada[which(is.na(denvga_seqmetada$Geo_Location)),]
collectiondatena <- denvga_seqmetada[which(is.na(denvga_seqmetada$Collection_Date)),]
sum(is.na(denvga_seqmetada$Collection_Date))

#* Begin the cleaning process. 
#* Start with length, use 1000 as a general and generous starting off cutoff (> 70% of envelope)
denvga_seqmetada_lengthcutoff <- denvga_seqmetada[denvga_seqmetada$Length > 1000,]



#* Remove everythiung without Date or a geographic location
denvga_seqmetada_lengthcutoff_dateloc <- denvga_seqmetada_lengthcutoff %>% 
  drop_na(Collection_Date) %>%
  drop_na(Geo_Location)

#Format dates
denvga_seqmetada_lengthcutoff_dateloc$NewDate <- NA
denvga_seqmetada_lengthcutoff_dateloc$NewDate <- parse_date_time(denvga_seqmetada_lengthcutoff_dateloc$Collection_Date, orders = c('mdy', 'ymd', 'my', 'ym', 'y'))

#Add city information
denvga_seqmetada_lengthcutoff_dateloc$City <- NA
for (i in 1:nrow(denvga_seqmetada_lengthcutoff_dateloc)) {
  country_city <- strsplit(denvga_seqmetada_lengthcutoff_dateloc$Geo_Location[i], ':')
  denvga_seqmetada_lengthcutoff_dateloc$City[i] <- country_city[[1]][2]
  denvga_seqmetada_lengthcutoff_dateloc$Country[i] <- country_city[[1]][1]
}


# View locations
location_counts <- as.data.frame(table(denvga_seqmetada_lengthcutoff_dateloc$Country), col.names = c('Country', 'Count'))
location_counts_byserotype  <- as.data.frame(table(denvga_seqmetada_lengthcutoff_dateloc$Country, denvga_seqmetada_lengthcutoff_dateloc$Serotype), col.names = c('Country', 'Serotype', 'Count'))
write.csv(location_counts, 'DENVGA/denvga_locationcounts.csv')
write.csv(location_counts_byserotype, 'DENVGA/denvga_locationcountsbysero.csv')

#Retain countries with more than about 15
location_counts_byserotype_15cutoff <- location_counts_byserotype[location_counts_byserotype$Freq > 15,]
table(location_counts_byserotype_15cutoff$Var2)
location_counts_byserotype_15cutoff_denv2 <- location_counts_byserotype_15cutoff[location_counts_byserotype_15cutoff$Var2 == 2,]
colnames(location_counts_byserotype_15cutoff_denv2) <- c('Country', 'Serotype', 'Frequency')

#Write the current metadata file. 
write.csv(denvga_seqmetada_lengthcutoff_dateloc, "DENV_global/global_seq_metada_L_mod_D.csv")
global_seq_metada_L_mod_D <- read.csv("DENV_global/global_seq_metada_L_mod_D.csv")

#For sequences with subregions extract that data
colnames(denvga_seqmetada_lengthcutoff_dateloc)
geolocations <- table(denvga_seqmetada_lengthcutoff_dateloc$Geo_Location)
city_locations <- table(denvga_seqmetada_lengthcutoff_dateloc$Country, denvga_seqmetada_lengthcutoff_dateloc$City)
write.csv(geolocations, 'DENV_global/geolocations.csv')
write.csv(city_locations, 'DENV_global/city_locations.csv')
write.csv(denvga_seqmetada_lengthcutoff_dateloc, 'DENV_global/denvga_seqmetada_lengthcutoff_dateloc.csv')

 # Download a list of the sequences fromGenbank
for (i in c(1:34)){
  
  country <- location_counts_byserotype_30cutoff_denv2$Country[i]
  size <- location_counts_byserotype_30cutoff_denv2$Frequency[i]
  sequence_file <- denvga_seqmetada_lengthcutoff_dateloc[denvga_seqmetada_lengthcutoff_dateloc$Country == country,]
  sequence_names <- sequence_file$Accession
  seq_files <- read.GenBank(sequence_names, as.character = T, quiet = F)
  
  #Write the files.
  write.dna(seq_files, file = paste('DENVGA/sequences_bycountry/denv2/', country, '_', size, '.fasta', sep = ''), format = 'fasta')
  write.csv(sequence_file, file = paste('DENVGA/sequences_bycountry/denv2/', country, '_', size, '.csv', sep = ''))
}


denvga_seqmetada_lengthcutoff_dateloc_denv2 <- denvga_seqmetada_lengthcutoff_dateloc[denvga_seqmetada_lengthcutoff_dateloc$Serotype == 2,]
all_seqs_list <- denvga_seqmetada_lengthcutoff_dateloc_denv2$Accession
global_seqs <- read.GenBank(all_seqs_list, as.character = T, 
                            chunk.size = 200, quiet = F)
write.dna(global_seqs, 'DENVGA/denv2_global_seqs.fasta', format = 'fasta')


