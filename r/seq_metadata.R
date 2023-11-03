#******************************************************************************#
#06/28/2022
#Sindiso Nyathi
#Goal: Retrieve metadata corresponding to the sequences in our study 
#Methods: 
 

#Edits. 

#******************************************************************************#

#******************************************************************************#
#Preliminaries
#Required libraries
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)

#Working directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/DENV13Phylo")
#******************************************************************************#

#******************************************************************************#
#*Read in data and begin formatting. 
#Read in the file
all_seq <- seqinr::read.fasta('Sequences/denv1/complete_study_sequences.fasta')
genbank_metadata <- read.csv('Data/denv1_global_modified.csv')

#Read in the region file
regions <- read.csv("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/Data/regions.csv")
no_seq <- length(all_seq)
seq_metadata <- as.data.frame(matrix(nrow = no_seq, ncol = 4))
colnames(seq_metadata) <- c('ID', 'Country', 'Date', 'Region')

for (i in c(1:no_seq)){
  
  seq <- all_seq[[i]]
  
  #Get the attributes
  seq_atts <- attributes(seq)
  name <- seq_atts[[1]] 
  name <- gdata::trim(name) #Removes leading and trailing white space
  
  date <- genbank_metadata$DateFormat[genbank_metadata$Accession == name]
  country <- genbank_metadata$Country[genbank_metadata$Accession == name]
  region <- regions$Location[regions$Country == country]
  
  #Add to dataframe
  seq_metadata[i , 1] <- name
  seq_metadata[i , 2] <- country
  seq_metadata[i , 3] <- date
  seq_metadata[i , 4] <- region
  
  
  #Sequence name
  # african_regions <- c("WesternAfrica", 'SouthernAfrica', 'CentralAfrica', 'EasternAfrica', 'NothernAfrica')
  # if (region %in% african_regions){
  #   true_name <- paste(name, '/', country, '/', date, sep = '')}
  # else{
  #   true_name <- paste(name, '/', region, '/', date, sep = '')
  # }
  # 
  true_name <- paste(name, '/', region, '/', date, sep = '')
  attributes(all_seq[[i]])$Annot <- true_name
  names(all_seq[[i]]) <- true_name
}

#******************************************************************************#

#******************************************************************************#
#Save the sequences. 
write.fasta(all_seq, getAnnot(all_seq), 'Sequences/denv1_study_sequences_n385_formatted.fasta')

#Save the file. 
write.csv(seq_metadata, 'Data/seq_metadata_denv1.csv')
#write.table(seq_metadata, 'final_seq_metadata_denv1.tsv', sep = '\t')
#******************************************************************************#

#******************************************************************************#
