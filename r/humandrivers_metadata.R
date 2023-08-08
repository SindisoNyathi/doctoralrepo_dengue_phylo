#*****************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Clean human drivers sequences
# in powerpoint. 
#Methods: 
#'The World Shall Know True Art' 
#******************************************************************************#

#******************************************************************************#
#* Prelims
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)
require(magrittr)

#Working directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENVA")

#******************************************************************************#

#******************************************************************************#
#*Read in sequences
all_seq <- seqinr::read.fasta('humandrivers_sequences.fasta')

#Get just names
for (i in c(1:19)){
  
  seq <- all_seq[[i]]
  
  #Get the attributes
  seq_atts <- attributes(seq)
  annotation <- seq_atts[[2]] 
  study_name_list <- strsplit(annotation, split = " ", fixed = T)
  study_name <- study_name_list[[1]][7]
  study_name_clean <- str_replace_all(study_name, "[[:punct:]]", " ") %>% 
    gdata::trim()
  
  study_name_clean_vec <- strsplit(study_name_clean, "")[[1]]
  str_length <- length(study_name_clean_vec)
  
  if (str_length < 7) {
    study_name_clean <- paste(substr(study_name_clean, 1, 3), '0', substr(study_name_clean, 4, str_length), sep = "")
  }
  
  names(all_seq)[[i]] <- study_name_clean
  
}

#Get just names
for (i in c(20:72)){
  
  seq <- all_seq[[i]]
  
  #Get the attributes
  seq_atts <- attributes(seq)
  annotation <- seq_atts[[2]] 
  study_name_list <- strsplit(annotation, split = "_", fixed = T)
  study_name <- study_name_list[[1]][2]
  study_name_clean <- str_replace_all(study_name, "[[:punct:]]", " ") %>% 
    gdata::trim()
  
  study_name_clean_vec <- strsplit(study_name_clean, "")[[1]]
  str_length <- length(study_name_clean_vec)
  
  if (str_length < 7) {
    study_name_clean <- paste(substr(study_name_clean, 1, 3), '0', substr(study_name_clean, 4, str_length), sep = "")
  }
  
  
  names(all_seq)[[i]] <- study_name_clean
  
}

names(all_seq)

#Save the file with clean names, and save a csv file with names as well. 
write.fasta(all_seq, names(all_seq), 'humandrivers.fasta')
seq_names <- as.data.frame(names(all_seq))
write.csv(seq_names, 'humandrivers_sequences.csv')
#******************************************************************************#

#******************************************************************************#
