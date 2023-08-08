#11/07/2022
#Sindiso Nyathi
#Goal: A burner script that I will use to correct any name issues with sequences. 
#******************************************************************************#
#*
#*#****************************************************************************#
#*#* Preliminaries. 
#* #* Preliminaries. 
#* 
#* Set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

# Load packages
require(seqinr)
require(stringr)
#******************************************************************************#
#*
#*#****************************************************************************#

# Load required files
# Seq. File
all_seq <- seqinr::read.fasta('DENV3/denv3_complete_cohort_n354_formatted_aligned_curated_norecc.fasta')

# Regions File
regions <- read.csv("Data/regions.csv")
countries <- regions$Country

for (i in 1:length(all_seq)){
  
  sequence_att <-  attributes(all_seq[i])$name
  sequence_att_sep <- strsplit(sequence_att,  split = '/', fixed = T)
  
  location_name <- sequence_att_sep[[1]][2]
  acc_num <- sequence_att_sep[[1]][1]
  date <- sequence_att_sep[[1]][3]
  
  print(location_name)
  
  if(location_name %in% countries) {
    region_name <- regions$Location[regions$Country == location_name]
    sequence_att <- paste(acc_num, region_name, date, sep = '/')
    attributes(all_seq[[i]])$Annot <- sequence_att
    print(paste(location_name, " replaced with ", region_name, sep = ""))
  }
  else {
    sequence_att <- paste(acc_num, location_name, date, sep = '/')
    attributes(all_seq[[i]])$Annot <- sequence_att
  }
}

#Save the new file. 
#Save the sequences. 
write.fasta(all_seq, getAnnot(all_seq), 'DENV3/denv3_complete_cohort_n354_formatted_aligned_curated_norecc.fasta')
#******************************************************************************#
#*
#*#****************************************************************************#
