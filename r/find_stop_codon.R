#******************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Trees
#Methods: 
#Find stop codon in an alignment file, and print out result. This code works best 
# when the alignment has been cleaned and has very few stop codons that are hard to find 
# visually. 
#******************************************************************************#

#******************************************************************************#
#*
#* Required libraries
require(seqinr)
require(stringr)
require(stringi)
require(gdata)
require(lubridate)

# Set working directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENV1")
#******************************************************************************#

#******************************************************************************#
# Read the alignment
alignment <- read.alignment('denv1_complete_cohort_n353_formatted_aligned_curated.fasta', format = 'fasta')
no_seq <- alignment$nb

# iterate through alignment
for (i in (1:no_seq)) {
  
  #Get the sequence.
  this_seq <- alignment$seq[[i]]
  
  #Split into codons
  this_seq_codons <- stri_sub(this_seq, seq(1, stri_length(this_seq), by=3), length=3)
  
  this_seq_stop_locations_taa <- grep("taa", this_seq_codons, ignore.case ="True")
  this_seq_stop_locations_tag <- grep("tag", this_seq_codons, ignore.case ="True")
  this_seq_stop_locations_tga <- grep("tga", this_seq_codons, ignore.case ="True")
  
  total_stops <- sum(this_seq_stop_locations_taa + this_seq_stop_locations_tag + this_seq_stop_locations_tga)
  
  if (any(this_seq_stop_locations_taa, this_seq_stop_locations_tag, this_seq_stop_locations_tga) > 0){
    
    #Name of seq
    this_seq_name <- alignment$nam[[i]]
    
    #Position
    taa_pos <- this_seq_stop_locations_taa*3
    tag_pos <- this_seq_stop_locations_tag*3
    tga_pos <- this_seq_stop_locations_tga*3
    
    print(paste(this_seq_name, " has stop codons at AA positions as follows: ", sep = ""))
    print(paste("TAA: ", taa_pos, ":", taa_pos + 3, sep = ""))
    print(paste("TAG: ", tag_pos, ":", tag_pos + 3, sep = ""))
    print(paste("TGA: ", tga_pos, ":", tga_pos + 3, sep = ""))
  }
}
