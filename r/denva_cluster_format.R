# Format Angola sequences
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)

#Read files
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENVA")

denva <- seqinr::read.fasta('denva_burkinafaso_N22.fasta')

#Iterate and reformat. 
for (i in c(1:23)){
  
  this_seq <- denva[[i]]
  seq_atts <- attributes(this_seq)
  name <- seq_atts[[1]] 
  name <- gdata::trim(name) #Removes leading and trailing white space
  seq_attributes <- seq_atts[[2]]
  seq_attributes <- strsplit(seq_attributes, split = '|', fixed = T)
  
  location <- seq_attributes[[1]][2]
  location <- strsplit(location, split = ':', fixed = T)
  location <- location[[1]][1]
  location <-"Burkinafaso"
  
  date <- seq_attributes[[1]][3]
  date <- decimal_date(parse_date_time(date, orders = c("ymd", "y")))
  
  new_name <- paste(name, location, date, sep = '/')
  
  # Correct
  attributes(denva[[i]])$Annot <- new_name
  
}

getAnnot(denva)

#Save the sequences. 
write.fasta(denva, getAnnot(denva), 'denva_burkinafaso_N23_formatted.fasta')
