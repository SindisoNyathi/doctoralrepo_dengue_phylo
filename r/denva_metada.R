#******************************************************************************#
#05/24/2022
#Sindiso Nyathi
#Goal: Align a group of sequences and reduce them to a smaller, but
#representative group. 

# Notes
# This file runs as is; chnage only the serotype and run line by line. The code 
# Modifies the format of the sequence name to match the genbak format. 
#******************************************************************************#
#*
#*
#*#******************************************************************************#
#*
#*#*
#*
#*#******************************************************************************#
#* Preliminaries
require(ape)
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)

setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENVA")

denva <- seqinr::read.fasta('denv_envelope_sequences_aligned_mod1_aligned_nodup.fasta')

no_seq <- length(denva)

#First format the genbank sequences. 
denva_metadata <- as.data.frame(matrix(nrow = no_seq, ncol = 5))
colnames(denva_metadata) <- c('ID', 'Country', 'City', 'Date', 'GDate')

for (i in c(1:92)){
  
  seq <- denva[[i]]
  
  #Get the attributes
  seq_atts <- attributes(seq)
  name <- seq_atts[[1]] 
  name <- gdata::trim(name) #Removes leading and trailing white space
  seq_attributes <- seq_atts[[2]]
  seq_attributes <- strsplit(seq_attributes, split = '|', fixed = T)
  
  #Get the date
  date <- NA
  if (length(seq_attributes[[1]]) > 2) {
    date <- seq_attributes[[1]][[3]]
    date2 <- decimal_date(parse_date_time(date, orders = c("y", "ym", "ymd")))
  }
  date <- gdata::trim(date)
  
  #Get the city
  country_city <- strsplit(seq_attributes[[1]][[2]], split = ':', fixed = T)
  if (length(country_city[[1]]) > 1) {
    city <- country_city[[1]][[2]]
    city <- gdata::trim(city)
    city <- strsplit(city, split = ' ')
    
    #If city is more than 1 element take only the first. 
    if (length(city[[1]]) > 1){
      city <- city[[1]][1]
    }
  } else {
    city <- NA
  }
  
  #Remove space or mpunctuation from string
  city <- str_replace(city, "\'", "")
  city <- str_replace(city, " ", "")
  city <- str_replace(city, "-", "")
  city <- str_replace(city, ",", "")
  
  #Get the country
  country_name <- seq_attributes[[1]][[2]]
  
  #Clean the country name
  country_name <- str_replace(country_name, "\'", "")
  country_name <- str_replace(country_name, " ", "")
  country_name <- str_replace(country_name, "-", "")
  country_name <- str_replace(country_name, ",", "")
  
  #Add to dataframe
  denva_metadata[i , 1] <- name
  denva_metadata[i , 2] <- country_name
  denva_metadata[i , 3] <- city
  denva_metadata[i , 4] <- date
  denva_metadata[i , 5] <- date2
  
  names(denva)[[i]] <- name
}

# Now add the study sequences
# Get dates file

datesfile  <- read.csv("predictor_data/testing_history.csv")

for (i in c(93:no_seq)){
  
  seq <- denva[[i]]
  
  #Get the attributes
  seq_atts <- attributes(seq)
  name <- seq_atts[[1]] 
  seq_attributes <- strsplit(name, split = '_', fixed = T)
  name <- seq_attributes[[1]][2]
  name <- substr(name, 1, 7)
  location_initial <- substr(name, 1, 1)
  
  location <- switch(location_initial, 
                     "U" = "Ukunda", "D" = "Ukunda", "L" = "Ukunda", "M" = "Msambweni", "C" = "Chulaimbo", "R" = "Chulaimbo", "K" = "Kisumu")
  
  #Get the date
  date <- 444
  date2 <- datesfile$interview_date_aic[which(datesfile$s == name)]
  if (length(date2) == 0 && substr(name, 1, 1) == "C") {
    name <- paste("R", substr(name, 2, 7), sep = "")
    date2 <- datesfile$interview_date_aic[which(datesfile$s == name)]
  }
 
  date3 <- decimal_date(mdy(date2))
  
  if (length(date3) == 0) {date3 <- NA}
  
  #Add to dataframe
  denva_metadata[i , 1] <- name
  denva_metadata[i , 2] <- "Kenya"
  denva_metadata[i , 3] <- location
  denva_metadata[i , 5] <- date3
  
  #Change the name of the sequence
  attributes(denva[[i]])$name <- name
  names(denva)[[i]] <- name
}


# Save the base metadata file nad read in the updated one. 
write.csv(denva_metadata, "denva_metadata_out.csv")
denva_metadata_upated <- read.csv("denva_metadata_in.csv")
new_names <- denva_metadata_upated$ID

denva_updated <- denva[which(names(denva) %in% new_names)]

for (i in c(1:length(denva_updated))) {
  
  seq <- denva_updated[[i]]
  seq_name <- attributes(seq)$name
  
  this_name <- denva_metadata_upated$ID[denva_metadata_upated$ID == seq_name]
  this_location <- denva_metadata_upated$City[denva_metadata_upated$ID == seq_name]
  this_date <- denva_metadata_upated$GDate[denva_metadata_upated$ID == seq_name]
  
  new_formatted_name <- paste(this_name, this_location, this_date, sep = '/') 

  denva_metadata_upated$RealID[denva_metadata_upated$ID == seq_name] <- new_formatted_name
  names(denva_updated)[[i]] <- new_formatted_name
} 

names(denva_updated)

#Reformat the date for the metadata file. 
denva_metadata_upated$Year <- year(date_decimal(denva_metadata_upated$GDate))
denva_metadata_upated$Month <- month(date_decimal(denva_metadata_upated$GDate), label = T)
  
#Save the files.
write.csv(denva_metadata_upated, "denva_metadata_final.csv")

#Save the sequences. 
write.fasta(denva_updated, names(denva_updated), 'denva_complete_cohort_n130_formatted.fasta')

#Complete. 