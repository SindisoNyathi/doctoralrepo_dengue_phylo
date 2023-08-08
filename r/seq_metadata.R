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
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENV1")
#******************************************************************************#

#******************************************************************************#
#*Read in data and begin formatting. 
#Read in the file
all_seq <- seqinr::read.fasta('denv1_complete_study_cohort.fasta')

#Read in the region file
regions <- read.csv("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/Data/regions.csv")
no_seq <- length(all_seq)
seq_metadata <- as.data.frame(matrix(nrow = no_seq, ncol = 4))
colnames(seq_metadata) <- c('ID', 'Country', 'City', 'Date')

for (i in c(1:no_seq)){
  
  seq <- all_seq[[i]]
  
  #Get the attributes
  seq_atts <- attributes(seq)
  name <- seq_atts[[1]] 
  name <- gdata::trim(name) #Removes leading and trailing white space
  seq_attributes <- seq_atts[[2]]
  seq_attributes <- strsplit(seq_attributes, split = '|', fixed = T)
  
  #Get the date
  date = NA
  if (length(seq_attributes[[1]]) > 3){
    date = seq_attributes[[1]][[length(seq_attributes[[1]])]] #Lists in R are so Stupid
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
  country_name <- seq_attributes[[1]][[3]]
  
  #Clean the country name
  country_name <- str_replace(country_name, "\'", "")
  country_name <- str_replace(country_name, " ", "")
  country_name <- str_replace(country_name, "-", "")
  country_name <- str_replace(country_name, ",", "")
  
  #Add to dataframe
  seq_metadata[i , 1] <- name
  seq_metadata[i , 2] <- country_name
  seq_metadata[i , 3] <- city
  seq_metadata[i , 4] <- date
  
}

# This runs with a few warnings leave as is/as are.

#If City columns is NA make country city, 
seq_metadata$City[which(is.na(seq_metadata$City))] <- seq_metadata$Country[which(is.na(seq_metadata$City))]

#Format dates. 
seq_metadata$Date2 <- parse_date_time(seq_metadata$Date, orders = c('ymd', 'y', 'ym'))
seq_metadata$Date3 <- year(seq_metadata$Date2)
seq_metadata$NewID <- NA

#At this step do the following, basically choosing which locations to include as cities vs. countries. If enough data is provided
# to include the city, include other wise include country. Generally include city for within africa sequences, but leave as country for everything else. 
#1. If locations have no city name, replace city with country, 
#2. IF sequences are missing both city and country remove the sequence
#3. If sequences are missing date, remove the sequences. 
#4. Include Kenya sequences that only have city information or some within country geographic locator. 
#NB No spaces between names
#NB Pay particular attention to India, China since they are large and if possible we want to include cities here. 
write.csv(seq_metadata, 'seq_metadata_denv1_manualfix.csv')
seq_metadata <- read.csv('seq_metadata_denv1_manualfixed.csv')
no_seq <- nrow(seq_metadata)

#Subset to include only sequences in the excel file. 
all_seq <- all_seq[which(names(all_seq) %in% seq_metadata[,1])]

#Fix the sequence names
for (i in c(1:no_seq)){
  
  #Retrieve sequence name
  seq <- all_seq[[i]]
  this_seq_att <- attributes(seq)
  this_seq_name <- attributes(seq)[1]
  
  #Retrieve the proper date. 
  this_seq_newdate <- seq_metadata$Date[which(seq_metadata$ID == this_seq_name)]
  this_seq_newloc <- seq_metadata$Location[which(seq_metadata$ID == this_seq_name)]
  
  # Get the region
  if (this_seq_newloc %in% regions$Country)
    {this_seq_newloc <- regions$Location[which(regions$Country == this_seq_newloc)]}
  
  #Split attributes
  this_seq_att_split <- strsplit(this_seq_att$Annot, split = '|', fixed = T)
  
  #Modify
  new_seq_att <- paste(this_seq_att$name, this_seq_newloc, this_seq_newdate, sep = '/')
  
  #Change original
  attributes(all_seq[[i]])$Annot <- new_seq_att
  
  #Change ID in file. 
  seq_metadata$Location[which(seq_metadata$ID == this_seq_name)] <- this_seq_newloc
  seq_metadata$ID[which(seq_metadata$ID == this_seq_name)] <- new_seq_att
  
  
}

#******************************************************************************#

#******************************************************************************#
#Save the sequences. 
write.fasta(all_seq, getAnnot(all_seq), 'denv1_complete_cohort_n353_formatted.fasta')

#Save the file. 
write.csv(seq_metadata, 'final_seq_metadata_denv1.csv')
write.table(seq_metadata, 'final_seq_metadata_denv1.tsv', sep = '\t')
#******************************************************************************#

#******************************************************************************#
