#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Format dates for treetiem input. .
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
#*
#******************************************************************************#
nicaragua_seqmetadata <- read.csv('DENV_global/city_metadata/nicaragua_metadata_merged.csv')
colnames(nicaragua_seqmetadata)
nicaragua_seqmetadata$Treetimedate <- decimal_date(parse_date_time(nicaragua_treetimedates$Date, orders = c('y', 'ymd', 'mdy')))
nicaragua_treetimedates <- nicaragua_seqmetadata[,c(2, 6)]
write.csv(nicaragua_treetimedates, 'DENV_global/city_metadata/nicaragua_treetimedates.csv')
#******************************************************************************#
#*
#******************************************************************************#
