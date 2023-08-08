#******************************************************************************#
# 15/05/2023
# Sindiso Nyathi
# Goal: Read in a timed beast phylogeography tree and extract all transitions 
# from the tree
# 
#******************************************************************************#

#******************************************************************************#
#* Preliminaries
#* Online Refernece
# https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/map.html
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

# Load required librarires
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel","ggspatial", 
#                   "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", 
#                   "geosphere"))

# Load libraries
require(ggplot2)
require(sf)
require(rnaturalearth)
require(rnaturalearthdata)
require(geosphere)
require(tidyverse)
require(lwgeom)
require(cowplot)
require(paletteer)
require(geomtextpath)

#* set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")
#******************************************************************************#

#******************************************************************************#
#Read in the tree
#this_tree <- read.beast('DENV1/beast/complex1_run1/denv1_beast_complex1_run_mcc.tree')
#this_tree <- read.beast('DENV2/beast/complex5/denv2_beast_complex5_run123_mcc.tree')
#this_tree <- read.beast('DENV3/beast/complex1_run1/denv3_beast_complex1_run_mcc.tree')
this_tree <- read.beast('DENV4/beast/complex1_run1/denv4_beast_complex1_run_mcc.tree')

tree_data <- as.data.frame(this_tree@data)
tree_edges <- as.data.frame(this_tree@phylo$edge)
tree_name <- strsplit(this_tree@file, '/')[[1]][4]
#******************************************************************************#

#******************************************************************************#
#*Now get the edges data, iterate through the edges 
n_edges <- nrow(tree_edges)
this_tree_geotransitions <- as.data.frame(matrix(ncol = 6))
colnames(this_tree_geotransitions) <- c('Source', 'Destination', 'Posterior', 'Height', 'Upper', 'Lower')

for (i in c(1:n_edges)){
  
  #Get source and destination
  source_node <- tree_edges[i,1]
  destination_node <- tree_edges[i,2]
  
  #Source location
  source_location <- tree_data$Location[tree_data$node == source_node]
  
  #Destination location
  destination_location <- tree_data$Location[tree_data$node == destination_node]
  
  # If source and destination locations are the same, skip to next. 
  if (source_location == destination_location) next
  
  posterior_probability <- tree_data$posterior[tree_data$node == source_node]
  height_median <- tree_data$height_median[tree_data$node == source_node]
  height_95range_upper <- tree_data$height_0.95_HPD[tree_data$node == source_node][[1]][1]
  height_95range_lower <- tree_data$height_0.95_HPD[tree_data$node == source_node][[1]][2]
  
  # Now save this information
  this_transition <- data.frame('Source' = source_location, 'Destination' = destination_location, 'Posterior' = posterior_probability, 
                                'Height' = height_median, 'Upper' = height_95range_upper, 'Lower' = height_95range_lower)
  
  #Add to the dataframe
  this_tree_geotransitions <- rbind(this_tree_geotransitions, this_transition)
}

#Save the file. 
write.csv(this_tree_geotransitions, paste('Results/', tree_name, '_transitions.csv', sep = ''))
#******************************************************************************#

#******************************************************************************#
#*"Human beings are not born once and for all on the day their mothers give
#* birth to them, but life obliges them over and over again to give birth to 
#* themselves."
#* Love in the Time of Cholera, Gabriel Marquez



