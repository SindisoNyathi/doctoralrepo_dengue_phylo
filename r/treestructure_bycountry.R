#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: R script to Run Treestructure on by country.
#Methods: 
# 
#******************************************************************************#

#******************************************************************************#
#Preliminaries. Set direcotry to the location of the libraries. 
setwd("/labs/dlabeaud/snyathi_1/phylo_analyses/Software/R/commons")

# Packages
require(ape)
require(phangorn)
require(treestructure, lib.loc = '/labs/dlabeaud/snyathi_1/phylo_analyses/Software/R/commons')
require(phytools, lib.loc = '/labs/dlabeaud/snyathi_1/phylo_analyses/Software/R/commons')
#******************************************************************************#

#******************************************************************************#
# Read in the tree
trees_to_structures <- read.table("/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg/timed_treenames.txt", row.names = NULL)
n_trees <- nrow(trees_to_structures)
structured_trees_dataset <- vector(mode = 'list', length = n_trees)

setwd('/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg')

for (i in c(1:n_trees)){
  
  this_country_filename <- trees_to_structures[i,]
  print(paste('Now working on ', this_country_filename))
  this_tree_to_structure <- read.tree(this_country_filename)
  this_tree_to_structure_bin <- multi2di(this_tree_to_structure)
  this_tree_to_structure_rooted <- midpoint.root(this_tree_to_structure_bin)
  this_structured_tree <- trestruct(this_tree_to_structure_rooted, minCladeSize = 2, ncpu = 16)
  structured_trees_dataset[[i]] <- this_structured_tree
  
  print(paste(this_country_filename, 'is Done. Moving hastily on!'))
  
}

#Save the file as an R object. 
saveRDS(structured_trees_dataset, file = "/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg/structured_trees_bycountry.rds")
#******************************************************************************#

#******************************************************************************#