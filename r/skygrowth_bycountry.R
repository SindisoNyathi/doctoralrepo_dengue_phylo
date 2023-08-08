#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Run models to create the skygrowth population size parameter estimate for each 
#cluster
#* Methods: 
#* 
#******************************************************************************#

#******************************************************************************#
#*Preliminaries. Load all files and directories.

#Installing skygrowth
#require(devtools)
#install_github("DeveloperName/PackageName")
#install_github("mdkarcher/phylodyn")
#require(phylodyn)

# Packages
require(ape)
require(skygrowth, lib.loc = '/labs/dlabeaud/snyathi_1/phylo_analyses/Software/R/commons')
#*#******************************************************************************#

#********************************************************************************#
#* Now read in a csv file with cluster information.
cluster_file <- read.table("/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg/skygrowth_nexus_treenames.txt", row.names = NULL)
n_clusters <- nrow(cluster_file)
skygrowth_output <- vector(mode = 'list', length = n_clusters)

#* Iterate through all the tree files in the cluster and generate skygrowth models
for (i in c(1:n_clusters)){
  
  try({
  
  this_cluster_name <- cluster_file[i,]
  print(paste('Now working on ', this_cluster_name))
  
  this_clustertimetree <- read.nexus(paste("/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg/Countrydatasets/", this_cluster_name, 'timetree.nexus', sep = ''))
  this_clustertimetree_dates <- read.table(paste("/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg/Countrydatasets/", this_cluster_name, 'dates.tsv', sep = ''))
  
  this_clustertimetree_dates$V3 <- as.numeric(this_clustertimetree_dates$V3)
  latestdate <-  ceiling(as.numeric(max(na.omit(this_clustertimetree_dates$V3))))
  earliestdate <-  floor(as.numeric(min(na.omit(this_clustertimetree_dates$V3))))
  
  duration <- latestdate - earliestdate
  delta_ne <- duration *12
  
  
  #Collpase polytomies
  #this_clustertimetree_strictlybifurcating <- multi2di(this_clustertimetree)
  
  # Fit the skygrowth model
  this_clustertimetree_bin <- multi2di(this_clustertimetree)
  mcmcfit <- skygrowth.map(this_clustertimetree_bin, 
                       res = delta_ne,  # Ne changes every 4 weeks
                       tau0 = .1, 
                       maxHeight = duration,
                       quiet = F)  # Smoothing parameter. If prior is not specified, this will also set the scale of the prior

  skygrowth_output[[i]] <- mcmcfit
  
  print(paste(this_cluster_name, 'is Done. Moving hastily on!'))
  })
  
}

#Save the file as an R object. 
saveRDS(skygrowth_output, file = "/labs/dlabeaud/snyathi_1/phylo_analyses/Data/denvg/skygrowth_output_duration.rds")
#******************************************************************************#

#******************************************************************************#

