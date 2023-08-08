#*****************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize ML Trees. Work on each serotype seperately and combine
# in powerpoint. 
#Methods: 
#'The World Shall Know True Art' 
#******************************************************************************#

#******************************************************************************#
#*Prelims
require(ape)
require(phangorn)
require(ggtree)
require(phytools)
require(magrittr)
library(treeio)
require(ggplot2)
library(ggtreeExtra)
library(ggnewscale)
require(BiocManager)
library(TDbook)
require(treestructure)
require(phytools)
library(TDbook)
require(tidyverse)

setwd("~/Desktop/treestructure")

#structured_rooted_denv2_tree <- readRDS('structured_tree.rds')
structured_rooted_denv2_tree_clade15_overlap0 <- readRDS('structured_tree_clade15_overlap0.rds')
#structured_rooted_denv2_tree_clade15_overlap1 <- readRDS('structured_tree_clade15_overlap1.rds')
#******************************************************************************#

#******************************************************************************#
#* Analyze visualize and plot
clustered_tree <- structured_rooted_denv2_tree_clade15_overlap0$tree
clusterset <- as.data.frame(structured_rooted_denv2_tree_clade15_overlap0$clustering)

#Get clusters for each node
clusterset$NodeID <- rownames(clusterset)
rownames(clusterset) <- NULL
clusterset <- clusterset[,c(2, 1)]
colnames(clusterset) <- c('Nodename', 'Cluster')

#Number of unique clusters
cluster_counts <- as.data.frame(table(clusterset$Cluster))
max(cluster_counts$Freq)
mean(cluster_counts$Freq)
median(cluster_counts$Freq)
min(cluster_counts$Freq)
hist(cluster_counts$Freq, breaks = 50)

#Get the tips in each cluster
alltips_per_cluster <- structured_rooted_denv2_tree_clade15_overlap0$clusterSets
alltips_per_partition <- structured_rooted_denv2_tree_clade15_overlap0$partitionSets

#Look at partitions
partitions <- as.data.frame(structured_rooted_denv2_tree_clade15_overlap0$partition)
partitions$NodeID <- rownames(partitions)
rownames(partitions) <- NULL
partitions <- partitions[,c(2, 1)]
colnames(partitions) <- c('Nodename', 'Partitions')
partitions_counts <- as.data.frame(table(partitions$Partitions))

#Make cluster and partition metadata
clusterset$Country <- lapply(clusterset$Nodename, function(x) {strsplit(x, '/')[[1]][2]})
clusterset$City <- lapply(clusterset$Nodename, function(x) {strsplit(x, '/')[[1]][3]})
clusterset$Date <- lapply(clusterset$Nodename, function(x) {strsplit(x, '/')[[1]][4]})
clusterset$Date <- as.numeric(clusterset$Date)
clusterset$Country <- as.factor(clusterset$Country)
cluster_partition <- merge(clusterset, partitions, by = 'Nodename')

cluster_partition$Cluster <- as.factor(cluster_partition$Cluster)
cluster_partition$Partitions <- as.factor(cluster_partition$Partitions)

#Save the cluster Information
cluster_partition <- as.data.frame(apply(cluster_partition,2,as.character))
write.csv(cluster_partition, 'denv2_completetree_clustermetadata.csv')

#Plot by partition
clustered_tree_subset <- tree_subset(clustered_tree, node = 8164, levels_back = 0)
rooted_binary_denv2_tree_plot <- ggtree(clustered_tree_subset)
clear_clustertree <- rooted_binary_denv2_tree_plot %<+% cluster_partition

# Colours
#color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

#Plot clusters by colour
clear_clustertree_plot <- groupOTU(clear_clustertree, alltips_per_cluster, 'Cluster') + 
  aes(color = Cluster) +
  #layout_circular() +# geom_nodelab(aes(label=node), size = 1) 
  #geom_tiplab(size = 0.5, alpha = 0.9) +
  geom_tippoint(aes(colour = Cluster), size = 1, alpha = 0.7) +
  #geom_hilight(node=mrcas_complete_tree, fill= sample(color, 91), alpha=0.5) +
  scale_colour_manual(
    name="Cluster",
    values=sample(color, 253),
    guide=NULL
  ) 
clear_clustertree_plot

#CLusters with bar for cluster on side
values = sample(color, 253)
clear_clustertree <- groupOTU(clear_clustertree, alltips_per_cluster, 'Cluster') + 
  aes(color = Cluster) +
  #layout_circular() +# geom_nodelab(aes(label=node), size = 1) 
  #geom_tiplab(size = 0.5, alpha = 0.9) +
  #geom_tippoint(aes(colour = Cluster), size = 1, alpha = 0.7) +
  new_scale_fill() +
  geom_fruit(geom=geom_tile, mapping = aes(fill=Cluster), width=4, offset=0) +
  scale_colour_manual(name="Cluster", values=values, guide=NULL) +
  scale_fill_manual(name="Cluster", values=values, guide=NULL) +
  theme_tree2(legend.position="none")


#Create a dataset fo characterize the clusters.
cluster_features <- as.data.frame(matrix(nrow = 252, ncol = 7))
colnames(cluster_features) <- c('ID', 'Size', 'MRCA', 'MRSeqDate', 
                                'MDSeqDate', 'Duration', 'No_locations')

cluster_features_locations_counts <- as.data.frame(matrix(ncol = 3))
colnames(cluster_features_locations_counts) <- c('Cluster', 'Location', 'Counts')

for (i in 1:252){
  
  cluster_subdataset <- cluster_partition[cluster_partition$Cluster == i, ]
  this_clusterset <- alltips_per_cluster[[i]]
  cluster_subdataset$Countrycity <- paste(cluster_subdataset$Country, cluster_subdataset$City, sep = "_")
  
  cluster_size <-nrow(cluster_subdataset)
  country_city <- length(unique(cluster_subdataset$Countrycity))
  country_city_count <- as.data.frame(table(cluster_subdataset$Countrycity))
  mrseqdate <- as.numeric(max(cluster_subdataset$Date))
  mdseqdate <- as.numeric(min(cluster_subdataset$Date))
  duration <- mrseqdate - mdseqdate
  mrca <- findMRCA(clustered_tree, this_clusterset, type = 'height')
  
  cluster_features$ID[i] <- i
  cluster_features$Size[i] <- cluster_size
  cluster_features$MRCA[i] <- mrca
  cluster_features$MRSeqDate[i] <- mrseqdate
  cluster_features$MDSeqDate[i] <- mdseqdate
  cluster_features$Duration[i] <- duration
  cluster_features$No_locations[i] <- country_city

  #Cluster freatures locations counts
  country_city_count$Cluster <- i
  colnames(country_city_count) <- c('Location', 'Counts', 'Cluster')
  cluster_features_locations_counts <- rbind(cluster_features_locations_counts, country_city_count)
}

cluster_features <- as.data.frame(apply(cluster_features,2,as.character))
write.csv(cluster_features, 'cluster_features.csv')

cluster_features_locations_counts <- as.data.frame(apply(cluster_features_locations_counts,2,as.character))
write.csv(cluster_features_locations_counts, 'cluster_features_locations_counts.csv')
#******************************************************************************#

#******************************************************************************#
"Your life you live by the light you find,
and follow it on as well as you can, 
carrying through darkness wherever you go, 
your one little fire that will start again."

by William Stafford, The Dream of Now