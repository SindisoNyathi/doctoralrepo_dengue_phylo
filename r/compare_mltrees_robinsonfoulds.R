#******************************************************************************#
#04/01/2022
#Sindiso Nyathi
#Goal: Compare two different RAXML trees with Robinson Foulds distance and visual metrics
#******************************************************************************#

#******************************************************************************#
#Preliminaries
#Set Working Directory
require(ggplot2)
require(stringr)
require(reshape2)
library(grid)
library(gridExtra)
require(stringi)
require(ggallin)
library("ggplot2")
library("ggtree")
require('phytools')
require('ggforce')
library(ggrepel)
require('phangorn')
require('colorspace')
require(paletteer)
require(treeio)
library('TreeDist')

setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/DENV2")
#******************************************************************************#

#******************************************************************************#
strictfilters_denv2_raxml_tree <- ape::read.tree("ml_filterSA/raxml_.raxml.support")
relaxesfilters_denv2_raxml_tree<- ape::read.tree("ml/raxml_.raxml.support")

#Calculate distance
tree_distance <- TreeDistance(strictfilters_denv2_raxml_tree, relaxesfilters_denv2_raxml_tree)

comparePhylo(strictfilters_denv2_raxml_tree, relaxesfilters_denv2_raxml_tree, plot = T)
#******************************************************************************#

#******************************************************************************#
