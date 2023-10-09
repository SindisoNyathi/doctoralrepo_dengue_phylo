#*****************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize ML Trees to compare the strict vs generous filters. 
#Methods: 
#'The World Shall Know True Art' 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
#Set Working Directory
setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/")

# Libraries
library("ggplot2")
library("ggtree")
require('phytools')
require('ggforce')
library(ggrepel)
require('phangorn')
require('colorspace')
require(paletteer)
require(treeio)
require(TreeTools)
require(tidytree)
#******************************************************************************#

#******************************************************************************#
#* Read the two trees
#* #Strict
strictfilters_denv2_raxml_tree_support <- ape::read.tree("DENV2/ml_filterSA/raxml_.raxml.support")
rooted_strictfilters_denv2_raxml_tree_support <- phangorn::midpoint(strictfilters_denv2_raxml_tree_support)

#original/relaxed
relaxesfilters_denv2_raxml_tree_support <- ape::read.tree("DENV2/ml/raxml_.raxml.support")
rooted_relaxesfilters_denv2_raxml_tree_support <- phangorn::midpoint(relaxesfilters_denv2_raxml_tree_support)

# Plot the strict fitlers tree
#Supplementary Material Plot
offset_value <- 0.022
offset.text_value = 0.003
denv2_tree@data$posterior <- round(as.numeric(denv2_tree@data$posterior), digits = 2)
supp_plot_strict <- ggtree(rooted_strictfilters_denv2_raxml_tree_support,  layout = "rectangular", ladderize = T,  branch.length = 'rate', size = 0.5) + 
  #ggtitle("Maximum Likelihood Tree of Global DENV2 Sequences") +
  geom_treescale(x=0.05, y=-10, width=0.01, color='black', offset = 2.5, linesize = 4, fontsize = 14) +

  geom_hilight(node=542, fill="#DC3220", alpha=0.4, extend = 0.02475) + #Kenya, Wajir and Mombasa
  geom_hilight(node=384, fill="#005AB5", alpha=0.4, extend = 0.0005) + #Kenya, Coastal
  geom_hilight(node=701, fill="#40B0A6", alpha=0.4, extend = 0.065) + #Kenya,Western
  
  #Scale
  xlim(c(0, 0.1)) + 
  geom_tiplab(size = 4, offset = 0.001) + 
  geom_tippoint(size = 2) +
  geom_nodelab(aes(label=label), nudge_y = 1, nudge_x = -0.003) +
  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.5, size = 70), 
              legend.title = element_text(size = 40), 
              legend.text = element_text(size = 32), 
              #legend.position = c(.2, 0.75),
              legend.key.width = unit(1.25, "cm"),
              legend.key.height = unit(2, "cm"),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())
plot(supp_plot_strict) #55 x 30

denv2_tree <- read.beast("DENV2/beast/simple_run_filterSA/denv_completecohort_n378_aligned_curated_filterSA_mcc.tree")
denv2_metadata <- read.csv("DENV2/final_seq_metadata_denv2.csv")
denv2_tree@data$posterior <- round(as.numeric(denv2_tree@data$posterior), digits = 2)

# Meta data
region_colours <- read.csv('manuscript_colours.csv')

# 
test_tree %<+% denv2_metadata

offset_bar <- 30
offset_text = -12
denv2_tree_plot_supp <- ggtree(denv2_tree, mrsd='2022-01-01',  size = 1.75,
                               ladderize = T) %<+% denv2_metadata + 
  geom_tiplab(size = 5, alpha = 1) +
  #geom_nodelab(aes(label=posterior), nudge_y =1, nudge_x = -2, size = 6, colour = 'black') +
  geom_tippoint(aes(colour = Location), size = 5, alpha = 0.9) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years)") +
  scale_x_continuous(limits = c(1850, 2125), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
  
  #Label The Genotypes
  geom_cladelab(node=558, label= "II", color='black', fontsize=28, linewidth = 6, offset.text = offset_text,
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
  geom_cladelab(node=465, label= "III", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
  geom_cladelab(node=415, label= "V", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#4F94CD") +
  geom_cladelab(node=395, label= "IV", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "lightsalmon") +
  geom_cladelab(node=380, label = "I", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
  
  geom_hilight(node=648, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Kenya, Wajir and Mombasa
  geom_hilight(node=603, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
  geom_hilight(node=396, fill="#40B0A6", alpha=0.6, extend = 0.08) + #Western Kenya, 
  
  geom_vline(xintercept = 1850, color = 'black', size = 0.05, alpha = 1) +
  geom_vline(xintercept = 1900, color = 'black', size = 0.05, alpha = 1) +
  geom_vline(xintercept = 1950, color = 'black', size = 0.05, alpha = 1) +
  geom_vline(xintercept = 2000, color = 'black', size = 0.05, alpha = 1) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.05, alpha = 1) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.05, alpha = 1) +
  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.9, size = 20), 
              axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
              legend.title = element_text(size = 20), 
              legend.text = element_text(size = 20))

denv2_tree_plot_supp #PDF 60 x 40
#******************************************************************************#

#******************************************************************************#
