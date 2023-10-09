#*****************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Bayesian Trees. Work on each serotype seperately and combine
# in powerpoint. 
#Methods: 
#'The World Shall Know True Art' 
#'Upadates
# Added code to plot the Sensitiivty analyses for relaxed Clock and strict filter parameters
#******************************************************************************#

#******************************************************************************#
#Preliminaries
library(ggtree)
require(ggtree)
require(ggplot2)
library(ape)
library(treeio)
require(RColorBrewer)
require(ggplot2)
library(gridExtra)
require(phangorn)
require(paletteer)

setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")
#******************************************************************************#

#******************************************************************************#
#*DENV2.
{
# Read tree
denv2_tree <- read.beast("DENV2/beast/complex5/denv2_beast_complex5_run123_mcc.tree")

# Meta data
regions <- read.csv("Data/denv2_regions_coords.csv")
regions_or <- ordered(regions[,1], levels = c(regions$Location[regions$Study_Site == 0], regions$Location[regions$Study_Site == 1]))
regions <- regions[order(regions_or),]
region_colours <- read.csv('manuscript_colours.csv')

#Test plot
ggtree(denv2_tree, mrsd='2022-01-01',  size = 1.25, aes(color = Location),
                          ladderize = T) + 
  geom_tiplab(size = 3, alpha = 0.9) +
  geom_nodelab(aes(label=node))

#Read in Metadata
denv2_metadata <- read.csv("DENV2/final_seq_metadata_denv2.csv")
denv2_metadata$Site <- factor(denv2_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                              "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))


#DENV2 Tree plot. 
offset_bar = -30
offset_text = -12
denv2_tree@data$posterior <- round(as.numeric(denv2_tree@data$posterior), digits = 2)
denv2_tree_plot <- ggtree(denv2_tree, mrsd='2022-01-01',  size = 2,# aes(color = Location),
                          ladderize = T) + 
  #geom_label(aes(x=branch, label=Location)) +
  geom_tippoint(aes(colour = Location), size = 15, alpha = 0.9) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years)") +
  ylim(c(0, 480)) + 
  scale_x_continuous(limits = c(1860, 2050), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
  
  #geom_text(aes(x=branch, label=labels, vjust=-.5)) +

  #geom_vline(xintercept = 1850, color = 'black', size = 0.1, alpha = 1) +
  geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
  geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
  geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +

  #Label The Genotypes
   geom_cladelab(node=392, label= "II", color='black', fontsize=28, linewidth = 6, offset.text = offset_text,
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
   geom_cladelab(node=591, label= "III", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
   geom_cladelab(node=706, label= "V", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#4F94CD") +
   geom_cladelab(node=685, label= "IV", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "lightsalmon") +
   geom_cladelab(node=380, label = "I", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
  
  geom_hilight(node=481, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Kenya, Wajir and Mombasa
  geom_hilight(node=507, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
  geom_hilight(node=688, fill="#40B0A6", alpha=0.6, extend = 0.08) + #Kenya, 

  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.9, size = 20), 
              axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
              legend.title = element_text(size = 20), 
              legend.text = element_text(size = 20)) +
  
  # Add arrows for time
  geom_segment(aes(x = 2012, y = 400, xend = 2012, yend = 272), linetype = 1, colour = "grey10",
              arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 2015, y = 435, label = "(Mombasa Introduction\n 2012 [2012, 2013])", angle = 90, size = 14) +

  geom_segment(aes(x = 2006, y = 400, xend = 2006, yend = 25), linetype = 1, colour = "grey10",
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 2004, y = 435, label = "(Kisumu Introduction\n 2006 [2005, 2007])", angle = 90, size = 14) +
  
  geom_segment(aes(x = 1993, y = 400, xend = 1993, yend = 229), linetype = 1, colour = "grey10",
              arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 1993, y = 435, label = "(Mombasa Introduction\n 1993 [1991, 1996])", angle = 90, size = 14)


##Save as 45 x 30
#man1/denv2_bayes_main.pdf
denv2_tree_plot

#Add the subtrees. for the three lineages
#Lineage 1: Node 502
denv2_tree_subset_L1 <- tree_subset(denv2_tree, node = 502, levels_back = 1)
denv2_tree_subset_L1_plot <- ggtree(denv2_tree_subset_L1, mrsd='2022-01-01',  size = 1.25, #aes(color = Location),
       ladderize = T) + 
  geom_tiplab(size = 12, alpha = 0.9, offset = 1, align = T) +
  #geom_nodelab(aes(label=posterior), nudge_y = 0.2, nudge_x = -2, size = 8, colour = 'black') +
  geom_tippoint(aes(colour = Location), size = 15, alpha = 0.8) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years") +
  scale_x_continuous(breaks = c(1970, 1980, 2000, 2020), limits = c(1970, 2100)) +
  #geom_vline(xintercept = 1960, color = 'black', size = 0.1, alpha = 0.2) +
  geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
  geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.9, size = 40), 
              axis.text.x = element_text(size = 40, angle = 90, vjust = 0.5), 
              legend.title = element_text(size = 40), 
              legend.text = element_text(size = 40)) 
denv2_tree_subset_L1_plot
#PDF 20 x 15

#Lineage 2: Node 507
denv2_tree_subset_L2 <- tree_subset(denv2_tree, node = 508, levels_back = 2)
denv2_tree_subset_L2_plot <- ggtree(denv2_tree_subset_L2, mrsd='2022-01-01',  size = 1.25, #aes(color = Location),
                                    ladderize = T) + 
  geom_tiplab(size = 8, alpha = 0.9, offset = 1, align = T) +
  geom_tippoint(aes(color = Location), size = 15, alpha = 0.8) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years") +
  scale_x_continuous(breaks = c(1980, 2000, 2020), limits = c(1980, 2075)) +
  geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
  geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
  #geom_vline(xintercept = 2005, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
  #geom_vline(xintercept = 2015, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.9, size = 40), 
              axis.text.x = element_text(size = 40, angle = 90, vjust = 0.5), 
              legend.title = element_text(size = 40), 
              legend.text = element_text(size = 40)) 
#PDF: 20x15
denv2_tree_subset_L2_plot

#Lineage 3: Node 688
denv2_tree_subset_L3 <- tree_subset(denv2_tree, node = 688, levels_back = 1)
denv2_tree_subset_L3_plot <- ggtree(denv2_tree_subset_L3, mrsd='2022-01-01',  size = 1.25, #aes(color = Location),
                                    ladderize = T) + 
  geom_tiplab(size = 14, alpha = 0.9, offset = 1, align = T) +
  geom_tippoint(aes(color = Location), size = 15, alpha = 0.7) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years") +
  scale_x_continuous(breaks = c(1950, 1975, 2000, 2010, 2020), limits = c(1940, 2125)) +
  geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
  geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
  #geom_vline(xintercept = 2005, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
  #geom_vline(xintercept = 2015, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.9, size = 40), 
              axis.text.x = element_text(size = 48, angle = 90, vjust = 0.5), 
              legend.title = element_text(size = 40), 
              legend.text = element_text(size = 40)) 

#PDF: 20 x 15
denv2_tree_subset_L3_plot

## Plot Legend

denv2_tree_plot_legend <- ggtree(denv2_tree, mrsd='2022-01-01',  size = 1.25, 
                          ladderize = T) + 
  layout_rectangular() +
  geom_tippoint(aes(color = Location), size = 3, alpha = 0.7) +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years") +
  ylim(c(0, 440)) + 
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2010, 2020)) +
    theme_tree2(plot.title = element_text(hjust = 0.9, size = 20), 
              axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5), 
              legend.title = element_text(size = 40), 
              legend.text = element_text(size = 30)) +
  guides(colour = guide_legend(override.aes = list(size=12)))

denv2_tree_plot_legend
#Save legend as 20 x 20

# Supplementary material
offset_bar <- 30
denv2_tree@treetext
denv2_tree_plot_supp <- ggtree(denv2_tree, mrsd='2022-01-01',  size = 1.75, aes(color = Location),
                          ladderize = T) + 
  geom_tiplab(size = 6, alpha = 1) +
  geom_nodelab(aes(label=age), nudge_y = 0.2, nudge_x = -2, size = 4, colour = 'black') +
  geom_tippoint(aes(fill = Location), size = 3, alpha = 0.7) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years)") +
  scale_x_continuous(limits = c(1850, 2125), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
  
  #Label The Genotypes
  geom_cladelab(node=392, label= "II", color='black', fontsize=28, linewidth = 6, offset.text = offset_text,
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
  geom_cladelab(node=591, label= "III", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
  geom_cladelab(node=706, label= "V", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#4F94CD") +
  geom_cladelab(node=685, label= "IV", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "lightsalmon") +
  geom_cladelab(node=380, label = "I", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
  
  geom_hilight(node=502, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Kenya, Wajir and Mombasa
  geom_hilight(node=507, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
  geom_hilight(node=688, fill="#40B0A6", alpha=0.6, extend = 0.08) + #Kenya, 
  
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

######*************************************##########
######*************************************##########
######*************************************##########
#Plotting the relaxed clock tree. 
# Read tree
denv2_tree_relaxed <- read.beast("DENV2/bayesian_relaxedclockSA/denv2_complete_cohort_n378_relaxedclockSA_mcc.tree")

# Meta data
regions <- read.csv("Data/denv2_regions_coords.csv")
regions_or <- ordered(regions[,1], levels = c(regions$Location[regions$Study_Site == 0], regions$Location[regions$Study_Site == 1]))
regions <- regions[order(regions_or),]
region_colours <- read.csv('manuscript_colours.csv')

#Test plot
ggtree(denv2_tree_relaxed, mrsd='2022-01-01',  size = 1.25,
       ladderize = T) + 
  geom_tiplab(size = 3, alpha = 0.9) +
  geom_nodelab(aes(label=node))

#Read in Metadata
denv2_metadata <- read.csv("DENV2/final_seq_metadata_denv2.csv")
denv2_metadata$Site <- factor(denv2_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                              "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))


#DENV2 Tree plot. 
offset_bar = 10
offset_text = -12
denv2_tree_relaxed@data$posterior <- round(as.numeric(denv2_tree_relaxed@data$posterior), digits = 2)
denv2_tree_plot_relaxed <- ggtree(denv2_tree_relaxed, mrsd='2022-01-01',  size = 1.75,
                                  ladderize = T) + 
  geom_tiplab(size = 3, alpha = 1) +
  geom_nodelab(aes(label=posterior), nudge_y = 0.4, nudge_x = -2, size = 4, colour = 'black') +
  geom_tippoint(size = 2, alpha = 0.7) +
  layout_rectangular() +
  scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
  xlab("Time (Years)") +
  scale_x_continuous(limits = c(1850, 2125), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
  
  #Label The Genotypes
  geom_cladelab(node=381, label= "II", color='black', fontsize=28, linewidth = 6, offset.text = offset_text,
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
  geom_cladelab(node=652, label= "III", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
  geom_cladelab(node=602, label= "V", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#4F94CD") +
  geom_cladelab(node=583, label= "IV", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "lightsalmon") +
  geom_cladelab(node=745, label = "I", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
  
  geom_hilight(node=558, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Kenya, Wajir and Mombasa
  geom_hilight(node=386, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
  geom_hilight(node=583, fill="#40B0A6", alpha=0.6, extend = 0.08) + #Kenya, Western
  
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

##Save as 45 x 30
#Plots/man1/denv2_bayes_relaxedSA.pdf
denv2_tree_plot_relaxed


######*************************************##########
######*************************************##########
######*************************************##########
#Plotting the strict bioinformatics paramters tree. 
# Read tree
denv2_tree_strictbioinf <- read.beast("DENV2/bayesian_filterSA/denv_completecohort_n378_aligned_curated_filterSA_mcc.tree")

# Meta data
region_colours <- read.csv('manuscript_colours.csv')

#Test plot
ggtree(denv2_tree_strictbioinf, mrsd='2022-01-01',  size = 1.25,
       ladderize = T) + 
  geom_tiplab(size = 3, alpha = 0.9) +
  geom_nodelab(aes(label=node))

#Read in Metadata
denv2_metadata <- read.csv("DENV2/final_seq_metadata_denv2.csv")
denv2_metadata$Site <- factor(denv2_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                              "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))


#DENV2 Tree plot. 
offset_bar = 10
offset_text = -12
denv2_tree_strictbioinf@data$posterior <- round(as.numeric(denv2_tree_strictbioinf@data$posterior), digits = 2)
denv2_tree_plot_strictbioinf <- ggtree(denv2_tree_strictbioinf, mrsd='2022-01-01',  size = 1.75,
                                  ladderize = T) + 
  geom_tiplab(size = 3, alpha = 1) +
  geom_nodelab(aes(label=posterior), nudge_y = 0.6, nudge_x = -2.5, size = 4, colour = 'black') +
  geom_tippoint(size = 2, alpha = 0.7) +
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
  geom_cladelab(node=394, label= "IV", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "lightsalmon") +
  geom_cladelab(node=380, label = "I", color='black', fontsize=28, offset.text = offset_text,linewidth = 6, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
  
  geom_hilight(node=648, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Kenya, Wajir and Mombasa
  geom_hilight(node=603, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
  geom_hilight(node=396, fill="#40B0A6", alpha=0.6, extend = 0.08) + #Kenya, Western
  
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

##Save as 45 x 30
#Plots/man1/denv2_bayes_filstersSA.pdf
denv2_tree_plot_strictbioinf
}
#******************************************************************************#

#******************************************************************************#
#* DENV1
{
  # Read tree
  denv1_tree <- treeio::read.beast("DENV1/beast/complex1_run1/denv1_beast_complex1_run_mcc.tree")

  # Meta data. Use the same regions csv so that we can match colours across all trees. 
  regions <- read.csv("Data/denv2_regions_coords.csv")
  regions_or <- ordered(regions[,1], levels = c(regions$Location[regions$Study_Site == 0], regions$Location[regions$Study_Site == 1]))
  regions <- regions[order(regions_or),]
  region_colours <- read.csv('manuscript_colours.csv')
  
  #Test plot
  test_tree <- ggtree(denv1_tree, mrsd='2022-01-01',  size = 1.25, #aes(color = Location),
         ladderize = T) + 
    geom_tiplab(size = 3, alpha = 0.9) +
    geom_nodelab(aes(label=node), nudge_y = 1, nudge_x = 10)
  
  #Read in Metadata
  denv1_metadata <- read.csv("DENV1/final_seq_metadata_denv1.csv")
  denv1_metadata <- denv1_metadata[,-1]
  denv1_metadata$Site <- factor(denv1_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                                "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))
 
  #Test tree with metadata
  test_tree_genotypes <- test_tree %<+% denv1_metadata
  test_tree_genotypes + 
    geom_tippoint(aes(color = Genotype), size = 4) 
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey40', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#1AFF1A', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'))
  
  #DENV1 Tree plot. 
  offset.text_value = -8
  offset_value = -30

  denv1_tree_plot <- ggtree(denv1_tree, mrsd='2023-01-01',  size = 2, #aes(color = Location),
                            ladderize = T) + 
    geom_tippoint(aes(colour = Location), size = 12, alpha = 0.9) +
    layout_rectangular() +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years)") +
    ylim(c(0, 400)) + 
    scale_x_continuous(limits = c(1850, 2060), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
    
    #Label The Genotypes
    geom_cladelab(node=393, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
    geom_cladelab(node=338, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
    geom_cladelab(node=624, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
    geom_cladelab(node=347, label= "IV", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "lightsalmon") +
    geom_cladelab(node=540, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "mediumorchid1") +
    
    #Highlight the Kenya sites
    geom_hilight(node=339, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Lineage 1
    geom_hilight(node=658, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
    
    geom_vline(xintercept = 1875, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +

    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 20), 
                legend.text = element_text(size = 20)) +
    
    #Force axes to zero
    expand_limits(x = 0, y = -100) +

    # # Add arrows for time
     geom_segment(aes(x = 1957, y = 355, xend = 1957, yend = 137.5), linetype = 1, colour = "grey10",
                  arrow = arrow(length = unit(0.5, "cm"))) +
     annotate("text", x = 1957, y = 355, label = "(Kisumu Introduction\n 1956 [1953, 1960])", angle = 90, size = 17, hjust = 0) +
    
     geom_segment(aes(x = 1980, y = 355, xend = 2015, yend = 25), linetype = 1, colour = "grey10",
              arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("text", x = 1980, y = 355, label = "(Ukunda Introduction\n 2017 [2017, 2018])", angle = 90, size = 17, hjust = 0)
  
  ##Save as 45 x 30
  denv1_tree_plot
  
  #Add the subtrees. for the three lineages
  #Lineage 1: Node 339
  denv1_tree_subset_L1 <- tree_subset(denv1_tree, node = 339, levels_back = 1)
  denv1_tree_subset_L1_plot <- ggtree(denv1_tree_subset_L1, mrsd='2016-01-01',  size = 1.25, #aes(color = Location),
                                      ladderize = T) + 
    geom_tiplab(size = 20, alpha = 0.9, offset = 1, align = T, colour = 'black') +
    geom_tippoint(aes(colour = Location), size = 15, alpha = 0.9) +
    layout_rectangular() +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years") +
    scale_x_continuous(breaks = c(1940, 1960, 1980, 2000, 2020), limits = c(1930, 2200)) +
    geom_vline(xintercept = 1940, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 1960, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 40), 
                axis.text.x = element_text(size = 48, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 40), 
                legend.text = element_text(size = 40)) 
  
  denv1_tree_subset_L1_plot
  #PDF 20 x 15
  
  #Lineage 2: Node 507
  denv1_tree_subset_L2 <- tree_subset(denv1_tree, node = 658, levels_back = 1)
  denv1_tree_subset_L2_plot <- ggtree(denv1_tree_subset_L2, mrsd='2020-01-01',  size = 1.25, #aes(color = Location),
                                      ladderize = T) + 
    geom_tiplab(size = 20, alpha = 0.9, offset = 1, align = T, colour = 'black') +
    geom_tippoint(aes(colour = Location), size = 15, alpha = 0.7) +
    layout_rectangular() +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years") +
    scale_x_continuous(breaks = c(2000, 2010, 2020), limits = c(2000, 2080)) +
    geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
    #geom_vline(xintercept = 2005, color = 'black', size = 0.1, alpha = 0.6) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
    #geom_vline(xintercept = 2015, color = 'black', size = 0.1, alpha = 0.6) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 40), 
                axis.text.x = element_text(size = 48, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 40), 
                legend.text = element_text(size = 40)) 
  
  denv1_tree_subset_L2_plot
  #PDF 20 x 15
  
  ## Plot Legend
  denv1_tree_plot_legend <- ggtree(denv1_tree, mrsd='2022-01-01',  size = 1.25,
                                   ladderize = T) + 
    layout_rectangular() +
    geom_tippoint(aes(color = Location), size = 3, alpha = 0.7) +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years") +
    ylim(c(0, 440)) + 
    scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000, 2010, 2020)) +
    theme_tree2(plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 40), 
                legend.text = element_text(size = 30)) +
    guides(colour = guide_legend(override.aes = list(size=12)))
  
  denv1_tree_plot_legend
  
  #Save legend as 20 x 20
  
  # Supplementary material
  offset_bar <- 30
  denv1_tree@data$posterior <- round(as.numeric(denv1_tree@data$posterior), digits = 2)
  denv1_tree_plot_supp <- ggtree(denv1_tree, mrsd='2022-01-01',  size = 1.75, aes(color = Location),
                                 ladderize = T) + 
    geom_tiplab(size = 6, alpha = 1) +
    geom_nodelab(aes(label=posterior), nudge_y = 2.5, nudge_x = -2, size = 8, color = 'black') +
    geom_tippoint(aes(fill = Location), size = 3, alpha = 0.7) +
    layout_rectangular() +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years)") +
    scale_x_continuous(limits = c(1850, 2125), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
    
    #Label The Genotypes
    geom_cladelab(node=393, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
    geom_cladelab(node=338, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
    geom_cladelab(node=624, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#4F94CD") +
    geom_cladelab(node=347, label= "IV", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "lightsalmon") +
    geom_cladelab(node=540, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
    
    #Highlight the Kenya sites
    geom_hilight(node=339, fill="#DC3220", alpha=0.6, extend = 0.0335) + #Lineage 1
    geom_hilight(node=658, fill="#005AB5", alpha=0.6, extend = 0.017) + #Kenya, Coastal
    
    geom_vline(xintercept = 1875, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 20), 
                legend.text = element_text(size = 20))
  
  denv1_tree_plot_supp #PDF 60 x 40
  #Plots/man2/denv1_bayes_supp.pdf
}
#
#******************************************************************************#

#******************************************************************************#
#*
#*#* DENV3
{
  # Read tree
  denv3_tree <- read.beast("DENV3/beast/complex1_run1/denv3_beast_complex1_run_mcc.tree")
  
  # Meta data. Use the same regions csv so that we can match colours across all trees. 
  regions <- read.csv("Data/denv2_regions_coords.csv")
  regions_or <- ordered(regions[,1], levels = c(regions$Location[regions$Study_Site == 0], regions$Location[regions$Study_Site == 1]))
  regions <- regions[order(regions_or),]
  region_colours <- read.csv('manuscript_colours.csv')
  
  #Test plot
  test_tree <- ggtree(denv3_tree, mrsd='2022-01-01',  size = 1.25, #aes(color = Location),
                      ladderize = T) + 
    geom_tiplab(size = 3, alpha = 0.9) +
    geom_nodelab(aes(label=node), nudge_y = 1, nudge_x = -10)
  
  #Read in Metadata
  denv3_metadata <- read.csv("DENV3/final_seq_metatyping_denv3.csv")
  denv3_metadata <- denv3_metadata[,-1]
  denv3_metadata$Site <- factor(denv3_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                                "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))
  
  #Test tree with metadata
  test_tree_genotypes <- test_tree %<+% denv3_metadata
  test_tree_genotypes + 
    geom_tippoint(aes(color = Genotype), size = 4) +
    scale_x_continuous(limits = c(1850, 2080), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) 
  
  # Fix posterior labels
  denv3_tree@data$posterior <- round(as.numeric(denv3_tree@data$posterior), digits = 2)
  
  #DENV3 Tree plot. 
  offset.text_value = -16
  offset_value = -34
  
  denv3_tree_plot <- ggtree(denv3_tree, mrsd='2021-01-01',  size = 2, #aes(color = Location),
                            ladderize = T) + 
    geom_tippoint(aes(colour = Location), size = 12, alpha = 0.9) +
    layout_rectangular() +
    #geom_tiplab(size = 3, alpha = 0.9) +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years)") +
    ylim(c(0, 470)) +        
    scale_x_continuous(limits = c(1900, 2060), breaks = c(1900, 1950, 2000, 2010, 2020)) +
    
    #Label The Genotypes
    geom_cladelab(node=631, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
    geom_cladelab(node=358, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
    geom_cladelab(node=437, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
    geom_cladelab(node=622, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "mediumorchid1") +
    
    #Highlight the Kenya sites
    geom_hilight(node=623, fill="#DC3220", alpha=0.6, extend = 3) + #Lineage 1
    geom_hilight(node=514, fill="#005AB5", alpha=0.6, extend = 5) + #Kenya, Coastal
    
    geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +
    
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 20), 
                legend.text = element_text(size = 20)) +
    
    #Force axes to zero
    #expand_limits(x = 0, y = -100) + 
    
    # # Add arrows for time
    geom_segment(aes(x = 1990, y = 370, xend = 2015, yend = 211), linetype = 1, colour = "grey10",
                 arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("text", x = 1990, y = 370, label = "(Mombasa Introduction\n 2015 [2015, 2017])", angle = 90, size = 18, hjust = 0) +
    
    geom_segment(aes(x = 1960, y = 370, xend = 2012, yend = 11.5), linetype = 1, colour = "grey10",
                 arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("text", x = 1960, y = 370, label = "(Chulaimbo Introduction\n 2011 [2010, 2013])", angle = 90, size = 18, hjust = 0)
  
  ##Save as 45 x 32.5
  denv3_tree_plot
  #Plots/man2/denv3_bayes_main.pdf
  
  #Add the subtrees. for the three lineages
  #Lineage 1: Node 623
  denv3_tree_subset_L1 <- tree_subset(denv3_tree, node = 623, levels_back = 1)
  denv3_tree_subset_L1_plot <- ggtree(denv3_tree_subset_L1, mrsd='2015-01-01',  size = 1.25, #aes(color = Location),
                                      ladderize = T) + 
    geom_tiplab(size = 20, alpha = 0.9, offset = 1, align = T, colour = 'black') +
    geom_tippoint(aes(colour = Location), size = 15, alpha = 0.9) +
    layout_rectangular() +
    #geom_nodelab(aes(label=posterior), nudge_y = 0.2, nudge_x = -2, size = 8, colour = 'black') +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years") +
    scale_x_continuous(breaks = c(1940, 1960, 1980, 2000, 2020), limits = c(1930, 2160)) +
    geom_vline(xintercept = 1940, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 1960, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 40), 
                axis.text.x = element_text(size = 48, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 40), 
                legend.text = element_text(size = 40)) 
  
  denv3_tree_subset_L1_plot
  Plots/man2/denv3_bayes_L1.pdf
  #PDF 25 x 15
  
  #Lineage 2: Node 514
  denv3_tree_subset_L2 <- tree_subset(denv3_tree, node = 514, levels_back = 1)
  denv3_tree_subset_L2_plot <- ggtree(denv3_tree_subset_L2, mrsd='2019-01-01',  size = 1.25, #aes(color = Location),
                                      ladderize = T) + 
    geom_tiplab(size = 20, alpha = 0.9, offset = 1, align = T, colour = 'black') +
    geom_tippoint(aes(colour = Location), size = 15, alpha = 0.9) +
    layout_rectangular() +
    #geom_nodelab(aes(label=posterior), nudge_y = 0.1, nudge_x = -1, size = 8, colour = 'black') +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years") +
    scale_x_continuous(breaks = c(2000, 2010, 2020), limits = c(2000, 2100)) +
    geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
    #geom_vline(xintercept = 2005, color = 'black', size = 0.1, alpha = 0.6) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
    #geom_vline(xintercept = 2015, color = 'black', size = 0.1, alpha = 0.6) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 40), 
                axis.text.x = element_text(size = 48, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 40), 
                legend.text = element_text(size = 40)) 
  
  denv3_tree_subset_L2_plot
  #25 x 15
  

  # Supplementary material
  offset_bar <- 30
  denv3_tree_plot_supp <- ggtree(denv3_tree, mrsd='2021-01-01',  size = 1.75, aes(color = Location),
                                 ladderize = T) + 
    geom_tiplab(size = 6, alpha = 1) +
    geom_nodelab(aes(label=posterior), nudge_y = 2.5, nudge_x = -2, size = 6, color = 'black') +
    geom_tippoint(aes(fill = Location), size = 3, alpha = 0.7) +
    layout_rectangular() +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years)") +
    scale_x_continuous(limits = c(1850, 2125), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
    
    #Label The Genotypes
    geom_cladelab(node=631, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
    geom_cladelab(node=358, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
    geom_cladelab(node=437, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#4F94CD") +
    geom_cladelab(node=622, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "mediumorchid1") +
    
    #Highlight the Kenya sites
    geom_hilight(node=623, fill="#DC3220", alpha=0.6, extend = 50) + #Lineage 1
    geom_hilight(node=514, fill="#005AB5", alpha=0.6, extend = 70) + #Kenya, Coastal
    
    geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 20), 
                legend.text = element_text(size = 20))
  
  denv3_tree_plot_supp #PDF 60 x 40
}
#
#******************************************************************************#

#******************************************************************************#
#*
#*DENV4
{
  # Read tree
  denv4_tree <- read.beast("DENV4/beast/complex2_run/denv4_beast_complex2_run123_mcc.tree")
  
  # Meta data. Use the same regions csv so that we can match colours across all trees. 
  regions <- read.csv("Data/denv2_regions_coords.csv")
  regions_or <- ordered(regions[,1], levels = c(regions$Location[regions$Study_Site == 0], regions$Location[regions$Study_Site == 1]))
  regions <- regions[order(regions_or),]
  region_colours <- read.csv('manuscript_colours.csv')
  
  #Test plot
  test_tree <- ggtree(denv4_tree, mrsd='2022-01-01',  size = 1.25, #aes(color = Location),
                      ladderize = T) + 
    geom_tiplab(size = 3, alpha = 0.9) +
    geom_nodelab(aes(label=node), nudge_y = 1, nudge_x = -10)
  
  #Read in Metadata
  denv4_metadata <- read.csv("DENV4/final_seq_metatyping_denv4.csv")
  denv4_metadata <- denv4_metadata[,-1]
  denv4_metadata$Site <- factor(denv4_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                                "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))
  
  #Test tree with metadata
  test_tree_genotypes <- test_tree %<+% denv4_metadata
  test_tree_genotypes + 
    geom_tippoint(aes(color = Genotype), size = 4) +
    scale_x_continuous(limits = c(1000, 2080), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) 
  
  # Fix posterior labels
  denv4_tree@data$posterior <- round(as.numeric(denv4_tree@data$posterior), digits = 2)
  
  #DENV3 Tree plot. 
  offset.text_value = -16
  offset_value = -34
  denv4_tree_subset <- tree_subset(denv4_tree, node = 208, levels_back = 1)
  denv4_tree_plot <- ggtree(denv4_tree, mrsd='2021-01-01',  size = 2, #aes(color = Location),
                            ladderize = T) + 
    geom_tippoint(aes(colour = Location), size = 12, alpha = 0.9) +
    layout_rectangular() +
    #geom_tiplab(size = 3, alpha = 0.9) +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years)") +
    ylim(c(0, 250)) +        
    scale_x_continuous(limits = c(1800, 2060), breaks = c(1900, 1950, 2000, 2010, 2020)) +
    
    #Label The Genotypes
    geom_cladelab(node=334, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
    geom_cladelab(node=208, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
    
    #Highlight the Kenya sites
    geom_hilight(node=328, fill="#DC3220", alpha=0.6, extend = 1) + #Lineage 1

    geom_vline(xintercept = 1800, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1850, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +
    
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 20), 
                legend.text = element_text(size = 20)) + 
    
    #Force axes to zero
    #expand_limits(x = 0, y = -100) + 
    
    # # Add arrows for time
    geom_segment(aes(x = 1990, y = 210, xend = 2015, yend = 125), linetype = 5, colour = "grey10",
                 arrow = arrow(length = unit(0.5, "cm"))) +
    annotate("text", x = 1990, y = 210, label = "(Kisumu Introduction\n 2015 [2015, 2015])", angle = 90, size = 18, hjust = 0)
  
  ##Save as 45x32.5
  #Plots/man2/denv4_bayes_main.pdf
  denv4_tree_plot
  
  #Change the colour of OCeania for visibility. 
  region_colours$V1[which(region_colours$V2 == 'Oceania')] <- '#E4AF69FF'
  
  #Add the subtrees. for the three lineages
  #Lineage 1: Node 328
  denv4_tree_subset_L1 <- tree_subset(denv4_tree, node = 247, levels_back = 0)
  denv4_tree_subset_L1_plot <- ggtree(denv4_tree_subset_L1, mrsd='2015-01-01',  size = 1.25, #aes(color = Location),
                                      ladderize = T) %>% #+ geom_nodelab(aes(label=node), nudge_y = 0.2, nudge_x = -2, size = 8, colour = 'black')

    collapse(node = 93) +
    geom_tiplab(size = 20, alpha = 0.9, offset = 1, align = T, colour = 'black') +
    geom_tippoint(aes(colour = Location), size = 15, alpha = 0.9) +
    layout_rectangular() +
    geom_point2(aes(subset=(node == 93)), size=10, shape=15, fill="steelblue") + 
    #geom_nodelab(aes(label=posterior), nudge_y = 0.2, nudge_x = -2, size = 8, colour = 'black') +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years") +
    scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), limits = c(1950, 2150)) +
    geom_vline(xintercept = 1960, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 1980, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 0.2) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 0.2) +
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 40), 
                axis.text.x = element_text(size = 48, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 40), 
                legend.text = element_text(size = 40)) 
  
  denv4_tree_subset_L1_plot
  #PDF 20 x 15
  #Plots/man2/denv4_bayes_L1.pdf
  
  # Supplementary material
  offset_bar <- 50
  denv4_tree_plot_supp <- ggtree(denv4_tree, mrsd='2021-01-01',  size = 1.75, aes(color = Location),
                                 ladderize = T) + 
    geom_tiplab(size = 6, alpha = 1) +
    geom_nodelab(aes(label=posterior), nudge_y = 1, nudge_x = -2, size = 6, color = 'black') +
    geom_tippoint(aes(fill = Location), size = 3, alpha = 0.7) +
    layout_rectangular() +
    scale_color_manual(values = region_colours[,1], breaks = region_colours[,2], labels = region_colours[,3]) + 
    xlab("Time (Years)") +
    scale_x_continuous(limits = c(1800, 2125), breaks = c(1875, 1900, 1950, 2000, 2010, 2020)) +
    
    #Label The Genotypes
    geom_cladelab(node=334, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#CD3333") + 
    geom_cladelab(node=208, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                  align = TRUE, angle = 90, hjust = 0.5, offset = offset_bar, barsize = 8, barcolour = "#00CD00") +
    
    #Highlight the Kenya sites
    geom_hilight(node=328, fill="#DC3220", alpha=0.6, extend = 1) + #Lineage 1
    
    geom_vline(xintercept = 1800, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1850, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1900, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 1950, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2000, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2010, color = 'black', size = 0.1, alpha = 1) +
    geom_vline(xintercept = 2020, color = 'black', size = 0.1, alpha = 1) +
    
    theme_tree2(legend.position="none", 
                plot.title = element_text(hjust = 0.9, size = 20), 
                axis.text.x = element_text(size = 56, angle = 90, vjust = 0.5), 
                legend.title = element_text(size = 20), 
                legend.text = element_text(size = 20))
  
  denv4_tree_plot_supp #PDF 60 x 40
  #Plots/man2/denv4_bayes_supp.pdf
}
#
#******************************************************************************#

#******************************************************************************#
#* CODE DUMP
# Try other colours
#colours_global <- paletteer_d("ggsci::category20b_d3")
#colours_global <- colours_global[1:16]

#colours_local <- paletteer_d("ggthemes::gdoc") 
#colours_local <- colours_local[1:10]
# study_sites <- regions$Location[regions$Study_Site == 1]
# other_sites <- regions$Location[regions$Study_Site == 0]
# colour_values <- list(name = unlist(c(study_sites, other_sites)),
#                       values = unlist(c(colours_local, colours_global)))


#BA6338FF #A9A9A9FF #5050FFFF #F0E685FF #5A655EFF #CC9900FF #AE1F63FF #D595A7FF #924822FF #749B58FF #CE3D32FF #7A65A5FF #466983FF #612A79FF #CC9900FF #E7C76FFF #C75127FF #6BD76BFF #D58F5CFF #99CC00FF #CDDEB7FF #E4AF69FF #837B8DFF #802268FF #3B1B53FF #5DB1DDFF 


region_colours <- as.data.frame(matrix(nrow = 26, ncol = 2))
region_colours[,1] <- regions[,1]
for (i in 1:26) {
  region_colours[i,2] <- colours[[i]]
}
#******************************************************************************#

#******************************************************************************#
#*
"Tell all the truth but tell it slant —
Success in Circuit lies
Too bright for our infirm Delight
The Truth's superb surprise
As Lightning to the Children eased
With explanation kind
The Truth must dazzle gradually
Or every man be blind —"

Emily Dickinson

