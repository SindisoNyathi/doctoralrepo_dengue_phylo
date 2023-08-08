#*****************************************************************************#
  #06/01/2022
  #Sindiso Nyathi
  #Goal: Annottate and Visualize ML Trees. Work on each serotype seperately and combine
  # in powerpoint. 
  #Methods: 
  #'The World Shall Know True Art' 
  #******************************************************************************#
  
  #******************************************************************************#
  #Preliminaries
  #Set Working Directory
  setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

#Load or install required pacakges. 
#require('wesanderson') # For this project we will use darjeeling 1 and 2.
#use to look up names wes_palette("name"Darjeeling1, 5, type = c("discrete"))
#Names
#Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
#Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
#library("ape")
library("ggplot2")
library("ggtree")
require('phytools')
require('ggforce')
library(ggrepel)
require('phangorn')
require('colorspace')
require(paletteer)
require(treeio)
#******************************************************************************#

#******************************************************************************#
#*
# DENV2
{
#*#For the complete plot. 
#*Read in the tree
denv2_raxml_tree <- read.tree("DENV2/ml/raxml_.raxml.support")
rooted_raxml_tree<- phangorn::midpoint(denv2_raxml_tree)

#Read in Metadata
denv2_metadata <- read.csv("DENV2/final_seq_metadata_denv2.csv")
denv2_metadata$Site <- factor(denv2_metadata$Site, levels = c("Western Kenya (Study)", "Coastal Kenya (Study)", 
                                                              "Central Kenya (Non-Study)", "Coastal Kenya (Non-Study)", 'External'))
# Examine original tree
test_tree <- ggtree(rooted_raxml_tree, branch.length='rate', ladderize = T, size = 1) +
  geom_tiplab(size = 3, alpha = 0.9) +
  xlim(c(0, 0.1)) +
  geom_nodelab(aes(label=node), size = 1) + geom_treescale(x=0.2, y=-20, width=0.1, color='black', offset = 5, linesize = 1.5, fontsize = 14)

test_tree_genotypes <- test_tree %<+% denv2_metadata
test_tree_genotypes + 
  geom_tippoint(aes(color = Genotype), size = 1) +
  geom_treescale(x=0.01) +
  xlim(c(0, 0.1))
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey40', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#1AFF1A', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'))

#Visualize to get nodes
ggtree(rooted_raxml_tree, branch.length='rate', ladderize = T, size = 1) +
  #geom_tippoint(size = 1, alpha = 0.9) +
  geom_tiplab(size = 1, alpha = 0.9) +
  geom_nodelab(aes(label=node), size = 1)

offset.text_value = 0.004
offset_value = -0.00

#Complete tree
offset_bar = -37.5
offset_text = -12
no_annot <- ggtree(rooted_raxml_tree, branch.length = 'rate', ladderize = T, size = 2.25) + #,  layout = "fan", open.angle=120) +
  geom_tippoint(size = 8, alpha = 0.9) +
  #ggtitle("Maximum Likelihood Tree of Global DENV2 Sequences") +
  #geom_nodelab(aes(label=node), nudge_y = 10, nudge_x = -0.01) +
  #geom_tiplab(size = 4, alpha = 0.9) +
  #geom_nodepoint(aes(color = as.numeric(label)), size=5) +
  #colorspace::scale_color_continuous_sequential(name = "Bootstrap\nSupport Value (%)", palette = "Heat") +
  geom_treescale(x=0.03, y=-10, width=0.025, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +

  #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
  #Highlight Relevant nodes
  geom_hilight(node=470, fill="#DC3220", alpha=0.4, extend = 0.02475) + #Kenya, Wajir and Mombasa
  geom_hilight(node=664, fill="#005AB5", alpha=0.4, extend = 0.0005) + #Kenya, Coastal
  geom_hilight(node=620, fill="#40B0A6", alpha=0.4, extend = 0.065) + #Kenya,Western
  
  #Scale
  xlim(c(0, 0.105)) + 
  #ylim(c(0, 400)) + 
  #Label The Genotypes
   geom_cladelab(node=471, label= "II", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
   geom_cladelab(node=474, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
   geom_cladelab(node=568, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
   geom_cladelab(node=618, label= "IV", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                 align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "lightsalmon") +
   geom_cladelab(node=639, label= "I", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "mediumorchid1") +
  #geom_cladelab(node=789, label= "VI", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
   #             align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 4, barcolour = "black") +
 
  theme_tree2(legend.position="none", 
    plot.title = element_text(hjust = 0.5, size = 40), 
    legend.title = element_text(size = 40), 
    legend.text = element_text(size = 32), 
    #legend.position = c(.2, 0.75),
    legend.key.width = unit(1.25, "cm"),
    legend.key.height = unit(2, "cm"),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

no_annot_data <- no_annot %<+% denv2_metadata
no_annot_data_label <- no_annot_data + 
  geom_tippoint(aes(color = Site), size = 5) +
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey40', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#1AFF1A', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'), 
                     labels = c("External", "Coastal Kenya", 
                                "Western Kenya", "Coastal Kenya", 
                                "Central Kenya"))

plot(no_annot_data_label) #PDF 35 x 25

jpeg("Plots/denv2_ml_tree.jpg", width = 600, height = 700)
plot(no_annot) 
dev.off()

#Lineage 1. 
rooted_raxml_tree_subset_lineage_1 <- tree_subset(rooted_raxml_tree, node = 450, levels_back = 3)
rooted_raxml_tree_subset_lineage_1_plot <- ggtree(rooted_raxml_tree_subset_lineage_1, branch.length='rate', ladderize = T, size = 1) + #geom_tiplab2(size = 3, align = T) +
  ggtitle("Kenya Clades: Lineage 1") +
  geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
  theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
    legend.position = "none",
    axis.text.x=element_blank(),
    axis.text.y=element_blank(), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank())

rooted_raxml_tree_subset_lineage_1_plot_nodes <- rooted_raxml_tree_subset_lineage_1_plot %<+% denv2_metadata
rooted_raxml_tree_subset_lineage_1_plot_nodes_label <- rooted_raxml_tree_subset_lineage_1_plot_nodes +   xlim(0, 0.08) + 
  geom_tiplab(size = 16, offset = 0.001, align = T) + 
  geom_tippoint(aes(color = Site), size = 14) +
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey80', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#DC3220', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'))

plot(rooted_raxml_tree_subset_lineage_1_plot_nodes_label) #20 x 20

#Lineage 2. 
rooted_raxml_tree_subset_lineage_2 <- tree_subset(rooted_raxml_tree, node = 662,  levels_back = 3)
rooted_raxml_tree_subset_lineage_2_plot <- ggtree(rooted_raxml_tree_subset_lineage_2, branch.length='rate', ladderize = T, size = 1) + # geom_tiplab2(size = 3, align =T) +
  ggtitle("Kenya Clades: Lineage 2") +
  geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
  theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
              legend.position = "none",
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())

rooted_raxml_tree_subset_lineage_2_plot_nodes <- rooted_raxml_tree_subset_lineage_2_plot %<+% denv2_metadata
rooted_raxml_tree_subset_lineage_2_plot_nodes_label <- rooted_raxml_tree_subset_lineage_2_plot_nodes +
  geom_tiplab(size = 12, offset = 0.001, align = T) + 
  xlim(c(0, 0.07)) +
  geom_tippoint(aes(color = Site), size = 14) +
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey80', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#DC3220', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'), drop = FALSE)

plot(rooted_raxml_tree_subset_lineage_2_plot_nodes_label) 


#Lineage 3. 
rooted_raxml_tree_subset_lineage_3 <- tree_subset(rooted_raxml_tree, node = 620, levels_back = 1)
rooted_raxml_tree_subset_lineage_3_plot <- ggtree(rooted_raxml_tree_subset_lineage_3, branch.length='rate', ladderize = T, size = 1) + # geom_tiplab2(size = 3, align =T) +
  ggtitle("Kenya Clades: Lineage 3") +
  geom_treescale(y = -1, width=0.004, color='black', linesize = 1.5, fontsize = 10) + 
  theme_tree2(plot.title = element_text(hjust = 0.5, size = 40), 
              legend.position = "none",
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())

rooted_raxml_tree_subset_lineage_3_plot_nodes <- rooted_raxml_tree_subset_lineage_3_plot %<+% denv2_metadata
rooted_raxml_tree_subset_lineage_3_plot_nodes_label <- rooted_raxml_tree_subset_lineage_3_plot_nodes + 
  xlim(c(0, 0.01)) + geom_tiplab(size = 12, offset = 0.001, align = T) + 
  geom_tippoint(aes(color = Site), size = 14) +
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey80', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#DC3220', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'), drop = FALSE)

plot(rooted_raxml_tree_subset_lineage_3_plot_nodes_label)

#Legend. 
rooted_raxml_tree_subset_legend <- ggtree(rooted_raxml_tree, branch.length='rate', ladderize = T, size = 1) + # geom_tiplab2(size = 3, align =T) +
  #ggtitle("Kenya Clades: Lineage 3") +
  xlim(c(0, 0.07)) +
  theme_tree2(plot.title = element_text(hjust = 0.5, size = 40), 
              legend.title = element_text(size = 34), 
              legend.text = element_text(size = 26), 
              legend.position = c(.2, 0.75),
              legend.key.width = unit(1.25, "cm"),
              legend.key.height = unit(2, "cm"),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())

rooted_raxml_tree_subset_legend_nodes <- rooted_raxml_tree_subset_legend %<+% denv2_metadata
rooted_raxml_tree_subset_legend_nodes_plot <- rooted_raxml_tree_subset_legend_nodes + geom_tiplab(size = 10, hjust = -0.06) + geom_tippoint(aes(color = Site), size = 10) +
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey80', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#DC3220', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'), drop = FALSE) + 
  guides(colour = guide_legend(override.aes = list(size=20)))
  

plot(rooted_raxml_tree_subset_legend_nodes_plot)

#Supplementary Material Plot
offset_value <- 0.022
offset.text_value = 0.003
supp_plot <- ggtree(rooted_raxml_tree, branch.length = 'rate', ladderize = T, size = 1) + #,  layout = "fan", open.angle=120) +
  #geom_tippoint(size = 1, alpha = 0.9) +
  ggtitle("Maximum Likelihood Tree of Global DENV2 Sequences") +
  #geom_label_repel(aes(label=label, fill=label)) + 
  #geom_tiplab(size = 4, alpha = 0.9) +
  #geom_nodepoint(aes(color = as.numeric(label)), size=5) +
  #colorspace::scale_color_continuous_sequential(name = "Bootstrap\nSupport Value (%)", palette = "Heat") +
  geom_treescale(x=0.05, y=-10, width=0.01, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +
  
  #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
  #Highlight Relevant nodes
  #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
  #Highlight Relevant nodes
  geom_hilight(node=470, fill="#DC3220", alpha=0.4, extend = 0.02475) + #Kenya, Wajir and Mombasa
  geom_hilight(node=664, fill="#005AB5", alpha=0.4, extend = 0.0005) + #Kenya, Coastal
  geom_hilight(node=620, fill="#40B0A6", alpha=0.4, extend = 0.065) + #Kenya,Western
  
  #Scale
  xlim(c(0, 0.15)) + 
  
  #Label The Genotypes
  geom_cladelab(node=471, label= "II", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 4, barcolour = "#CD3333") + 
  geom_cladelab(node=474, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 4, barcolour = "#00CD00") +
  geom_cladelab(node=568, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 4, barcolour = "#4F94CD") +
  geom_cladelab(node=618, label= "IV", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 4, barcolour = "lightsalmon") +
  geom_cladelab(node=639, label= "I", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 4, barcolour = "mediumorchid1") +
  
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

supp_plot_data <- supp_plot %<+% denv2_metadata
supp_plot_data_label <- supp_plot_data + 
  #xlim(c(0, 0.13)) + 
  geom_tiplab(size = 4, offset = 0.001) + 
  geom_tippoint(aes(color = Site), size = 4) +
  geom_nodelab(aes(label=label), nudge_y = 1, nudge_x = -0.003) +
  scale_color_manual(name = "Study Sites", values = c('External' = 'grey80', "Coastal Kenya (Non-Study)" = '#1AFF1A', 
                                                      "Western Kenya (Study)" = '#FFC20A', "Coastal Kenya (Study)" = '#DC3220', 
                                                      "Central Kenya (Non-Study)" = '#1A85FF'), drop = FALSE)

plot(supp_plot_data_label) #55 x 30

#Kenya nodes.
}
#******************************************************************************#

#******************************************************************************#
#* DENV1
{
#*#For the complete plot. 
#*Read in the tree
denv1_raxml_tree <- read.tree("DENV1/ml/denv1_raxml_.raxml.support")
rooted_raxml_tree_d1 <- phangorn::midpoint(denv1_raxml_tree)

denv1_metadata <- read.csv("DENV1/final_seq_metadata_denv1.csv")
denv1_metadata <- denv1_metadata[,-1]

colours <- sample(paletteer_d("ggsci::default_igv", 7))
colours <- paletteer_d("ggsci::default_ucscgb", 7)

test_tree <- ggtree(rooted_raxml_tree_d1, branch.length='rate', ladderize = T, size = 1) +
  geom_tiplab(size = 3, alpha = 0.9) +
  xlim(c(0, 0.1)) +
  geom_nodelab(aes(label=node), size = 8) + 
  geom_treescale(x=0.2, y=-20, width=0.1, color='black', offset = 5, linesize = 1.5, fontsize = 14)


# Create new frogdata with same number of species as frogtree.2
test_tree %<+% denv1_metadata +
  geom_tippoint(aes(colour = Site), size = 4) +
  scale_colour_manual(values = colours) 


offset_value = 0.004
offset.text_value = 0.004
no_annot <- ggtree(rooted_raxml_tree_d1, branch.length = 'rate', ladderize = T, size = 2.25) + 
  #geom_tippoint(size = 8, alpha = 0.9) +
  geom_treescale(x=0.03, y=-10, width=0.025, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +

  #Highlight Relevant Kenya nodes
  geom_hilight(node=473, fill="#DC3220", alpha=0.4, extend = 0.008) + #Kenya, Wajir and Mombasa
  geom_hilight(node=569, fill="#005AB5", alpha=0.4, extend = 0.0005) + #Kenya, Coastal
  
  #Scale
  xlim(c(0, 0.09)) + 

  #Label The Genotypes
  geom_cladelab(node=434, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
  geom_cladelab(node=569, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
  geom_cladelab(node=440, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
  geom_cladelab(node=577, label= "IV", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "lightsalmon") +
  geom_cladelab(node=480, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "mediumorchid1") +

  theme_tree2(legend.position="none", 
              plot.title = element_text(hjust = 0.5, size = 40), 
              legend.title = element_text(size = 40), 
              legend.text = element_text(size = 32), 
              #legend.position = c(.2, 0.75),
              legend.key.width = unit(1.25, "cm"),
              legend.key.height = unit(2, "cm"),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())

no_annot_data <- no_annot %<+% denv1_metadata
no_annot_data_label <- no_annot_data + 
  geom_tippoint(aes(colour = Site), size = 5) + 
  scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220', 
                                                       "Coastal Kenya (Study site)" = '#1AFF1A'), 
                    labels = c("External", "Coastal Kenya", 
                               "Western Kenya"))

plot(no_annot_data_label) #PDF 35 x 25
  
# Lineage 1. 
rooted_raxml_tree_subset_lineage_1 <- tree_subset(rooted_raxml_tree_d1, node = 473, levels_back = 2)
rooted_raxml_tree_subset_lineage_1_plot <- ggtree(rooted_raxml_tree_subset_lineage_1, branch.length='rate', ladderize = T, size = 1) + #geom_tiplab2(size = 3, align = T) +
  ggtitle("Kenya Clades: Lineage 1") +
  geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
  theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
              legend.position = "none",
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())

rooted_raxml_tree_subset_lineage_1_plot_nodes <- rooted_raxml_tree_subset_lineage_1_plot %<+% denv1_metadata
rooted_raxml_tree_subset_lineage_1_plot_nodes_label <- rooted_raxml_tree_subset_lineage_1_plot_nodes +   xlim(0, 0.08) + 
  geom_tippoint(aes(colour = Site), size = 8) + 
  geom_tiplab(size = 16, offset = 0.001, align = T) + 
  geom_tippoint(aes(color = Site), size = 14) +
  scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220', 
                                                       "Coastal Kenya (Study site)" = '#1AFF1A'), 
                      labels = c("External", "Coastal Kenya", 
                                 "Western Kenya"))

plot(rooted_raxml_tree_subset_lineage_1_plot_nodes_label) #15 x 15

# Lineage 2. 
rooted_raxml_tree_subset_lineage_2 <- tree_subset(rooted_raxml_tree_d1, node = 569, levels_back = 0)
rooted_raxml_tree_subset_lineage_2_plot <- ggtree(rooted_raxml_tree_subset_lineage_2, branch.length='rate', ladderize = T, size = 1) + #geom_tiplab2(size = 3, align = T) +
  ggtitle("Kenya Clades: Lineage 2") +
  geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
  theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
              legend.position = "none",
              axis.text.x=element_blank(),
              axis.text.y=element_blank(), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank())

rooted_raxml_tree_subset_lineage_2_plot_nodes <- rooted_raxml_tree_subset_lineage_2_plot %<+% denv1_metadata
rooted_raxml_tree_subset_lineage_2_plot_nodes_label <- rooted_raxml_tree_subset_lineage_2_plot_nodes +   xlim(0, 0.08) + 
  geom_tippoint(aes(colour = Site), size = 5) + 
  geom_tiplab(size = 16, offset = 0.001, align = T) + 
  geom_tippoint(aes(color = Site), size = 14) +
  scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220', 
                                                       "Coastal Kenya (Study site)" = '#1AFF1A'), 
                      labels = c("External", "Coastal Kenya", 
                                 "Western Kenya"))

plot(rooted_raxml_tree_subset_lineage_2_plot_nodes_label) #20 x 20

offset_value = 0.03
offset.text_value = 0.004
supp_plot <- ggtree(rooted_raxml_tree_d1, branch.length = 'rate', ladderize = T, size = 1) + #,  layout = "fan", open.angle=120) +
  #geom_tippoint(size = 1, alpha = 0.9) +
  ggtitle("Maximum Likelihood Tree of Global DENV1 Sequences") +
  #geom_label_repel(aes(label=label, fill=label)) + 
  geom_tiplab(size = 4, alpha = 0.9) +
  geom_nodelab(aes(label=label), size = 4, nudge_x = -0.002, nudge_y = 1) +
  #geom_nodepoint(aes(color = as.numeric(label)), size=5) +
  #colorspace::scale_color_continuous_sequential(name = "Bootstrap\nSupport Value (%)", palette = "Heat") +
  geom_treescale(x=0.05, y=-10, width=0.01, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +
  
  #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
  #Highlight Relevant nodes
  #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
  #Highlight Relevant Kenya nodes
  geom_hilight(node=473, fill="#DC3220", alpha=0.4, extend = 0.015) + 
  geom_hilight(node=569, fill="#005AB5", alpha=0.4, extend = 0.02) + 
  
  #Scale
  xlim(c(0, 0.15)) + 
  
  #Label The Genotypes
  geom_cladelab(node=434, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
  geom_cladelab(node=569, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
  geom_cladelab(node=440, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
  geom_cladelab(node=577, label= "IV", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "lightsalmon") +
  geom_cladelab(node=480, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "mediumorchid1") +
  
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

supp_annot_data <- supp_plot %<+% denv1_metadata
sup_plot_annot_data_label <- supp_annot_data + 
  geom_tippoint(aes(colour = Site), size = 5) + 
  scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220', 
                                                       "Coastal Kenya (Study site)" = '#1AFF1A'), 
                      labels = c("External", "Coastal Kenya", 
                                 "Western Kenya"))

plot(sup_plot_annot_data_label) #55 x 30
}
#******************************************************************************#

#******************************************************************************#
#*#* DENV3
{
    #*#For the complete plot. 
    #*Read in the tree
    denv3_raxml_tree <- read.tree("DENV3/ml/denv3_raxml_.raxml.support")
    rooted_raxml_tree_d3 <- phangorn::midpoint(denv3_raxml_tree)
    
    denv3_metadata <- read.csv("DENV3/final_seq_metatyping_denv3.csv")
    # Add a realID column to metadata
    denv3_metadata$RealID <- NA
    for (i in 1:nrow(denv3_metadata)){
      
      sequence_att <-  denv3_metadata$ID[i]
      sequence_att_sep <- strsplit(sequence_att,  split = '/', fixed = T)
      id <- sequence_att_sep[[1]][1]
      denv3_metadata$RealID[i] <-  id
    }
    
    denv3_metadata <- denv3_metadata[,-1]
    
    colours <- sample(paletteer_d("ggsci::default_igv", 8))
    colours <- paletteer_d("ggsci::default_ucscgb", 8)
    
    #Correct Node names
    for (i in c(1:length(denv3_raxml_tree$tip.label))){
      
      tip_attributes <- strsplit(denv3_raxml_tree$tip.label[i], '/')
      tipname <- tip_attributes[[1]][1]
      location <- tip_attributes[[1]][2]
      date <- tip_attributes[[1]][3]
      
      # Get metadata
      metadata_attributes <- denv3_metadata$ID[denv3_metadata$RealID == tipname]
      metadata_attributes <- strsplit(metadata_attributes, '/')
      metadata_location <- metadata_attributes[[1]][2]
      
      #Change location
      if (location != metadata_location) {
        newtipname <- paste(tipname, metadata_location, date, sep = '/')
        denv3_raxml_tree$tip.label[i] <- newtipname
      }
    }
    
    
    test_tree <- ggtree(rooted_raxml_tree_d3, branch.length='rate', ladderize = T, size = 1) +
      geom_tiplab(size = 3, alpha = 0.9) +
      xlim(c(0, 0.1)) +
      geom_nodelab(aes(label=node), size = 8) + 
      geom_treescale(x=0.2, y=-20, width=0.1, color='black', offset = 5, linesize = 1.5, fontsize = 14)
    
    
    # Create new frogdata with same number of species as frogtree.2
    test_tree %<+% denv3_metadata +
      geom_tippoint(aes(colour = Genotype), size = 4) +
      scale_colour_manual(values = colours) 
    
    
    offset_value = 0.004
    offset.text_value = 0.004
    no_annot <- ggtree(rooted_raxml_tree_d3, branch.length = 'rate', ladderize = T, size = 2.25) + 
      #geom_tippoint(size = 8, alpha = 0.9) +
      geom_treescale(x=0.03, y=-10, width=0.025, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +
      
      #Highlight Relevant Kenya nodes
      geom_hilight(node=435, fill="#DC3220", alpha=0.4, extend = 0.008) + #Kenya, Wajir and Mombasa
      geom_hilight(node=632, fill="#005AB5", alpha=0.4, extend = 0.0005) + #Kenya, Coastal
      
      #Scale
      xlim(c(0, 0.1)) + 
      
      #Label The Genotypes
      geom_cladelab(node=444, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
      geom_cladelab(node=521, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
      geom_cladelab(node=432, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
      geom_cladelab(node=435, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "lightsalmon") +

      theme_tree2(legend.position="none", 
                  plot.title = element_text(hjust = 0.5, size = 40), 
                  legend.title = element_text(size = 40), 
                  legend.text = element_text(size = 32), 
                  #legend.position = c(.2, 0.75),
                  legend.key.width = unit(1.25, "cm"),
                  legend.key.height = unit(2, "cm"),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(), 
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank())
    
    no_annot_data <- no_annot %<+% denv3_metadata
    no_annot_data_label <- no_annot_data + 
      geom_tippoint(aes(colour = Site), size = 5) + 
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study)" = '#DC3220', 
                                                           "Coastal Kenya (Study)" = '#1AFF1A', "Coastal Kenya (Non-study)" = '#1A85FF'), 
                          labels = c("External", "Coastal Kenya (Study Site)", 
                                     "Western Kenya", "Coastal Kenya (Non-Study Site)"))
    
    plot(no_annot_data_label) #PDF 35 x 25
    
    # Lineage 1. 
    rooted_raxml_tree_subset_lineage_1 <- tree_subset(rooted_raxml_tree_d3, node = 435, levels_back = 0)
    rooted_raxml_tree_subset_lineage_1_plot <- ggtree(rooted_raxml_tree_subset_lineage_1, branch.length='rate', ladderize = T, size = 1) + #geom_tiplab2(size = 3, align = T) +
      ggtitle("Kenya Clades: Lineage 1") +
      geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
      theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
                  legend.position = "none",
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(), 
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank())
    
    rooted_raxml_tree_subset_lineage_1_plot_nodes <- rooted_raxml_tree_subset_lineage_1_plot %<+% denv3_metadata
    rooted_raxml_tree_subset_lineage_1_plot_nodes_label <- rooted_raxml_tree_subset_lineage_1_plot_nodes +   xlim(0, 0.08) + 
      geom_tippoint(aes(colour = Site), size = 8) + 
      geom_tiplab(size = 16, offset = 0.001, align = T) + 
      geom_tippoint(aes(color = Site), size = 14) +
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study)" = '#DC3220', 
                                                           "Coastal Kenya (Study)" = '#1AFF1A', "Coastal Kenya (Non-study)" = '#1A85FF'), 
                          labels = c("External", "Coastal Kenya (Study Site)", 
                                     "Western Kenya", "Coastal Kenya (Non-Study Site)"))
    
    plot(rooted_raxml_tree_subset_lineage_1_plot_nodes_label) #15 x 15
    
    # Lineage 2. 
    rooted_raxml_tree_subset_lineage_2 <- tree_subset(rooted_raxml_tree_d3, node = 630, levels_back = 0)
    rooted_raxml_tree_subset_lineage_2_plot <- ggtree(rooted_raxml_tree_subset_lineage_2, branch.length='rate', ladderize = T, size = 1) + #geom_tiplab2(size = 3, align = T) +
      ggtitle("Kenya Clades: Lineage 2") +
      geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
      theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
                  legend.position = "none",
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(), 
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank())
    
    rooted_raxml_tree_subset_lineage_2_plot_nodes <- rooted_raxml_tree_subset_lineage_2_plot %<+% denv3_metadata
    rooted_raxml_tree_subset_lineage_2_plot_nodes_label <- rooted_raxml_tree_subset_lineage_2_plot_nodes +   xlim(0, 0.08) + 
      geom_tippoint(aes(colour = Site), size = 5) + 
      geom_tiplab(size = 16, offset = 0.001, align = T) + 
      geom_tippoint(aes(color = Site), size = 14) +
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study)" = '#DC3220', 
                                                           "Coastal Kenya (Study)" = '#1AFF1A', "Coastal Kenya (Non-study)" = '#1A85FF'), 
                          labels = c("External", "Coastal Kenya (Study Site)", 
                                     "Western Kenya", "Coastal Kenya (Non-Study Site)"))
    
    plot(rooted_raxml_tree_subset_lineage_2_plot_nodes_label) #20 x 20
    
    offset_value = 0.025
    offset.text_value = 0.004
    supp_plot <- ggtree(rooted_raxml_tree_d3, branch.length = 'rate', ladderize = T, size = 1) + #,  layout = "fan", open.angle=120) +
      #geom_tippoint(size = 1, alpha = 0.9) +
      ggtitle("Maximum Likelihood Tree of Global DENV3 Sequences") +
      #geom_label_repel(aes(label=label, fill=label)) + 
      geom_tiplab(size = 4, alpha = 0.9) +
      geom_nodelab(aes(label=label), size = 4, nudge_x = -0.002, nudge_y = 1) +
      #geom_nodepoint(aes(color = as.numeric(label)), size=5) +
      #colorspace::scale_color_continuous_sequential(name = "Bootstrap\nSupport Value (%)", palette = "Heat") +
      geom_treescale(x=0.05, y=-10, width=0.01, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +
      
      #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
      #Highlight Relevant nodes
      #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
      #Highlight Relevant Kenya nodes
      #Highlight Relevant Kenya nodes
      geom_hilight(node=435, fill="#DC3220", alpha=0.4, extend = 0.008) + #Kenya, Wajir and Mombasa
      geom_hilight(node=632, fill="#005AB5", alpha=0.4, extend = 0.0005) + #Kenya, Coastal
      
      #Scale
      xlim(c(0, 0.125)) + 
      
      #Label The Genotypes
      geom_cladelab(node=444, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
      geom_cladelab(node=521, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
      geom_cladelab(node=432, label= "III", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#4F94CD") +
      geom_cladelab(node=435, label= "V", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "lightsalmon") +
      
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
    
    supp_annot_data <- supp_plot %<+% denv3_metadata
    sup_plot_annot_data_label <- supp_annot_data + 
      geom_tippoint(aes(colour = Site), size = 5) + 
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study)" = '#DC3220', 
                                                           "Coastal Kenya (Study)" = '#1AFF1A', "Coastal Kenya (Non-study)" = '#1A85FF'), 
                          labels = c("External", "Coastal Kenya (Study Site)", 
                                     "Western Kenya", "Coastal Kenya (Non-Study Site)"))
    
    
    plot(sup_plot_annot_data_label) #55 x 30
  }
#******************************************************************************#
  
#******************************************************************************#
#* DENV4
{
    #*#For the complete plot. 
    #*Read in the tree
    denv4_raxml_tree <- read.tree("DENV4/ml/raxml_.raxml.support")
    rooted_raxml_tree_d4 <- phangorn::midpoint(denv4_raxml_tree)
    
    denv4_metadata <- read.csv("DENV4/final_seq_metatyping_denv4.csv")

    denv4_metadata <- denv4_metadata[,-1]
    
    colours <- sample(paletteer_d("ggsci::default_igv", 8))
    colours <- paletteer_d("ggsci::default_ucscgb", 8)
    
    #rooted_raxml_tree_d4_2 <- tree_subset(rooted_raxml_tree_d4, node = 407, levels_back = 0)
    test_tree <- ggtree(rooted_raxml_tree_d4, branch.length='rate', ladderize = T, size = 1) +
      geom_tiplab(size = 3, alpha = 0.9) +
      #xlim(c(0, 0.1)) +
      geom_nodelab(aes(label=node), size = 8) + 
      geom_treescale(x=0.2, y=-20, width=0.1, color='black', offset = 5, linesize = 1.5, fontsize = 14)
    
    
    # Create new frogdata with same number of species as frogtree.2
    test_tree %<+% denv4_metadata +
      geom_tippoint(aes(colour = Genotype), size = 4) +
      scale_colour_manual(values = colours) 
    
    
    offset_value = 0.004
    offset.text_value = 0.004
    no_annot <- ggtree(rooted_raxml_tree_d4, branch.length = 'rate', ladderize = T, size = 2.25) + 
      #geom_tippoint(size = 8, alpha = 0.9) +
      geom_treescale(x=0.03, y=-10, width=0.025, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +
      
      #Highlight Relevant Kenya nodes
      geom_hilight(node=275, fill="#DC3220", alpha=0.4, extend = 0.008) + #Kenya, Wajir and Mombasa

      #Scale
      #xlim(c(0, 0.1)) + 
      
      #Label The Genotypes
      geom_cladelab(node=268, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
      geom_cladelab(node=270, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +

      theme_tree2(legend.position="none", 
                  plot.title = element_text(hjust = 0.5, size = 40), 
                  legend.title = element_text(size = 40), 
                  legend.text = element_text(size = 32), 
                  #legend.position = c(.2, 0.75),
                  legend.key.width = unit(1.25, "cm"),
                  legend.key.height = unit(2, "cm"),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(), 
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank())
    
    no_annot_data <- no_annot %<+% denv4_metadata
    no_annot_data_label <- no_annot_data + 
      geom_tippoint(aes(colour = Site), size = 5) + 
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220'), 
                          labels = c("External", "Western Kenya (Study site)"))
    
    plot(no_annot_data_label) #PDF 35 x 25
    
    # Lineage 1. 
    rooted_raxml_tree_subset_lineage_1 <- tree_subset(rooted_raxml_tree_d4, node = 271, levels_back = 0)
    rooted_raxml_tree_subset_lineage_1_plot <- ggtree(rooted_raxml_tree_subset_lineage_1, branch.length='rate', ladderize = T, size = 1) %>% #+ geom_nodelab(aes(label=node), nudge_x = -0.005, size = 8, colour = 'black')
      collapse(node = 99) + #geom_tiplab2(size = 3, align = T) +
      geom_point2(aes(subset=(node == 99)), size=10, shape=15, fill="steelblue") +
      ggtitle("Kenya Clades: Lineage 1") +
      geom_treescale(y = -1, width=0.02, color='black', linesize = 1.5, fontsize = 10) +
      theme_tree2(plot.title = element_text(hjust = 0.5, size = 60), 
                  legend.position = "none",
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(), 
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank())
    
    rooted_raxml_tree_subset_lineage_1_plot_nodes <- rooted_raxml_tree_subset_lineage_1_plot %<+% denv4_metadata
    rooted_raxml_tree_subset_lineage_1_plot_nodes_label <- rooted_raxml_tree_subset_lineage_1_plot_nodes +   xlim(0, 0.08) + 
      geom_tippoint(aes(colour = Site), size = 8) + 
      geom_tiplab(size = 16, offset = 0.001, align = T) + 
      geom_tippoint(aes(color = Site), size = 14) +
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220'), 
                          labels = c("External", "Western Kenya (Study site)"))
    
    plot(rooted_raxml_tree_subset_lineage_1_plot_nodes_label) #15 x 15
    
    
    offset_value = 0.025
    offset.text_value = 0.004
    supp_plot <- ggtree(rooted_raxml_tree_d4_2, branch.length = 'rate', ladderize = T, size = 1) + #,  layout = "fan", open.angle=120) +
      #geom_tippoint(size = 1, alpha = 0.9) +
      ggtitle("Maximum Likelihood Tree of Global DENV4 Sequences") +
      #geom_label_repel(aes(label=label, fill=label)) + 
      geom_tiplab(size = 4, alpha = 0.9) +
      geom_nodelab(aes(label=label), size = 4, nudge_x = -0.002, nudge_y = 1) +
      #geom_nodepoint(aes(color = as.numeric(label)), size=5) +
      #colorspace::scale_color_continuous_sequential(name = "Bootstrap\nSupport Value (%)", palette = "Heat") +
      geom_treescale(x=0.05, y=-10, width=0.01, color='black', offset = 2.5, linesize = 1.5, fontsize = 14) +
      
      #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
      #Highlight Relevant nodes
      #geom_label2(aes(subset > 50, label=label), size=2, color="darkred", alpha=0.5) +
      #Highlight Relevant Kenya nodes
      
      #Highlight Relevant Kenya nodes
      geom_hilight(node=288, fill="#DC3220", alpha=0.4, extend = 0.008) + #Kenya, Wajir and Mombasa
      
      #Scale
      #xlim(c(0, 0.1)) + 
      
      #Label The Genotypes
      geom_cladelab(node=333, label= "I", color='black', fontsize=35, linewidth = 8, offset.text = offset.text_value,
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#CD3333") + 
      geom_cladelab(node=207, label= "II", color='black', fontsize=35, offset.text = offset.text_value,linewidth = 8, 
                    align = TRUE, angle = 90, hjust = 0.5, offset = offset_value, barsize = 8, barcolour = "#00CD00") +
      
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
    
    supp_annot_data <- supp_plot %<+% denv4_metadata
    sup_plot_annot_data_label <- supp_annot_data + 
      geom_tippoint(aes(colour = Site), size = 5) + 
      scale_colour_manual(name = "Study Sites", values = c('External' = 'grey40', "Western Kenya (Study site)" = '#DC3220'), 
                          labels = c("External", "Western Kenya (Study site)"))
    
    
    plot(sup_plot_annot_data_label) #55 x 30
  }
#******************************************************************************#
  
#******************************************************************************#
#"Talent is insignificant. I know a lot of talented ruins. 
  
#Beyond talent lie all the usual words: discipline, love, 
#luck, but, most of all, endurance."

#James Baldwin