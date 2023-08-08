#******************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Trees
#Methods: 
#'The World Shall Know True Art' 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
#Set Working Directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Bioinformatics/Results/denv2/Consensus/phylo")

#Load or install required pacakges. 
require('wesanderson') # For this project we will use darjeeling 1 and 2.
#use to look up names wes_palette("name"Darjeeling1, 5, type = c("discrete"))
#Names
#Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
#Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
library("ape")
library("Biostrings")
library("ggplot2")
library("ggtree")
library('castor')
require('phytools')
require('ggforce')
require('phangorn')

#******************************************************************************#

#******************************************************************************#
#*
#*#For the complete plot. 
#*Read in the tree
raxml_tree <- read.tree("RaxML/RAxML_bipartitions.result.nwk")
rooted_raxml_tree<- midpoint(raxml_tree)

#Group the clades
clade <- c(A = 1353, B = 1135, C = 988)
cols <- c(A1 = "#00A08A", A2 = "#046C9A", A3 = "#D69C4E")
clade_fontsize <- 8
rooted_raxml_tree_grouped <- groupClade(rooted_raxml_tree, clade)

ml_tree_plot <- ggtree(rooted_raxml_tree_grouped, aes(color = group), 
                       layout="roundrect", branch.length='rate', ladderize = T, size = 1.5)


#Collapse relevant clades
{
  #Nodes, from top to bottom. 
  #Malaysia/Central - West Africa (1960 - 1980)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1531) + 
    geom_point2(aes(subset=(node==1531)), shape=21, size=5, fill='black')
  
  #Central America/Carribean (1965 - 2000)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1112) + 
    geom_point2(aes(subset=(node==1112)), shape=21, size=5, fill='black')
  
  #Pakisatan/Saudi Arabia (2005 - 2015)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1010) + 
    geom_point2(aes(subset=(node==1010)), shape=21, size=5, fill='#4F94CD')
  
  #West Africa/S.E. Asia/Reunion (2010 - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=994) + 
    geom_point2(aes(subset=(node==994)), shape=21, size=5, fill='#4F94CD')
  
  #India Clade/2021
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1093) + 
    geom_point2(aes(subset=(node==1093)), shape=21, size=5, fill='#4F94CD')
  
  #Middle East/West Africa/Luanda Clade
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1026) + 
    geom_point2(aes(subset=(node==1026)), shape=21, size=5, fill='#4F94CD')
  
  #Coastal Kenya Clade (2010 - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1054) + 
    geom_point2(aes(subset=(node==1054)), shape=21, size=5, fill='#4F94CD')
  
  # Coastal Kenya/Central-West Africa (2010 - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=807) + 
    geom_point2(aes(subset=(node==807)), shape=21, size=5, fill='#4F94CD')
  
  #S. E. Asia (2010 - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=811) + 
    geom_point2(aes(subset=(node==811)), shape=21, size=5, fill='#4F94CD')
  
  # S.E. Asia/E. Asia/West Africa (2005 - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=922) + 
    geom_point2(aes(subset=(node==922)), shape=21, size=5, fill='#4F94CD')
  
  # W. Asia/S.E. Asia/E. Asia ( 2005 - 2021)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=828) + 
    geom_point2(aes(subset=(node==828)), shape=21, size=5, fill='#4F94CD')
  
  # Cuba (1981)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1357) + 
    geom_point2(aes(subset=(node==1357)), shape=21, size=5, fill='#CD3333')
  
  # Coastal Kenya/Western Kenya/USA (1944  - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1361) + 
    geom_point2(aes(subset=(node==1361)), shape=21, size=5, fill='#CD3333')
  
  #Thailand/Myanmar/China (2005 - 2020)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1386) + 
    geom_point2(aes(subset=(node==1386)), shape=21, size=5, fill='#CD3333')
  
  #Taiwan/Vietnam (1995 - 2000)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1136) + 
    geom_point2(aes(subset=(node==1136)), shape=21, size=5, fill='#00CD00')
  
  # Central, Southern America/Carribean (2005 - 2021)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1257) + 
    geom_point2(aes(subset=(node==1257)), shape=21, size=5, fill='#00CD00')
  
  # North/Central/South America (1990 - 2015)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1178) + 
    geom_point2(aes(subset=(node==1178)), shape=21, size=5, fill='#00CD00')
  
  # French Guiana (1995 - 2005)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1172) + 
    geom_point2(aes(subset=(node==1172)), shape=21, size=5, fill='#00CD00')
  
  # North America/South America (1995 - 2005)
  ml_tree_plot <- ggtree::collapse(ml_tree_plot, node=1145) + 
    geom_point2(aes(subset=(node==1145)), shape=21, size=5, fill='#00CD00', show.legend = FALSE)
}

#Add additional formatting. 

ml_tree_plot_formatted <- ml_tree_plot + 
  
  #Label The Genotypes
  geom_cladelab(node=1353, label= "Genotype IV - \nAsian II", color='black', fontsize=12, linewidth = 10, offset.text = 0.005,
                align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#CD3333") + 
  geom_cladelab(node=1135, label= "Genotype III - \nSouthern Asian-American", color='black', fontsize=12, offset.text = 0.005,
                align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#00CD00") +
  geom_cladelab(node=988, label= "Genotype II - Cosmopolitan", color='black', fontsize=12, offset.text = 0.005,
                align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#4F94CD") +
  
  
  # Label tips
  geom_tiplab(size = 6, linesize=.05, colour = 'grey') + 
  
  # Label clades
  #Sylvatic
  geom_cladelabel(node=1531, label="Malaysia/Central - West Africa (1960 - 1980)", color='black', fontsize=clade_fontsize) + #Sylvatic
  # GV            
  geom_cladelabel(node=1112, label="Central America/Carribean (1965 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S1
  
  #Cosmopolitan
  geom_cladelabel(node=1010, label="Pakisatan/Saudi Arabia (2005 - 2015)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=994, label="West Africa/S.E. Asia/Reunion (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1093, label="India (2021)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1026, label="Middle East/West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1054, label="Coastal Kenya (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=807, label="Coastal Kenya/Central-West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=811, label="Couth East Asia (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=922, label="S.E. Asia/E. Asia/West Africa (2005 - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=828, label="W. Asia/S.E. Asia/E. Asia ( 2005 - 2021)", color='black', fontsize=clade_fontsize) +
  
  # Asian II
  geom_cladelabel(node=1357, label="Cuba (1981)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1361, label="Coastal Kenya/Western Kenya/USA (1944  - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1386, label="Thailand/Myanmar/China (2005 - 2020)", color='black', fontsize=clade_fontsize) +
  
  #AsianIII
  geom_cladelabel(node=1136, label="Taiwan/Vietnam (1995 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S5
  geom_cladelabel(node=1257, label="Central, Southern America/Carribean (2005 - 2021)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1178, label= "North/Central/South America (1990 - 2015)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1145, label= "North America/South America (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  
  #Label Node.
  #This is adding Support
  #geom_nodelab(aes(label=label)) +
  #geom_zoom_clade(node = 1361, xexpand = 1.5) + 
  
  #Node labels
  #geom_text(aes(label=node), size = 4, nudge_x = -0.008, nudge_y = -0.005) + 
  
  #Highlight Relevant nodes
  geom_hilight(node=1054, fill="#9FB6CD", alpha=0.5, extend = 0.05) + #Kenya
  geom_hilight(node=807, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya
  geom_hilight(node=1361, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya

#OTher things
xlim_tree(0.3) + #vexpand(ratio = 0.003) + 
  
  #Set clade coours
  scale_color_manual(values = c("black", "#CD3333", "#00CD00", "#4F94CD")) +
  guides(colour = "none") +
  theme(axis.title = element_text(hjust = 0.5)) #+ 
 
# labs(caption="Substitution Rate")

#Collapse specific nodes to increase visibility
plot(ml_tree_plot_formatted) 

#Plot and Save.
jpeg("Plots/ML Tree. Complete. Collapsed.jpeg", width = 2500, height = 1900)
plot(ml_tree_plot_formatted) 
dev.off()

#******************************************************************************#

#******************************************************************************#
#*For the clade 1 plot. 
#*
#*Read in the tree
raxml_tree <- read.tree("RaxML/RAxML_bipartitions.result.nwk")
rooted_raxml_tree<- midpoint(raxml_tree)

#Group the clades
clade <- c(A = 1353, B = 1135, C = 988)
cols <- c(A1 = "#00A08A", A2 = "#046C9A", A3 = "#D69C4E")
clade_fontsize <- 8
rooted_raxml_tree_grouped <- groupClade(rooted_raxml_tree, clade)

ml_tree_plot_kenya_clade1 <- ggtree(rooted_raxml_tree_grouped, aes(color = group), 
                       layout="roundrect", branch.length='rate', ladderize = T, size = 1.5)


{
#Collpase the genotype nodes
#Malaysia/Central - West Africa (1960 - 1980)
ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=1531) + 
  geom_point2(aes(subset=(node==1531)), shape=21, size=5, fill='black')

#Central America/Carribean (1965 - 2000)
ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=1112) + 
  geom_point2(aes(subset=(node==1112)), shape=21, size=5, fill='black')

# Cuba (1981)
ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=1357) + 
  geom_point2(aes(subset=(node==1357)), shape=21, size=5, fill='#CD3333')

# Coastal Kenya/Western Kenya/USA (1944  - 2020)
#ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=1361) + 
#  geom_point2(aes(subset=(node==1361)), shape=21, size=5, fill='#CD3333')

#Thailand/Myanmar/China (2005 - 2020)
ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=1386) + 
  geom_point2(aes(subset=(node==1386)), shape=21, size=5, fill='#CD3333')

#Genotype III - \nSouthern Asian-American
ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=1135) + 
  geom_point2(aes(subset=(node==1135)), shape=21, size=5, fill='#00CD00')

#Genotype II - Cosmopolitan
ml_tree_plot_kenya_clade1 <- ggtree::collapse(ml_tree_plot_kenya_clade1, node=988) + 
  geom_point2(aes(subset=(node==988)), shape=21, size=5, fill='#4F94CD')
#Add additional formatting. 
}

ml_tree_plot_kenya_clade1 <- ml_tree_plot_kenya_clade1 + 
  
  # #Label The Genotypes
  geom_cladelab(node=1353, label= "Genotype IV - Asian II", color='black', fontsize=12, linewidth = 10, offset.text = 0.005,
                 align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#CD3333") + 
  # geom_cladelab(node=1135, label= "Genotype III - \nSouthern Asian-American", color='black', fontsize=12, offset.text = 0.005,
  #               align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#00CD00") +
  # geom_cladelab(node=988, label= "Genotype II - Cosmopolitan", color='black', fontsize=12, offset.text = 0.005,
  #               align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#4F94CD") +
  # 
  
  # Label tips
  geom_tiplab(size = 6, linesize=.05, colour = 'black') + 
  
  # Label clades
  #Sylvatic
  
  geom_cladelabel(node=1531, label="Malaysia/Central - West Africa (1960 - 1980)", color='black', fontsize=clade_fontsize) + #Sylvatic
  # GV            
  geom_cladelabel(node=1112, label="Central America/Carribean (1965 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S1
  
  #Cosmopolitan
  geom_cladelabel(node=988, label="Genotype II - Cosmopolitan", color='black', fontsize=clade_fontsize) +
  
  #Asian III
  geom_cladelabel(node=1135, label="Genotype III - Southern Asian-American", color='black', fontsize=clade_fontsize) + 
  
  #Cosmopolitan
  
  #Cosmopolitan
  # geom_cladelabel(node=1010, label="Pakisatan/Saudi Arabia (2005 - 2015)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=994, label="West Africa/S.E. Asia/Reunion (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1093, label="India (2021)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1026, label="Middle East/West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1054, label="Coastal Kenya (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=807, label="Coastal Kenya/Central-West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=811, label="Couth East Asia (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=922, label="S.E. Asia/E. Asia/West Africa (2005 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=828, label="W. Asia/S.E. Asia/E. Asia ( 2005 - 2021)", color='black', fontsize=clade_fontsize) +
  # 
  # Asian II
  geom_cladelabel(node=1357, label="Cuba (1981)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1361, label="Coastal Kenya/Western Kenya/USA (1944  - 2020)", color='black', fontsize=clade_fontsize) +
  geom_cladelabel(node=1386, label="Thailand/Myanmar/China (2005 - 2020)", color='black', fontsize=clade_fontsize) +
  
  # #AsianIII
  # geom_cladelabel(node=1136, label="Taiwan/Vietnam (1995 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S5
  # geom_cladelabel(node=1257, label="Central, Southern America/Carribean (2005 - 2021)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1178, label= "North/Central/South America (1990 - 2015)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1145, label= "North America/South America (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # 
  #Label Node.
  #This is adding Support
  #geom_nodelab(aes(label=label)) +
  #geom_zoom_clade(node = 1361, xexpand = 1.5) + 
  
  #Node labels
  #geom_text(aes(label=node), size = 4, nudge_x = -0.008, nudge_y = -0.005) + 
  
  #Highlight Relevant nodes
  #geom_hilight(node=1054, fill="#9FB6CD", alpha=0.5, extend = 0.05) + #Kenya
  #geom_hilight(node=807, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya
  #geom_hilight(node=1361, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya
  
  #OTher things
  xlim_tree(0.3) + #vexpand(ratio = 0.003) + 
  
  #Set clade coours
  scale_color_manual(values = c("black", "#CD3333", "#00CD00", "#4F94CD")) +
  guides(colour = "none") +
  theme(axis.title = element_text(hjust = 0.5)) #+ 

# labs(caption="Substitution Rate")

#Collapse specific nodes to increase visibility
plot(ml_tree_plot_kenya_clade1) 

#Plot and Save.
jpeg("Plots/ML Tree. Clade 1. Asian.jpeg", width = 2000, height = 1500)
plot(ml_tree_plot_kenya_clade1) 
dev.off()


#******************************************************************************#

#******************************************************************************#
#*For the clade 2 plot. 
#*
#*Read in the tree
raxml_tree <- read.tree("RaxML/RAxML_bipartitions.result.nwk")
rooted_raxml_tree<- midpoint(raxml_tree)

#Group the clades
clade <- c(A = 1353, B = 1135, C = 988)
cols <- c(A1 = "#00A08A", A2 = "#046C9A", A3 = "#D69C4E")
clade_fontsize <- 9
rooted_raxml_tree_grouped <- groupClade(rooted_raxml_tree, clade)

ml_tree_plot_kenya_clade2 <- ggtree(rooted_raxml_tree_grouped, aes(color = group), 
                                    layout="roundrect", branch.length='rate', ladderize = T, size = 1.5)


{
  #Collpase the genotype nodes
  #Malaysia/Central - West Africa (1960 - 1980)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1531) + 
    geom_point2(aes(subset=(node==1531)), shape=21, size=5, fill='black')
  
  #Central America/Carribean (1965 - 2000)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1112) + 
    geom_point2(aes(subset=(node==1112)), shape=21, size=5, fill='black')
  
  # Collapse Asian clade
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1353) + 
    geom_point2(aes(subset=(node==1353)), shape=21, size=5, fill='#CD3333')
  
  # Collapse Upper Cosmopolitan clade clade
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=808) + 
    geom_point2(aes(subset=(node==808)), shape=21, size=5, fill='#4F94CD')

  #Genotype III - Southern Asian-American
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1135) + 
    geom_point2(aes(subset=(node==1135)), shape=21, size=5, fill='#00CD00')
  
  #Pakisatan/Saudi Arabia (2005 - 2015)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1010) + 
    geom_point2(aes(subset=(node==1010)), shape=21, size=5, fill='#4F94CD')
  
  #West Africa/S.E. Asia/Reunion (2010 - 2020)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=994) + 
    geom_point2(aes(subset=(node==994)), shape=21, size=5, fill='#4F94CD')
  
  #India Clade/2021
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1093) + 
    geom_point2(aes(subset=(node==1093)), shape=21, size=5, fill='#4F94CD')
  
  #Middle East/West Africa/Luanda Clade
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1026) + 
    geom_point2(aes(subset=(node==1026)), shape=21, size=5, fill='#4F94CD')

}

ml_tree_plot_kenya_clade2 <- ml_tree_plot_kenya_clade2 + 
  
  # #Label The Genotypes
  #geom_cladelab(node=1353, label= "Genotype IV - Asian II", color='black', fontsize=12, linewidth = 10, offset.text = 0.005,
               # align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#CD3333") + 
  # geom_cladelab(node=1135, label= "Genotype III - \nSouthern Asian-American", color='black', fontsize=12, offset.text = 0.005,
  #               align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#00CD00") +
   geom_cladelab(node=988, label= "Genotype II - Cosmopolitan", color='black', fontsize=13, offset.text = 0.005,
                 align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#4F94CD") +
   
  
  # Label tips
  geom_tiplab(size = 6, linesize=.05, colour = 'black') + 
  
  # Label clades
  #Sylvatic
  
  geom_cladelabel(node=1531, label="Malaysia/Central - West Africa (1960 - 1980)", color='black', fontsize=clade_fontsize) + #Sylvatic
  
  # GV            
  geom_cladelabel(node=1112, label="Central America/Carribean (1965 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S1
  
  #Cosmopolitan
  geom_cladelabel(node=808, label="Genotype II - Cosmopolitan", color='black', fontsize=clade_fontsize) +
  
  #Asian III
  geom_cladelabel(node=1135, label="Genotype III - Southern Asian-American", color='black', fontsize=clade_fontsize) + 
  
  #Asian II
  geom_cladelabel(node=1353, label="Genotype IV - Asian II", color='black', fontsize=clade_fontsize) + 
  
  #Cosmopolitan
  
  #Cosmopolitan
   geom_cladelabel(node=1010, label="Pakisatan/Saudi Arabia (2005 - 2015)", color='black', fontsize=clade_fontsize) +
   geom_cladelabel(node=994, label="West Africa/S.E. Asia/Reunion (2010 - 2020)", color='black', fontsize=clade_fontsize) +
   geom_cladelabel(node=1093, label="India (2021)", color='black', fontsize=clade_fontsize) +
   geom_cladelabel(node=1026, label="Middle East/West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
   #geom_cladelabel(node=1054, label="Coastal Kenya (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=807, label="Coastal Kenya/Central-West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=811, label="Couth East Asia (2010 - 2020)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=922, label="S.E. Asia/E. Asia/West Africa (2005 - 2020)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=828, label="W. Asia/S.E. Asia/E. Asia ( 2005 - 2021)", color='black', fontsize=clade_fontsize) +
# 
# Asian II
#geom_cladelabel(node=1357, label="Cuba (1981)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1361, label="Coastal Kenya/Western Kenya/USA (1944  - 2020)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1386, label="Thailand/Myanmar/China (2005 - 2020)", color='black', fontsize=clade_fontsize) +
  
  # #AsianIII
  # geom_cladelabel(node=1136, label="Taiwan/Vietnam (1995 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S5
  # geom_cladelabel(node=1257, label="Central, Southern America/Carribean (2005 - 2021)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1178, label= "North/Central/South America (1990 - 2015)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1145, label= "North America/South America (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
  # 
  #Label Node.
#This is adding Support
#geom_nodelab(aes(label=label)) +
#geom_zoom_clade(node = 1361, xexpand = 1.5) + 

#Node labels
#geom_text(aes(label=node), size = 4, nudge_x = -0.008, nudge_y = -0.005) + 

#Highlight Relevant nodes
#geom_hilight(node=1054, fill="#9FB6CD", alpha=0.5, extend = 0.05) + #Kenya
#geom_hilight(node=807, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya
#geom_hilight(node=1361, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya

#OTher things
xlim_tree(0.3) + #vexpand(ratio = 0.003) + 
  
  #Set clade coours
  scale_color_manual(values = c("black", "#CD3333", "#00CD00", "#4F94CD")) +
  guides(colour = "none") +
  theme(axis.title = element_text(hjust = 0.5)) #+ 

# labs(caption="Substitution Rate")

#Collapse specific nodes to increase visibility
plot(ml_tree_plot_kenya_clade2) 

#Plot and Save.
jpeg("Plots/ML Tree. Clade 2. Cosmopolitan. Lineage 1.jpg", width = 2000, height = 1500)
plot(ml_tree_plot_kenya_clade2) 
dev.off()

#******************************************************************************#

#******************************************************************************#
#*For the clade 3 plot. 
#*#*Read in the tree
raxml_tree <- read.tree("RaxML/RAxML_bipartitions.result.nwk")
rooted_raxml_tree<- midpoint(raxml_tree)

#Group the clades
clade <- c(A = 1353, B = 1135, C = 988)
cols <- c(A1 = "#00A08A", A2 = "#046C9A", A3 = "#D69C4E")
clade_fontsize <- 9
rooted_raxml_tree_grouped <- groupClade(rooted_raxml_tree, clade)

ml_tree_plot_kenya_clade2 <- ggtree(rooted_raxml_tree_grouped, aes(color = group), 
                                    layout="roundrect", branch.length='rate', ladderize = T, size = 1.5)


{
  #Collpase the genotype nodes
  #Malaysia/Central - West Africa (1960 - 1980)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1531) + 
    geom_point2(aes(subset=(node==1531)), shape=21, size=5, fill='black')
  
  #Central America/Carribean (1965 - 2000)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1112) + 
    geom_point2(aes(subset=(node==1112)), shape=21, size=5, fill='black')
  
  # Collapse Asian clade
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1353) + 
    geom_point2(aes(subset=(node==1353)), shape=21, size=5, fill='#CD3333')
  
  #Genotype III - Southern Asian-American
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=1135) + 
    geom_point2(aes(subset=(node==1135)), shape=21, size=5, fill='#00CD00')
  
  # Collapse Lower Cosmopolitan clade clade
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=989) + 
    geom_point2(aes(subset=(node==989)), shape=21, size=5, fill='#4F94CD')
  
  #S. E. Asia (2010 - 2020)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=811) + 
    geom_point2(aes(subset=(node==811)), shape=21, size=5, fill='#4F94CD')
  
  # S.E. Asia/E. Asia/West Africa (2005 - 2020)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=922) + 
    geom_point2(aes(subset=(node==922)), shape=21, size=5, fill='#4F94CD')
  
  # W. Asia/S.E. Asia/E. Asia ( 2005 - 2021)
  ml_tree_plot_kenya_clade2 <- ggtree::collapse(ml_tree_plot_kenya_clade2, node=828) + 
    geom_point2(aes(subset=(node==828)), shape=21, size=5, fill='#4F94CD')
  
}

ml_tree_plot_kenya_clade3 <- ml_tree_plot_kenya_clade2 + 
  
  # #Label The Genotypes
  #geom_cladelab(node=1353, label= "Genotype IV - Asian II", color='black', fontsize=12, linewidth = 10, offset.text = 0.005,
  # align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#CD3333") + 
  # geom_cladelab(node=1135, label= "Genotype III - \nSouthern Asian-American", color='black', fontsize=12, offset.text = 0.005,
  #               align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#00CD00") +
  geom_cladelab(node=988, label= "Genotype II - Cosmopolitan", color='black', fontsize=13, offset.text = 0.005,
                align = TRUE, angle = 90, hjust = 0.5, offset = 0.08, barsize = 2.5, barcolour = "#4F94CD") +
  
  
  # Label tips
  geom_tiplab(size = 6, linesize=.05, colour = 'black') + 
  
  # Label clades
  #Sylvatic
  geom_cladelabel(node=1531, label="Malaysia/Central - West Africa (1960 - 1980)", color='black', fontsize=clade_fontsize) + #Sylvatic
  
  # GV            
  geom_cladelabel(node=1112, label="Central America/Carribean (1965 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S1
  
  #Cosmopolitan
  geom_cladelabel(node=989, label="Genotype II - Cosmopolitan", color='black', fontsize=clade_fontsize) +
  
  #Asian III
  geom_cladelabel(node=1135, label="Genotype III - Southern Asian-American", color='black', fontsize=clade_fontsize) + 
  
  #Asian II
  geom_cladelabel(node=1353, label="Genotype IV - Asian II", color='black', fontsize=clade_fontsize) + 
  
  #Cosmopolitan
  
  #Cosmopolitan
  #geom_cladelabel(node=1010, label="Pakisatan/Saudi Arabia (2005 - 2015)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=994, label="West Africa/S.E. Asia/Reunion (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1093, label="India (2021)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1026, label="Middle East/West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1054, label="Coastal Kenya (2010 - 2020)", color='black', fontsize=clade_fontsize) +
  # geom_cladelabel(node=807, label="Coastal Kenya/Central-West Africa (2010 - 2020)", color='black', fontsize=clade_fontsize) +
   geom_cladelabel(node=811, label="Couth East Asia (2010 - 2020)", color='black', fontsize=clade_fontsize) +
   geom_cladelabel(node=922, label="S.E. Asia/E. Asia/West Africa (2005 - 2020)", color='black', fontsize=clade_fontsize) +
   geom_cladelabel(node=828, label="W. Asia/S.E. Asia/E. Asia ( 2005 - 2021)", color='black', fontsize=clade_fontsize) +
  # 
  # Asian II
  #geom_cladelabel(node=1357, label="Cuba (1981)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1361, label="Coastal Kenya/Western Kenya/USA (1944  - 2020)", color='black', fontsize=clade_fontsize) +
  #geom_cladelabel(node=1386, label="Thailand/Myanmar/China (2005 - 2020)", color='black', fontsize=clade_fontsize) +
  
# #AsianIII
# geom_cladelabel(node=1136, label="Taiwan/Vietnam (1995 - 2000)", color='black', fontsize=clade_fontsize) + #DENV2S5
# geom_cladelabel(node=1257, label="Central, Southern America/Carribean (2005 - 2021)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=1178, label= "North/Central/South America (1990 - 2015)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=1145, label= "North America/South America (1995 - 2005)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
# geom_cladelabel(node=1172, label= "French Guiana (1995 - 2005)", color='black', fontsize=clade_fontsize) +
# 
#Label Node.
#This is adding Support
#geom_nodelab(aes(label=label)) +
#geom_zoom_clade(node = 1361, xexpand = 1.5) + 

#Node labels
#geom_text(aes(label=node), size = 4, nudge_x = -0.008, nudge_y = -0.005) + 

#Highlight Relevant nodes
#geom_hilight(node=1054, fill="#9FB6CD", alpha=0.5, extend = 0.05) + #Kenya
#geom_hilight(node=807, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya
#geom_hilight(node=1361, fill="#9FB6CD", alpha=0.5, extend = 0.075) + #Kenya

#OTher things
xlim_tree(0.3) + #vexpand(ratio = 0.003) + 
  
  #Set clade coours
  scale_color_manual(values = c("black", "#CD3333", "#00CD00", "#4F94CD")) +
  guides(colour = "none") +
  theme(axis.title = element_text(hjust = 0.5)) #+ 

# labs(caption="Substitution Rate")

#Collapse specific nodes to increase visibility
plot(ml_tree_plot_kenya_clade3) 

#Plot and Save.
jpeg("Plots/ML Tree. Clade 3. Cosmopolitan. Lineage 2.jpg", width = 2000, height = 1500)
plot(ml_tree_plot_kenya_clade3) 
dev.off()

#******************************************************************************#

#******************************************************************************#
#*Generate complete plot. 
#*
#*
setEPS()
jpeg("Plots/ML Tree. All Lineages.jpg", width = 4000, height = 3000)
grid.arrange(ml_tree_plot_formatted, ml_tree_plot_kenya_clade1, ml_tree_plot_kenya_clade2, ml_tree_plot_kenya_clade3, 
             widths = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
             layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA ), 
                                   c(2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)))
dev.off()
