#*****************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Show all detected lineages in Kenya
#Methods: 
#'The World Shall Know True Art' 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
require(ggtree)
require(ggplot2)
require(RColorBrewer)
require(ggplot2)
library(gridExtra)
require(paletteer)
require(stringr)
library(ggbreak)
require(ggimage)

setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# The data. 
lineages <- read.csv('Results/denv_tmrca_estimates.csv')
sequences <- read.csv('DENV_global/denv_kenya_allseqs.csv')
#*#******************************************************************************#

#********************************************************************************#
#* Mod
#* Remove mtwapa clade
lineages <- lineages[-6,]

#* Plot
#* 
lineages_plot <- ggplot(lineages, aes(x = reorder(Lineage, Lorder), y = Median)) + 

  #geom_errorbar(aes(ymin=LowerHPD, ymax=UpperHPD), width=.2,
   #             position=position_dodge(.9)) + 
  #geom_image(aes(image = sequences$image)) +
  
  geom_jitter(data = sequences, aes(x = Lineage, y = Date), 
              shape = 18, color = 'firebrick', size = 12, width = 0.25, height = 0.25) +
  geom_point(size = 8) +
  theme_bw() +
  #labs(title = 'Dengue Lineages Detected in Kenya ', 
   #    subtitle = 'tMRCA') + 
  ylab('Time (years)') + 
  xlab('Lineage') +
  scale_y_continuous(limits = c(1950, 2026), breaks = seq(1950, 2022, 5)) +
  #Expand plot
  #coord_cartesian(ylim=c(1950, 2022), clip="off") +
  #Order the x-axis
  scale_x_discrete(breaks = lineages$Lineage, labels = function(x) gsub('N','\n', lineages$Label)) +
  
  # Add lines to show regional divisions
  geom_vline(xintercept = 2.5, color = 'black', linewidth = 0.8, alpha = 0.6, linetype = 2) +
  geom_vline(xintercept = 5.5, color = 'black', linewidth = 0.8, alpha = 0.6, linetype = 2) +
  geom_vline(xintercept = 7.5, color = 'black', linewidth = 0.8, alpha = 0.6, linetype = 2) +
  

  
  # Serotype Labels
  annotate("label", x = 1.5, y = 2024, label = "DENV1", size = 18, hjust = 0.5) +
  annotate("label", x = 4.5, y = 2024, label = "DENV2", size = 18, hjust = 0.5) +
  annotate("label", x = 6.5, y = 2024, label = "DENV3", size = 18, hjust = 0.5) +
  annotate("label", x = 8, y = 2024, label = "DENV4", size = 18, hjust = 0.5) +
  


  theme(plot.title = element_text(hjust = 0.5, size = 48),
        plot.subtitle = element_text(hjust = 0.5, size = 40),
        legend.title = element_blank(),  
        axis.text.y = element_text(size=36),
        axis.text.x = element_text(size=36, hjust = 0.5),
        axis.title.x = element_text(size = 40), 
        axis.title.y = element_text(size = 40),
        plot.margin=unit(c(60, 5.5, 5.5, 5.5), "points"),
        legend.position = "none") 

#Add breaks to the axis.
lineages_plot_broken <- lineages_plot + scale_y_break(c(1960, 1990))
lineages_plot_broken
#Save 12 x 15 
#*#******************************************************************************#

#********************************************************************************#
#*
"I a stranger and afraid, 
In a world I never made.
They will be master, right or wrong, 
Though both are foolish, both are strong,
And since my soul we cannot fly,
To Saturn or to Mercury,
Keep we must if keep we can,
These foreign laws of god and man"

The Laws of God, The laws of Man, by A.E. Housman
