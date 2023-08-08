#******************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Complete Cohort
#Methods: 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
#Set Working Directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

#Load or install required pacakges. 
require('wesanderson') # For this project we will use darjeeling 1 and 2.
#use to look up names wes_palette("name"Darjeeling1, 5, type = c("discrete"))
#Names
#Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
#Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
require('ggplot2')
require(tidyverse)
#******************************************************************************#

#******************************************************************************#
#*
#*#For the complete plot. 
#*Read in the tree
denv_cohort <- read.csv("Data/denv_cohort.csv")
denv_cohort <- denv_cohort[denv_cohort$Proceed == 1,]


table(denv_cohort$Serotype)

# MAke Serotype Categorical
denv_cohort$Serotype <- as.character(denv_cohort$Serotype)

# Summarize the information we have
denv_cohort <- denv_cohort %>% 
  group_by(Serotype, Site, Year) %>%
  summarise(N = n())

#Divide by serotype
denv_cohort_1 <- denv_cohort[denv_cohort$Serotype == 1,]
denv_cohort_2 <- denv_cohort[denv_cohort$Serotype == 2,]
denv_cohort_3 <- denv_cohort[denv_cohort$Serotype == 3,]
denv_cohort_4 <- denv_cohort[denv_cohort$Serotype == 4,]

# Plot the plot now. 
cohort_plot <- ggplot() +
  
  #DENV1
  geom_point(data = denv_cohort_1, aes(y = Site, x = Year, colour = Serotype, size = N), fill = '#1A85FF',
             position = position_nudge(x = 0.2, y = 0.225)) +
  
  #DENV2
  geom_point(data = denv_cohort_2, aes(y = Site, x = Year, colour = Serotype, size = N), fill = '#1AFF1A', 
             position = position_nudge(x = -0.2, y = 0.225), show.legend = T) +
  
  #DENV3
  geom_point(data = denv_cohort_3, aes(y = Site, x = Year, colour = Serotype, size = N), fill = '#FFC20A', 
             position = position_nudge(x = 0.2, y = 0.225)) +
  
  #DENV4
  geom_point(data = denv_cohort_4, aes(y = Site, x = Year, colour = Serotype, size = N), fill = '#DC3220', 
             position = position_nudge(x = 0.2, y = -0.225)) +
  
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020), 
                      limits = c(2013.5, 2020.5)) +
  
  scale_y_discrete(labels = c('Chulaimbo\n(Western Kenya)', 'Kisumu\n(Western Kenya)', 'Msambweni\n(Coastal Kenya)', 'Ukunda\n(Coastal Kenya)')) +
  
  # Add lines
  # Vertical
  geom_hline(yintercept = 1.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_hline(yintercept = 2.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_hline(yintercept = 3.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_hline(yintercept = 4.5, color = 'black', size = 0.1, alpha = 0.6) +

  # Horizontal
  geom_vline(xintercept = 2013.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2014.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2015.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2016.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2017.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2018.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2019.5, color = 'black', size = 0.1, alpha = 0.6) +
  geom_vline(xintercept = 2020.5, color = 'black', size = 0.1, alpha = 0.6) +

  theme_bw() +
  
  #Scale colour
  scale_colour_manual(name = 'Serotype', breaks = c('1', '2', '3', '4'),
                      values = c('1' = '#1A85FF', '2' = '#1AFF1A', '3' = '#FFC20A', '4' = '#DC3220')) +
  
  # Scale size
  scale_size_continuous(name = "No. of Isolates", limits = c(1, 6), breaks = c(2, 4, 6), range = c(4, 20)) +

  ggtitle(label = "Sequences Collected", subtitle = "by Site/Year/Serotype") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 22, color = 'black'),
        axis.text.y = element_text(size = 22, hjust = 0.5, color = 'black'),
        axis.title = element_text(size = 26), 
        legend.title=element_text(size=20), 
        #legend.position = c(0.2, 0.75),
        legend.text=element_text(size=20),
        plot.title = element_text(size = 30, hjust = 0.5), 
        plot.subtitle = element_text(size = 24, hjust = 0.5)) +

guides(colour = guide_legend(override.aes = list(size=12)), 
       size = guide_legend(override.aes = list(size=c(6, 8, 10), colour = 'black')))

plot(cohort_plot)

# Save the plot. 
jpeg("Plots/man1/DENV Cohort. Sequences.jpg", width = 1200, height = 600)
plot(cohort_plot) 
dev.off()
#******************************************************************************#

#******************************************************************************#






