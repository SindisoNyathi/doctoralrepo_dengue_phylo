#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Markov Jumps heat map
#******************************************************************************#
#*
#*#******************************************************************************#
#* Preliminaries. 
#* 
#* Set wd
setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Load required files
require(ggplot2)

#******************************************************************************#
#*
#*******************************************************************************#
#* Get file.
travelHistPaths <- read.table('DENV2/beast/markovjumps/jumptimes.txt', header = T)

#Relabel Keny sites to Kenya
kenya_sites <- c("Malindi", "Mtwapa", "Chulaimbo", "Msambweni", "Voi", "Wajir", "Ukunda", "Lamu", "Kisumu", "Mombasa")

#Aggregate
travelHistPaths$from[travelHistPaths$from %in% kenya_sites] <- 'Kenya'
travelHistPaths$to[travelHistPaths$to %in% kenya_sites] <- 'Kenya'

#Get the total number of states
n_states <- length(unique(travelHistPaths$state))

travelHistPaths_counts <- as.data.frame(table(travelHistPaths[,2:3]))
colnames(travelHistPaths_counts) <- c('Source', 'Destination', 'Jumps')

travelHistPaths_counts$Jumps[travelHistPaths_counts$Source == "Kenya" & travelHistPaths_counts$Destination == "Kenya"] <- 0
travelHistPaths_counts$Jumps[travelHistPaths_counts$Jumps == 0] <- NA

travelHistPaths_counts$AveJumps <- travelHistPaths_counts$Jumps/n_states

sum(travelHistPaths_counts$AveJumps, na.rm = T)

travelHistPaths_counts$Source <- factor(travelHistPaths_counts$Source, 
                                          levels = c("Australia", "Oceania", "CentralAfrica", "EasternAfrica", "WesternAfrica", 
                                                     "NothernAfrica", "SouthernAfrica", "NorthAmerica", "CentralAmericaandCaribbean", 
                                                     "SouthAmerica", "UpperSouthEasternAsia", "CentralSouthEasternAsia", "LowerSouthEasternAsia", 
                                                     "EasternAsia", "WesternAsia", "Europe", "Kenya"))
travelHistPaths_counts$Destination <- factor(travelHistPaths_counts$Destination, 
                                               levels = c("Australia", "Oceania", "CentralAfrica", "EasternAfrica", "WesternAfrica", 
                                                          "NothernAfrica", "SouthernAfrica", "NorthAmerica", "CentralAmericaandCaribbean", 
                                                          "SouthAmerica", "UpperSouthEasternAsia", "CentralSouthEasternAsia", "LowerSouthEasternAsia", 
                                                          "EasternAsia", "WesternAsia", "Europe", "Kenya"))

ggplot(travelHistPaths_counts, aes(x = Source, y = Destination, fill = AveJumps)) + 
  
  geom_raster() +
  
  # Add box foe Kenya sites
  geom_rect(
    xmin = 0,
    ymin = 0,
    xmax = 16.5,
    ymax = 16.5,
    fill = 'NA', 
    colour = "black",
    size = 1, alpha = 0.1
  ) + 
  
  
  
  ggtitle("Complete Markov Jump History across Study Regions") +
  xlab("Source") + 
  ylab("Destination") +
  
  #Colour Scale
  scale_fill_gradient(name = "Markov Jumps", low = "gold", high = "red", na.value = 'white') +
  
  # Size scale
  #scale_size_manual(name = "Markov Jumps", values = c('0' = 0, '1' = 12, '2' = 14, '3' = 16), guide = "legend") +
  
  
  
  theme(plot.title = element_text(size = 50, hjust = 0.5),
        panel.background = element_rect(fill = "white"), 
        legend.title=element_text(size=35), 
        legend.text=element_text(size=25),
        axis.text.x=element_text(size = 30, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y=element_text(size = 30), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_text(size = 40),
        axis.title.y = element_text(size = 40), 
        legend.key = element_rect(fill = "white"))
#PDF 25 x 25 
#/Plots/man1/global_markjumps.pdf
#Plots/man1/global_markovjumpsjpg

#Save the markov jump history file. 
write.csv(travelHistPaths_counts, 'Results/markovjump_history.csv')
#******************************************************************************#
#*
#*******************************************************************************#
