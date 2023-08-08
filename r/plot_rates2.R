#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Rates heat map
#******************************************************************************#
#*
#*#******************************************************************************#
#* Preliminaries. 
#* 
#* Set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

# Load required files
require(beastio)
require(mcmcr)
require(coda)

#******************************************************************************#
#*
#*******************************************************************************#
#* Read rate file. 
rate_parameters_d1 <- read.csv("Results/denv1_rate_parameters.csv")
rate_parameters_d3 <- read.csv("Results/denv3_rate_parameters.csv")
rate_parameters_d4 <- read.csv("Results/denv4_rate_parameters.csv")
all_rate_parameters <- list(rate_parameters_d1, rate_parameters_d3, rate_parameters_d4)

rate_evidence_d1 <- read.table("DENV1/beast/complex1_run1/denv1_bayes.txt", sep = "\t", header = TRUE)
rate_evidence_d3 <- read.table("DENV3/beast/complex1_run1/denv3_bayes.txt", sep = "\t", header = TRUE)
rate_evidence_d4 <- read.table("DENV4/beast/complex1_run1/denv4_bayes.txt", sep = "\t", header = TRUE)
all_rate_evidence <- list(rate_evidence_d1, rate_evidence_d3, rate_evidence_d4)

all_rate_parameters_evidence <-  vector("list", 3)

for (i in c(1:3)){
  
  rate_parameters <- all_rate_parameters[[i]]
  rate_evidence <- all_rate_evidence[[i]]
  
  # Modify and merge the evidence and parameters
  rate_parameters <- rate_parameters[, c(-1, -2)]
  rate_evidence <- rate_evidence[,c(1, 2, 3)]
  colnames(rate_evidence) <- c("Source", "Destination", "Bayes_Factor")
  rate_parameters_evidence <- merge(rate_parameters, rate_evidence, by = c("Source", "Destination"))
  
  
  rate_parameters_evidence[which(rate_parameters_evidence$Median == max(rate_parameters_evidence$Median)),]
  ordered <- rate_parameters_evidence[order(rate_parameters_evidence$Median),]
  
  kenya_sites <- c("Chulaimbo", "Ukunda", "Msambweni", "Kisumu", "Mombasa", 
                   "Malindi", "Wajir", "Voi", "Mtwapa", "Lamu")
  
  rate_parameters_evidence_local <- rate_parameters_evidence[which(rate_parameters_evidence$Source %in% kenya_sites & rate_parameters_evidence$Destination %in% kenya_sites),]
  median(rate_parameters_evidence_local$Median)
  ordered_local <- rate_parameters_evidence_local[order(rate_parameters_evidence_local$Median),]
  
  #******************************************************************************#
  #*
  #******************************************************************************#
  #*Reorder bayes factors. 
  rate_parameters_evidence$Bayes <- NA
  rate_parameters_evidence$Bayes[rate_parameters_evidence$Bayes_Factor < 3] <- 0
  rate_parameters_evidence$Bayes[rate_parameters_evidence$Bayes_Factor < 10 & rate_parameters_evidence$Bayes_Factor > 3] <- 1
  rate_parameters_evidence$Bayes[rate_parameters_evidence$Bayes_Factor > 10 & rate_parameters_evidence$Bayes_Factor < 30] <- 2
  rate_parameters_evidence$Bayes[rate_parameters_evidence$Bayes_Factor > 30] <- 3
  rate_parameters_evidence$Bayes <- as.factor(rate_parameters_evidence$Bayes)
  
  all_rate_parameters_evidence[[i]] <- rate_parameters_evidence
}

#*#******************************************************************************#
#*
#******************************************************************************#
#*Plot
#*
#* General Histogram
rate_parameters_evidence$Source <- factor(rate_parameters_evidence$Source, 
                                          levels = c("Australia", "Oceania", "CentralAfrica", "EasternAfrica", "WesternAfrica", 
                                                     "NothernAfrica", "SouthernAfrica", "NorthAmerica", "CentralAmericaandCaribbean", 
                                                     "SouthAmerica", "UpperSouthEasternAsia", "CentralSouthEasternAsia", "LowerSouthEasternAsia", 
                                                     "EasternAsia", "WesternAsia", "Europe", "Chulaimbo","Kisumu", "Ukunda", "Msambweni","Mombasa", 
                                                     "Malindi", "Mtwapa", "Lamu", "Wajir", "Voi"))
rate_parameters_evidence$Destination <- factor(rate_parameters_evidence$Destination, 
                                               levels = c("Australia", "Oceania", "CentralAfrica", "EasternAfrica", "WesternAfrica", 
                                                          "NothernAfrica", "SouthernAfrica", "NorthAmerica", "CentralAmericaandCaribbean", 
                                                          "SouthAmerica", "UpperSouthEasternAsia", "CentralSouthEasternAsia", "LowerSouthEasternAsia", 
                                                          "EasternAsia", "WesternAsia", "Europe", "Chulaimbo","Kisumu", "Ukunda", "Msambweni","Mombasa", 
                                                                                                               "Malindi", "Mtwapa", "Lamu", "Wajir", "Voi"))

ggplot(all_rate_parameters_evidence[[3]], aes(x = Source, y = Destination, color = Median, size = Bayes)) + 
  
  geom_point() +
  
  ggtitle("Complete Global Transmission Rates of DENV2 by Study Region") +
  xlab("Source") + 
  ylab("Destination") +
  
  # Color scale
  scale_color_gradient(name = "Inferred Transmission \nRate (units)", low = "yellow", high = "red", limits = c(0.25, 4), breaks = c(0.5, 2, 4)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('0' = 10, '1' = 12, '2' = 14, '3' = 16), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  
  
  theme(plot.title = element_text(size = 50, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
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

# PDF 25 x 25
#******************************************************************************#
#*
#******************************************************************************#
"I grow old ... I grow old ...
I shall wear the bottoms of my trousers rolled.

Shall I part my hair behind? Do I dare to eat a peach?
I shall wear white flannel trousers, and walk upon the beach.
I have heard the mermaids singing, each to each.

I do not think that they will sing to me.

I have seen them riding seaward on the waves
Combing the white hair of the waves blown back
When the wind blows the water white and black.
We have lingered in the chambers of the sea
By sea-girls wreathed with seaweed red and brown
Till human voices wake us, and we drown."

The Lovesong of J. Alfred Prufrock by T.S Elliot