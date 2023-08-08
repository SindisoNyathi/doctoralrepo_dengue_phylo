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
rate_parameters <- read.csv("Results/denv2_rate_parameters.csv")
rate_evidence <- read.table("DENV2/beast/complex5/denv2_bayes.txt", sep = "\t", header = TRUE)

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
#*#******************************************************************************#
#*
#******************************************************************************#
#*Plot
#*
#* General Histogram
summary(rate_parameters$Median)
ggplot(rate_parameters, aes(x = Median)) + geom_histogram()
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
ggplot(rate_parameters_evidence, aes(x = Source, y = Destination, color = Median, size = Bayes)) + 
  
  # Add box foe Kenya sites
  geom_rect(
    xmin = 16.5,
    ymin = 16.5,
    xmax = 26.5,
    ymax = 26.5,
    fill = 'NA', 
    colour = "black",
    size = 1.2, alpha = 0.1
  ) + 
  
  geom_point() +
  
  ggtitle("Complete Global Transmission Rates of DENV2 by Study Region") +
  xlab("Source") + 
  ylab("Destination") +
  
  # Color scale
  scale_color_gradient(name = "Inferred Transmission \nRate (units)", low = "yellow", high = "red", limits = c(0.4, 2.7), breaks = c(0.5, 1.5, 2.5)) +
  
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
