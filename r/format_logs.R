#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Read in log files and save medians and 95% HPD for variables
#******************************************************************************#
#*
#*#******************************************************************************#
#* Preliminaries. 
#* 
#* Set wd
setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Load required files
#install.packages("mcmcr")
#remotes::install_github("laduplessis/beastio")
require(beastio)
require(mcmcr)
require(coda)

#********************************************************************************#
#*
#*#******************************************************************************#
#*
#this_log <- readLog("DENV2/beast/complex5/denv2_beast_complex5_run123.log", burnin = 0.1)
this_log <- readLog("DENV4/beast/complex2_run/denv4_beast_complex2_run123.log", burnin = 0.1)

hpds <- getHPDMedian(this_log, prob = 0.95)

# Save in a csv files. 
variable_names <- rownames(hpds)
median_hpds <- as.data.frame(cbind(variable_names, hpds[,1], hpds[,2], hpds[,3]))

# First retrieve the TMRCA data and save that. 
rownames(median_hpds) <- NULL
tmrca_denv <- median_hpds[c(7),]
tmrca_denv$Location <- NA

for (i in c(1:(nrow(tmrca_denv)))) {
  
  this_variable <- tmrca_denv[i, 1]
  this_variable <- strsplit(this_variable[[1]], split = "\\.")
  this_variable_location <- this_variable[[1]][2]
  tmrca_denv$Location[i] <- this_variable_location
}

tmrca_denv <- tmrca_denv[,c(5, 2, 3, 4)]
colnames(tmrca_denv) <- c("Location", "UpperHPD", "Median", "LowerHPD")
rownames(tmrca_denv) <- NULL

# Save TMRCA estimates. 
write.csv(tmrca_denv, "Results/denv4_tmrca_estimates.csv")
#********************************************************************************#
#*
#*#******************************************************************************#
# Work on other rates. 
median_hpds <- median_hpds[c(26:157), ]
median_hpds$Variable <- NA
median_hpds$Source <- NA
median_hpds$Destitation <- NA

#Change the file into a source and destination file
for (i in c(1:(nrow(median_hpds)))) {
  this_variable <- median_hpds[i, 1]
  
  this_variable <- strsplit(this_variable[[1]], split = "\\.")
  
  this_variable_var <- this_variable[[1]][2]
  this_variable_source <- this_variable[[1]][3]
  this_variable_destination <- this_variable[[1]][4]
  
  median_hpds$Variable[i] <- this_variable_var
  median_hpds$Source[i] <- this_variable_source
  median_hpds$Destitation[i] <- this_variable_destination
}

median_hpds <- median_hpds[,c(5, 6, 7, 2, 3, 4)]

colnames(median_hpds) <- c("Variable", "Source", "Destination", "LowerHPD", "Median", "UpperHPD")

write.csv(median_hpds, "Results/denv4_rate_parameters.csv")
#******************************************************************************#
#*
#*#******************************************************************************#
#*Fin
#*"We must not look at goblin men,
#*We must not buy their fruits:
#*Who knows upon what soil they fed
#*Their hungry thirsty roots?"
#*From Goblin Market by Christina Rossetti. 