#******************************************************************************#
#06/01/2022
#Sindiso Nyathi
#Goal: Sample Appendix for Manuscript
#Methods: 
#******************************************************************************#

#******************************************************************************#
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

#required files
require(stringr)
require(purrr)

#Read files. 
typing_data <- read.csv("DENV2/d2_typing_data.csv")
metadata <- read.csv("DENV2/final_seq_metadata_denv2.csv")
old_data <- read.csv("DENV2/seq_metadata_denv2_manualfix.csv")
#******************************************************************************#

#******************************************************************************#
#*Format names in typing file. 
#*
typing_data$Acc <- substring(typing_data$name, 1, 10)
metadata$Acc <- map(strsplit(metadata$ID, split = "/"), 1)
old_data$Acc <- old_data$ID

#Merge 1 and 2. 
dengue_cohort_data <- merge(typing_data, metadata, by = "Acc", all.x = T)

#Merge that with 3.
dengue_cohort_data_samples <- merge(dengue_cohort_data, old_data, by = "Acc", all.x = T)

#Take only relevant columns. 
final <- dengue_cohort_data_samples[,c(14, 1, 20, 15, 16, 9, 17)]

colnames(final) <- c("Study ID", "Genbank Acc.", "Country", "Year", "Study Region", "Genotype", "Study Site")
#******************************************************************************#

#******************************************************************************#
#Save the file. 
write.csv(final, "Results/denv2_sample_appendix.csv")
#******************************************************************************#

#******************************************************************************#

denv2 <- read.csv("Results/denv2_sample_appendix_norecc.csv")
table(denv2$Country)
study_regions_appendix <- as.data.frame(table(denv2$Study.Region))
colnames(study_regions_appendix) <- c("StudyRegion", "N")
write.csv(study_regions_appendix, "Results/denv2_study_regions_appendix.csv")

table(denv2$Genotype)
72+128
