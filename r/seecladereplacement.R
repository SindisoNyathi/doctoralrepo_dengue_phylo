#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Find clade replacements
#Methods:
# 
#******************************************************************************#

#******************************************************************************#
#*Prelims
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
require(stats)
require(tidyr)
#******************************************************************************#

#******************************************************************************#
#*Read data
nicaragua_seqmetadata <- read.csv('DENV_global/city_metadata/nicaragua_metadata_merged_clusters.csv')
nicaragua_seqmetadata$Cluster <- rowSums(nicaragua_seqmetadata[,c('cluster', "cluster.x", 'cluster.y')], na.rm=TRUE) 
nicaragua_seqmetadata$Partition <- rowSums(nicaragua_seqmetadata[,c('partition', "partition.x", 'partition.y')], na.rm=TRUE) 
nicaragua_seqmetadata <- nicaragua_seqmetadata[,c(2, 10:17)]

#format dates
nicaragua_seqmetadata$DateFormat <- NA
nicaragua_seqmetadata$DateFormat <- decimal_date(parse_date_time(nicaragua_seqmetadata$Date, orders = c('mdy', 'ymd', 'my', 'ym', 'y')))
nicaragua_seqmetadata$Year <- floor(nicaragua_seqmetadata$DateFormat)
nicaragua_seqmetadata$Lineages <- paste(nicaragua_seqmetadata$Serotype, nicaragua_seqmetadata$Genotype, nicaragua_seqmetadata$Partition, sep = '_')
nicaragua_seqmetadata$Serotype <- as.character(nicaragua_seqmetadata$Serotype)
nicaragua_seqmetadata$Genotype <- as.factor(nicaragua_seqmetadata$Genotype)

#Create the dataset by year. 
min_year <- min(nicaragua_seqmetadata$Year)
max_year <- max(nicaragua_seqmetadata$Year)

#Create a dataset for the yaers. 
nica_years <- as.data.frame(matrix(nrow = 0, ncol = 6))
all_types <- unique(nicaragua_seqmetadata$Lineages)
number_all_types <- length(all_types)
colnames(nica_years) <- c('Year', 'N', 'Type', 'Count', 'Proportion', 'Blind')

for (i in c(1:(max_year-min_year))){
  year <- min_year +(i-1)
  year_seqs <- nicaragua_seqmetadata %>% filter(Year == year)
  year_n <- nrow(year_seqs)
  
  for (j in c(1:number_all_types)){
    
    #Check whether we have enough sequences to call a dominant lineage
    blind = 1
    if (year_n > 20) {blind = 0}
    
    this_type_name <- all_types[[j]]
    this_type_count <- nrow(year_seqs[which(year_seqs$Lineages == this_type_name),])
    type_1_prop <- nrow(year_seqs[which(year_seqs$Lineages == this_type_name),])/year_n
    nica_years[nrow(nica_years) + 1,] = c(year, year_n, this_type_name, this_type_count, type_1_prop, blind)
  }
}

nica_years$Count <- as.numeric(nica_years$Count)
nica_years$N <- as.numeric(nica_years$N)
nica_years$Year <- as.numeric(nica_years$Year)
nica_years$Proportion <- as.numeric(nica_years$Proportion)

#blinded regions 
nica_years_nonblind <- nica_years[nica_years$Blind == 0,]
nica_years_blind <-    nica_years[nica_years$Blind == 1,]
nica_years_blind_years <- unique(nica_years_blind$Year)
nica_years_nonblind$Dominant <- 0
nica_years_nonblind$Dominant[nica_years_nonblind$Proportion >= 0.75] <- 1
nica_years_nonblind_dominant <- nica_years_nonblind[nica_years_nonblind$Dominant == 1,]

for (i in 1:nrow(nica_years_nonblind_dominant)) {
  
  type <- nica_years_nonblind_dominant$Type[i]
  type <- strsplit(type, '_')
  label <- paste('DENV', type[[1]][1], '\n', 
                 'Type', type[[1]][2], '\n',
                 'Lineage', type[[1]][3])
  nica_years_nonblind_dominant$Label[i] <-  label
}

#Get plot limits
roundUP <- function(x, m){x + m - x %% m}
min_xaxis <- roundUP(min(nica_years$Year), 2)
max_xaxis <- roundUP(max(nica_years$Year), 2)

# Now plot this. 
nica_years$Genotype <- substr(nica_years$Type, 1, 5)
nica_years_plot <- ggplot(nica_years, aes(x=Year, y=Proportion)) +
  geom_area(aes(fill = Type)) +
  geom_rect(data = nica_years_blind, aes(xmin=Year-.5, xmax=Year+.5, ymin=0, ymax=1), alpha=.4) + 
  geom_segment(data = nica_years_nonblind_dominant, aes(x = Year, y = 1.125, xend = Year, yend = 1),
               color = 'black',
               arrow = arrow(angle = 20, length = unit(0.15, "inches"),
                             ends = "last", type = "closed")) +
  geom_text(data = nica_years_nonblind_dominant, aes(x = Year, y = 1.2, label = Label),
               color = 'black') + 
  scale_x_continuous(breaks=seq(min_xaxis, max_xaxis, 2)) +
  scale_y_continuous(limits = c(0, 1.25), breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  theme_bw()
nica_years_plot

(min(nica_years$Year))