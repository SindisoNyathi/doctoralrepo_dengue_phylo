#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Create a serotype agnositc country specific dataset build.
#Methods: 
# 
#******************************************************************************#

#******************************************************************************#
#Preliminaries
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")

# Packages
require(seqinr)
require(tidyverse)
require(lubridate)
require(ape)
#******************************************************************************#
#*
#******************************************************************************#
#*Read the data
nicaragua_seqmetadata <- read.csv('DENV_global/city_metadata/nicaragua_metadata_merged.csv')
global_countdata <- read.csv('DENV_global/Countdata/globalcountdata.csv')
nic_countdata <- global_countdata %>% filter(adm_0_name == 'NICARAGUA')
nic_countdata <- nic_countdata[,c(9, 12)]
nic_countdata$Year <- year(nic_countdata$calendar_start_date)
nic_countdata$Month <- month(nic_countdata$calendar_start_date)
nic_countdata$Decimaldate <- decimal_date(as.Date(nic_countdata$calendar_start_date))
nic_countdata_aggregate <- nic_countdata %>%
  group_by(Year, Month) %>%
  summarize(Count = sum(dengue_total))
nic_countdata_aggregate <- nic_countdata_aggregate %>% filter(Year > 1999)
nic_countdata_aggregate$Date <- paste(nic_countdata_aggregate$Year, '/', nic_countdata_aggregate$Month, '/', '15',  sep = '')
nic_countdata_aggregate$Decimaldate <- decimal_date(parse_date_time(nic_countdata_aggregate$Date, orders = c('ymd')))

#format dates
nicaragua_seqmetadata$DateFormat <- NA
nicaragua_seqmetadata$DateFormat <- decimal_date(parse_date_time(nicaragua_seqmetadata$Date, orders = c('mdy', 'ymd', 'my', 'ym', 'y')))
nicaragua_seqmetadata$Serotype <- as.character(nicaragua_seqmetadata$Serotype)
nicaragua_seqmetadata$Genotype <- as.factor(nicaragua_seqmetadata$Genotype)
#******************************************************************************#
#*
#******************************************************************************#
#*
#date_nume <- sao_paulo_countdata$Year + (sao_paulo_countdata$month/12)
s#ao_paulo_countdata$Date <- date_nume
Y = 40000
ggplot(nic_countdata_aggregate, aes(x = Decimaldate, y = Count)) + 
  geom_bar(stat="identity", fill="forestgreen") + 
  #geom_line(aes(x = Decimaldate, y = Count), colour="forestgreen") + 
  #geom_point(aes(x = Decimaldate, y = Count), colour="forestgreen") + 
  theme_bw() +
  ylim(c(0, 50000)) +
  ggtitle(label = "DENV Case Counts", subtitle = "Nicaragua") +
  geom_jitter(data = nicaragua_seqmetadata, aes(y = Y, x = DateFormat, color = Genotype), shape=18, size=5, height = 10000, width = 0.5) +
  #scale_x_discrete(name = "Date", breaks = seq(2000, 2025, 2)) +
  theme(axis.text.x = element_text(size = 28, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5))




