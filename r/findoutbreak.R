#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Using surveillance package to find outbreak periods for dengue. 
#Methods: 
# 
#******************************************************************************#

#******************************************************************************#
#*Prelims
install.packages('surveillance')
library(surveillance)
library(ggplot2)
require(dplyr)

setwd("/Users/sindiso/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")
#******************************************************************************#

#******************************************************************************#
#*Load data. 
nicaragua <- read.csv('DENV_global/city_metadata/nicaragua.csv')
nicaragua <- nic_countdata_aggregate
knitr::kable(head(nicaragua))
belgium <- readr::read_csv("https://epistat.wiv-isp.be/data/public_cases.csv")
knitr::kable(head(belgium))

nic_sts <- sts(observed = nic_countdata_aggregate$Count, # weekly number of cases
               start = c(min(nic_countdata_aggregate$Decimaldate), 1), # first week of the time series
               frequency = 12, # weekly data
               epochAsDate = TRUE, # we do have dates, not only index
               epoch = as.numeric(nic_countdata_aggregate$Decimaldate))
plot(nic_sts)   
no_of_month <- length(nic_sts@observed)
monitored_nic <- earsC(nic_sts,
                              control = list(range = NULL,
                                             method = "C1",
                                             alpha = 0.05,
                                             baseline = 50))
plot(monitored_nic)

con.farrington <- list(noPeriods = 1, b = 2, w = 2, 
                        range = c(1:10), weightsThreshold = 2.58,
                       pThresholdTrend = 0.05, 
                       thresholdMethod = 'delta')

farrington_nic <- farrington(nic_sts, con.farrington)
