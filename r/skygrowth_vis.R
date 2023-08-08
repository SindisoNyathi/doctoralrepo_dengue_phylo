#*****************************************************************************#
#20/03/2023
#Sindiso Nyathi
#Goal: Visualize tree struct. 
#* Methods: 
#* 
#******************************************************************************#

#******************************************************************************#
#* Preliminaries
#* Working directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics/DENV_global")

#Packages
require(ape)
require(skygrowth)
require(ggplot2)
require(ggtree)
require(dplyr)
#******************************************************************************#

#******************************************************************************#
#*Read in the file
skygrowth_output <- readRDS('skygrowth_output.rds')
n_countries <- length(skygrowth_output)
country_growth_rates <- as.data.frame(matrix(nrow = 100, ncol = n_countries))
country_popsizes <-     as.data.frame(matrix(nrow = 100, ncol = n_countries))
country_times <-        as.data.frame(matrix(nrow = 100, ncol = n_countries))
other_country_growth_features <- as.data.frame(matrix(nrow = n_countries, ncol = 5))
colnames(other_country_growth_features) <- c('Country', 'Earliest', 'Latest', 'Count')

for (i in c(1:n_countries)){
  
  #Get country
  this_country <- skygrowth_output[[i]]
  
  #R.plot(this_country, 73)
  
  #Check if it is null
  if (is.null(this_country)) next
  
  #Get country information
  #Time
  this_country_time <- as.data.frame(this_country$time)
  
  # Growthrate
  this_country_growthrate <- as.data.frame(this_country$growthrate_ci)
  this_country_growthrate$Time <- this_country_time[,1]
  colnames(this_country_growthrate) <- c('Lower', 'Median', 'Upper', 'Time')
  
  #Populationsize
  this_country_popsize <- as.data.frame(this_country$ne_ci)
  this_country_popsize$Time <- this_country_time[,1]
  colnames(this_country_popsize) <- c('Lower', 'Median', 'Upper', 'Time')
  
  #Tips
  this_country_tiplabs <- this_country$tre$tip.label
  this_country_name <- strsplit(this_country_tiplabs[1], '/')[[1]][2]
  seq_count <- length(this_country_tiplabs)
  this_country_dates <- as.data.frame(matrix(nrow = seq_count, ncol = 1))
  
  #Tip dates
  for (j in c(1:length(this_country_tiplabs))){
 this_country_dates[j,] <- strsplit(this_country_tiplabs[j], '/')[[1]][4]
  }
  
  this_country_dates <- as.data.frame((this_country_dates))
  
  #Create the plot
  colnames(this_country_dates) <- c('Sampling')
  this_country_dates$Y <- -2
  this_country_dates$Sampling <- as.numeric(this_country_dates$Sampling)
  
  #Sampling information
  earliest_sampling_date <- min(this_country_dates$Sampling)
  latest_sampling_date <- max(this_country_dates$Sampling)
  
  #Make the plot
  this_country_growthrate$RealTime <- this_country_growthrate$Time + as.numeric(latest_sampling_date)

  this_country_growthrate_plot <- ggplot() + theme_bw() +
  
  #Plot growth Rate
  geom_line(data = this_country_growthrate, aes(y = Median, x = RealTime)) +
  geom_ribbon(data = this_country_growthrate, aes(x = RealTime, ymin=Lower, ymax=Upper), linetype=2, alpha=0.4, fill = 'lightblue') +

  #Plot sampling times
  geom_jitter(data = this_country_dates, aes(y = Y, x = Sampling), shape=18, color="darkred", size=5) +
  
  #Other things
  scale_x_continuous(name = "Date", limits = c(floor(earliest_sampling_date), ceiling(latest_sampling_date))) +
    scale_y_continuous(name = 'Growth Rate', limits = c(-4, 5)) +
  ggtitle(label = "DENV2 Population Growth Rate", 
          subtitle = paste(this_country_name, ' (', seq_count, ' sequences collected ', floor(earliest_sampling_date), ' - ', ceiling(latest_sampling_date), ')', sep = '')) + 
  theme(axis.text.x = element_text(size = 28, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5), 
        panel.grid.minor.x = element_blank())
  
  
  #Plot growthrate
  #Make the plot
  this_country_popsize$RealTime <- this_country_popsize$Time + as.numeric(latest_sampling_date)
  
  this_country_popsize_plot <- ggplot() + theme_bw() +
    
    #Plot growth Rate
    geom_line(data = this_country_popsize, aes(y = Median, x = RealTime)) +
    geom_ribbon(data = this_country_popsize, aes(x = RealTime, ymin=Lower, ymax=Upper), linetype=2, alpha=0.4, fill = 'lightblue') +
    
    #Plot sampling times
    geom_jitter(data = this_country_dates, aes(y = Y+5, x = Sampling), shape=18, color="darkred", size=5) +
    
    #Other things
    scale_x_continuous(name = "Date", limits = c(floor(earliest_sampling_date), ceiling(latest_sampling_date))) +
   # scale_y_continuous(name = 'Growth Rate', limits = c(-4, 5)) +
    scale_y_log10(name = 'Population Size', limits=c(.01, 1e7)) +
    ggtitle(label = "DENV2 Population Growth Rate", 
            subtitle = paste(this_country_name, ' (', seq_count, ' sequences collected ', floor(earliest_sampling_date), ' - ', ceiling(latest_sampling_date), ')', sep = '')) + 
    theme(axis.text.x = element_text(size = 28, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 32, hjust = 0.5),
          axis.title = element_text(size = 40), 
          plot.title = element_text(size = 46, hjust = 0.5), 
          plot.subtitle = element_text(size = 40, hjust = 0.5), 
          panel.grid.minor.x = element_blank())
  
  #Save the plot2
  #Growthrate
  jpeg(file = paste("Plots/Growthrate/", this_country_name, 'No', i, '.jpeg', sep = ''), width = 1200, height = 800)
  plot(this_country_growthrate_plot)
  dev.off()
  
  #Time
  jpeg(file = paste("Plots/Popsize/", this_country_name, '.jpeg', sep = ''), width = 1200, height = 800)
  plot(this_country_popsize_plot)
  dev.off()
  
  #add the growth rate information to the bigger file
  colnames(country_growth_rates)[i] <- this_country_name
  country_growth_rates[,i] <- this_country_growthrate$Median
  
  # Pop sizes
  colnames(country_popsizes)[i] <- this_country_name
  country_popsizes[,i] <- this_country_popsize$Median
  
  # Time
  colnames(country_times)[i] <- this_country_name
  country_times[,i] <- this_country_time
  
  #Other features
  other_country_growth_features$Country[i] <- this_country_name
  other_country_growth_features$Earliest[i] <- earliest_sampling_date
  other_country_growth_features$Latest[i] <- latest_sampling_date
  other_country_growth_features$Count[i] <- seq_count
}


### Work with speciifc locations. 
brazil <- skygrowth_output[[6]]
indonesia <- skygrowth_output[[26]]
china <- skygrowth_output[[10]]
mexico <- skygrowth_output[[34]]
singapore <- skygrowth_output[[50]]
nicaragua <- skygrowth_output[[39]]
thailand <- skygrowth_output[[57]]

# Read each into R and aggregate
brazil_data <- read.csv('Countdata/BRA_cleaned_2010.csv')
colnames(brazil_data)
brazil_data <- brazil_data[,-1]
brazil_data_agg <- brazil_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
brazil_data_agg$Country <- 'Brazil'

indonesia_data <- read.csv('Countdata/IDN_joined_2005.csv')
colnames(indonesia_data)
indonesia_data <- indonesia_data[,c(-1, -3, -4, -5, -6, -10)]
indonesia_data_agg <- indonesia_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
indonesia_data_agg$Country <- 'Indonesia'

china_data <- read.csv('Countdata/CHN_joined_2004.csv')
colnames(singapore_data)
china_data <- china_data[,c(2, 7, 8, 9)]
china_data_agg <- china_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
china_data_agg$Country <- 'China'

mexico_data <- read.csv('Countdata/MEX_joined_2011.csv')
colnames(mexico_data)
mexico_data <- mexico_data[,c(2, 7, 8, 9)]
mexico_data_agg <- mexico_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
mexico_data_agg$Country <- 'Mexico'

singapore_data <- read.csv('Countdata/SLV_joined_2005.csv')
colnames(singapore_data)
singapore_data <- singapore_data[,c(2, 7, 8, 9)]
singapore_data_agg <- singapore_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
singapore_data_agg$Country <- 'Singapore'

nicaragua_data <- read.csv('Countdata/NIC_joined_2020.csv')
colnames(nicaragua_data)
nicaragua_data <- nicaragua_data[,c(2, 7, 8, 9)]
nicaragua_data_agg <- nicaragua_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
nicaragua_data_agg$Country <- 'Nicaragua'

thailand_data <- read.csv('Countdata/THA_joined_2015.csv')
colnames(thailand_data)
thailand_data <- thailand_data[,c(2, 7, 8, 9)]
thailand_data_agg <- thailand_data %>%
  group_by(Year, month) %>%
  summarise(counts = sum(CountValue))
thailand_data_agg$Country <- 'Thailand'


major_count_data <- rbind(thailand_data_agg, nicaragua_data_agg, singapore_data_agg, 
                          mexico_data_agg, china_data_agg, indonesia_data_agg, brazil_data_agg)

colnames(major_count_data)
major_count_data$Time <- major_count_data$Year + (major_count_data$month/12)

#Plot Count data
ggplot(data = major_count_data, aes(x = Time, y = counts)) + 
  geom_bar(stat="identity", fill = 'steelblue') + 
  theme_bw() + 
  facet_wrap(~Country, scales = "free_y", ncol = 1) +
  ggtitle(label = "DENV2 Case Counts", subtitle = "Major Countries") + 
  xlab('Time') + 
  ylab('Counts') +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 30), 
        plot.title = element_text(size = 36, hjust = 0.5), 
        strip.text.x = element_text(size = 30),
        plot.subtitle = element_text(size = 30, hjust = 0.5))
#Save 15 x 20




#Now matc countries with sky growth estimates. 
brazil <- skygrowth_output[[6]]
major_count_data_brazil <- major_count_data[major_count_data$Country == 'Brazil',]

#Get country information
#Time
this_country <- brazil
this_country_time <- as.data.frame(this_country$time)

# Growthrate
this_country_growthrate <- as.data.frame(this_country$growthrate_ci)
this_country_growthrate$Time <- this_country_time[,1]
colnames(this_country_growthrate) <- c('Lower', 'Median', 'Upper', 'Time')

#Populationsize
this_country_popsize <- as.data.frame(this_country$ne_ci)
this_country_popsize$Time <- this_country_time[,1]
colnames(this_country_popsize) <- c('Lower', 'Median', 'Upper', 'Time')

#Tips
this_country_tiplabs <- this_country$tre$tip.label
this_country_name <- strsplit(this_country_tiplabs[1], '/')[[1]][2]
seq_count <- length(this_country_tiplabs)
this_country_dates <- as.data.frame(matrix(nrow = seq_count, ncol = 1))

#Tip dates
for (j in c(1:length(this_country_tiplabs))){
  this_country_dates[j,] <- strsplit(this_country_tiplabs[j], '/')[[1]][4]
}

this_country_dates <- as.data.frame((this_country_dates))

#Create the plot
colnames(this_country_dates) <- c('Sampling')
this_country_dates$Y <- -2
this_country_dates$Sampling <- as.numeric(this_country_dates$Sampling)

#Sampling information
earliest_sampling_date <- min(this_country_dates$Sampling)
latest_sampling_date <- max(this_country_dates$Sampling)

#Make the plot
this_country_growthrate$RealTime <- this_country_growthrate$Time + as.numeric(latest_sampling_date)

this_country_growthrate_plot <- ggplot() + theme_bw() +
  
  #Plot growth Rate
  geom_bar(data = major_count_data_brazil, aes(x = Time, y = counts), stat="identity", fill = 'steelblue') + 
  geom_line(data = this_country_growthrate, aes(y = Median*100000, x = RealTime)) +
  geom_ribbon(data = this_country_growthrate, aes(x = RealTime, ymin=Lower*100000, ymax=Upper*100000), linetype=2, alpha=0.4, fill = 'lightblue') +
  
  #Plot sampling times
  geom_jitter(data = this_country_dates, aes(y = 500000, x = Sampling), shape=18, color="darkred", size=5, height = 100000) +
  
  #Other things
  scale_x_continuous(name = "Date", limits = c(floor(earliest_sampling_date), ceiling(latest_sampling_date))) +
  #scale_y_continuous(name = 'Growth Rate', limits = c(-4, 5)) +
  scale_y_continuous(name = 'Case Counts', sec.axis = sec_axis(~.*0.00001)) +
  ggtitle(label = "DENV2 Population Growth Rate Validation", 
          subtitle = paste(this_country_name, ' (', seq_count, ' sequences collected ', floor(earliest_sampling_date), ' - ', ceiling(latest_sampling_date), ')', sep = '')) + 
  theme(axis.text.x = element_text(size = 28, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5), 
        panel.grid.minor.x = element_blank())

#Now matc countries with sky growth estimates. 
nicaragua <- skygrowth_output[[39]]
major_count_data_brazil <- major_count_data[major_count_data$Country == 'Thailand',]

#Get country information
#Time
this_country <- nicaragua
this_country_time <- as.data.frame(this_country$time)

# Growthrate
this_country_growthrate <- as.data.frame(this_country$growthrate_ci)
this_country_growthrate$Time <- this_country_time[,1]
colnames(this_country_growthrate) <- c('Lower', 'Median', 'Upper', 'Time')

#Populationsize
this_country_popsize <- as.data.frame(this_country$ne_ci)
this_country_popsize$Time <- this_country_time[,1]
colnames(this_country_popsize) <- c('Lower', 'Median', 'Upper', 'Time')

#Tips
this_country_tiplabs <- this_country$tre$tip.label
this_country_name <- strsplit(this_country_tiplabs[1], '/')[[1]][2]
seq_count <- length(this_country_tiplabs)
this_country_dates <- as.data.frame(matrix(nrow = seq_count, ncol = 1))

#Tip dates
for (j in c(1:length(this_country_tiplabs))){
  this_country_dates[j,] <- strsplit(this_country_tiplabs[j], '/')[[1]][4]
}

this_country_dates <- as.data.frame((this_country_dates))

#Create the plot
colnames(this_country_dates) <- c('Sampling')
this_country_dates$Y <- -2
this_country_dates$Sampling <- as.numeric(this_country_dates$Sampling)

#Sampling information
earliest_sampling_date <- min(this_country_dates$Sampling)
latest_sampling_date <- max(this_country_dates$Sampling)

#Make the plot
this_country_growthrate$RealTime <- this_country_growthrate$Time + as.numeric(latest_sampling_date)

this_country_growthrate_plot <- ggplot() + theme_bw() +
  
  #Plot growth Rate
  geom_bar(data = major_count_data_brazil, aes(x = Time, y = counts), stat="identity", fill = 'steelblue') + 
  geom_line(data = this_country_growthrate, aes(y = Median*1000, x = RealTime)) +
  geom_ribbon(data = this_country_growthrate, aes(x = RealTime, ymin=Lower*1000, ymax=Upper*1000), linetype=2, alpha=0.4, fill = 'lightblue') +
  
  #Plot sampling times
  geom_jitter(data = this_country_dates, aes(y = 30000, x = Sampling), shape=18, color="darkred", size=5, height = 3000) +
  
  #Other things
  scale_x_continuous(name = "Date", limits = c(floor(earliest_sampling_date), ceiling(latest_sampling_date))) +
  #scale_y_continuous(name = 'Growth Rate', limits = c(-4, 5)) +
  scale_y_continuous(name = 'Case Counts', sec.axis = sec_axis(~.*0.001)) +
  ggtitle(label = "DENV2 Population Growth Rate Validation", 
          subtitle = paste(this_country_name, ' (', seq_count, ' sequences collected ', floor(earliest_sampling_date), ' - ', ceiling(latest_sampling_date), ')', sep = '')) + 
  theme(axis.text.x = element_text(size = 28, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5), 
        panel.grid.minor.x = element_blank())


#Now matc countries with sky growth estimates. 
brazil <- readRDS('brazil_mcmcfit.RDS')
major_count_data_brazil <- major_count_data[major_count_data$Country == 'Brazil',]

#Get country information
#Time
this_country <- brazil
this_country_time <- as.data.frame(this_country$time)

# Growthrate
this_country_growthrate <- as.data.frame(this_country$growthrate)
this_country_growthrate$Time <- this_country_time
colnames(this_country_growthrate) <- c('Median', 'Time')

#Populationsize
this_country_popsize <- as.data.frame(this_country$ne_ci)
this_country_popsize$Time <- this_country_time[,1]
colnames(this_country_popsize) <- c('Lower', 'Median', 'Upper', 'Time')

#Tips
this_country_tiplabs <- this_country$tre$tip.label
this_country_name <- strsplit(this_country_tiplabs[1], '/')[[1]][2]
seq_count <- length(this_country_tiplabs)
this_country_dates <- as.data.frame(matrix(nrow = seq_count, ncol = 1))

#Tip dates
for (j in c(1:length(this_country_tiplabs))){
  this_country_dates[j,] <- strsplit(this_country_tiplabs[j], '/')[[1]][4]
}

this_country_dates <- as.data.frame((this_country_dates))

#Create the plot
colnames(this_country_dates) <- c('Sampling')
this_country_dates$Y <- -2
this_country_dates$Sampling <- as.numeric(this_country_dates$Sampling)

#Sampling information
earliest_sampling_date <- min(this_country_dates$Sampling)
latest_sampling_date <- max(this_country_dates$Sampling)

duration <- latest_sampling_date - earliest_sampling_date

#Make the plot
this_country_popsize<- as.data.frame(this_country_popsize)
colnames(this_country_popsize) <- c('Median', 'Time')
realtime <- 1990 + (this_country_popsize$Time/2.09)   
realtime <- unlist(realtime)
realtime <- as.numeric(realtime)
this_country_popsize$RealTime <- realtime

#this_country_growthrate_plot <- 
  ggplot() + theme_bw() +
#  plot( brazil )  + scale_y_log10(limits=c(.01, 1e5))
#growth.plot( brazil )
#Plot growth Rate
  geom_bar(data = major_count_data_brazil, aes(x = Time, y = counts), stat="identity", fill = 'steelblue') + 
  geom_line(data = this_country_popsize, aes(y = Median*100000, x = RealTime), color = 'red', size = 1.25) +
  #geom_ribbon(data = this_country_growthrate, aes(x = RealTime, ymin=Lower*100000, ymax=Upper*100000), linetype=2, alpha=0.4, fill = 'lightblue') +
  
  #Plot sampling times
  geom_jitter(data = this_country_dates, aes(y = 500000, x = Sampling), shape=18, color="darkred", size=5, height = 100000) +
  
  #Other things
 # scale_x_continuous(name = "Date", limits = c(floor(earliest_sampling_date), ceiling(latest_sampling_date))) +
  #scale_y_continuous(name = 'Growth Rate', limits = c(-4, 5)) +
  scale_y_continuous(name = 'Case Counts', sec.axis = sec_axis(~.*0.00001)) +
  ggtitle(label = "DENV2 Population Growth Rate Validation", 
          subtitle = paste(this_country_name, ' (', seq_count, ' sequences collected ', floor(earliest_sampling_date), ' - ', ceiling(latest_sampling_date), ')', sep = '')) + 
  theme(axis.text.x = element_text(size = 28, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5), 
        panel.grid.minor.x = element_blank())
  
  
  
  #Make the plot
  this_country_popsize <- as.data.frame(this_country_popsize)
  realtime <- 1990 + (this_country_popsize$Time/2.09)   
  realtime <- unlist(realtime)
  realtime <- as.numeric(realtime)
  this_country_popsize$RealTime <- realtime
  
  #this_country_growthrate_plot <- 
  ggplot() + theme_bw() +
    #plot( brazil )  + scale_y_log10(limits=c(.01, 1e5))
    #growth.plot( brazil )
    #Plot growth Rate
    geom_bar(data = major_count_data_brazil, aes(x = Time, y = counts), stat="identity", fill = 'steelblue') + 
    geom_line(data = this_country_popsize, aes(y = Median*100000, x = RealTime), color = 'red', size = 1.25) +
    #geom_ribbon(data = this_country_growthrate, aes(x = RealTime, ymin=Lower*100000, ymax=Upper*100000), linetype=2, alpha=0.4, fill = 'lightblue') +
    
    #Plot sampling times
    geom_jitter(data = this_country_dates, aes(y = 500000, x = Sampling), shape=18, color="darkred", size=5, height = 100000) +
    
    #Other things
    # scale_x_continuous(name = "Date", limits = c(floor(earliest_sampling_date), ceiling(latest_sampling_date))) +
    #scale_y_continuous(name = 'Growth Rate', limits = c(-4, 5)) +
    scale_y_continuous(name = 'Case Counts', sec.axis = sec_axis(~.*0.00001)) +
    ggtitle(label = "DENV2 Population Growth Rate Validation", 
            subtitle = paste(this_country_name, ' (', seq_count, ' sequences collected ', floor(earliest_sampling_date), ' - ', ceiling(latest_sampling_date), ')', sep = '')) + 
    theme(axis.text.x = element_text(size = 28, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 32, hjust = 0.5),
          axis.title = element_text(size = 40), 
          plot.title = element_text(size = 46, hjust = 0.5), 
          plot.subtitle = element_text(size = 40, hjust = 0.5), 
          panel.grid.minor.x = element_blank())

