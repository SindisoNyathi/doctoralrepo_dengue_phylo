# Format DENV2 Case series and plot it
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)
require(tidyverse)

#Read files
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENVA")

denva_caseseries <- read.csv('denv2_case_series.csv')

#First add site.
denva_caseseries$Location <- NA
denva_caseseries$GDate <- NA
denva_caseseries$Month <- NA
denva_caseseries$Year <- NA

for (i in 1:nrow(denva_caseseries)){
  
  person_id <- denva_caseseries$person_id[[i]]
  date <- denva_caseseries$date[[i]]
  
  id_firstletter <- strsplit(person_id, split = "")[[1]][1]
  
  location <- switch(id_firstletter,
                     "U" = "Ukunda", "D" = "Ukunda", "L" = "Ukunda", "M" = "Msambweni", "C" = "Chulaimbo", "R" = "Chulaimbo", "K" = "Kisumu")
  
  
  gdate <- mdy(date)
  gyear <- year(gdate)
  gmonth <- month(gdate)
  gdate <- format(mdy(date), '%Y/%b')
  
  denva_caseseries$Location[[i]] <- location
  denva_caseseries$GDate[[i]] <- gdate
  denva_caseseries$Month[[i]] <- gmonth
  denva_caseseries$Year[[i]] <- gyear
  
}


#Summarise the West Case series
west_cases <- denva_caseseries[which(denva_caseseries$Location == "Kisumu" |denva_caseseries$Location == "Chulaimbo"),]

#Combine the counts
west_cases_summary <- west_cases %>% 
  group_by(GDate) %>% 
  summarize(n = n())

#create a complete series of dates
#Months
these_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
these_years <- c("2014", "2015", "2016", "2017", "2018")

df <- tibble(
  these_years <- c("2014", "2015", "2016", "2017", "2018")
)
df2 <- df %>% expand(these_years, these_months)
df2$GDate <- paste(df2$these_years, df2$these_months, sep = "/")
df2$Label <- paste(df2$these_years, df2$these_months, sep = " ")
df2$GDate2 <- df2$GDate
df2$GDate2 <- ym(df2$GDate2)
df2 <- df2[order(df2$GDate2),]
df2$DateN <- c(1:60)
df2 <- df2[,-5]

#Do an outer merge
complete_west_cases_summary <- merge(west_cases_summary, df2, by = "GDate", all.y = T)
complete_west_cases_summary$n[which(is.na(complete_west_cases_summary$n))] <- 0
complete_west_cases_summary <- complete_west_cases_summary[,c(1, 3, 4, 2, 5, 6)]
colnames(complete_west_cases_summary) <- c("Date", "Year", "Month", "Cases", "Label", "DateN")

#Now plot. 
complete_west_cases_summary <-  complete_west_cases_summary[order(complete_west_cases_summary$DateN),]

#Save the file
write.csv(complete_west_cases_summary, "complete_west_cases_summary.csv")

complete_west_cases_summary <- complete_west_cases_summary[c(1:42),]
ggplot(complete_west_cases_summary, aes(x = reorder(Date, DateN), y = Cases)) + 
  geom_bar(stat="identity", fill="forestgreen") + 
  theme_bw() +
  ggtitle(label = "DENV2 Case Counts", subtitle = "Western Cluster") + 
  scale_x_discrete(name = "Date", breaks = complete_west_cases_summary$Date, labels = complete_west_cases_summary$Label) +
  theme(axis.text.x = element_text(size = 28, angle = 90, hjust = 0, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5)) +
  geom_vline(xintercept = 12.5, linetype = 2, color = "black", linewidth = 1.5) +
  geom_vline(xintercept = 24.5, linetype = 2, color = "black", linewidth = 1.5) +
  geom_vline(xintercept = 36.5, linetype = 2, color = "black", linewidth = 1.5)


#Plot case series with R.
denva_cluster2_env <- read.csv("beast/prelim/denva_cluster2_env_r.csv")
denva_cluster2_env$Time2

complete_west_cases_summary$DateNum <- decimal_date(ym(complete_west_cases_summary$Date))

ggplot() + 
  geom_bar(data = complete_west_cases_summary, aes(x = DateNum, y = Cases), stat="identity", fill="forestgreen") + 
  geom_line(data = denva_cluster2_env, aes(y = R*10, x = Time)) +
  geom_ribbon(data = denva_cluster2_env, aes(x = Time, ymin=Lower*10, ymax=Upper*10), linetype=2, alpha=0.4, fill = 'lightblue') +
  scale_y_continuous(limits = c(0, 60), sec.axis = sec_axis(~.*0.1)) +
  theme_bw() +
  ggtitle(label = "DENV2 Case Counts", subtitle = "Western Cluster") + 
  scale_x_continuous(name = "Date", limits = c(2013.96, 2017.5), breaks = complete_west_cases_summary$DateNum, labels = complete_west_cases_summary$Label) +
  theme(axis.text.x = element_text(size = 28, angle = 90, hjust = 0, vjust = 0.5),
        axis.text.y = element_text(size = 32, hjust = 0.5),
        axis.title = element_text(size = 40), 
        plot.title = element_text(size = 46, hjust = 0.5), 
        plot.subtitle = element_text(size = 40, hjust = 0.5), 
        panel.grid.minor.x = element_blank(),) +
  geom_vline(xintercept = 2014.96, linetype = 2, color = "black", linewidth = 1.5) +
  geom_vline(xintercept = 2015.96, linetype = 2, color = "black", linewidth = 1.5) +
  geom_vline(xintercept = 2016.96, linetype = 2, color = "black", linewidth = 1.5)






