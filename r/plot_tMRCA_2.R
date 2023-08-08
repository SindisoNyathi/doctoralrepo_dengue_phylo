#11/07/2022
#Sindiso Nyathi
#Goal: Read in log files and save medians and 95% HPD for variables
#******************************************************************************#
#*
#*#****************************************************************************#
#* Preliminaries. 
#* #* Preliminaries. 
#* 
#* Set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

# Load required files
require(ggplot2)
require(ggtree)
library(patchwork)
library(ggbreak)
#******************************************************************************#
#*
#******************************************************************************#
#*
#*Make maps
#* Retrieve za warudo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Remove Anterctica from map
world <- world[world$name == "Kenya",]

#Get Kenya Sites
kenya_sites_coords <- read.csv('Data/kenya_sites_2.csv')
kenya_sites_sf <- st_as_sf(kenya_sites_coords, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

time_points <- data.frame(x = 30, y = c(8, 6, 4, 2, 0, -2, -4, -6, -8))

# Plot map.
global_transmission_network_importations 
ggplot(data = world, size = 1) +
  
  geom_sf() + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf, size = 7, shape = 23, fill = "black") + 
  
  # Expand Axes
  coord_sf(xlim=c(29,47), ylim=c(-9, 9)) +
  
  #Add time line. 
  geom_segment(aes(x = 30, y = 8, xend = 30, yend = -8), color = 'black', linewidth = 0.8, alpha = 0.6, linetype = 1) +
  
  #Add points 
  geom_point(data = time_points, aes(x = x, y = y), color = 'black', size = 4) +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=35), 
        legend.text=element_text(size=25),
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.key.width = unit(5, "line"), 
        legend.key = element_rect(fill = "white"))



# Add connecting paths
geom_curve(data=rate_parameters_source_destination_coords_importations, aes(x = Long.source, y = Lat.source, xend = Long.destination, yend = Lat.destination, color = Median, size = Bayes), curvature = 0.2, 
           arrow = arrow(angle = 30, length = unit(0.25, "inches"),
                         ends = "last", type = "open"), linetype = 'solid', alpha = 0.6) +
  
  
  #******************************************************************************#
  #*
  #******************************************************************************#
  tmrca <-read.csv('Results/denv2_tmrca_estimates.csv')
tmrca <- tmrca[,-1]

# Get correct years, by subtracting from 2022, which is mrsd (most recent sampling date)
tmrca$Median <- 2022 - tmrca$Median
tmrca$UpperHPD <- 2022 - tmrca$UpperHPD
tmrca$LowerHPD <- 2022 - tmrca$LowerHPD

tmrca$Location <- factor(tmrca$Location, levels = c("Kenya", "Lower_Coastal", "Upper_Coastal",
                                                    "Western_Kenya", "Chulaimbo", "Kisumu", 
                                                    "Msambweni", "Ukunda", "Mtwapa", "Mombasa", "Malindi"))


# Define section to store
rects = data.frame(xmin = c(4.5), xmax = c(5.5), 
                   ymin = c(1900), ymax = c(2022))

tmrca_plot <- ggplot(tmrca, aes(x=Location, y = Median)) + 
  #geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin=LowerHPD, ymax=UpperHPD), width= 0.2,
                position=position_dodge(0.05)) + 
  theme_bw() +
  labs(title = 'DENV2 Time to Most Recent Common Ancestor', 
       subtitle = 'Clades Circulating in Kenya') + 
  ylab('Median (95% HPD)') + 
  xlab('Location') +
  scale_y_continuous(limits = c(1900, 2022), breaks = seq(1900, 2022, 5)) +
  
  #Order the x-axis
  scale_x_discrete(labels = c("Kenya", "Lower\nCoast", "Upper\nCoast",
                              "Western\nKenya", "Chulaimbo", "Kisumu", 
                              "Msambweni", "Ukunda", "Mtwapa", "Mombasa", "Malindi")) +
  
  # Add lines to show regional divisions
  geom_vline(xintercept = 1.5, color = 'black', linewidth = 0.4, alpha = 0.6, linetype = 2) +
  geom_vline(xintercept = 4.5, color = 'black', linewidth = 0.4, alpha = 0.6, linetype = 2) +
  
  # Highlight our sites of data collection
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=4.5, xmax=8.5, ymin=1900,
                                               ymax=2022), color="transparent", fill="orange", alpha=0.3) +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.title = element_blank(),  
        axis.text = element_text(size=18),
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        legend.position = "none") 

# Dates we want are 1910 - 1920, 1945 - 1955, 2005 - 2020, so insert appropriate 
#breaks
#Add breaks to the axis.
tmrca_plot_broken <- tmrca_plot + scale_y_break(c(1920, 1940))
tmrca_plot_broken_broken <- tmrca_plot_broken + scale_y_break(c(1960, 2000))

plot(tmrca_plot_broken_broken)

setEPS()
postscript(paste("Plots/TMRCA. DENV2.eps", sep = ""),  width = 13, height = 10)
plot(tmrca_plot_broken_broken)
dev.off()

#******************************************************************************#
#*
#******************************************************************************#
#*"And straight against that great array
#*Forth went the dauntless Three.
#*For Romans in Rome's quarrel
#*Spared neither land nor gold,
#*Nor son nor wife, nor limb nor life,
#*In the brave days of old"
#*From Thomas Babbington Macauly's Horatius. 
