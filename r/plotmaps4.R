#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Tranmission Rate maps using spartial tools in R. DENV2
# Last Edit: Mar 8/2023
#******************************************************************************#

#******************************************************************************#
#* Preliminaries
#* Online Refernece
# https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/map.html
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

# Load required librarires
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel","ggspatial", 
#                   "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", 
#                   "geosphere"))

# Load libraries
require(ggplot2)
require(sf)
require(rnaturalearth)
require(geosphere)
require(tidyverse)
require(lwgeom)
require(cowplot)
require(paletteer)

#* set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")
#******************************************************************************#

#******************************************************************************#
# Format required data. 

# Define variables
kenya_sites <- c("Chulaimbo", "Ukunda", "Msambweni", "Kisumu", "Mombasa", 
                 "Malindi", "Wajir", "Voi", "Mtwapa", "Lamu")

# Get required data. 
kenya_sites_coords <- read.csv('Data/kenya_sites.csv')
denv2_sites <- read.csv("Data/denv2_regions_coords.csv")
study_regions <- read.csv('Data/study_regions_r.csv')
rate_parameters <- read.csv("Results/denv2_rate_parameters.csv")
rate_evidence <- read.table("DENV2/beast/complex5/denv2_bayes.txt", sep = "\t", header = TRUE)

# Modify and merge the evidence and parameters
rate_parameters <- rate_parameters[, c(-1, -2)]
rate_evidence <- rate_evidence[,c(1, 2, 3)]
colnames(rate_evidence) <- c("Source", "Destination", "Bayes_Factor")
rate_parameters_evidence <- merge(rate_parameters, rate_evidence, by = c("Source", "Destination"))

#Find the median rate and visualize
summary(rate_parameters_evidence$Median)
hist(rate_parameters_evidence$Median, breaks = 20)
hist(rate_parameters_evidence$Bayes_Factor, breaks = 20)
median_rate <- median(rate_parameters_evidence$Median)
third_quantile <- quantile(rate_parameters_evidence$Median, 0.75)
ninetieth_quantile <- quantile(rate_parameters_evidence$Median, 0.90)

# Leave only links that have at least moderate support
rate_parameters_evidence_support <- subset(rate_parameters_evidence, Bayes_Factor > 3 | Bayes_Factor < -3)

# Now format the bayes factors
rate_parameters_evidence_support$Bayes[rate_parameters_evidence_support$Bayes_Factor < 3] <- 0
rate_parameters_evidence_support$Bayes[rate_parameters_evidence_support$Bayes_Factor < 10 & rate_parameters_evidence_support$Bayes_Factor > 3] <- 1
rate_parameters_evidence_support$Bayes[rate_parameters_evidence_support$Bayes_Factor > 10 & rate_parameters_evidence_support$Bayes_Factor < 30] <- 2
rate_parameters_evidence_support$Bayes[rate_parameters_evidence_support$Bayes_Factor > 30] <- 3
rate_parameters_evidence_support$Bayes <- as.factor(rate_parameters_evidence_support$Bayes)
table(rate_parameters_evidence_support$Bayes)

#Show only rates greater than the median. 
rate_parameters_evidence_support_median <- rate_parameters_evidence_support
rate_parameters_evidence_support_median <- rate_parameters_evidence_support[which(rate_parameters_evidence_support$Median > third_quantile),]

# Add an ID column
rate_parameters_evidence_support_median_ids <- rowid_to_column(rate_parameters_evidence_support_median, var = "ID")

#get all the sources and destinations coordinates
rate_parameters_sources <- rate_parameters_evidence_support_median_ids[,c(1, 2, 5, 8)]
rate_parameters_sources_coords <- merge(rate_parameters_sources, denv2_sites, 
                                        by.x = "Source", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_sources_coords)[c(1, 5, 6)] <- c("Source", "Lat", "Long") 

rate_parameters_destinations <- rate_parameters_evidence_support_median_ids[,c(1, 3, 5, 8)]
rate_parameters_destinations_coords <- merge(rate_parameters_destinations, denv2_sites, 
                                             by.x = "Destination", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_destinations_coords)[c(1, 5, 6)] <- c("Destination", "Lat", "Long") 

#Remerge after adding coordinates. 
rate_parameters_source_destination_coords <- merge(rate_parameters_sources_coords, rate_parameters_destinations_coords, by = c("ID", "Median", "Bayes"))
rate_parameters_source_destination_coords$Median <- as.numeric(rate_parameters_source_destination_coords$Median)
#******************************************************************************#

#******************************************************************************#
#*#Leave only the importations and exportations from our sites. 
#Leave only the importations
rate_parameters_evidence_importations <- rate_parameters_source_destination_coords[which(rate_parameters_source_destination_coords$Destination %in% kenya_sites),]
rate_parameters_evidence_importations <- rate_parameters_evidence_importations[!(rate_parameters_evidence_importations$Source %in% kenya_sites),]

#Leave only the exportations
rate_parameters_evidence_exportations <- rate_parameters_source_destination_coords[which(rate_parameters_source_destination_coords$Source %in% kenya_sites),]
rate_parameters_evidence_exportations <- rate_parameters_evidence_exportations[!(rate_parameters_evidence_exportations$Destination %in% kenya_sites),]

#Local gene flow
rate_parameters_evidence_local <- rate_parameters_source_destination_coords[which(rate_parameters_source_destination_coords$Source %in% kenya_sites & rate_parameters_source_destination_coords$Destination %in% kenya_sites),]

#Global gene flow
rate_parameters_evidence_global <- rate_parameters_source_destination_coords[which(!(rate_parameters_source_destination_coords$Source %in% kenya_sites & rate_parameters_source_destination_coords$Destination %in% kenya_sites )),]
#******************************************************************************#

#******************************************************************************#
#*
# Convert Kenya sites into an sf object
kenya_sites_sf <- st_as_sf(kenya_sites_coords, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")
kenya_sites_coords_moved <- data.frame(kenya_sites_coords$Location, st_coordinates(kenya_sites_sf), kenya_sites_coords$Study_Site, kenya_sites_coords$Label)
colnames(kenya_sites_coords_moved) <-c("Location","Longitude", "Latitude", "Study_Site", "Label")

# Retrieve coordinates for other sites. 
denv2_sites <- denv2_sites[!(denv2_sites$Location %in% kenya_sites),]
denv2_sites <- rbind(denv2_sites, kenya_sites_coords_moved)

#Repeat for all sites in our study
denv2_sites_sf <- st_as_sf(denv2_sites, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

#* Retrieve za warudo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Remove Anterctica, Kenya from map
world <- world[!world$name == "Antarctica",]

# Remove Anterctica from map
world_kenya <- world[world$name == "Kenya",]

# Create the study region variable
world$study_region <- NA

# Correct Sao Tome and Pricnipe and Cote D'ivoire
world$name[which(world$name == "São Tomé and Principe")] <- "Sao Tome and Principe"
world$name[which(world$name == "Côte d'Ivoire")] <- "Cote d'Ivoire"

# Add the study region varibale using a for loop. 
for (i in c(1:length(world$name))){
  country_name <- study_regions$Country[i]
  world$study_region[which(world$name == country_name)] <- study_regions$Study.Region[i]
}

# Correct missing regions
world$study_region[which(world$name == "Cote d'Ivoire")] <- "Western Africa"
world$study_region[which(world$name == "Zambia")] <- "Southern Africa"
world$study_region[which(world$name == "Zimbabwe")] <- "Southern Africa"
world$study_region[which(world$name == "Yemen")] <- "Western Asia"
world$study_region[which(world$name == "Wallis and Futuna Is.")] <- "Oceania"

rate_parameters_evidence_importations$Lat.y <- -0.39605
rate_parameters_evidence_importations$Long.y <- 37.55609

#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###
#Manually shift arrows for visiblity. 
rate_parameters_evidence_importations$Lat.y[which(rate_parameters_evidence_importations$Lat.x > -0.39605)] <- rate_parameters_evidence_importations$Lat.y[which(rate_parameters_evidence_importations$Lat.x > -0.39605)] + 2
rate_parameters_evidence_importations$Lat.y[which(rate_parameters_evidence_importations$Lat.x < -0.39605)] <- rate_parameters_evidence_importations$Lat.y[which(rate_parameters_evidence_importations$Lat.x < -0.39605)] - 2

rate_parameters_evidence_importations$Long.y[which(rate_parameters_evidence_importations$Long.x > 37.55609)] <- rate_parameters_evidence_importations$Long.y[which(rate_parameters_evidence_importations$Long.x > 37.55609)] + 1
rate_parameters_evidence_importations$Long.y[which(rate_parameters_evidence_importations$Long.x < 37.55609)] <- rate_parameters_evidence_importations$Long.y[which(rate_parameters_evidence_importations$Long.x < 37.55609)] - 1
#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###

#Repeating the above.
#Manually shift arrows for visiblity.
rate_parameters_evidence_importations$Nudge_long <- 0
rate_parameters_evidence_importations$Nudge_lat <- 0
rate_parameters_evidence_importations$Nudge_lat[which(rate_parameters_evidence_importations$Lat.x > -0.39605)] <- rate_parameters_evidence_importations$Nudge_lat[which(rate_parameters_evidence_importations$Lat.x > -0.39605)] + 6
rate_parameters_evidence_importations$Nudge_lat[which(rate_parameters_evidence_importations$Lat.x < -0.39605)] <- rate_parameters_evidence_importations$Nudge_lat[which(rate_parameters_evidence_importations$Lat.x < -0.39605)] - 6

rate_parameters_evidence_importations$Nudge_long[which(rate_parameters_evidence_importations$Long.x > 37.55609)] <- rate_parameters_evidence_importations$Nudge_long[which(rate_parameters_evidence_importations$Long.x > 37.55609)] + 32
rate_parameters_evidence_importations$Nudge_long[which(rate_parameters_evidence_importations$Long.x < 29.55609)] <- rate_parameters_evidence_importations$Nudge_long[which(rate_parameters_evidence_importations$Long.x < 29.55609)] - 18
#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###


global_transmission_network_importations <- ggplot() +
  geom_sf(data = world, aes(fill = study_region), show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'grey10', fill = 'grey') + 
  
  # Add other regions
  geom_point(data = rate_parameters_evidence_importations, aes(x = Long.x, y = Lat.x), size = 5, color = "black", shape = 16) + 
  
  #Add connection lines
  # Will add median and support and legend. 
  geom_curve(data=rate_parameters_evidence_importations, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.1, 
             arrow = arrow(angle = 15, length = unit(0.2, "inches"),
                           ends = "last", type = "closed"), linetype = 1, colour = 'black') + 
  geom_text(data=rate_parameters_evidence_importations, aes(label = Label.x, x = Long.x, y = Lat.x),
            nudge_y = rate_parameters_evidence_importations$Nudge_lat, nudge_x = rate_parameters_evidence_importations$Nudge_long, size = 6) + 
  
  scale_fill_manual(name = "Region", values = paletteer_dynamic("cartography::pastel.pal", 19)) + 
  
  # Color scale
  #scale_color_viridis(discrete = T, option = 'turbo', name = "Support (Bayes Factor)", labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  #scale_color_viridis(name = "Support (Bayes Factor)", values = c('0' = "#FEE5D9", '1' = "#FCAE91", '2' = "#FB6A4A", '3' = "#CB181D"), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('1' = 0.4, '2' = 1.2, '3' = 2.2), labels = c("Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12),
        legend.key.width = unit(2.5, "line"), 
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

#global_transmission_network_importations

#******************************************************************************#

#******************************************************************************#
#*Exportations
#Set Kenya
rate_parameters_evidence_exportations$Lat.x <- -0.39605
rate_parameters_evidence_exportations$Long.x <- 37.55609

#Manually shift arrows for visiblity. 
rate_parameters_evidence_exportations$Lat.x[which(rate_parameters_evidence_exportations$Lat.y > -0.39605)] <- rate_parameters_evidence_exportations$Lat.x[which(rate_parameters_evidence_exportations$Lat.y > -0.39605)] + 2
rate_parameters_evidence_exportations$Lat.x[which(rate_parameters_evidence_exportations$Lat.y < -0.39605)] <- rate_parameters_evidence_exportations$Lat.x[which(rate_parameters_evidence_exportations$Lat.y < -0.39605)] - 2

rate_parameters_evidence_exportations$Long.x[which(rate_parameters_evidence_exportations$Long.y > 37.55609)] <- rate_parameters_evidence_exportations$Long.x[which(rate_parameters_evidence_exportations$Long.y > 37.55609)] + 1
rate_parameters_evidence_exportations$Long.x[which(rate_parameters_evidence_exportations$Long.y < 37.55609)] <- rate_parameters_evidence_exportations$Long.x[which(rate_parameters_evidence_exportations$Long.y < 37.55609)] - 1
#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###

#Manually shift text names for visiblity.
rate_parameters_evidence_exportations$Nudge_long <- 0
rate_parameters_evidence_exportations$Nudge_lat <- 0
rate_parameters_evidence_exportations$Nudge_lat[which(rate_parameters_evidence_exportations$Lat.y > -0.39605)] <- rate_parameters_evidence_exportations$Nudge_lat[which(rate_parameters_evidence_exportations$Lat.y > -0.39605)] + 6
rate_parameters_evidence_exportations$Nudge_lat[which(rate_parameters_evidence_exportations$Lat.y < -0.39605)] <- rate_parameters_evidence_exportations$Nudge_lat[which(rate_parameters_evidence_exportations$Lat.y < -0.39605)] - 6

rate_parameters_evidence_exportations$Nudge_long[which(rate_parameters_evidence_exportations$Long.y > 37.55609)] <- rate_parameters_evidence_exportations$Nudge_long[which(rate_parameters_evidence_exportations$Long.y > 37.55609)] + 32
rate_parameters_evidence_exportations$Nudge_long[which(rate_parameters_evidence_exportations$Long.y < 29.55609)] <- rate_parameters_evidence_exportations$Nudge_long[which(rate_parameters_evidence_exportations$Long.y < 29.55609)] - 18
#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###

global_transmission_network_exportations <- ggplot() +
  geom_sf(data = world, aes(fill = study_region), show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'grey10', fill = 'grey') + 
  
  # Add other regions
  geom_point(data = rate_parameters_evidence_exportations, aes(x = Long.y, y = Lat.y), size = 5, color = "black", shape = 16) + 
  
  #Add connection lines
  # Will add median and support and legend. 
  geom_curve(data=rate_parameters_evidence_exportations, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.1, 
             arrow = arrow(angle = 15, length = unit(0.2, "inches"),
                           ends = "last", type = "closed"), linetype = 1, colour = 'black') + 
  
  geom_text(data=rate_parameters_evidence_exportations, aes(label = Label.y, x = Long.y, y = Lat.y),
            nudge_y = rate_parameters_evidence_exportations$Nudge_lat, nudge_x = rate_parameters_evidence_exportations$Nudge_long, size = 6) + 
  
  scale_fill_manual(name = "Region", values = paletteer_dynamic("cartography::pastel.pal", 19)) + 
  
  # Color scale
  #scale_color_manual(name = "Support (Bayes Factor)", values = c('0' = "#FEE5D9", '1' = "#FCAE91", '2' = "#FB6A4A", '3' = "#CB181D"), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('1' = 0.4, '2' = 1.2, '3' = 2.2), labels = c("Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12),
        legend.key.width = unit(2.5, "line"), 
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())  

#global_transmission_network_exportations

#******************************************************************************#

#******************************************************************************#
#Combine importations and exportations
rate_parameters_evidence_exportations_importations <- rbind(rate_parameters_evidence_exportations, rate_parameters_evidence_importations)

global_transmission_network_exportations_importations_plot <- ggplot() +
  geom_sf(data = world, aes(fill = study_region), show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'grey10', fill = 'grey') + 
  
  # Add other regions
  geom_point(data = rate_parameters_evidence_exportations, aes(x = Long.y, y = Lat.y), size = 5, color = "grey40", shape = 16) + 
  geom_point(data = rate_parameters_evidence_importations, aes(x = Long.x, y = Lat.x), size = 5, color = "grey40", shape = 16) +  
  
  #Add connection lines
  
  ##Exportations
  geom_curve(data=rate_parameters_evidence_exportations, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.3, 
             arrow = arrow(angle = 20, length = unit(0.2, "inches"),
                           ends = "last", type = "closed"), linetype = 1, colour = 'black') + 
  
  geom_text(data=rate_parameters_evidence_exportations, aes(label = Label.y, x = Long.y, y = Lat.y),
            nudge_y = rate_parameters_evidence_exportations$Nudge_lat, nudge_x = rate_parameters_evidence_exportations$Nudge_long, size = 6) + 
  
  
  ##Importations
  geom_curve(data=rate_parameters_evidence_importations, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.3, 
             arrow = arrow(angle = 20, length = unit(0.2, "inches"),
                           ends = "last", type = "closed"), linetype = 1, colour = 'black') + 
  geom_text(data=rate_parameters_evidence_importations, aes(label = Label.x, x = Long.x, y = Lat.x),
            nudge_y = rate_parameters_evidence_importations$Nudge_lat, nudge_x = rate_parameters_evidence_importations$Nudge_long, size = 6) + 
  
  scale_fill_manual(name = "Region", values = paletteer_dynamic("cartography::pastel.pal", 19)) + 
  
  # Color scale
  #scale_color_manual(name = "Support (Bayes Factor)", values = c('0' = "#FEE5D9", '1' = "#FCAE91", '2' = "#FB6A4A", '3' = "#CB181D"), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('1' = 0.4, '2' = 1.2, '3' = 2.2), labels = c("Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12),
        legend.key.width = unit(3, "line"), 
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

global_transmission_network_exportations_importations_plot #PDF 7 x 18
#******************************************************************************#

#******************************************************************************#
#*Local gene flow
#*Local Gene flow
# Write and edit Kisumu, Chulaimob and Wajir
kenya_sites_sf_centre_4 <- kenya_sites_sf[kenya_sites_sf$Location %in% c('Voi', 'Wajir'),]
kenya_sites_sf_west_4 <- kenya_sites_sf[kenya_sites_sf$Location %in% c('Chulaimbo', 'Kisumu'),]
kenya_sites_sf_coast_4 <- kenya_sites_sf[kenya_sites_sf$Location %in% c("Ukunda", "Msambweni",  "Mombasa", 
                                                                        "Malindi", "Mtwapa", "Lamu"),]
global_transmission_local <- ggplot(data = world, size = 5) +
  geom_sf(data = world, fill = 'grey', show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'black', fill = '#99C5E3') + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf, size = 11.5, shape = 20, color = "grey40") + 
  
  # Add Kenya site labels
  # Add Kenya site labels
  geom_sf_text(data = kenya_sites_sf_centre_4, aes(label = Location), size = 9.5, nudge_x = -0.25) +
  geom_sf_text(data = kenya_sites_sf_coast_4, aes(label = Location), size = 9.5, nudge_x = 0.55) +
  geom_sf_text(data = kenya_sites_sf_west_4, aes(label = Location), size = 9.5, nudge_x = -0.5) +
  
  # Add connecting paths
  # Add connecting paths
  geom_curve(data=rate_parameters_evidence_local, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.3, 
             arrow = arrow(angle = 15, length = unit(0.25, "inches"),
                           ends = "last", type = "closed"), linetype = 'solid', color = 'black') +
  # Expand Axes
  coord_sf(xlim=c(37,42), ylim=c(-5, -1.5)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('1' = 0.4, '2' = 1.2, '3' = 2.2), labels = c("Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=24), 
        legend.text=element_text(size=20),
        legend.key.width = unit(3.5, "line"), 
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

global_transmission_local
#******************************************************************************#

#******************************************************************************#
#*
#*Global gene flow
global_transmission_global <- ggplot() +
  geom_sf(data = world, aes(fill = study_region), show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'black', fill = '#99C5E3') + 
  
  # Add other regions
  geom_point(data = rate_parameters_evidence_exportations, aes(x = Long.y, y = Lat.y), size = 5, color = "grey40", shape = 16) + 
  
  #Add connection lines
  # Will add median and support and legend. 
  geom_curve(data=rate_parameters_evidence_global, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.3, 
             arrow = arrow(angle = 15, length = unit(0.2, "inches"),
                           ends = "last", type = "closed"), linetype = 1, color = 'black') + 
  
  geom_text(data=rate_parameters_evidence_global, aes(label = Label.y, x = Long.y, y = Lat.y),
            nudge_y = rate_parameters_evidence_global$Nudge_lat, nudge_x = rate_parameters_evidence_global$Nudge_long, size = 6) + 
  
  scale_fill_manual(name = "Region", values = paletteer_dynamic("cartography::pastel.pal", 19)) + 
  
  # Color scale
  #scale_color_gradient(name = "Inferred Transmission \nRate (units)", low = "blue", high = "red", limits = c(0.5, 2.5), breaks = c(0.5, 1.5, 2.5)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('0' = 0.1, '1' = 0.55, '2' = 1.05, '3' = 1.7), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=16),
        legend.key.width = unit(2.5, "line"), 
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

global_transmission_global
#******************************************************************************#

#******************************************************************************#
#*"Now hollow fires burn out to black,
#*And flames are flickering low.
#*Square your shoulders, lift your pack,
#*And leave your friends and go"
#*
#*A. E. Housman, A Shropshire Lad 
