#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Tranmission Rate maps using spartial tools in R
#******************************************************************************#

#******************************************************************************#
#* Preliminaries
#* Online Refernece
# https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/map.html
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

#* set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics")

# Load required librarires
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel","ggspatial", 
#                   "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", 
#                   "geosphere"))

require(ggplot2)
require(sf)
require(rnaturalearth)
require(geosphere)
require(tidyverse)
require(lwgeom)
require(cowplot)
require(ggimage)
#******************************************************************************#

#******************************************************************************#
# Format data required. 
# Define variables
kenya_sites <- c("Chulaimbo", "Ukunda", "Msambweni", "Kisumu", "Mombasa", 
                 "Malindi", "Wajir", "Voi", "Mtwapa", "Lamu")

# Get required data. 
kenya_sites_coords <- read.csv('Data/kenya_sites_2.csv')
denv2_sites <- read.csv("Data/denv2_regions_coords_3.csv")
study_regions <- read.csv('Data/study_regions_r.csv')
rate_parameters <- read.csv("Results/denv2_rate_parameters.csv")
rate_evidence <- read.table("DENV2/beast/run456/denv2_bayesfactor.txt", sep = "\t", header = TRUE)
#******************************************************************************#

#******************************************************************************#
#*Create a dataframe for region image location. 
#*
region_image <- as.data.frame(unique(study_regions$Study.Region))
region_image$Image <- paste("Images/", region_image[,1], ".jpg", sep = "")
colnames(region_image) <- c('Region', 'Image')

#Now merge with denv2_sites
region_image_coords <- merge(denv2_sites, region_image, by.x = 'Label', by.y = 'Region', all.x = T)
#******************************************************************************#

#******************************************************************************#
#Create the parameters. 

# Modify and merge the evidence and parameters
rate_parameters <- rate_parameters[, c(-1, -2)]
rate_evidence <- rate_evidence[,c(1, 2, 3)]
colnames(rate_evidence) <- c("Source", "Destination", "Bayes_Factor")
rate_parameters_evidence <- merge(rate_parameters, rate_evidence, by = c("Source", "Destination"))

#Find the median rate
summary(rate_parameters_evidence$Median)
median_rate <- median(rate_parameters_evidence$Median)
third_percentile_rate <- quantile(rate_parameters_evidence$Median, 0.9)

#Filter by size.
# Leave only links that have at least moderate support
rate_parameters_evidence_reduced <- subset(rate_parameters_evidence, Median > third_percentile_rate)
rate_parameters_evidence_reduced <- rate_parameters_evidence
summary(rate_parameters_evidence_reduced$Median)

# Filter by evidence
#Leave only links that have at least moderate support
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor < 3] <- 0
#rate_parameters_evidence_reduced <- subset(rate_parameters_evidence, Bayes_Factor > 3 | Bayes_Factor < -3)
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor < 10 & rate_parameters_evidence_reduced$Bayes_Factor > 3] <- 1
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor > 10 & rate_parameters_evidence_reduced$Bayes_Factor < 30] <- 2
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor > 30] <- 3
rate_parameters_evidence_reduced$Bayes <- as.factor(rate_parameters_evidence_reduced$Bayes)


# Remove 1st column from denv2_sites df
colnames(denv2_sites)[1] <- "Location"
rate_parameters_evidence_reduced_ids <- rowid_to_column(rate_parameters_evidence_reduced, var = "ID")

#get all the sources and destinations
rate_parameters_sources <- rate_parameters_evidence_reduced_ids[,c(1, 2, 5, 8)]
rate_parameters_sources_coords <- merge(rate_parameters_sources, denv2_sites, 
                                        by.x = "Source", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_sources_coords)[c(1, 7, 8)] <- c("Location", "Long", "Lat") 

rate_parameters_destinations <- rate_parameters_evidence_reduced_ids[,c(1, 3, 5, 8)]
rate_parameters_destinations_coords <- merge(rate_parameters_destinations, denv2_sites, 
                                             by.x = "Destination", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_destinations_coords)[c(1, 7, 8)] <- c("Location", "Long", "Lat") 

rate_parameters_source_destination_coords <- merge(rate_parameters_sources_coords, rate_parameters_destinations_coords, by = c("ID", "Median", "Bayes"))
rate_parameters_source_destination_coords$Median <- as.numeric(rate_parameters_source_destination_coords$Median)
rate_parameters_source_destination_coords$Bayes <- as.factor(rate_parameters_source_destination_coords$Bayes)
rate_parameters_source_destination_coords <- rate_parameters_source_destination_coords[,c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 2, 3)]
colnames(rate_parameters_source_destination_coords) <- c('ID', 'Location.source', 'Label.source', 'Label2.source', 'Long.source', 'Lat.source', 
                                                         'Location.destination', 'Label.destination', 'Label2.destination', 'Long.destination', 
                                                         'Lat.destination', 'Median', 'Bayes')
#******************************************************************************#

#******************************************************************************#
#* Seperate Importations from exportations. 
#* 
#8 Immportations
rate_parameters_source_destination_coords_importations <- rate_parameters_source_destination_coords[which(rate_parameters_source_destination_coords$Location.destination %in% kenya_sites),]
rate_parameters_source_destination_coords_importations <- rate_parameters_source_destination_coords_importations[!(rate_parameters_source_destination_coords_importations$Location.source %in% kenya_sites),]

#8 Exportatations
rate_parameters_source_destination_coords_exportations <- rate_parameters_source_destination_coords[which(rate_parameters_source_destination_coords$Location.source %in% kenya_sites),]
rate_parameters_source_destination_coords_exportations <- rate_parameters_source_destination_coords_exportations[!(rate_parameters_source_destination_coords_exportations$Location.destination %in% kenya_sites),]

#8 Local
rate_parameters_source_destination_coords_local <- rate_parameters_source_destination_coords[which(rate_parameters_source_destination_coords$Location.destination %in% kenya_sites),]
rate_parameters_source_destination_coords_local <- rate_parameters_source_destination_coords_local[(rate_parameters_source_destination_coords_local$Location.source %in% kenya_sites),]
#******************************************************************************#

#******************************************************************************#
#*Make maps
#* Retrieve za warudo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Remove Anterctica from map
world <- world[world$name == "Kenya",]

kenya_sites_coords <- read.csv('Data/kenya_sites_2.csv')
kenya_sites_sf <- st_as_sf(kenya_sites_coords, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

kenya_sites_sf_centre <- kenya_sites_sf[kenya_sites_sf$Location %in% c('Voi', 'Wajir'),]
kenya_sites_sf_west <- kenya_sites_sf[kenya_sites_sf$Location %in% c('Chulaimbo', 'Kisumu'),]
kenya_sites_sf_coast <- kenya_sites_sf[kenya_sites_sf$Location %in% c("Ukunda", "Msambweni",  "Mombasa", 
                                                                        "Malindi", "Mtwapa", "Lamu"),]

region_image_coords <- region_image_coords[!is.na(region_image_coords$Image),]
denv2_sites <- read.csv("Data/denv2_regions_coords_3.csv")
denv2_sites_sf <- st_as_sf(region_image_coords, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")
#******************************************************************************#

#******************************************************************************#
# Plot importations
global_transmission_network_importations <- ggplot(data = world, size = 1) +
  geom_image(data = region_image_coords, aes(x = Longitude, y = Latitude, image = Image),
             size = 0.1) +
  geom_sf() + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf, size = 7, shape = 23, fill = "black") + 
    
  # Add Kenya site labels
  geom_sf_text(data = kenya_sites_sf_centre, aes(label = Location), size = 7, nudge_y = 0.7) +
  geom_sf_text(data = kenya_sites_sf_coast, aes(label = Location), size = 7, nudge_x = 1.15) +
  geom_sf_text(data = kenya_sites_sf_west, aes(label = Location), size = 7, nudge_x = -1.1) +
   
  # Add other cartooned locations 
  geom_sf_text(data = denv2_sites_sf, aes(label = Label2), size = 7, nudge_y = -1.35, parse = F) +
    
  # Add connecting paths
 geom_curve(data=rate_parameters_source_destination_coords_importations, aes(x = Long.source, y = Lat.source, xend = Long.destination, yend = Lat.destination, color = Median, size = Bayes), curvature = 0.2, 
              arrow = arrow(angle = 15, length = unit(0.25, "inches"),
                             ends = "last", type = "closed"), linetype = 'solid') +

  # Color scale
  scale_color_gradient(name = "Inferred Transmission \nRate (units)", low = "blue", high = "red", limits = c(0.8, 1.05), breaks = c(0.8, 0.9, 1)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('0' = 1,'1' = 1.5, '2' = 2, '3' = 2.5), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Expand Axes
 coord_sf(xlim=c(29,47), ylim=c(-9, 9)) +

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

# Plot importations
global_transmission_network_exportations <- ggplot(data = world, size = 1) +
  geom_image(data = region_image_coords, aes(x = Longitude, y = Latitude, image = Image),
             size = 0.1) +
  geom_sf() + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf, size = 7, shape = 23, fill = "black") + 
  
  # Add Kenya site labels
  geom_sf_text(data = kenya_sites_sf_centre, aes(label = Location), size = 7, nudge_y = 0.7) +
  geom_sf_text(data = kenya_sites_sf_coast, aes(label = Location), size = 7, nudge_x = 1.15) +
  geom_sf_text(data = kenya_sites_sf_west, aes(label = Location), size = 7, nudge_x = -1.1) +
  
  # Add other cartooned locations 
  geom_sf_text(data = denv2_sites_sf, aes(label = Label2), size = 7, nudge_y = -1.35, parse = F) +
  
  # Add connecting paths
  geom_curve(data=rate_parameters_source_destination_coords_exportations, aes(x = Long.source, y = Lat.source, xend = Long.destination, yend = Lat.destination, color = Median, size = Bayes), curvature = 0.2, 
             arrow = arrow(angle = 15, length = unit(0.25, "inches"),
                           ends = "last", type = "closed"), linetype = 'solid') +
  
  # Color scale
  scale_color_gradient(name = "Inferred Spread \nRate", low = "blue", high = "red", limits = c(0.8, 1.05), breaks = c(0.8, 0.9, 1)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('0' = 1,'1' = 1.5, '2' = 2, '3' = 2.5), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Expand Axes
  coord_sf(xlim=c(29,47), ylim=c(-9, 9)) +
  
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


# Write and edit Kisumu, Chulaimob and Wajir
write.csv(rate_parameters_source_destination_coords_local, 'local_trans.csv')
rate_parameters_source_destination_coords_local_edited <- read.csv('local_trans_edit.csv')
rate_parameters_source_destination_coords_local_edited$Median <- as.numeric(rate_parameters_source_destination_coords_local_edited$Median)
rate_parameters_source_destination_coords_local_edited$Bayes <- as.factor(rate_parameters_source_destination_coords_local_edited$Bayes)


kenya_sites_coords_4 <- read.csv('Data/kenya_sites_4.csv')
kenya_sites_sf_4 <- st_as_sf(kenya_sites_coords_4, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

kenya_sites_sf_centre_4 <- kenya_sites_sf_4[kenya_sites_sf_4$Location %in% c('Voi', 'Wajir'),]
kenya_sites_sf_west_4 <- kenya_sites_sf_4[kenya_sites_sf_4$Location %in% c('Chulaimbo', 'Kisumu'),]
kenya_sites_sf_coast_4 <- kenya_sites_sf_4[kenya_sites_sf_4$Location %in% c("Ukunda", "Msambweni",  "Mombasa", 
                                                                      "Malindi", "Mtwapa", "Lamu"),]

global_transmission_local <- ggplot(data = world, size = 5) +
  
  geom_sf() + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf_4, size = 9.5, shape = 20, fill = "black") + 
  
  # Add Kenya site labels
  # Add Kenya site labels
  geom_sf_text(data = kenya_sites_sf_centre_4, aes(label = Location), size = 9.5, nudge_x = -0.25) +
  geom_sf_text(data = kenya_sites_sf_coast_4, aes(label = Location), size = 9.5, nudge_x = 0.50) +
  geom_sf_text(data = kenya_sites_sf_west_4, aes(label = Location), size = 9.5, nudge_x = -0.5) +
  
  # Add connecting paths
  # Add connecting paths
  geom_curve(data=rate_parameters_source_destination_coords_local_edited, aes(x = Long.source, y = Lat.source, xend = Long.destination, yend = Lat.destination, color = Median, size = Bayes), curvature = 0.2, 
             arrow = arrow(angle = 15, length = unit(0.25, "inches"),
                           ends = "last", type = "closed"), linetype = 'solid') +
  
  # Color scale
  scale_color_gradient(name = "Inferred Transmission \nRate (units)", low = "blue", high = "red", limits = c(0.8, 1.05), breaks = c(0.8, 0.9, 1)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('0' = 1,'1' = 1.5, '2' = 2, '3' = 2.5), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Expand Axes
  coord_sf(xlim=c(37,42), ylim=c(-5, -1.5)) +
  
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
        axis.title.y = element_blank())

global_transmission_local
#******************************************************************************#
# Npoints = 16
# points = 8*exp(2i * pi * (1:Npoints)/Npoints)
# plot(points)
# 
# points.Cartesian = data.frame(x=Re(points) + 38, y=Im(points) + 0.75)
# 
# write.csv(points.Cartesian, "circel_points.csv")

#******************************************************************************#

