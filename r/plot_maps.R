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
require(RcolorBrewer)
require(paletteer)

#******************************************************************************#

#******************************************************************************#
# Format data required. 
# Define variables
kenya_sites <- c("Chulaimbo", "Ukunda", "Msambweni", "Kisumu", "Mombasa", 
                 "Malindi", "Wajir", "Voi", "Mtwapa", "Lamu")
# Get required data. 
kenya_sites_coords <- read.csv('Data/kenya_sites.csv')
denv2_sites <- read.csv("Data/denv2_regions_coords.csv")
study_regions <- read.csv('Data/study_regions_r.csv')
rate_parameters <- read.csv("Results/denv2_rate_parameters.csv")
rate_evidence <- read.table("DENV2/beast/run3/denv2_bayesfactor.txt", sep = "\t", header = TRUE)

#Modify the Kenya sites. 
# Centre
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] - 0.676281
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] - 37.782615
# 
# # Magnify
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)]*4
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)]*4
# 
# # Return
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] + 0.676281
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] + 37.782615
# 
# # Shift
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] - 25 
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] + 45


#Fix the coordinates 
# Convert sites into an sf object
kenya_sites_sf <- st_as_sf(kenya_sites_coords, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

#Change the Kenya sites in the same way
# st_geometry(kenya_sites_sf) <- st_geometry(kenya_sites_sf) - c(37.782615, 0.676281)
# st_geometry(kenya_sites_sf) <- st_geometry(kenya_sites_sf)*8
# st_geometry(kenya_sites_sf) <-  st_geometry(kenya_sites_sf) + c(37.782615, 0.676281)
# st_geometry(kenya_sites_sf) <-  st_geometry(kenya_sites_sf) + c(-20, -85)
# kenya_sites_sf <- st_set_crs(kenya_sites_sf, 4326)

kenya_sites_coords_moved <- data.frame(kenya_sites_coords$Location, st_coordinates(kenya_sites_sf), kenya_sites_coords$Study_Site, kenya_sites_coords$Label)
colnames(kenya_sites_coords_moved) <-c("Location","Longitude", "Latitude", "Study_Site", "Label")
denv2_sites <- denv2_sites[!(denv2_sites$Location %in% kenya_sites),]
denv2_sites <- rbind(denv2_sites, kenya_sites_coords_moved)

#Repeat for all sites in our study
denv2_sites_sf <- st_as_sf(denv2_sites, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

# Modify and merge the evidence and parameters
rate_parameters <- rate_parameters[, c(-1, -2)]
rate_evidence <- rate_evidence[,c(1, 2, 3)]
colnames(rate_evidence) <- c("Source", "Destination", "Bayes_Factor")
rate_parameters_evidence <- merge(rate_parameters, rate_evidence, by = c("Source", "Destination"))

#Find the median rate
median_rate <- median(rate_parameters_evidence$Median)

#Cate
#Leave only the importations and exportations from our sites. 
#Leave only the importations
#rate_parameters_evidence <- rate_parameters_evidence[which(rate_parameters_evidence$Destination %in% kenya_sites| rate_parameters_evidence$Source %in% kenya_sites),]
rate_parameters_evidence_importations <- rate_parameters_evidence[which(rate_parameters_evidence$Destination %in% kenya_sites),]
rate_parameters_evidence_importations <- rate_parameters_evidence_importations[!(rate_parameters_evidence_importations$Source %in% kenya_sites),]

#Show only rates greater than the median. 
rate_parameters_evidence_reduced <- rate_parameters_evidence_importations[which(rate_parameters_evidence_importations$Median > median_rate),]
# summary(rate_parameters_evidence_reduced$Median)
# rate_parameters_evidence_reduced$Median_Factor <- NA
# rate_parameters_evidence_reduced$Median_Factor[rate_parameters_evidence_reduced$Median < 1.12] <- 1
# rate_parameters_evidence_reduced$Median_Factor[rate_parameters_evidence_reduced$Median > 1.12 & rate_parameters_evidence_reduced$Median < 1.27] <- 2
# rate_parameters_evidence_reduced$Median_Factor[rate_parameters_evidence_reduced$Median > 1.27] <- 3
# rate_parameters_evidence_reduced$Median <- as.factor(rate_parameters_evidence_reduced$Median_Factor)

# Leave only links that have at least moderate support
rate_parameters_evidence_reduced <- subset(rate_parameters_evidence_reduced, Bayes_Factor > 3 | Bayes_Factor < -3)
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor < 10] <- 1
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor > 10 & rate_parameters_evidence_reduced$Bayes_Factor < 30] <- 2
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor > 30] <- 3
rate_parameters_evidence_reduced$Bayes <- as.factor(rate_parameters_evidence_reduced$Bayes)
rate_parameters_evidence_reduced <- rate_parameters_evidence_reduced[, -6]

# Remove 1st column from denv2_sites df
#denv2_sites <- denv2_sites[,c(2, 3, 4)]
colnames(denv2_sites)[1] <- "Location"
rate_parameters_evidence_reduced_ids <- rowid_to_column(rate_parameters_evidence_reduced, var = "ID")

#get all the sources and destinations
rate_parameters_sources <- rate_parameters_evidence_reduced_ids[,c(1, 2, 5, 7)]
rate_parameters_sources_coords <- merge(rate_parameters_sources, denv2_sites, 
                                        by.x = "Source", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_sources_coords)[c(1, 5, 6)] <- c("Location", "Lat", "Long") 

rate_parameters_destinations <- rate_parameters_evidence_reduced_ids[,c(1, 3, 5, 7)]
rate_parameters_destinations_coords <- merge(rate_parameters_destinations, denv2_sites, 
                                             by.x = "Destination", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_destinations_coords)[c(1, 5, 6)] <- c("Location", "Lat", "Long") 

rate_parameters_source_destination_coords <- merge(rate_parameters_sources_coords, rate_parameters_destinations_coords, by = c("ID", "Median", "Bayes"))
rate_parameters_source_destination_coords$Median <- as.numeric(rate_parameters_source_destination_coords$Median)
#rate_parameters_source_destination_coords <- rate_parameters_source_destination_coords[,c(1, 4, 5, 6, 9, 10, 11, 2, 3)]

#rate_parameters_source_destination_coords <- rbind(rate_parameters_destinations_coords, rate_parameters_sources_coords)
#rate_parameters_source_destination_coords <- rate_parameters_source_destination_coords[,c(1, 4, 5, 6, 7, 8, 9, 2, 3)]

# A function that makes a dateframe per connection (we will use these connections to plot each lines)
#dep_lon <- rate_parameters_source_destination_coords$Source_Long[i]
#dep_lat <- rate_parameters_source_destination_coords$Source_Lat[i]
#arr_lon <- rate_parameters_source_destination_coords$Destination_Long[i]
#arr_lat <- rate_parameters_source_destination_coords$Destination_Lat[i]
#group <- i
# 
# data_for_connection=function(dep_lon, dep_lat, arr_lon, arr_lat, group){
#   inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=100, addStartEnd=TRUE, breakAtDateLine=F)             
#   inter=data.frame(inter)
#   inter$group=NA
#   diff_of_lon=abs(dep_lon) + abs(arr_lon)
#   if(diff_of_lon > 180){
#     inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
#     inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
#   }else{
#     inter$group=group
#   }
#   return(inter)
# }
# 
# # Creation d'un dataframe complet avec les points de toutes les lignes faire.
# data_ready_plot=data.frame()
# for(i in c(1:nrow(rate_parameters_source_destination_coords))){
#   tmp=data_for_connection(rate_parameters_source_destination_coords$Source_Long[i], rate_parameters_source_destination_coords$Source_Lat[i], 
#                           rate_parameters_source_destination_coords$Destination_Long[i], rate_parameters_source_destination_coords$Destination_Lat[i], i)
#   tmp$Median=rate_parameters_source_destination_coords$Median[i]
#   tmp$Support=rate_parameters_source_destination_coords$Bayes[i]
#   tmp$ID=rate_parameters_source_destination_coords$ID[i]
#   data_ready_plot=rbind(data_ready_plot, tmp)
# }
# 

#data_ready_plot$lon[which(data_ready_plot$lon < -180)] <- data_ready_plot$lon[which(data_ready_plot$lon < -180)] + 360
# data_ready_plot$NewGroup <- data_ready_plot$group
# data_ready_plot$Number <- str_split(data_ready_plot$NewGroup, "")[[1]][1]
# data_ready_plot$Letter <- str_split(data_ready_plot$NewGroup, "")[[1]][2]
# new_data_ready_plot <- data_ready_plot[data_ready_plot$Letter == "A",]
#******************************************************************************#

#******************************************************************************#
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

# Get Kenya country
#kenya_map <- world[world$name == 'Kenya',]
#ggplot(data = kenya_map) + geom_sf() + coord_sf(xlim=c(-125, 190), ylim=c(-22,40))

#Move the kenya map coordinates. 
# st_geometry(world[world$name == "Kenya",]) <- st_geometry(world[world$name == "Kenya",]) - c(37.782615, 0.676281)
# st_geometry(world[world$name == "Kenya",]) <- st_geometry(world[world$name == "Kenya",])*8
# st_geometry(world[world$name == "Kenya",]) <-  st_geometry(world[world$name == "Kenya",]) + c(37.782615, 0.676281)
# st_geometry(world[world$name == "Kenya",]) <-  st_geometry(world[world$name == "Kenya",]) + c(-20, -85)
rate_parameters_source_destination_coords$Lat.y <- -0.39605
rate_parameters_source_destination_coords$Long.y <- 37.55609

#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###
#Manually shift arrows for visiblity. 
rate_parameters_source_destination_coords$Lat.y[which(rate_parameters_source_destination_coords$Lat.x > -0.39605)] <- rate_parameters_source_destination_coords$Lat.y[which(rate_parameters_source_destination_coords$Lat.x > -0.39605)] + 2
rate_parameters_source_destination_coords$Lat.y[which(rate_parameters_source_destination_coords$Lat.x < -0.39605)] <- rate_parameters_source_destination_coords$Lat.y[which(rate_parameters_source_destination_coords$Lat.x < -0.39605)] - 2

rate_parameters_source_destination_coords$Long.y[which(rate_parameters_source_destination_coords$Long.x > 37.55609)] <- rate_parameters_source_destination_coords$Long.y[which(rate_parameters_source_destination_coords$Long.x > 37.55609)] + 1
rate_parameters_source_destination_coords$Long.y[which(rate_parameters_source_destination_coords$Long.x < 37.55609)] <- rate_parameters_source_destination_coords$Long.y[which(rate_parameters_source_destination_coords$Long.x < 37.55609)] - 1
#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###


#Repeating the above.
#Manually shift arrows for visiblity.
rate_parameters_source_destination_coords$Nudge_long <- 0
rate_parameters_source_destination_coords$Nudge_lat <- 0
rate_parameters_source_destination_coords$Nudge_lat[which(rate_parameters_source_destination_coords$Lat.x > -0.39605)] <- rate_parameters_source_destination_coords$Nudge_lat[which(rate_parameters_source_destination_coords$Lat.x > -0.39605)] + 3
rate_parameters_source_destination_coords$Nudge_lat[which(rate_parameters_source_destination_coords$Lat.x < -0.39605)] <- rate_parameters_source_destination_coords$Nudge_lat[which(rate_parameters_source_destination_coords$Lat.x < -0.39605)] - 3

rate_parameters_source_destination_coords$Nudge_long[which(rate_parameters_source_destination_coords$Long.x > 37.55609)] <- rate_parameters_source_destination_coords$Nudge_long[which(rate_parameters_source_destination_coords$Long.x > 37.55609)] + 8
rate_parameters_source_destination_coords$Nudge_long[which(rate_parameters_source_destination_coords$Long.x < 29.55609)] <- rate_parameters_source_destination_coords$Nudge_long[which(rate_parameters_source_destination_coords$Long.x < 29.55609)] - 10
#### DO NOT TOUCH THESE FOUR LINES. THEY ARE CURSED BUT THEY WORK###


global_transmission_network_importations <- ggplot() +
  geom_sf(data = world, aes(fill = study_region), show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'grey10', fill = 'navyblue') + 
  # Circle around Kenya
  #geom_point(aes(x=20, y=-85), size=225, shape=21, fill = "grey99", alpha = 0.01) +
  #geom_sf(fill= "antiquewhite", size = 1) +
  #ggtitle("Local and Regional Spread of DENV2", 
   #       subtitle = "Importations into Kenya Sites") +
  
  # Add other regions
  geom_point(data = rate_parameters_source_destination_coords, aes(x = Long.x, y = Lat.x), size = 4, color = "black", shape = 16) + 
  #geom_sf_text(data = denv2_sites_sf, aes(label = Label), nudge_y = 4, size = 8) + 

  # Add Kenya sites. 
  #geom_sf(data = kenya_sites_sf, size = 4, shape = 23, fill = "black") + 
  
  
  #geom_sf_text(data = kenya_sites_sf, aes(label = Location), nudge_y = -4, size = 5) + 
  
  #Add connection lines
  # Will add median and support and legend. 
  #geom_path(data=rate_parameters_source_destination_coords, aes(x=Long, y=Lat, group = ID, color = Median), size = 1, #alpha = Support, 
   #        arrow=arrow(angle=30,length=unit(0.15,"inches"), type="closed")) +
  geom_curve(data=rate_parameters_source_destination_coords, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, color = Median, size = Bayes), curvature = 0.1, 
             arrow = arrow(angle = 15, length = unit(0.2, "inches"),
                   ends = "last", type = "closed"), linetype = 1) + 
  geom_text(data=rate_parameters_source_destination_coords, aes(label = Label.x, x = Long.x, y = Lat.x),
             nudge_y = rate_parameters_source_destination_coords$Nudge_lat, nudge_x = rate_parameters_source_destination_coords$Nudge_long, size = 4) + 
  
  scale_fill_manual(name = "Region", values = paletteer_dynamic("cartography::pastel.pal", 19)) + 
  #scale_fill_brewer(palette="Set3") +
  #scale_color_brewer(type="qual", palette="heat") +
  #colorspace::scale_color_continuous_sequential(name = "Transmission Rate", palette ="plasma", limits = c(0.5, 1), breaks = c(0.5, 0.75, 1)) +
  
  # Add legend
  #scale_alpha_manual(name = "Support \n (Bayes Factors)", range = c(0.4, 1.0), breaks = c(1, 2, 3), labels = c("Moderate", "Strong", "Very Strong")) + 
  #cale_colour_manual(name = "Transmission Rate", labels = c("Low", "Intermediate", "High"), values = c("1" = "#67a9cf","2" = "#1b9e77", "3" = "#de2d26")) + 
  
  # Color scale
  scale_color_gradient(name = "Inferred Transmission \nRate (units)", low = "blue", high = "red", limits = c(0.65, 0.85), breaks = c(0.65, 0.75, 0.85)) +
  
  # Size scale
  scale_size_manual(name = "Support \n(Bayes Factors)", values = c('1' = 0.2, '2' = 0.6, '3' = 1.1), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +
  
  # Add rectangle for Kenya sites
  # geom_rect(
  #   xmin = -40,
  #   ymin = -46,
  #   xmax = 115,
  #   ymax = -95,
  #   fill = NA, 
  #   colour = "black",
  #   size = 0.6
  # ) + 
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=16),
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
  
  # Set scale limitis
  #coord_sf(xlim=c(-100, 150), ylim=c(-120,50))

global_transmission_network_importations


  
  geeom#Label Sites
  annotate("text", x = 37.5, y = -90, label = "Kenya Locations", size = 7, fontface = "bold") +
  annotate("text", x = -20, y = -50, label = "Western Kenya", size = 5, fontface = "bold") +
  annotate("text", x = -20, y = -65, label = "Central Kenya", size = 5, fontface = "bold") +
  annotate("text", x = -20, y = -80, label = "Coastal Kenya", size = 5, fontface = "bold")
#******************************************************************************#

#******************************************************************************#
# Exportations

#Modify the Kenya sites. 
# Centre
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] - 0.676281
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] - 37.782615
# 
# # Magnify
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)]*4
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)]*4
# 
# # Return
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] + 0.676281
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] + 37.782615
# 
# # Shift
# denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Latitude[which(denv2_sites$Country %in% kenya_sites)] - 25 
# denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] <- denv2_sites$Longitude[which(denv2_sites$Country %in% kenya_sites)] + 45

#Cate
#Leave only the importations and exportations from our sites. 
#Leave only the importations
#rate_parameters_evidence <- rate_parameters_evidence[which(rate_parameters_evidence$Destination %in% kenya_sites| rate_parameters_evidence$Source %in% kenya_sites),]
rate_parameters_evidence_exportations <- rate_parameters_evidence[which(rate_parameters_evidence$Source %in% kenya_sites),]

#Show only rates greater than the median. 
rate_parameters_evidence_reduced <- rate_parameters_evidence_exportations[which(rate_parameters_evidence_exportations$Median > 0.95),]
rate_parameters_evidence_reduced$Median_Factor <- NA
rate_parameters_evidence_reduced$Median_Factor[rate_parameters_evidence_reduced$Median < 1.15] <- 1
rate_parameters_evidence_reduced$Median_Factor[rate_parameters_evidence_reduced$Median > 1.15 & rate_parameters_evidence_reduced$Median < 1.35] <- 2
rate_parameters_evidence_reduced$Median_Factor[rate_parameters_evidence_reduced$Median > 1.35] <- 3
rate_parameters_evidence_reduced$Median <- as.factor(rate_parameters_evidence_reduced$Median_Factor)

# Leave only links that have at least moderate support
rate_parameters_evidence_reduced <- subset(rate_parameters_evidence_reduced, Bayes_Factor > 3 | Bayes_Factor < -3)
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor < 10] <- 1
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor > 10 & rate_parameters_evidence_reduced$Bayes_Factor < 30] <- 2
rate_parameters_evidence_reduced$Bayes[rate_parameters_evidence_reduced$Bayes_Factor > 30] <- 3
rate_parameters_evidence_reduced <- rate_parameters_evidence_reduced[,-7]

# Remove 1st column from denv2_sites df
#denv2_sites <- denv2_sites[,c(2, 3, 4)]
colnames(denv2_sites)[1] <- "Location"
rate_parameters_evidence_reduced_ids <- rowid_to_column(rate_parameters_evidence_reduced, var = "ID")

#get all the sources and destinations
rate_parameters_sources <- rate_parameters_evidence_reduced_ids[,c(1, 2, 5, 8)]
rate_parameters_sources_coords <- merge(rate_parameters_sources, denv2_sites, 
                                        by.x = "Source", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_sources_coords)[c(1, 5, 6)] <- c("Location", "Lat", "Long") 

rate_parameters_destinations <- rate_parameters_evidence_reduced_ids[,c(1, 3, 5, 8)]
rate_parameters_destinations_coords <- merge(rate_parameters_destinations, denv2_sites, 
                                             by.x = "Destination", by.y = "Location", all.x = TRUE)
colnames(rate_parameters_destinations_coords)[c(1, 5, 6)] <- c("Location", "Lat", "Long") 

#old merge
#rate_parameters_source_destination_coords <- merge(rate_parameters_sources_coords, rate_parameters_destinations_coords, by = c("ID", "Median", "Bayes"))
#rate_parameters_source_destination_coords <- rate_parameters_source_destination_coords[,c(1, 4, 5, 6, 7, 8, 9, 2, 3)]

# A function that makes a dateframe per connection (we will use these connections to plot each lines)
#dep_lon <- rate_parameters_source_destination_coords$Source_Long[i]
#dep_lat <- rate_parameters_source_destination_coords$Source_Lat[i]
#arr_lon <- rate_parameters_source_destination_coords$Destination_Long[i]
#arr_lat <- rate_parameters_source_destination_coords$Destination_Lat[i]
#group <- i

data_for_connection=function(dep_lon, dep_lat, arr_lon, arr_lat, group){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=100, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  inter$group=NA
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
    inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
  }else{
    inter$group=group
  }
  return(inter)
}

# Creation d'un dataframe complet avec les points de toutes les lignes faire.
data_ready_plot=data.frame()
for(i in c(1:nrow(rate_parameters_source_destination_coords))){
  tmp=data_for_connection(rate_parameters_source_destination_coords$Source_Long[i], rate_parameters_source_destination_coords$Source_Lat[i], 
                          rate_parameters_source_destination_coords$Destination_Long[i], rate_parameters_source_destination_coords$Destination_Lat[i], i)
  tmp$Median=rate_parameters_source_destination_coords$Median[i]
  tmp$Support=rate_parameters_source_destination_coords$Bayes[i]
  tmp$ID=rate_parameters_source_destination_coords$ID[i]
  data_ready_plot=rbind(data_ready_plot, tmp)
}


#data_ready_plot$lon[which(data_ready_plot$lon < -180)] <- data_ready_plot$lon[which(data_ready_plot$lon < -180)] + 360
# data_ready_plot$NewGroup <- data_ready_plot$group
# data_ready_plot$Number <- str_split(data_ready_plot$NewGroup, "")[[1]][1]
# data_ready_plot$Letter <- str_split(data_ready_plot$NewGroup, "")[[1]][2]
# new_data_ready_plot <- data_ready_plot[data_ready_plot$Letter == "A",]
#******************************************************************************#

#******************************************************************************#
#* Retrieve za warudo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Remove Anterctica, Kenya from map
world <- world[!world$name == "Antarctica",]
# Remove Anterctica from map
world <- world[!world$name == "Kenya",]

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


# Convert sites into an sf object
kenya_sites_sf <- st_as_sf(kenya_sites_coords, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")

#Repeat for all sites in our study
denv2_sites_sf <- st_as_sf(denv2_sites, coords = c("Longitude", "Latitude"), 
                           crs = 4326, agr = "constant")



# Get Kenya country
#kenya_map <- world[world$name == 'Kenya',]
#ggplot(data = kenya_map) + geom_sf() + coord_sf(xlim=c(-125, 190), ylim=c(-22,40))

#Move hte kenya map coordinates. 
# st_geometry(world[world$name == "Kenya",]) <- st_geometry(world[world$name == "Kenya",]) - c(37.782615, 0.676281)
# st_geometry(world[world$name == "Kenya",]) <- st_geometry(world[world$name == "Kenya",])*4
# st_geometry(world[world$name == "Kenya",]) <-  st_geometry(world[world$name == "Kenya",]) + c(37.782615, 0.676281)
# st_geometry(world[world$name == "Kenya",]) <-  st_geometry(world[world$name == "Kenya",]) + c(45, -25)

global_transmission_network_exportations <- ggplot(data = world) +
  geom_sf(aes(fill = study_region))+#, show.legend = F) + 
  #geom_sf(fill= "antiquewhite") +
  ggtitle("Local and Regional Spread of DENV2", 
          subtitle = "Exportations from Kenya Sites") +
  
  # Add other regions
  geom_sf(data = denv2_sites_sf, size = 4, color = "black", shape = 16) + 
  #geom_sf_label(data = denv2_sites_sf, aes(label = Location)) + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf, size = 4, shape = 23, fill = "black") + 
  geom_sf_text(data = kenya_sites_sf, aes(label = Country), nudge_y = -4, size = 5) + 
  
  #Add connection lines
  # Will add median and support and legend. 
  geom_path(data=data_ready_plot, aes(x=Long, y=Lat, alpha = Support, group = ID, color = Median), size = 1, 
            arrow=arrow(angle=30,length=unit(0.15,"inches"), type="closed")) +
  #scale_color_brewer(palette="Set3") +
  
  # Add legend
  scale_alpha_continuous(name = "Support \n (Bayes Factors)", range = c(0.4, 1.0), breaks = c(1, 2, 3), labels = c("Moderate", "Strong", "Very Strong")) + 
  scale_colour_manual(name = "Transmission Rate", labels = c("Low", "Intermediate", "High"), values = c("1" = "#67a9cf","2" = "#1b9e77", "3" = "#de2d26")) + 
  
  scale_fill_discrete(name = "Region") +
  
  # Add rectangle for Kenya sites
  geom_rect(
    xmin = -40,
    ymin = -46,
    xmax = 115,
    ymax = -95,
    fill = NA, 
    colour = "black",
    size = 0.6
  ) + 
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=16),
        #legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  
  # Set scale limitis
  coord_sf(xlim=c(-100, 150), ylim=c(-100,50)) +
  
  #Label Sites
  annotate("text", x = 37.5, y = -90, label = "Kenya Locations", size = 7, fontface = "bold") +
  annotate("text", x = -20, y = -50, label = "Western Kenya", size = 5, fontface = "bold") +
  annotate("text", x = -20, y = -65, label = "Central Kenya", size = 5, fontface = "bold") +
  annotate("text", x = -20, y = -80, label = "Coastal Kenya", size = 5, fontface = "bold")
#******************************************************************************#

#******************************************************************************#
