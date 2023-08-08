#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Annottate and Visualize Tranmission Rate maps using spartial tools in R. 
# For DENV1, 3 and 4
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
require(rnaturalearthdata)
require(geosphere)
require(tidyverse)
require(lwgeom)
require(cowplot)
require(paletteer)
require(geomtextpath)

#* set wd
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathis Files/Dengue Evolution/Phylodynamics")
#******************************************************************************#

#******************************************************************************#
#* Read in the data
# denv1_transitions <- read.csv('Results/denv1_beast_complex1_run1_mcc.tree_transitions.csv')
# denv2_transitions <- read.csv('Results/denv2_beast_complex5_run123_mcc.tree_transitions.csv')
# denv3_transitions <- read.csv('Results/denv3_beast_complex1_run1_mcc.tree_transitions.csv')
# denv4_transitions <- read.csv('Results/denv4_beast_complex1_run1_mcc.tree_transitions.csv')
# 
# denv1_transitions$Serotype <- 1
# denv2_transitions$Serotype <- 2
# denv3_transitions$Serotype <- 3
# denv4_transitions$Serotype <- 4
# 
# denv_transitions <- rbind(denv1_transitions, denv2_transitions, denv3_transitions, denv4_transitions)
# denv_transitions$Source[which(denv_transitions$Source %in% kenya_sites)] <- 'Kenya'
# denv_transitions$Destination[which(denv_transitions$Destination %in% kenya_sites)] <- 'Kenya'
# table(denv_transitions$Destination)

# *** Stopping point, manually annotate linaeges before reading in again***
#write.csv(denv_transitions, 'Results/denv_transitions.csv') 
denv_transitions_lineages <- write.csv(denv_transitions_lineages, 'Results/denv_transitions_lineages.csv') 

denv_transitions_lineages$Date <- round(2022 - denv_transitions_lineages$Height)
denv_transitions_lineages$Upper <- round(2022 - denv_transitions_lineages$Upper)
denv_transitions_lineages$Lower <- round(2022 - denv_transitions_lineages$Lower)

Locations <- c(denv_transitions_lineages$Source, denv_transitions_lineages$Destination)
Locations <- unique(Locations)
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

#*
#*#Prep map
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
#******************************************************************************#

#******************************************************************************#
#* Plot data
denv_transitions_lineages$Lineage <- as.factor(denv_transitions_lineages$Lineage)
denv_transitions_lineages_locations <- read.csv('Results/rate_parameters_evidence_exportations_importations_locations.csv')
denv_transitions_lineages$DateF <- NA
denv_transitions_lineages$DateF <- paste(denv_transitions_lineages$Date, '\n [', 
                                         denv_transitions_lineages$Upper, ' - ', 
                                         denv_transitions_lineages$Lower, ']', sep = '')

# MAke posterior categorical to label
denv_transitions_lineages$Posterior_lab <- NA
denv_transitions_lineages$Posterior_lab[denv_transitions_lineages$Posterior < 0.75] <- '< 0.75' 
denv_transitions_lineages$Posterior_lab[denv_transitions_lineages$Posterior < 0.9 & denv_transitions_lineages$Posterior > 0.75] <- '0.75 - 0.90'
denv_transitions_lineages$Posterior_lab[denv_transitions_lineages$Posterior > 0.9] <- '> 0.90'


rate_parameters_evidence_exportations_importations_L3 <- rate_parameters_evidence_exportations_importations[rate_parameters_evidence_exportations_importations$Lineage == 3,]
rate_parameters_evidence_exportations_importations_L2 <- rate_parameters_evidence_exportations_importations[rate_parameters_evidence_exportations_importations$Lineage == 2,]
rate_parameters_evidence_exportations_importations_L1 <- rate_parameters_evidence_exportations_importations[rate_parameters_evidence_exportations_importations$Lineage == 1,]

rate_parameters_evidence_exportations_importations_locations_L1 <- rate_parameters_evidence_exportations_importations_locations[rate_parameters_evidence_exportations_importations_locations$L1 == 1,]
rate_parameters_evidence_exportations_importations_locations_L2 <- rate_parameters_evidence_exportations_importations_locations[rate_parameters_evidence_exportations_importations_locations$L2 == 1,]
rate_parameters_evidence_exportations_importations_locations_L3 <- rate_parameters_evidence_exportations_importations_locations[rate_parameters_evidence_exportations_importations_locations$L3 == 1,]

this_lineage_plot_data <- denv_transitions_lineages
this_lineage_locations <- denv_transitions_lineages_locations#rate_parameters_evidence_exportations_importations_locations_L1

global_transmission_network_exportations_importations_plot <- ggplot() +
  geom_sf(data = world, aes(fill = study_region), show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'grey10', fill = 'grey') + 
  xlim(-20, 180) +
  # Add other regions
  geom_point(data = this_lineage_locations, aes(x = Long.x, y = Lat.x), size = 5, color = "grey40", shape = 16) + 
 # geom_text(data=this_lineage_locations, aes(label = Label, x = Long.x, y = Lat.x), 
  #          nudge_y = this_lineage_locations$Nudge_lat, nudge_x = this_lineage_locations$Nudge_long, size = 7) + 
  
  #geom_point(data = rate_parameters_evidence_importations, aes(x = Long.x, y = Lat.x), size = 5, color = "grey40", shape = 16) +  
  
  #Add connection lines
  
  ##Exportations
  #geom_curve(data=rate_parameters_evidence_exportations_importations_L3, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Posterior_lab), curvature = -0.3, 
   #          arrow = arrow(angle = 20, length = unit(0.2, "inches"),
    #                       ends = "last", type = "closed"), linetype = 1) + 
  
  geom_textcurve(data = this_lineage_plot_data, 
                 aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, label = Date), curvature = -0.4, 
                           arrow = arrow(angle = 20, length = unit(0.2, "inches"),
                                        ends = "last", type = "closed"), linetype = 1, size = 10, linewidth = 2) +

  # Size scale
  scale_size_manual(name = "Support \n(Posterior Probability)", values = c('< 0.75' = 0.5, '0.75 - 0.90' = 1.5, '> 0.90' = 2), labels = c("< 0.75", "0.75 - 0.90", "> 0.90"), guide = "legend", drop = FALSE) +
  
  #geom_curve(data=rate_parameters_evidence_exportations_importations_L2, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, colour = Lineage), curvature = 0.3, 
    #         arrow = arrow(angle = 20, length = unit(0.2, "inches"),
     #                      ends = "last", type = "closed"), linetype = 1) + 
  
 # geom_curve(data=rate_parameters_evidence_exportations_importations_L1, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, colour = Lineage), curvature = 0.1, 
  #           arrow = arrow(angle = 20, length = unit(0.2, "inches"),
   #                        ends = "last", type = "closed"), linetype = 1) + 
  
  #geom_text(data=rate_parameters_evidence_exportations_importations, aes(label = Label.y, x = Long.y, y = Lat.y),
  #         nudge_y = rate_parameters_evidence_exportations_importations$Nudge_lat, nudge_x = rate_parameters_evidence_exportations_importations$Nudge_long, size = 6) + 
  
  
  ##Importations
  #geom_curve(data=rate_parameters_evidence_importations, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, size = Bayes), curvature = 0.3, 
  #           arrow = arrow(angle = 20, length = unit(0.2, "inches"),
  #                        ends = "last", type = "closed"), linetype = 1, colour = 'black') + 
  #geom_text(data=rate_parameters_evidence_importations, aes(label = Label.x, x = Long.x, y = Lat.x),
  #         nudge_y = rate_parameters_evidence_importations$Nudge_lat, nudge_x = rate_parameters_evidence_importations$Nudge_long, size = 6) + 

scale_fill_manual(name = "Region", values = paletteer_dynamic("cartography::pastel.pal", 19)) + 
  
  # Color scale
  #scale_color_manual(name = "Support (Bayes Factor)", values = c('0' = "#FEE5D9", '1' = "#FCAE91", '2' = "#FB6A4A", '3' = "#CB181D"), labels = c("Low", "Moderate", "Strong", "Very Strong"), guide = "legend") +

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

global_transmission_network_exportations_importations_plot #L 2 and L3: PDF 8 x 18, L1: PDF 8 x 12

#******************************************************************************#

#******************************************************************************#
#* Local trnasmission
rate_parameters_evidence_exportations_importations_local <- read.csv('Results/rate_parameters_evidence_exportations_importations_local.csv')
rate_parameters_evidence_exportations_importations_local$Lineage <- as.factor(rate_parameters_evidence_exportations_importations_local$Lineage)

rate_parameters_evidence_exportations_importations_local$DateF <- NA
rate_parameters_evidence_exportations_importations_local$DateF <- paste(rate_parameters_evidence_exportations_importations_local$Date, '\n [', 
                                                                        rate_parameters_evidence_exportations_importations_local$Upper, ' - ', 
                                                                        rate_parameters_evidence_exportations_importations_local$Lower, ']', sep = '')

rate_parameters_evidence_exportations_importations_local_L3 <- rate_parameters_evidence_exportations_importations_local[rate_parameters_evidence_exportations_importations_local$Lineage == 3 | rate_parameters_evidence_exportations_importations_local$Lineage == 1,]
rate_parameters_evidence_exportations_importations_local_L2 <- rate_parameters_evidence_exportations_importations_local[rate_parameters_evidence_exportations_importations_local$Lineage == 2,]
rate_parameters_evidence_exportations_importations_local_L1 <- rate_parameters_evidence_exportations_importations_local[rate_parameters_evidence_exportations_importations_local$Lineage == 1,]

kenya_sites_sf_L1_4 <- kenya_sites_sf[kenya_sites_sf$Location %in% c('Wajir', 'Mombasa'),]
kenya_sites_sf_L2_4 <- kenya_sites_sf[kenya_sites_sf$Location %in% c("Ukunda", "Msambweni", 
                                                                     "Malindi", "Mtwapa", "Lamu", 'Voi'),]
kenya_sites_sf_L3_4 <- kenya_sites_sf[kenya_sites_sf$Location %in% c("Kisumu", "Chulaimbo"),]

global_transmission_local <- ggplot(data = world, size = 5) +
  geom_sf(data = world, fill = 'grey', show.legend = F, colour = NA) + 
  geom_sf(data = world_kenya, colour = 'black', fill = '#99C5E3') + 
  
  # Add Kenya sites. 
  geom_sf(data = kenya_sites_sf, size = 11.5, shape = 20, color = "grey40") + 
  
  # Add Kenya site labels
  # Add Kenya site labels
  #geom_sf_text(data = kenya_sites_sf_L1_4, aes(label = Location), size = 9.5, nudge_x = -0.55) +
  #geom_sf_text(data = kenya_sites_sf_L2_4, aes(label = Location), size = 9.5, nudge_x = 0.6) +
  geom_sf_text(data = kenya_sites_sf_L3_4, aes(label = Location), size = 9.5, nudge_x = 0.7) +
  
  # Add connecting paths
  geom_curve(data=rate_parameters_evidence_exportations_importations_local_L3, aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, colour = Lineage), curvature = 0.5, 
            arrow = arrow(angle = 15, length = unit(0.25, "inches"),
                          ends = "last", type = "closed"), linetype = 'solid', size = 1.75) +
  
  #geom_textcurve(data = rate_parameters_evidence_exportations_importations_local_L2, 
   #              aes(x = Long.x, y = Lat.x, xend = Long.y, yend = Lat.y, label = DateF), curvature = 0.6, 
    #             arrow = arrow(angle = 20, length = unit(0.2, "inches"),
                               #ends = "last", type = "closed"), linetype = 1, size = 6) +
  # Expand Axes
  coord_sf(xlim=c(37, 42), ylim=c(-5, -1.5)) +
  
  # Size scale
  #scale_size_manual(name = "Support \n(Bayes Factors)", values = c('1' = 0.4, '2' = 1.2, '3' = 2.2), labels = c("Moderate", "Strong", "Very Strong"), guide = "legend") +
  scale_color_manual(name = "Lineage", values = c('1' = "#D62728", '2' = "#2CA02C", '3' = "#FF7F0E"), labels = c("Lineage 1 (Cosmopolitan) \n Circulated 2013", "Lineage 2 (Cosmopolitan)", "Lineage 3 (Asian II)"), guide = "none") +
  
  theme(plot.title = element_text(size = 28, hjust = 0.5),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue"), 
        #legend.title=element_text(size=24), 
        #legend.text=element_text(size=20),
        #legend.key.width = unit(3.5, "line"), 
        legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

global_transmission_local
#save 8 x 10

