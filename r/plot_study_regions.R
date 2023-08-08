#******************************************************************************#
#12/12/2022
#Sindiso Nyathi
#Goal: Plot silhouettes of world regions for use in other plots
#******************************************************************************#

#******************************************************************************#
#*#Get study regions
study_regions <- read.csv('Data/study_regions_r.csv')

unique_study_regions <- unique(study_regions$Study.Region)
  
#* Retrieve za warudo
world <- ne_countries(scale = "medium", returnclass = "sf")

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


this_study_region <- unique_study_regions[[1]]
n_study_regions <- length(unique_study_regions)

for (i in c(1:n_study_regions)){
  
  this_study_region <- unique_study_regions[[i]]
  this_world <- world[world$study_region == this_study_region,] 
  
  this_region_plot <- ggplot(data = this_world, size = 1) +
  geom_sf(fill = 'grey35', show.legend = F) + 
    theme(panel.background = element_rect(fill = "aliceblue"), 
      axis.text.x=element_blank(),
      axis.text.y=element_blank(), 
      axis.line = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), unit="mm"), 
      plot.background = element_rect(fill = "aliceblue"))
  
  jpeg(paste("Images/", this_study_region, ".jpg", sep = ""), quality = 1)
  plot(this_region_plot) 
  dev.off()

}
#******************************************************************************#

#******************************************************************************#
