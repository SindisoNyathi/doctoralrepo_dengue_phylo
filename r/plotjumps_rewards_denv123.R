#******************************************************************************#
#11/07/2022
#Sindiso Nyathi
#Goal: Markov Jumps plots
#******************************************************************************#
#*
#*#******************************************************************************#
#* Preliminaries. 
#* 
path <- "Choose/Your/Path"
setwd(path)

# Load required files
require(ggplot2)
library(MarkovJumpR)
library(lubridate)
require(magrittr)
require(tidyverse)
#******************************************************************************#
#*
#*******************************************************************************#
#* Get files.
travelHist_raw <- read.csv('Data/denv2_alljumps.csv', header = T)
travelHistPaths <- loadPaths(fileName = 'Data/denv2_jumps_l2.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv2_jumps_l1.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv2_jumps_wa.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv2_jumps_sa.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv2_jumps_ea.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv3_keta_lineage_d.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv3_ea_lineage_c.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv3_waca_lineage_b.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv3_waca_lineage_a.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv1_ea_lineage_a.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv1_cawa_lineage_c.csv')
travelHistPaths <- loadPaths(fileName = 'Data/denv1_cawa_lineage_b.csv')
colnames(travelHistPaths)

#Read colours
region_colours <- read.csv('Data/manuscript_colours_jumps.csv')
locationmap <- region_colours[,c(2, 4, 1, 3)]
locationmap$V1 <- as.character(locationmap$V1)
colnames(locationmap) <- c('location', 'position', 'color', 'label')

#Get the total number of states
n_states <- length(unique(travelHistPaths$location))

#Choose locations to focus on based on MCC tree transitions
Other <- c("Europe", 
           "NorthernAfrica",  
           "CentralAmericaandCaribbean", 
           "Australia", 
           "CentralAfrica", 
           "SouthAmerica", 
           "NorthAmerica", 
           #"CentralSouthEasternAsia", 
           "WesternAfrica", 
           "SouthernAfrica",
           #"EasternAfrica",
           "EasternAsia", 
           "Kenya",
           #"UpperSouthEasternAsia",
           "LowerSouthEasternAsia",
           "WesternAsia",
           "Oceania"
           )
# Set non-focus locations to other
travelHistPaths$location[travelHistPaths$location %in% Other] <- "Other"
travelHistPaths$paths$location[travelHistPaths$paths$location %in% Other] <- "Other" 

# Additional formatting to understand what locations are being plotted and order them etc
output_locations <- unique(travelHistPaths$location)
unique(travelHistPaths$paths$location)
travelHistPaths$paths <- travelHistPaths$paths[!(travelHistPaths$paths$location == 'Other'),]
these_locs <- unique(travelHistPaths$paths$location)
locationmap <- locationmap[(locationmap$location %in% these_locs),]
locationmap <-locationmap[order(locationmap$position, decreasing = F),] 
locationmap$position <- c(1:nrow(locationmap))

# set x-axis date labels
dateLabels <- seq(1920, 2023, 20)
truedate <- decimal_date(parse_date_time(dateLabels, orders = c('y')))

# plot trajectories
plotPaths(travelHistPaths$paths, locationMap = locationmap,
          yJitterSd = 0.1, alpha = 0.025, addLocationLine = TRUE,
          xAt = truedate,
          #xlim = c(1920, 2022), 
          xLabels = dateLabels, verticalColor = NULL,
          mustDisplayAllLocations = TRUE, cex.labels = 1.75)

#save plots 15 x 9 
#Plots/man1/denv2_markov_jumps_base_l2.pdf
#Plots/man1/denv2_markov_jumps_base_l1.pdf
#Plots/man1/denv2_markov_jumps_base_wa.pdf
#Plots/man1/denv2_markov_jumps_base_sa.pdf
#Plots/man1/denv2_markov_jumps_base_ea.pdf

#Plots/denv3_markov_jumps_base_ld
#Plots/denv3_markov_jumps_base_lc
#Plots/denv3_markov_jumps_base_lb
#Plots/denv3_markov_jumps_base_la

#Plots/denv1_markov_jumps_base_lc
#Plots/denv1_markov_jumps_base_lb
#Plots/denv1_markov_jumps_base_la
#Fin
#******************************************************************************#
#*
#*******************************************************************************#
#* Terrence this is stupid stuff by A. E. Housman
#* "Terence, this is stupid stuff!
# You eat your victuals fast enough;
# There can't be much amiss, 'tis clear,
# To see the rate you drink your beer.
# But oh, good Lord, the verse you make,
# It gives a chap the belly-ache!
#   The cow, the old cow, she is dead;
# It sleeps well, the horned head...
# We poor lads, 'tis our turn now
# To hear such tunes as killed the cow!
# Pretty friendship 'tis to rhyme
# Your friends to death before their time
# Moping melancholy mad!
#   Come, pipe a tune to dance to, lad!"
# 
# Why, if 'tis dancing you would be,
# There's brisker pipes than poetry.
# Say, for what were hop-yards meant,
# Or why was Burton built on Trent?
# Oh many a peer of England brews
# Livelier liquor than the Muse,
# And malt does more than Milton can
# To justify God's ways to man.
# Ale, man, ale's the stuff to drink
# For fellows whom it hurts to think:
# Look into the pewter pot
# To see the world as the world's not.
# And faith, 'tis pleasant till 'tis past:
# The mischief is that 'twill not last.
# Oh I have been to Ludlow fair
# And left my necktie God knows where,
# And carried half way home, or near,
# Pints and quarts of Ludlow beer:
# Then the world seemed none so bad,
# And I myself a sterling lad;
# And down in lovely muck I've lain,
# Happy till I woke again.
# Then I saw the morning sky:
# Heigho, the tale was all a lie;
# The world, it was the old world yet,
# I was I, my things were wet,
# And nothing now remained to do
# But begin the game anew.
# 
# 
# Therefore, since the world has still
# Much good, but much less good than ill,
# And while the sun and moon endure
# Luck's a chance, but trouble's sure,
# I'd face it as a wise man would,
# And train for ill and not for good.
# 'Tis true, the stuff I bring for sale
# Is not so brisk a brew as ale:
# Out of a stem that scored the hand
# I wrung it in a weary land.
# But take it: if the smack is sour,
# The better for the embittered hour;
# It should do good to heart and head
# When your soul is in my soul's stead;
# And I will friend you, if I may,
# In the dark and cloudy day.
# 
# There was a king reigned in the East:
# There, when kings will sit to feast,
# They get their fill before they think
# With poisoned meat and poisoned drink.
# He gathered all the springs to birth
# From the many-venomed earth;
# First a little, thence to more,
# He sampled all her killing store;
# And easy, smiling, seasoned sound,
# Sate the king when healths went round.
# They put arsenic in his meat
# And stared aghast to watch him eat;
# They poured strychnine in his cup
# And shook to see him drink it up:
# They shook, they stared as white's their shirt:
# Them it was their poison hurt.
# --I tell the tale that I heard told.
# Mithridates, he died old.

