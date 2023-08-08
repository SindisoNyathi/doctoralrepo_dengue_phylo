#******************************************************************************#
#05/24/2022
#Sindiso Nyathi
#Goal: Plot BDSKY Output

# Notes
# This file runs as is; chnage only the serotype and run line by line. The code 
# Modifies the format of the sequence name to match the genbak format. 
#******************************************************************************#
#*
#*
#*#******************************************************************************#
#*
library(devtools)
library(bdskytools)

setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/DENVA")

prelim_log <- readLogfile("beast/prelim/run1_cluster2_com/denva_beast_run1_cluster2_com.log", burnin = 0.1)
#******************************************************************************#
#*
#*
#*#******************************************************************************#
#*
re_sky <- getSkylineSubset(prelim_log, "reproductiveNumber")
Re_hpd <- getMatrixHPD(re_sky)
delta_hpd <- getHPD(prelim_log$becomeUninfectiousRate_BDSKY_Serial)

#Plot
plotSkyline(1:10, Re_hpd, ylab = "R")

#Plot better
timegrid <- seq(1, 4, length.out = 201)
re_gridded <- gridSkyline(re_sky, prelim_log$origin_BDSKY_Serial, timegrid)
re_gridded_hpd <- getMatrixHPD(re_gridded)

times = 2018 - timegrid
plotSkyline(times, re_gridded_hpd, type= 'smooth' , xlab = "Time" , ylab="R" )

# Make a pretty plot
plotSkylinePretty(range(times), as.matrix(delta_hpd), type='step', axispadding=0.0, 
                  col=pal.dark(cblue), fill=pal.dark(cblue, 0.5), col.axis=pal.dark(cblue), 
                  side=4, xaxis=FALSE)

plotSkylinePretty(times, re_gridded_hpd, type='smooth', axispadding=0.0, 
                  col=pal.dark(corange), fill=pal.dark(corange, 0.5), #col.axis=pal.dark(corange), 
                  xlab="Time", ylab=expression("R"[e]), yline=1, xline=2, xgrid=TRUE, 
                  ygrid=TRUE, gridcol=pal.dark(cgray), ylims=c(0,8), new=TRUE,
                  xticklabels = c('Jan 2014', 'Jun 2014', 'Jan 2015', 'Jun 2015', 
                                  'Jan 2016', 'Jun 2016', 'Jan 2017'))

#save the file.
re_gridded_hpd <- as.data.frame(t(re_gridded_hpd))
colnames(re_gridded_hpd) <- c("Lower", "R", "Upper")
denva_cluster1_env <- data.frame("Time" = times, "Lower" = re_gridded_hpd$Lower, "R" = re_gridded_hpd$R, "Upper" = re_gridded_hpd$Upper)
write.csv(denva_cluster1_env, "beast/prelim/denva_cluster2_com_r.csv")
#******************************************************************************#
#*
#*
#******************************************************************************#
