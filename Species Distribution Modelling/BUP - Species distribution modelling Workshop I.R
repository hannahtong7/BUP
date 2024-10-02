install.packages("raster",dependencies=TRUE,repos="https://cloud.r-project.org")
library(raster)

#Set working directory 
setwd("~/Documents/temp/BUP/Data")

#Data preparation - download ring ouzel and climate data
avi_dat <- read.table('Data_SwissBreedingBirds.csv', header=T, sep=',')
head(avi_dat)
summary(avi_dat)

#Subset data of ring ouzels 
ouzel_cols <- c('Turdus_torquatus', 'bio_5', 'bio_2', 'bio_14', 'blockCV_tile')
ouzel_df <- data.frame(avi_dat)[ouzel_cols]
summary(ouzel_df)

#Import current and future climate data for Switzerland and clip it to the extent of Switzerland
#download and import the same climate variables in directly from CLIMWIN.
output_dir<-"~/Documents/temp/BUP/Species Distribution Modelling"
options(timeout=20000000)

bio_curr <- getData('worldclim', var='bio', res=0.5, lon=5.5, lat=45.5, path=output_dir)[[c(2,5,14)]]
bio_fut <- getData('CMIP5', var='bio', res=0.5, lon=5.5, lat=45.5, rcp=45, model='NO', year=50,path=output_dir)[[c(2,5,14)]]



