install.packages("raster",dependencies=TRUE,repos="https://cloud.r-project.org")
library(raster)

#Set working directory 
setwd("~/Documents/temp/BUP/Data")

#Data preparation - download ring ouzel and climate data
avi_dat <- read.table('Data_SwissBreedingBirds.csv', header=T, sep=',')
head(avi_dat)

