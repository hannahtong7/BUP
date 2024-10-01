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