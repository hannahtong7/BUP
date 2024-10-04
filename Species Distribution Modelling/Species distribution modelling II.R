install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
## 
## The downloaded binary packages are in
##  /var/folders/wr/gm_zfh2x5sl16kqzlk87swm00000gp/T//RtmpMNJezT/downloaded_packages
install.packages("predicts",dependencies=TRUE,repos="https://cloud.r-project.org")
## 
## The downloaded binary packages are in
##  /var/folders/wr/gm_zfh2x5sl16kqzlk87swm00000gp/T//RtmpMNJezT/downloaded_packages
install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")
## 
## The downloaded binary packages are in
##  /var/folders/wr/gm_zfh2x5sl16kqzlk87swm00000gp/T//RtmpMNJezT/downloaded_packages
library(geodata)
library(predicts)
library(terra)

#By stating geo = TRUE we select only records that have latitude and longitude recorded.
occdata <- geodata::sp_occurrence("Indri", "indri", geo=FALSE,removeZeros=TRUE,start=1,end=10000)

#get dimensions of the data.frame
dim(occdata)
occdata[1:10,]

#Now we have the data for our species we can plot the global distribution to make sure it fits with our expectation.
wrld <- world(path="~/Documents/temp/BUP/Data")
#this function gives us an outline of the world's political boundaries. Reminder, if ever you want to know more about an R function, you can write ?function.name, e.g., ?world
plot(wrld, xlim=c(-180,180), ylim=c(-80,80), col="light yellow", border="light gray")
# add the points
points(occdata$lon, occdata$lat, col='blue', pch=20)

#remove duplicate records
dups <- duplicated(occdata[, c('lon', 'lat')])
#This identifies observations that have already appeared above
sum(dups)
#There are a lot of them, so removing them will give us a smaller dataset to work with
occ <- occdata[!dups, ]










  
