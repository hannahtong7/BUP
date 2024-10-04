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

#Downloading worldclim data 
output_dir<-"~/Documents/temp/BUP/Data"
bio_glob<-worldclim_global(var="bio", res=10,path=output_dir, version="2.1")
dim(bio_glob)

#we will also clip the spatraster so it only covers the spatial extent of our study species. First its longitudes then latitudes
summary(occ$lon)
summary(occ$lat)

e <- ext(40, 60, -30, -10)

predictors <- crop(bio_glob, e)

names(predictors)<-substring(names(predictors),11,16)
#here we're just shortening the names of predictors by taking the 11th to 16th characters.

plot(predictors,1:9)

#Add species data into a plot of plate for the first variable
plot(predictors,1)
points(occ$lon,occ$lat, col='orange',pch=16,cex=0.2)
  
