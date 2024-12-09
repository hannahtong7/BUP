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
 
#here I'm setting the spatial extent to be broadly consistent with that of my study species (you need to make sure it is sampling from the same extent). Remember to find out how a function works you can do ?function
bg<-spatSample(predictors,2000,"random", na.rm=TRUE, as.points=TRUE,ext=e)

#Here we'll plot our background points on a map of climwin variable 1 (you could change this to any of the worldclim variables)
plot(predictors, 1)
points(bg, cex=0.1)

occlatlon<-cbind(occ$lon,occ$lat)
presvals <- extract(predictors, occlatlon)
#presvals is the climate data for where the species is present
backvals <- values(bg)
#backvals is the climate data for the background data
bg_lonlat<-geom(bg)
lonlats<-rbind(occlatlon, bg_lonlat[,c("x","y")])
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(backvals)))
#The first column of the dataset is a vector of 1s for presences and 0s for background data.
sdmdata <- data.frame(cbind(lonlats,pb, rbind(presvals, backvals)))
#here we combine the presence and background data into a single data frame
pairs(sdmdata[,4:7], cex=0.1)

sdmdata<-subset(sdmdata,is.na(bio_1)==F)
#here we're just removing a couple of rows where the climate data are NAs.


specdata<-as.data.frame(cbind(rep("Indri indri",length(sdmdata[,1])),
                              sdmdata))

names(specdata)[1:4]<-c("species","longitude","latitude","presabs")

specdata<-subset(specdata,presabs==1)

backdata<-as.data.frame(cbind(rep("background",length(sdmdata[,1])),
                              sdmdata))

names(backdata)[1:4]<-c("","longitude","latitude","presabs")

backdata<-subset(backdata,presabs==0)


write.table(specdata[,-4],paste(output_dir,"/Indriindri_swd.csv",sep=""),col.names=T,row.names=F,sep=",")
write.table(backdata[,-4],paste(output_dir,"/background.csv",sep=""),col.names=T,row.names=F,sep=",")

model<-MaxEnt(sdmdata[,-c(1:3)],sdmdata[,3],removeDuplicates=TRUE)



plot(model)







