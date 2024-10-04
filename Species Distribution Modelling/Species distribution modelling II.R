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



  