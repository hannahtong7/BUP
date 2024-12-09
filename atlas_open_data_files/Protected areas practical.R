rm(list=ls())
gc()
graphics.off()
quartz()

library(tidyverse)
library(maps)
library(vegan)
library(sf)
library(betapart)
install.packages("maps")
install.packages("sf")
install.packages("betapart")

#####
##Data treatment
#####

bird.dat.tot <- read.csv("atlas_open_data_files/distributions.csv")
head(bird.dat.tot)
str(bird.dat.tot)
#Tells you the different years that there are in the period column 
unique(bird.dat.tot$period)
#Write a line of code so that the data frame only contains rows for which the resolution is 10. 
bird.dat.tot <- bird.dat.tot[which(bird.dat.tot$resolution==10),]
#load coords.dat data frame
coords.dat <- read.csv("atlas_open_data_files/grid_square_coordinates_lookup.csv")
head(coords.dat)
str(coords.dat)
#Only contains rows for which the resolution is 10. 
coords.dat <- coords.dat[which(coords.dat$resolution==10),]
##grid: https://britishnationalgrid.uk/#NC148273

## https://jncc.gov.uk/our-work/uk-protected-area-datasets-for-download/




map(database="world",regions="UK")
points(coords.dat$long,coords.dat$lat)

UK <- map(database="world",regions="UK",fill=TRUE)
#Store the polygons defining the uk and covert them into a simple feature. 
UK.sf <- st_as_sf(UK)
UK.sf <- st_cast(UK.sf, "POLYGON") ##don't worry about the warning
plot(st_geometry(UK.sf))
points(coords.dat)

##keep cells with at least one corner on the main island
# coords.sf <- st_multipoint(as.matrix(coords.dat[,1:2]))
# coords.dat.keep <- st_intersection(coords.sf,UK.sf)
#Keep grid cells with at least one corner on the main island.
coords.sf <- st_as_sf(coords.dat[,1:2],coords = c("long", "lat"), crs = st_crs(UK.sf))
#Isolate the points intersecting the different polygons representing the UK and only keep the points intersecting the 15th polygon, which corresponds to the main island. 
coords.dat.island.ind <- st_intersects(UK.sf,coords.sf)[[15]]
coords.dat.island <- coords.dat[coords.dat.island.ind,]
#Visualise the output to confirm that you kept the correct points. 
plot(st_geometry(UK.sf))
points(coords.dat.island)

#Select the most recent time frame
#Create one data frame for summer and one for winter

##select bird data for the desired period
bird.dat <- bird.dat.tot[which(bird.dat.tot$period=="2008-11" | bird.dat.tot$period=="2007/08-10/11"),c("period","speccode","grid")]
#dimension of the matrix
dim(coords.dat.island)
coords.dat.island <- coords.dat.island[which(coords.dat.island$grid %in% bird.dat$grid),]
dim(coords.dat.island)
bird.dat <- bird.dat[which(bird.dat$grid %in% coords.dat.island$grid),]

bird.summer.dat <- bird.dat[which(bird.dat$period=="2008-11"),c("speccode","grid")]
bird.winter.dat <- bird.dat[which(bird.dat$period=="2007/08-10/11"),c("speccode","grid")]

# coords.summer.dat <- coords.dat.island[which(coords.dat.island$grid %in% bird.summer.dat$grid),]
# coords.summer.dat <- coords.summer.dat[order(coords.summer.dat$grid),]
# coords.winter.dat <- coords.dat.island[which(coords.dat.island$grid %in% bird.winter.dat$grid),]
# coords.winter.dat <- coords.winter.dat[order(coords.winter.dat$grid),]

#Transform the data frames into site by species data frames - use tidyverse
bird.summer.dat$presence <- 1 ##add a column with the values to populate the site-by-species data frame
bird.summer.dat.pa <- bird.summer.dat %>% 
  pivot_wider(names_from=speccode,values_from=c(presence)) ##site-by-species data frames with NAs
list0 <- as.list(rep(0,ncol(bird.summer.dat.pa))) ##values to replace the NAs
names(list0) <- names(bird.summer.dat.pa)
bird.summer.dat.pa <- as.data.frame(bird.summer.dat.pa %>% replace_na(list0)) ##replace the NAs by 0’s
row.names(bird.summer.dat.pa) <- bird.summer.dat.pa$grid ##change row names
bird.summer.dat.pa <- bird.summer.dat.pa[,-1] ##remove the first column with site names
bird.summer.dat.pa <- bird.summer.dat.pa[order(row.names(bird.summer.dat.pa)),] ##sort by grid cell names


bird.winter.dat$presence <- 1
bird.winter.dat.pa <- bird.winter.dat %>% 
  pivot_wider(names_from=speccode,values_from=c(presence))
list0 <- as.list(rep(0,ncol(bird.winter.dat.pa)))
names(list0) <- names(bird.winter.dat.pa)
bird.winter.dat.pa <- as.data.frame(bird.winter.dat.pa %>% replace_na(list0))
row.names(bird.winter.dat.pa) <- bird.winter.dat.pa$grid
bird.winter.dat.pa <- bird.winter.dat.pa[,-1]
bird.winter.dat.pa <- bird.winter.dat.pa[order(row.names(bird.winter.dat.pa)),]

#Confirm there are no empty cells, nor species occuring in no cell (since we only kept species occuring on the main island)
which(colSums(bird.summer.dat.pa)==0)
which(rowSums(bird.summer.dat.pa)==0)
which(colSums(bird.winter.dat.pa)==0)
which(rowSums(bird.winter.dat.pa)==0)




#Compute the different patters for the data - obtain an overview of how bird diversity is structured over GB
###Gamma and alpha diversity
#Gamma diversity is the number of columns of the data frams.
##Gamma
ncol(bird.summer.dat.pa)
ncol(bird.winter.dat.pa)
#Alpha diversity is the average sum of each row.
##Alpha
mean(rowSums(bird.summer.dat.pa))
mean(rowSums(bird.winter.dat.pa))
#Plot the distribution of alpha diversity values 
par(mfrow=c(1,2))
hist(rowSums(bird.summer.dat.pa),main="Summer",xlab="Richness")
hist(rowSums(bird.winter.dat.pa),main="Winter",xlab="Richness")

#Species accumulation curves
##species accumulation curves & Chao estimator
SAC.summer <- specaccum(bird.summer.dat.pa)
SAC.winter <- specaccum(bird.winter.dat.pa)
#Do you use the chao estimator? 
Estim.summer <- poolaccum(bird.summer.dat.pa)
Estim.winter <- poolaccum(bird.winter.dat.pa)


par(mfrow=c(1,2))
plot(SAC.summer$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim.summer$chao))),ylab="Richness",main="Summer")
points(3:nrow(bird.summer.dat.pa),rowMeans(Estim.summer$chao),pch=2,lty=2,lwd=2,type="b",col="red")
plot(SAC.winter$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim.winter$chao))),ylab="Richness",main="Winter")
points(3:nrow(bird.winter.dat.pa),rowMeans(Estim.winter$chao),pch=2,lty=2,lwd=2,type="b",col="red")

last(rowMeans(Estim.summer$chao))/last(SAC.summer$richness)
last(rowMeans(Estim.winter$chao))/last(SAC.winter$richness)


#Compute the histogram of the species occupancy for the simpson and soresen dissimilarity indices and plot thier distributions
##Beta diversity
beta.summer <- beta.pair(bird.summer.dat.pa)
beta.winter <- beta.pair(bird.winter.dat.pa)

mean(beta.summer$beta.sim)
mean(beta.winter$beta.sim)

mean(beta.summer$beta.sor)
mean(beta.winter$beta.sor)

#Plot simpson summer and winter
par(mfrow=c(1,2))
hist(beta.summer$beta.sim)
hist(beta.winter$beta.sim)
#Sorensen plot
par(mfrow=c(1,2))
hist(beta.summer$beta.sor)
hist(beta.winter$beta.sor)

#Compute the histogram of the species occupancy which is the number of grid cells in which they occur 
##OFD
freq.summer <- colSums(bird.summer.dat.pa)/nrow(bird.summer.dat.pa)
freq.winter <- colSums(bird.winter.dat.pa)/nrow(bird.winter.dat.pa)

par(mfrow=c(1,2))
hist(freq.summer,main="Summer",xlab="Occupancy",breaks = seq(0,1,0.1))
hist(freq.winter,main="Winter",xlab="Occupancy",breaks = seq(0,1,0.1))

#Increased diversity in the summer and reduced diversity in the winter


##################
##Special Protected areas
##################

SPA <- read_sf(dsn = "GB-SPA-OSGB36-20220930")

##keep cells with at least one corner in an SPA
coords.island.sf <- st_as_sf((coords.dat.island[,1:2]),coords = c("long", "lat"), crs = 4326) #for WGS84
coords.island.sf <- st_transform(coords.island.sf,crs=st_crs(SPA))
coords.dat.keep.SPA.ind <- unlist(st_intersects(SPA,coords.island.sf))
coords.dat.keep.SPA <- coords.dat.island[coords.dat.keep.SPA.ind,]

#coords.dat.keep.SPA <- coords.island.sf[sort(unique(unlist(st_contains(SPA,coords.island.sf)))),]

plot(st_geometry(UK.sf))
points(coords.dat.keep.SPA)

grid.SPA <- unique(coords.dat.keep.SPA$grid)


bird.summer.dat.pa.SPA <- bird.summer.dat.pa[which(row.names(bird.summer.dat.pa) %in% grid.SPA),]
bird.winter.dat.pa.SPA <- bird.winter.dat.pa[which(row.names(bird.winter.dat.pa) %in% grid.SPA),]

which(colSums(bird.summer.dat.pa.SPA)==0)
which(rowSums(bird.summer.dat.pa.SPA)==0)
which(colSums(bird.winter.dat.pa.SPA)==0)
which(rowSums(bird.winter.dat.pa.SPA)==0)

bird.summer.dat.pa.SPA <- bird.summer.dat.pa.SPA[,-which(colSums(bird.summer.dat.pa.SPA)==0)]
bird.winter.dat.pa.SPA <- bird.winter.dat.pa.SPA[,-which(colSums(bird.winter.dat.pa.SPA)==0)]


###Gamma and alpha diversity
##Gamma
ncol(bird.summer.dat.pa.SPA)
ncol(bird.winter.dat.pa.SPA)
##Alpha
mean(rowSums(bird.summer.dat.pa.SPA))
mean(rowSums(bird.winter.dat.pa.SPA))


##species accumulation curves & Chao estimator
SAC.summer.SPA <- specaccum(bird.summer.dat.pa.SPA)
SAC.winter.SPA <- specaccum(bird.winter.dat.pa.SPA)

Estim.summer.SPA <- poolaccum(bird.summer.dat.pa.SPA)
Estim.winter.SPA <- poolaccum(bird.winter.dat.pa.SPA)


par(mfrow=c(1,2))
plot(SAC.summer.SPA$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim.summer.SPA$chao))),ylab="Richness",main="Summer")
points(3:nrow(bird.summer.dat.pa.SPA),rowMeans(Estim.summer.SPA$chao),pch=2,lty=2,lwd=2,type="b",col="red")
plot(SAC.winter.SPA$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim.winter.SPA$chao))),ylab="Richness",main="Winter")
points(3:nrow(bird.winter.dat.pa.SPA),rowMeans(Estim.winter.SPA$chao),pch=2,lty=2,lwd=2,type="b",col="red")

last(rowMeans(Estim.summer.SPA$chao))/last(SAC.summer.SPA$richness)
last(rowMeans(Estim.winter.SPA$chao))/last(SAC.winter.SPA$richness)


##Beta diversity
beta.summer.SPA <- beta.pair(bird.summer.dat.pa.SPA)
beta.winter.SPA <- beta.pair(bird.winter.dat.pa.SPA)

mean(beta.summer.SPA$beta.sim)
mean(beta.winter.SPA$beta.sim)

mean(beta.summer.SPA$beta.sor)
mean(beta.winter.SPA$beta.sor)

par(mfrow=c(1,2))
hist(beta.summer.SPA$beta.sim)
hist(beta.winter.SPA$beta.sor)

par(mfrow=c(1,2))
hist(beta.summer.SPA$beta.sor)
hist(beta.winter.SPA$beta.sor)

##OFD
freq.summer.SPA <- colSums(bird.summer.dat.pa.SPA)/nrow(bird.summer.dat.pa.SPA)
freq.winter.SPA <- colSums(bird.winter.dat.pa.SPA)/nrow(bird.winter.dat.pa.SPA)

par(mfrow=c(1,2))
hist(freq.summer.SPA,main="Summer",xlab="Occupancy",breaks = seq(0,1,0.1))
hist(freq.winter.SPA,main="Winter",xlab="Occupancy",breaks = seq(0,1,0.1))






################
##Random small##
################

sites.summer <- sort(sample(nrow(bird.summer.dat.pa),nrow(bird.summer.dat.pa.SPA)))
sites.winter <- sort(sample(nrow(bird.winter.dat.pa),nrow(bird.winter.dat.pa.SPA)))

bird.summer.dat.pa.SS <- bird.summer.dat.pa[sites.summer,]
bird.winter.dat.pa.SS <- bird.winter.dat.pa[sites.winter,]

which(colSums(bird.summer.dat.pa.SS)==0)
which(rowSums(bird.summer.dat.pa.SS)==0)
which(colSums(bird.summer.dat.pa.SS)==0)
which(rowSums(bird.summer.dat.pa.SS)==0)

bird.summer.dat.pa.SS <- bird.summer.dat.pa.SS[,-which(colSums(bird.summer.dat.pa.SS)==0)]
bird.winter.dat.pa.SS <- bird.winter.dat.pa.SS[,-which(colSums(bird.winter.dat.pa.SS)==0)]


###Gamma and alpha diversity
##Gamma
ncol(bird.summer.dat.pa.SS)
ncol(bird.winter.dat.pa.SS)
##Alpha
mean(rowSums(bird.summer.dat.pa.SS))
mean(rowSums(bird.winter.dat.pa.SS))


##species accumulation curves & Chao estimator
SAC.summer <- specaccum(bird.summer.dat.pa.SS)
SAC.winter <- specaccum(bird.winter.dat.pa.SS)

Estim.summer <- poolaccum(bird.summer.dat.pa.SS)
Estim.winter <- poolaccum(bird.winter.dat.pa.SS)


par(mfrow=c(1,2))
plot(SAC.summer$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim.summer$chao))),ylab="Richness",main="Summer")
points(3:nrow(bird.summer.dat.pa.SS),rowMeans(Estim.summer$chao),pch=2,lty=2,lwd=2,type="b",col="red")
plot(SAC.winter$richness,pch=1,lty=1,lwd=2,type="b",col="blue",ylim=c(0,max(rowMeans(Estim.winter$chao))),ylab="Richness",main="Winter")
points(3:nrow(bird.winter.dat.pa.SS),rowMeans(Estim.winter$chao),pch=2,lty=2,lwd=2,type="b",col="red")

last(rowMeans(Estim.summer$chao))/last(SAC.summer$richness)
last(rowMeans(Estim.winter$chao))/last(SAC.winter$richness)


##Beta diversity
beta.summer.SS <- beta.pair(bird.summer.dat.pa.SS)
beta.winter.SS <- beta.pair(bird.winter.dat.pa.SS)

mean(beta.summer.SS$beta.sim)
mean(beta.winter.SS$beta.sim)

mean(beta.summer.SS$beta.sor)
mean(beta.winter.SS$beta.sor)

par(mfrow=c(1,2))
hist(beta.summer.SS$beta.sim)
hist(beta.winter.SS$beta.sor)

par(mfrow=c(1,2))
hist(beta.summer.SS$beta.sor)
hist(beta.winter.SS$beta.sor)

##OFD
freq.summer.SS <- colSums(bird.summer.dat.pa.SS)/nrow(bird.summer.dat.pa.SS)
freq.winter.SS <- colSums(bird.winter.dat.pa.SS)/nrow(bird.winter.dat.pa.SS)

par(mfrow=c(1,2))
hist(freq.summer.SS,main="Summer",xlab="Occupancy",breaks = seq(0,1,0.1))
hist(freq.winter.SS,main="Winter",xlab="Occupancy",breaks = seq(0,1,0.1))





















