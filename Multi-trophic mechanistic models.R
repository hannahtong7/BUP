rm(list = ls())
graphics.off()
quartz()

library(ggplot2)


#############
##Model skeleton
#############

##set up parameter values
sP <- 0.5
Kp <- 240
lP <- 2.68275
hp <- 300
sL <- 0.07
Kl <- 870
lL <- 2.68275
hl <- 400
sH <- 0.5
Kh <- 970
lH <- 3.1682
hh <- 1000
fC <- 1
eC <- 1.85
mC <-0 
fM <- 1.5
eM <- 0.6
mM <- 0
mW <- 0
b <- 0.8
g <- 0.38
dC <- 460
dM <- 46



nsteps <- 200
pop.df.0 <- data.frame(time=1:nsteps,P=numeric(nsteps),L=numeric(nsteps),H=numeric(nsteps),C=numeric(nsteps),M=numeric(nsteps),W=numeric(nsteps))

pop.df.0 <- within(pop.df.0,{
  P[1] <- 240
  L[1] <- 870
  H[1] <- 970
  C[1] <- 7
  M[1] <- 25
  W[1] <- 8
})


for(t in 1:(nsteps-1)){
  pop.df.0 <- within(pop.df.0,{
    Plant.birth<- sP*P[t]*(1-(P[t-1-]/Kp))
    P[t+1] <- max(0,...) ##plants consumed by Caribou
    L[t+1] <- max(0,...) ##lichen consumed by Caribou
    H[t+1] <- max(0,...) ##plants consumed by Moose
    
    C[t+1] <- max(0,...)
    
    M[t+1] <- max(0,...)
    
    W[t] <- max(0,...)
  })
}


colors <- c("Plants"="brown","Lichen"="orange","Shrubs"="green","Caribou"="purple","Moose"="blue","Wolf"="red")
ggplot(data = pop.df.0)+
  geom_line(mapping=aes(x=time,y=P,color="Plants")) +
  geom_line(mapping=aes(x=time,y=L,color="Lichen")) +
  geom_line(mapping=aes(x=time,y=H,color="Shrubs")) +
  geom_line(mapping=aes(x=time,y=C,color="Caribou")) +
  geom_line(mapping=aes(x=time,y=M,color="Moose")) +
  geom_line(mapping=aes(x=time,y=W,color="Wolf")) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "Densities", color="Legend")+
  scale_color_manual(values = colors)



