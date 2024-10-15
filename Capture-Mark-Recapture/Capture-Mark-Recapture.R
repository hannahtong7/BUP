rm(list=ls(all=TRUE))

my_packages <- c('dplyr', 'tidyr', 'marked', 'ggplot2', 'R2ucare')
new_packages <- my_packages[!(my_packages %in% installed.packages()[,'Package'])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)
library(tidyr)
library(marked)
library(ggplot2)
library(R2ucare)

longdata <- read.table("~/Documents/temp/BUP/Data/sparrowrecap.txt", header = TRUE, sep = '\t')
head(longdata)

length(unique(longdata$id)) # the number of unique individuals in the dataframe

table(longdata$sex) # equal number of observations of males and females 

table(longdata$year) # captures from 1998-2007

table(longdata$island) # at 4 different island locations

temp <- longdata[,1:2] # take the first two columns, id and year and put into a temporary dataframe
temp$detect <- 1 # add column for detection (all 1s because these represent captures) 

temp <- temp %>%
  # remove duplicates, which may occur when individuals are caught multiple times in an sampling event
  distinct() %>%
  # spread out data. The fill = 0 adds rows for combinations of id and year where individuals were not observed
  spread(year, detect, fill = 0) %>% 
  # for every individual....
  group_by(id) %>%
  # paste together 0's and 1's using unite()
  # here we are pasting the strings together from the second column (first capture event)
  # to the last capture event ("tail(names(.),1)")
  # use sep="" so there are no characters separating 0's and 1's
  unite("ch", 2:tail(names(.),1), sep = "")

sparrow <- as.data.frame(temp) # new dataframe called sparrow
head(sparrow)

sparrow$island <- longdata$island[match(sparrow$id, longdata$id)] 
# this creates a new column called island in the sparrow df...
# using the entry from the island column in the longdata df... 
# where id in the sparrow df matches the id in the longdata df

sparrow$sex <- as.factor(longdata$sex[match(sparrow$id, longdata$id)])

sparrow <- droplevels(subset(sparrow, select = -id)) # remove id column so capture histories appear in first column
head(sparrow)

mod1 <- crm(sparrow) # capture-mark-recapture (cmr) model
mod1 # examine model and coefficient estimates

mod1 <- cjs.hessian(mod1) # refit model with precision estimates

mod1$results$reals

plogis(mod1$results$beta$Phi)

plogis(mod1$results$beta$p)

predict(mod1, newdata=data.frame(sex = c('Female', 'Male')), se=T) # N.b. In this case, there are no groups or covariates in the model and so the 'newdata' argument is not used 

mod2 <- crm(sparrow, time.intervals = c(1,2,1,1,1,1,1,3,4))
mod2$results$reals

sparrow.proc <- process.data(sparrow) # built in function for data processing
str(sparrow.proc)
head(sparrow.proc[[1]])
head(sparrow.proc$data)
sparrow.ddl <- make.design.data(sparrow.proc) # built in function for building design matrix 
str(sparrow.ddl)
head(sparrow.ddl[[1]])
head(sparrow.ddl$Phi)
# specify model formulation: capture probability depends on island
p.island <- list(formula=~island) 

mod3 <- crm(sparrow.proc, 
            sparrow.ddl, 
            model.parameters = list(p = p.island), 
            accumulate=FALSE, hessian = TRUE)
mod3$results$reals
(mod3$results$AIC)
(mod1$results$AIC)
sparrow.proc <- process.data(sparrow) 
sparrow.ddl <- make.design.data(sparrow.proc) 

Phi.island <- list(formula=~island) # survival probability depends on island
p.island <- list(formula=~island) # capture probability depends on island

mod4 <- crm(sparrow.proc, 
            sparrow.ddl, 
            model.parameters = list(Phi = Phi.island, 
                                    p = p.island), 
            accumulate=FALSE, hessian = TRUE)
mod4$results$reals
(mod4$results$AIC)
(mod3$results$AIC)

sparrow.proc <- process.data(sparrow)
sparrow.ddl <- make.design.data(sparrow.proc)

fit.models <- function() {
  Phi.dot <- list(formula=~1) # constant survival
  Phi.sex <- list(formula=~sex) # survival differs between sexes
  Phi.island <- list(formula=~island) # survival differs between islands
  Phi.sex.island <- list(formula=~sex+island) # survival differs between sexes and islands
  p.dot <- list(formula=~1) # constant detection
  p.sex <- list(formula=~sex) # detection probability differs between sexes
  p.island <- list(formula=~island) # detection probability differs between islands
  p.sex.island <- list(formula=~sex+island) # detection probability differs between sexes and islands
  cml <- create.model.list(c("Phi","p"))
  results <- crm.wrapper(cml, data=sparrow.proc, ddl=sparrow.ddl,
                         external=FALSE, accumulate=FALSE, hessian=TRUE)
  return(results)
}

sparrow.models <- fit.models() # run function 


sparrow.models # display model table

mod5 <- sparrow.models[[2]]

ggplot(mod5$results$reals$p, aes(island, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)

mod6 <- sparrow.models[[10]]
ggplot(mod6$results$reals$Phi, aes(sex, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)

mod7 <- sparrow.models[[6]]
ggplot(mod7$results$reals$Phi, aes(island, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)


sparrow.ddl$Phi$cold <- "Cold" # new column 
sparrow.ddl$Phi$cold[sparrow.ddl$Phi$time==2 | sparrow.ddl$Phi$time==5 | sparrow.ddl$Phi$time==8] <- "VeryCold" # very cold winters between capture events 2 and 3, 5 and 6, and 8 and 9
head(sparrow.ddl$Phi)

Phi.cold <- list(formula=~cold) 
p.island <- list(formula=~island) 

mod8 <- crm(sparrow.proc, 
            sparrow.ddl, 
            model.parameters = list(Phi = Phi.cold, 
                                    p = p.island), 
            accumulate=FALSE, hessian = TRUE)

mod8$results$reals
(mod8$results$AIC)
(mod5$results$AIC)
ggplot(mod8$results$reals$Phi, aes(cold, estimate, ymin=lcl, ymax=ucl)) + 
  geom_errorbar(width=0.2) + geom_point() + ylim(0,1)

