###THE PREDATOR PREY LOTKA-VOLTERRA MODEL###
############################################

#Below is a logistic growth function

LG <- function(t,state,parameters){ ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    
    dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
    
    return(list(dP)) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}


#Need to implement two different equations to represent:
#The prey population growth (x)
#The predator population growth (y)

###EQUATION 1###
#dx/dt=alpha*x-beta*x*y#
#Models the change in prey population 
#alpha*x - prey growth 
#beta*x*y - reduction in prey due to predation 

###EQUATION 2###
#dy/dt=gamma*x*y-gamma*y#
#Models the change in predator population 
#gamma*x*y - the growth of the predator population based on food (prey)
#gamma*y - predator death rate 


#Modified function#
LV <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    dx <- alpha * x - beta * x * y  ## Prey equation
    dy <- delta * x * y - gamma * y ## Predator equation
    
    return(list(c(dx, dy))) ## Return a list of both rates of change
  }) # end with(as.list ...
}

#Solve the Lotka-Volterra, using the ode() function from the desolve package
library(deSolve)  # Ensure you load the deSolve package

# Define initial population values
state <- c(x = 10, y = 10)  # Initial prey (x) and predator (y) populations

# Define the equation parameters
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.02, gamma = 0.4)

# Define a sequence of time steps (500 steps with intervals of 0.01)
times <- seq(0, 500, by = 0.01)  # from 0 to 5, in intervals of 0.01 (500 steps total)

# Solve the system of differential equations using the ode() function
out <- ode(y = state, times = times, func = LV, parms = parameters)

# Convert the output into a data frame for easier handling and plotting
out.df <- data.frame(out)

# View the first few rows of the output
head(out.df)
#Plot the output
#This gives the changes in the two populations through time. 
library(ggplot2)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=x),color="blue") +
  geom_line(mapping=aes(x=time,y=y),color="red") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

ggplot(data = out.df)+
  geom_path(mapping=aes(x=x,y=y),color="red") +
  xlim(0,70) +
  ylim(0,40) +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Prey", y = "Predator")


#Prey growth rate: exponential vs logistic
#In the original LV model, prey growth is exponential if no predators exist.
#To add logistic growth to the prey population you modify the equation to:
#dx/dt=alpha*x(1-x/K)-beta*x*y (K=carrying capacity)
LV_logistic <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    dx <- alpha * x * (1 - x / K) - beta * x * y  ## Prey logistic growth
    dy <- delta * x * y - gamma * y               ## Predator equation
    
    return(list(c(dx, dy))) ## Return both rates of change
  }) 
}
#Adjust paramters and solve
# Adding carrying capacity K
parameters <- c(alpha = 0.1, beta = 0.02, delta = 0.02, gamma = 0.4, K = 30)

# Solve the modified system
out_logistic <- ode(y = state, times = times, func = LV_logistic, parms = parameters)

# Convert to data frame and plot
out_logistic.df <- data.frame(out_logistic)

ggplot(data = out_logistic.df) +
  geom_line(mapping = aes(x = time, y = x), color = "blue") +
  geom_line(mapping = aes(x = time, y = y), color = "red") +
  labs(x = "Time", y = "Population") +
  ggtitle("Logistic Prey Growth in Lotka-Volterra Model")


#Incorporating functional responsee (Type II)
LV_func_response <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    dx <- alpha * x - (beta * x * y) / (1 + A * x)  ## Prey with functional response
    dy <- (delta * x * y) / (1 + A * x) - gamma * y ## Predator equation with functional response
    
    return(list(c(dx, dy))) ## Return both rates of change
  }) 
}
# Adding A to parameters
parameters <- c(alpha = 0.1, beta = 0.9, delta = 0.02, gamma = 0.4, A = 0.1)

# Solve the modified system
out_func_response <- ode(y = state, times = times, func = LV_func_response, parms = parameters)

# Convert to data frame and plot
out_func_response.df <- data.frame(out_func_response)

ggplot(data = out_func_response.df) +
  geom_line(mapping = aes(x = time, y = x), color = "blue") +
  geom_line(mapping = aes(x = time, y = y), color = "red") +
  labs(x = "Time", y = "Population") +
  ggtitle("Type II Functional Response in Lotka-Volterra Model")

#Increasing/Decreasing α(prey growth rate): Increasing α will make prey grow faster, leading to a larger prey population and possibly more predators. Decreasing it will have the opposite effect, potentially collapsing both populations.
#Increasing/Decreasing β(predation rate): Higher values mean predators are more efficient at consuming prey, leading to more dramatic oscillations and possibly predator overpopulation.
#Increasing A(in functional response): Higher A values indicate less efficient predation, which reduces predator growth and stabilizes populations.

#A three-species competition LV model: limiting similarity 
##################################
#Two species competition LV model#
##################################
library(deSolve)

# Parameters for the two-species system
parameters <- c(r1 = 0.3, r2 = 0.3, K1 = 50, K2 = 100, alpha12 = 1, alpha21 = 0.9)

# Initial population values for species 1 and species 2
state <- c(X1 = 50, X2 = 10)

# Define the two-species Lotka-Volterra competition model
LV_two_species <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    dX1 <- r1 * X1 * (1 - X1 / K1 - alpha12 * X2 / K1)  # Growth rate of species 1
    dX2 <- r2 * X2 * (1 - X2 / K2 - alpha21 * X1 / K2)  # Growth rate of species 2
    
    return(list(c(dX1, dX2)))  # Return both rates of change
  }) 
}

# Time steps for solving the system
times <- seq(0, 200, by = 0.01)

# Solve the system of differential equations
out_two_species <- ode(y = state, times = times, func = LV_two_species, parms = parameters)

# Convert the output into a data frame for plotting
out.df <- data.frame(out_two_species)

# Plot the population changes over time
library(ggplot2)
ggplot(data = out.df) +
  geom_line(aes(x = time, y = X1), color = "blue") +
  geom_line(aes(x = time, y = X2), color = "red") +
  labs(x = "Time", y = "Population") +
  ggtitle("Two-Species Competition: Lotka-Volterra Model")

####################################
#Three species competition LV model#
####################################
# Parameters for the three-species system
parameters <- c(r1 = 0.3, r2 = 0.3, r3 = 0.3, 
                K1 = 200, K2 = 250, K3 = 200, 
                alpha12 = 1, alpha13 = 1, alpha21 = 0.9, alpha23 = 0.9, alpha31 = 0.8, alpha32 = 0.8)

# Initial population values for species 1, 2, and 3
state <- c(X1 = 50, X2 = 10, X3 = 10)

# Define the three-species Lotka-Volterra competition model
LV_three_species <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    dX1 <- r1 * X1 * (1 - (X1 + alpha12 * X2 + alpha13 * X3) / K1)  # Species 1
    dX2 <- r2 * X2 * (1 - (X2 + alpha21 * X1 + alpha23 * X3) / K2)  # Species 2
    dX3 <- r3 * X3 * (1 - (X3 + alpha31 * X1 + alpha32 * X2) / K3)  # Species 3
    
    return(list(c(dX1, dX2, dX3)))  # Return rates of change for all three species
  }) 
}

# Solve the three-species system
times <- seq(0, 200, by = 0.01)
out_three_species <- ode(y = state, times = times, func = LV_three_species, parms = parameters)

# Convert the output to a data frame
out_three_species.df <- data.frame(out_three_species)

# Plot the population dynamics for the three species
ggplot(data = out_three_species.df) +
  geom_line(aes(x = time, y = X1), color = "blue") +
  geom_line(aes(x = time, y = X2), color = "red") +
  geom_line(aes(x = time, y = X3), color = "green") +
  labs(x = "Time", y = "Population") +
  ggtitle("Three-Species Competition: Lotka-Volterra Model")

alpha.func <- function(mu1,sig1,mu2,sig2,K1,K2,start,end){ ##this is the function to compute the alpha coefficients from the mean and standard deviations of the Gaussian niches of the species and the start and end values of the environment
  niche1 <- K1 *  dnorm(seq(start,end,length.out=100),mean=mu1,sd=sig1) ##dnorm() generates the values of the Gaussian. Check ?dnorm
  niche2 <- K2 * dnorm(seq(start,end,length.out=100),mean=mu2,sd=sig2)
  a <- sum(niche1*niche2)/sum(niche1*niche1) ##because we have discrete values, we use a sum to approximate the integral
  return(a)
}

##Let's try different parameter values
D <- 5 ##distance between the niche optima
mu1 <- 10 ##niche optima of species 1
mu2 <- mu1+D ##niche optima of species 2
mu3 <- mu1+2*D ##niche optima of species 3
sig1 <- sig2 <- sig3 <- 10 ##all species niches have the same standard deviation for simplicity
K1 <- 200 ##carrying capacity species 1 and 3
K2 <- 250 ##carrying capacity species 2
start <- 0
end <- 30
a12 <- alpha.func(mu1,sig1,mu2,sig2,K1,K2,start,end)
a13 <- alpha.func(mu1,sig1,mu3,sig3,K1,K1,start,end)
a21 <- alpha.func(mu2,sig2,mu1,sig1,K2,K1,start,end)
a23 <- alpha.func(mu2,sig2,mu3,sig3,K2,K1,start,end)
a31 <- alpha.func(mu3,sig3,mu1,sig1,K1,K1,start,end)
a32 <- alpha.func(mu3,sig3,mu2,sig2,K1,K2,start,end)

##visualise the niches
resource <- seq(start,end,length.out=100)
niche1 <- dnorm(resource,mean=mu1,sd=sig1)*K1
niche2 <- dnorm(resource,mean=mu2,sd=sig2)*K2
niche3 <- dnorm(resource,mean=mu3,sd=sig3)*K1
ggplot()+
  geom_line(mapping=aes(x=resource,y=niche1),color="blue")+
  geom_line(mapping=aes(x=resource,y=niche2),color="red")+
  geom_line(mapping=aes(x=resource,y=niche3),color="darkgreen")


##setup and solve the system of differential equations
parameters <- c(a12=a12, a13=a13, a21=a21, a23=a23, a31=a31, a32=a32, r=0.3, K1 = K1, K2 = K2)
state <- c(X1=10, X2=10, X3=10)

LS2 <- function(t,state,parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    ##you need to complete this
    
    # return the rate of change
    list() ##you need to complete this
  }) # end with(as.list ...
}

times <- seq(0,200,by=0.01)
out <- ode(y=state, times = times, func = LS2, parms = parameters)
out.df <- data.frame(out)

##plot the populations
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=X1),color="blue") +
  geom_line(mapping=aes(x=time,y=X2),color="red") +
  geom_line(mapping=aes(x=time,y=X3),color="darkgreen") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  labs(x = "Time", y = "P")

















