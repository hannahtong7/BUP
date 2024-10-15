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



