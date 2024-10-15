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






