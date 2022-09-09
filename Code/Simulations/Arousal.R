##Activation Functions##
sigmoid <- function(x){
  1/ (1+exp(-x))
}

tanh <- function(x){
  exp(x)/sum(exp(x))
}

#test various activation functions
sum(tanh(c(-3:3))[1:2])

sigmoid(-3:3)



### Arousal Model: ###

## Neigh is the initiator, focal is the second individual, who, in the arousal model follows the neighbor
# IF THE NEIGHBOR VOCALIZES, THE FOCAL VOCALIZES WITH AN INTERVAL OF 0-PREV NEIGH INTERVAL
# THEN IT WAITS
# WE HAVE NO DURATION HERE, IT DOESN*T MATTER FOR THE MODEL, BUT WE MIGHT ADD IT.


# Authors:
#                       ## Variables ##
# n: Number of observations before filtering for inhibited calls.
# 
#                       ## Latency ##
# mu_latency: The average wait time between consecutive calls for each individual.
# sd_latency: The uncertainty regarding the latency between calls.


ArousalModel_Stochastic <- function(n, mu_latency, sd_latency){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")
  
  #Initiator
  IOI_neigh <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Onset = NA, caller = "Initiator") # The neighboring individual is freely vocalizing at its tempo
  
  for (i in 1:n) {
    if (i == 1) {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
    } else {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1]
    }
  }
  
  
  #Receiver
  IOI_focal <- tibble(Interval = rep(NA, n), Onset = NA, caller = "Responder") # The focal individual is yet undetermined
  
  n_since_call <- 1
  for (i in 1:n){
    prob_of_responding <- sigmoid(n_since_call - 3) 
    Talk_boolean <- rbinom(1,1, prob = prob_of_responding)
    
    if (Talk_boolean == 1){
      IOI_focal$Interval[i] <- runif(1, 0, IOI_neigh$Interval[i])
      IOI_focal$Onset[i] <- IOI_neigh$Onset[i] + IOI_focal$Interval[i]
      n_since_call <- 1
    }
    else{
      IOI_focal$Interval[i] <- NA
      IOI_focal$Onset[i] <- NA
      n_since_call <- n_since_call + 1
    }  
  }
  IOI <- rbind(IOI_focal, IOI_neigh) %>% 
    filter(Onset != "NA") %>% 
    arrange(Onset) %>% 
    rename(Latency = Interval) %>% 
    mutate(ID = caller) %>% 
    mutate(CallNr = 1)
  IOI$Latency[1] <- NA 
  
  return(IOI)
}

ArousalModel_NoStochasticity <- function(n, mu_latency, sd_latency){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")
  
  #Initiator
  IOI_neigh <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Onset = NA, caller = "Initiator") # The neighboring individual is freely vocalizing at its tempo
  
  for (i in 1:n) {
    if (i == 1) {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
    } else {
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1]
    }
  }
  
  
  #Receiver
  IOI_focal <- tibble(Interval = rep(NA, n), Onset = NA, caller = "Responder") # The focal individual is yet undetermined
  
  for (i in 1:n){
      IOI_focal$Interval[i] <- runif(1, 0, IOI_neigh$Interval[i])
      IOI_focal$Onset[i] <- IOI_neigh$Onset[i] + IOI_focal$Interval[i]
  }  
  
  IOI <- rbind(IOI_focal, IOI_neigh) %>% 
    arrange(Onset) %>% 
    rename(Latency = Interval) %>% 
    mutate(ID = caller) %>% 
    mutate(CallNr = 1)
  
  IOI$Latency[1] <- NA 
  
  return(IOI)
}

#Try Functions
df_arousal <- ArousalModel_Stochastic(n = 100, mu_latency = 150, sd_latency = 15)
df_arousal2 <- ArousalModel_NoStochasticity(n = 100, mu_latency = 150, sd_latency = 15)

ArousalModel_Stochastic(n = 100, mu_latency = 150, sd_latency = 15)
ArousalModel_NoStochasticity(n = 100, mu_latency = 150, sd_latency = 15)



#Plotting (needs update)
ggplot(aes()) +
  geom_point(aes(NeighborInterval, FocalInterval)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()