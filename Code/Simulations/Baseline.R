#Baseline Non interactive communication of dyads.
# Authors:
#                       ## Variables ##
# n: Number of observations before filtering for inhibited calls.
# 
#                       ## Latency ##
# mu_latency: The average wait time between consecutive calls for each individual.
# sd_latency: The uncertainty regarding the latency between calls.
# 
#                   ## Call Duration ##
# mu_duration: What is the average call duration of our callers.
# sd_duration: What is the uncertainity in the average call duration. 

Baseline <- function(n, mu_latency, sd_latency, mu_duration, sd_duration){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")
  if ( any( mu_duration%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_duration should be numeric and between 1:Infinity")
  if ( any( sd_duration%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_duration should be numeric and between 1:Infinity")
  
  IOI_1 <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, ID = 1) # The neighboring individual is freely vocalizing at its tempo
  IOI_2 <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, ID = 2) # The neighboring individual is freely vocalizing at its tempo
  
  for (i in 1:n){
    if (i == 1){ 
      IOI_1$Onset[i] = IOI_1$Interval[i]
      IOI_2$Onset[i] = IOI_2$Interval[i]
      
      IOI_1$Offset[i] = IOI_1$Interval[i] + IOI_1$Duration[i]
      IOI_2$Offset[i] = IOI_2$Interval[i] + IOI_2$Duration[i]
    }
    else{
      IOI_1$Onset[i] = IOI_1$Onset[i-1] + IOI_1$Duration[i-1] + IOI_1$Interval[i] 
      IOI_2$Onset[i] = IOI_2$Onset[i-1] + IOI_2$Duration[i-1] + IOI_2$Interval[i] 
      
      IOI_1$Offset[i] = IOI_1$Onset[i] + IOI_1$Duration[i]
      IOI_2$Offset[i] = IOI_2$Onset[i] + IOI_2$Duration[i]
      
    }
  }
  
  IOI <- rbind(IOI_1, IOI_2) %>% 
    arrange(Onset) %>% 
    mutate(Latency = Onset - lag(Offset)) %>% 
    mutate(CallNr = 1)
    
  return(IOI)
}


## TESTING
n = 100
mu_latency = 500 ; sd_latency = 50 ; mu_duration = 100 ; sd_duration = 20
IOI_1 <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, Caller = 1) # The neighboring individual is freely vocalizing at its tempo
IOI_2 <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, Caller = 2) # The neighboring individual is freely vocalizing at its tempo


for (i in 1:n){
  if (i == 1){ 
    IOI_1$Onset[i] = IOI_1$Interval[i]
    IOI_2$Onset[i] = IOI_2$Interval[i]
    
    IOI_1$Offset[i] = IOI_1$Interval[i] + IOI_1$Duration[i]
    IOI_2$Offset[i] = IOI_2$Interval[i] + IOI_2$Duration[i]
  }
  else{
    IOI_1$Onset[i] = IOI_1$Onset[i-1] + IOI_1$Duration[i-1] + IOI_1$Interval[i] 
    IOI_2$Onset[i] = IOI_2$Onset[i-1] + IOI_2$Duration[i-1] + IOI_2$Interval[i] 
    
    IOI_1$Offset[i] = IOI_1$Onset[i] + IOI_1$Duration[i]
    IOI_2$Offset[i] = IOI_2$Onset[i] + IOI_2$Duration[i]
    
  }
}

IOI <- rbind(IOI_1, IOI_2) %>% 
  arrange(Onset) %>% 
  mutate(Latency = Onset - lag(Offset))
