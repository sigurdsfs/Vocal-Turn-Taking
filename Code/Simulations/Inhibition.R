## Inhibition

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

### WE NEED TO GENERALIZE TO CASES WHERE A CALLER CAN MAKE MULTIPLE CALLS BEFORE THE OTHER ENTERS.
# We can do this if we create different latency and duration parameters for every individual caller. (just add two more variables to our function)

InhibitionHypothesis <- function(n, mu_latency, sd_latency, mu_duration, sd_duration){
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be numeric and between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be numeric and between 1:Infinity")
  if ( any( mu_duration%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_duration should be numeric and between 1:Infinity")
  if ( any( sd_duration%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_duration should be numeric and between 1:Infinity")
  
  IOI_neigh <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, Inhibited = NA) # The neighboring individual is freely vocalizing at its tempo
  IOI_focal <- tibble(Interval = abs(rnorm(n, mu_latency, sd_latency)), Duration = abs(rnorm(n, mu_duration, sd_duration)), Onset = NA, Offset = NA, Inhibited = NA) # The neighboring individual is freely vocalizing at its tempo
  
  for (i in 1:n){
    if (i == 1){
      if (IOI_neigh$Interval[i] < IOI_focal$Interval[i]){
        IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
        IOI_focal$Onset[i] = IOI_focal$Interval[i] + runif(1, 0, mu_latency)
      }
      else{
        IOI_focal$Onset[i] = IOI_focal$Interval[i]
        IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + runif(1, 0, mu_latency)
      }
    }
    
    else{
      IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1] + IOI_neigh$Duration[i - 1]
      IOI_focal$Onset[i] = IOI_focal$Interval[i] + IOI_focal$Onset[i - 1] + IOI_focal$Duration[i - 1]
    }
    
    IOI_neigh$Offset[i] = IOI_neigh$Onset[i] + IOI_neigh$Duration[i]
    IOI_focal$Offset[i] = IOI_focal$Onset[i] + IOI_focal$Duration[i]
    
    IOI_neigh$Inhibited[i] <- ifelse(
      IOI_neigh$Onset[i] > IOI_focal$Onset[i] & IOI_neigh$Onset[i] < IOI_focal$Offset[i], 1 , 0)
    
    IOI_focal$Inhibited[i] <- ifelse(
      IOI_focal$Onset[i] > IOI_neigh$Onset[i] & IOI_focal$Onset[i] < IOI_neigh$Offset[i] & IOI_neigh$Inhibited[i] == 0, 1 , 0)
    
  }
  
  
  
  
  #Plot stuff
  data_inhibition <- tibble(
    NeighborOnset = IOI_neigh$Onset,
    NeighborDuration = IOI_neigh$Duration,
    NeighborInterval = IOI_neigh$Interval,
    NeighborOffset = IOI_neigh$Offset,
    NeighborInhibited = IOI_neigh$Inhibited,
    FocalOnset = IOI_focal$Onset,
    FocalDuration = IOI_focal$Duration,
    FocalInterval = IOI_focal$Interval,
    FocalOffset = IOI_focal$Offset,
    FocalInhibited = IOI_focal$Inhibited
  ) 
  
  for (i in seq(n)) {
    if (data_inhibition$NeighborInhibited[i] == 1) {
      data_inhibition$NeighborOnset[i] <- NA
      data_inhibition$NeighborDuration[i] <- NA
      data_inhibition$NeighborInterval[i + 1] <- data_inhibition$NeighborInterval[i + 1] + data_inhibition$NeighborInterval[i]
      data_inhibition$NeighborInterval[i] <- NA
    }
    
    if (data_inhibition$FocalInhibited[i] == 1) {
      data_inhibition$FocalOnset[i] <- NA
      data_inhibition$FocalDuration[i] <- NA
      data_inhibition$FocalInterval[i + 1] <- data_inhibition$FocalInterval[i + 1] + data_inhibition$FocalInterval[i]
      data_inhibition$FocalInterval[i] <- NA
    }
  }
  #Final changes to the combined data
  data_neigh <- data_inhibition %>% 
    select(starts_with("Neigh"))
  
  data_focal <- data_inhibition %>% 
    select(starts_with("Focal"))
  
  
  names(data_focal) <- gsub(pattern = "Focal*", replacement = "", x = names(data_focal))
  names(data_neigh) <- gsub(pattern = "Neighbor*", replacement = "", x = names(data_neigh))
  
  IOI <- rbind(data_focal, data_neigh) %>% 
    filter(Inhibited != 1) %>% 
    arrange(Onset) %>% 
    rename(Latency = Interval)
  
  IOI <- mutate_at(IOI, vars(Onset, Offset), function(x) x - IOI$Latency[1])
  IOI$Latency[1] <- NA
  
  return(IOI)
}
