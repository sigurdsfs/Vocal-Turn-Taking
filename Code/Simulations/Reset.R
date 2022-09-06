## RESET Hypothesis


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


ResetHypothesis <- function(n, mu_latency, sd_latency, mu_duration, sd_duration){
  #Condition/warnings
  if ( any( n%%1 != 0 | n < 0 | length(n) != 1 )) stop("n should be an integer between 1:Infinity")
  if ( any( mu_latency%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_latency should be an integer between 1:Infinity")
  if ( any( sd_latency%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_latency should be an integer between 1:Infinity")
  if ( any( mu_duration%%1 != 0 | mu_latency < 0 | length(mu_latency) != 1)) stop("mu_duration should be numeric and between 1:Infinity")
  if ( any( sd_duration%%1 != 0 | sd_latency < 0 | length(sd_latency) != 1)) stop("sd_duration should be numeric and between 1:Infinity")
  
  
  #Function Start
  IOI_neigh <- tibble(Interval = rnorm(1, mu_latency, sd_latency), 
                      Duration = rnorm(1, mu_duration, sd_duration)) %>% # The neighboring individual is freely vocalizing at its tempo
    mutate(
      Onset = Interval,
      Offset = Onset + Duration
    )
  
  IOI_focal <- tibble(Interval = rnorm(1, mu_latency, sd_latency), 
                      Duration = rnorm(1, mu_duration, sd_duration)) %>% # The neighboring individual is freely vocalizing at its tempo
    mutate(
      Onset = Interval + runif(1, 0, mu_latency),
      Offset = Onset + Duration
    )
  
  if (IOI_focal$Onset > IOI_neigh$Onset) {
    IOI_focal$Reset <- ifelse(IOI_focal$Onset > IOI_neigh$Onset & IOI_focal$Onset < IOI_neigh$Offset, 1 , 0)
    IOI_neigh$Reset <- 0
  } else if (IOI_focal$Onset < IOI_neigh$Onset) {
    IOI_neigh$Reset <- ifelse(IOI_neigh$Onset > IOI_focal$Onset & IOI_neigh$Onset < IOI_focal$Offset, 1 , 0)
    IOI_focal$Reset <- 0
  }
  
  if (IOI_focal$Reset == 1) {
    IOI_focal$Onset <- IOI_neigh$Offset + IOI_focal$Interval
  }
  if (IOI_neigh$Reset == 1) {
    IOI_neigh$Onset <- IOI_focal$Offset + IOI_neigh$Interval
  }
  
  IOI_neigh$Caller <- "Neighbor"
  IOI_focal$Caller <- "Focal"
  
  IOI <- rbind(IOI_neigh, IOI_focal)
  IOI <- IOI[order(IOI$Onset),]
  
  ## NOW STEP 2
  step = 2
  while (TRUE) {
    
    temp_prevNeigh <- filter(IOI, Caller == "Neighbor")[nrow(filter(IOI, Caller == "Neighbor")),] %>%
      mutate(Interval = rnorm(1, mu_latency, sd_latency))
    temp_prevFocal <- filter(IOI, Caller == "Focal")[nrow(filter(IOI, Caller == "Focal")),] %>%
      mutate(Interval = rnorm(1, mu_latency, sd_latency))
    
    if (IOI$Offset[nrow(IOI)] +  temp_prevFocal$Interval > IOI$Offset[nrow(IOI)] + temp_prevNeigh$Interval) {
      
      temp_neigh <- tibble(Interval = temp_prevNeigh$Interval, 
                           Duration = rnorm(1, 500, 100))
      temp_neigh$Onset <- temp_prevNeigh$Offset + temp_neigh$Interval
      temp_neigh$Offset <- temp_neigh$Onset + temp_neigh$Duration
      temp_neigh$Reset <- ifelse(temp_neigh$Onset > temp_prevFocal$Onset & temp_neigh$Onset < temp_prevFocal$Offset, 1 , 0)
      
      if (temp_neigh$Reset == 1) {
        temp_neigh$Onset <- temp_prevFocal$Offset + temp_neigh$Interval
        temp_neigh$Offset <- temp_neigh$Onset + temp_neigh$Duration
        temp_neigh$Reset <- 0
      }
      temp_neigh$Caller <- "Neighbor"
      IOI <- rbind(IOI, temp_neigh)
      
    } else {
      temp_focal <- tibble(Interval = temp_prevFocal$Interval, 
                           Duration = rnorm(1, 500, 100))
      temp_focal$Onset <- temp_prevFocal$Offset + temp_focal$Interval
      temp_focal$Offset <- temp_focal$Onset + temp_focal$Duration
      temp_focal$Reset <- ifelse(temp_focal$Onset > temp_prevNeigh$Onset & temp_focal$Onset < temp_prevNeigh$Offset, 1 , 0)
      if (temp_focal$Reset == 1) {
        temp_focal$Onset <- temp_prevNeigh$Offset + temp_focal$Interval
        temp_focal$Offset <- temp_focal$Onset + temp_focal$Duration
        temp_focal$Reset <- 0
      }
      temp_focal$Caller <- "Focal"
      IOI <- rbind(IOI, temp_focal)
    }
    
    if ((nrow(filter(IOI, Caller == "Focal")) > n & nrow(filter(IOI, Caller == "Neighbor")) > n) | nrow(IOI) > (n*3)) {
      break
    }
   
  }
  IOI <- IOI %>% 
    rename(Latency = Interval) #Rename with proper name
    
  IOI <- mutate_at(IOI, vars(Onset, Offset), function(x) x - IOI$Latency[1])
  IOI$Latency[1] <- NA
  
  #Force first Onset to 0.
  return(IOI)
}

