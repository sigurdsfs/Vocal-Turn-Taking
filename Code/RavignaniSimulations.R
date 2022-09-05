
# i time
# T[i] signalling period of the focal animal
# Tprime[i] signalling period of the neighboring animal
# T_free free running period (if nobody else was around)
# a, b, c, k scaling parameters
# l length of signal

a <- 1/2
b <- 1/10
c <- 1/5
k <- 4

T_free <- 1500 
n <- 100

# Creating vectors for vocalizations

## Neigh is the initiator, focal is the second individual, who, in the arousal model follows the neighbor

IOI_neigh <- tibble(Interval = rnorm(n, T_free, 150)) # The neighboring individual is freely vocalizing at its tempo

for (i in 1:n) {
  if (i == 1) {
    IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
  } else {
    IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1]
  }
}
IOI_focal <- tibble(Interval = rep(NA, n), Onset = NA) # The focal individual is yet undetermined

## Arousal model: 
# IF THE NEIGHBOR VOCALIZES, THE FOCAL VOCALIZES WITH AN INTERVAL OF 0-PREV NEIGH INTERVAL
# THEN IT WAITS
# WE HAVE NO DURATION HERE, IT DOESN*T MATTER FOR THE MODEL, BUT WE MIGHT ADD IT.

for (i in 1:n) {
  IOI_focal$Interval[i] <- runif(1, 0, IOI_neigh$Interval[i])
  IOI_focal$Onset[i] <- IOI_neigh$Onset[i] + IOI_focal$Interval[i]
}

data__arousal_plot <- tibble(
  NeighborOnset = IOI_neigh$Onset,
  NeighborInterval = IOI_neigh$Interval,
  FocalOnset = IOI_focal$Onset,
  FocalInterval = IOI_focal$Interval
)

ggplot(data_plot) +
  geom_point(aes(NeighborInterval, FocalInterval)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()


## Phase-Delay Model
 # synchrony is detectable as arbitrarily small phase relationships
 # Phase-delay synchrony occurs when an individual delays its signal 
 ## by an interval equivalent to the delay between its previous signal and a neighbour’s signal


## RESET/INHIBITION

IOI_neigh <- tibble(Interval = rnorm(n, T_free, 150), Duration = rnorm(n, 500, 100)) # The neighboring individual is freely vocalizing at its tempo
IOI_focal <- tibble(Interval = rnorm(n, T_free, 150), Duration = rnorm(n, 500, 100)) # The neighboring individual is freely vocalizing at its tempo
                    
for (i in 1:n) {
  if (i == 1) {
    IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
    IOI_focal$Onset[i] = IOI_focal$Interval[i] + runif(1, 0, T_free)
  } else {
    IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1] + IOI_neigh$Duration[i - 1]
    IOI_focal$Onset[i] = IOI_focal$Interval[i] + IOI_focal$Onset[i - 1] + IOI_focal$Duration[i - 1]
  }
  
  IOI_neigh$Offset[i] = IOI_neigh$Onset[i] + IOI_neigh$Duration[i]
  IOI_focal$Offset[i] = IOI_focal$Onset[i] + IOI_focal$Duration[i]
}


data__nointeraction_plot <- tibble(
  NeighborOnset = IOI_neigh$Onset,
  NeighborInterval = IOI_neigh$Interval,
  NeighborOffset = IOI_neigh$Offset,
  FocalOnset = IOI_focal$Onset,
  FocalInterval = IOI_focal$Interval,
  FocalOffset = IOI_focal$Offset
)

## Inhibition
IOI_neigh <- tibble(Interval = rnorm(n, T_free, 150), Duration = rnorm(n, 500, 100)) # The neighboring individual is freely vocalizing at its tempo
IOI_focal <- tibble(Interval = rnorm(n, T_free, 150), Duration = rnorm(n, 500, 100)) # The neighboring individual is freely vocalizing at its tempo

for (i in 1:n) {
  if (i == 1) {
    IOI_neigh$Onset[i] = IOI_neigh$Interval[i]
    IOI_focal$Onset[i] = IOI_focal$Interval[i] + runif(1, 0, T_free)
  } else {
    IOI_neigh$Onset[i] = IOI_neigh$Interval[i] + IOI_neigh$Onset[i - 1] + IOI_neigh$Duration[i - 1]
    IOI_focal$Onset[i] = IOI_focal$Interval[i] + IOI_focal$Onset[i - 1] + IOI_focal$Duration[i - 1]
  }
  
  IOI_neigh$Offset[i] = IOI_neigh$Onset[i] + IOI_neigh$Duration[i]
  IOI_focal$Offset[i] = IOI_focal$Onset[i] + IOI_focal$Duration[i]
  
  IOI_neigh$Inhibited[i] <- ifelse(
    IOI_neigh$Onset[i] > IOI_focal$Onset[i] & IOI_neigh$Onset[i] < IOI_focal$Offset[i], 1 , 0
  )
  
  IOI_focal$Inhibited[i] <- ifelse(
    IOI_focal$Onset[i] > IOI_neigh$Onset[i] & IOI_focal$Onset[i] < IOI_neigh$Offset[i] & IOI_neigh$Inhibited[i] == 0, 1 , 0
  )
  
}

data__inhibition_plot <- tibble(
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
  if (data__inhibition_plot$NeighborInhibited[i] == 1) {
    data__inhibition_plot$NeighborOnset[i] <- NA
    data__inhibition_plot$NeighborDuration[i] <- NA
    data__inhibition_plot$NeighborInterval[i + 1] <- data__inhibition_plot$NeighborInterval[i + 1] + data__inhibition_plot$NeighborInterval[i]
    data__inhibition_plot$NeighborInterval[i] <- NA
  }
  
  if (data__inhibition_plot$FocalInhibited[i] == 1) {
    data__inhibition_plot$FocalOnset[i] <- NA
    data__inhibition_plot$FocalDuration[i] <- NA
    data__inhibition_plot$FocalInterval[i + 1] <- data__inhibition_plot$FocalInterval[i + 1] + data__inhibition_plot$FocalInterval[i]
    data__inhibition_plot$FocalInterval[i] <- NA
  }
    
}


ggplot(data__inhibition_plot) +
  geom_point(aes(NeighborInterval, FocalInterval)) 

## WE NEED TO TUNR THEM INTO LONG FORMAT AND MAKE THE FORMAT COMPATIBLE WITH THE EMPIRICAL DATA

## WE NEED TO GENERALIZE TO CASES WHERE A CALLER CAN MAKE MULTIPLE CALLS BEFORE THE OTHER ENTERS

## RESET
## WE HAVE A FIRST STEP: ONE INITIATES

IOI_neigh <- tibble(Interval = rnorm(1, T_free, 150), 
                    Duration = rnorm(1, 500, 100)) %>% # The neighboring individual is freely vocalizing at its tempo
 mutate(
   Onset = Interval,
   Offset = Onset + Duration
 )

IOI_focal <- tibble(Interval = rnorm(1, T_free, 150), 
                    Duration = rnorm(1, 500, 100)) %>% # The neighboring individual is freely vocalizing at its tempo
  mutate(
    Onset = Interval + runif(1, 0, T_free),
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
    mutate(Interval = rnorm(1, T_free, 150))
  temp_prevFocal <- filter(IOI, Caller == "Focal")[nrow(filter(IOI, Caller == "Focal")),] %>%
    mutate(Interval = rnorm(1, T_free, 150))

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


