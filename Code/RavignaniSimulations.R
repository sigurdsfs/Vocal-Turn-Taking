
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
 ## by an interval equivalent to the delay between its previous signal and a neighbours signal


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


## WE NEED TO TUNR THEM INTO LONG FORMAT AND MAKE THE FORMAT COMPATIBLE WITH THE EMPIRICAL DATA

## WE NEED TO GENERALIZE TO CASES WHERE A CALLER CAN MAKE MULTIPLE CALLS BEFORE THE OTHER ENTERS



