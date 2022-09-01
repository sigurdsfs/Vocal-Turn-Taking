
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
IOI_neigh <- tibble(Interval = rnorm(n, T_free, 150)) # The neighboring individual is freely vocalizing at its tempo
for (i in 1:n) {
  if (i == 1) {
    IOI_neigh$Timing[i] = IOI_neigh$Interval[i]
  } else {
    IOI_neigh$Timing[i] = IOI_neigh$Interval[i] + IOI_neigh$Timing[i - 1]
  }
}
IOI_focal <- tibble(Interval = rep(NA, n), Timing = NA) # The focal individual is yet undetermined

## Arousal model: 
# IF THE NEIGHBOR VOCALIZES, THE FOCAL VOCALIZES WITH AN INTERVAL OF 0-PREV NEIGH INTERVAL
# THEN IT WAITS

for (i in 1:n) {
  IOI_focal$Interval[i] <- runif(1, 0, IOI_neigh$Interval[i])
  IOI_focal$Timing[i] <- IOI_neigh$Timing[i] + IOI_focal$Interval[i]
}

data_plot <- tibble(
  NeighborTiming = IOI_neigh$Timing,
  NeighborInterval = IOI_neigh$Interval,
  FocalTiming = IOI_focal$Timing,
  FocalInterval = IOI_focal$Interval
)

ggplot(data_plot) +
  geom_point(aes(NeighborInterval, FocalInterval)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()


