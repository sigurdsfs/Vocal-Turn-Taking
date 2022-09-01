
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
IOI_neigh = rep(T_free, n)
IOI_focal <- rep(NA, n)

## Neighboring individual vocalizes with period T_free


for (i in seq(n)) {
  IOI_focal[i] <- runif(1, 0, IOI_neigh[i])
}

plot(IOI_focal, IOI_neigh)
plot(IOI_focal)
