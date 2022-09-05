## Inhibition
T_free <- 1500 
n <- 100

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