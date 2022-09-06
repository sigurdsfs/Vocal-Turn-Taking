# Coupled Oscillators Generative Model:
############################################
# According to (2013,Takahashi). 
# 
# A:(animal 1), B:(animal2)
# 
# - The coupling of time dependencies in phase and anti-phase allows for the derivation of 
# timing of animal B given that we know the timings of animal A's speech production.
# 
# # Measures
# - R(response intervals): A_offset - B_onset
# - PR (Phase response): 
# - T0: Interval call of animal A that did not have a intervening call from marmoset 2. 
# 
# PR - T0 = Will indicate if animal A respond quickly or slowly to animal B
# 
# ########################################

n = 10 #Number of calls A
u <- 0 #Average communication duration.   
period <- 9 #The periodicity of animal A calls. (can be changed to stochastic)
n_onsets <- seq(1,n)

#Animal A
onset_a <- period * n_onsets 
offset_a <- onset_a + abs(rnorm(n, 0, log(period)))


#Animal B
onset_b <-  


data.frame(onset_a, offset_a)




