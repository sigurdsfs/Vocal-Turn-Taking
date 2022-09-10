
#### MODEL TESTING ####

############ Reset Hypothesis ############
ResetHypothesis <- function(data){
  
  if (!"Group" %in% colnames(data) ){ #If we only have 1 group 
    data <- data %>% 
      arrange(CallNr, Onset) %>%
      mutate(Interindividual = 0)
  } 
  
  else{  #Multiple groups
    data <- data %>% 
      arrange(Group, CallNr, Onset) %>%
      mutate(Interindividual = 0)
  }
  for(i in 2:nrow(data)){
    data[i,"Interindividual"] = ifelse(data$ID[i] == data$ID[i-1], 0, 1) #Check if inter- or intra-timing
  }
  data <- data %>% 
    mutate(Interindividual = as.factor(Interindividual))
  
  #plotting
  plot <- data %>% 
    filter(Latency != "NA") %>%
    ggplot(aes(x = Latency, color = Interindividual)) + geom_density() + ggtitle("Density Plot of Latencies: Reset Hypothesis")
  print(plot)
  
  #Testing
  df_Inter <- data %>% 
    filter(Latency != "NA" & Interindividual == 1) 
  
  df_Intra <- data %>%
    filter(Latency != "NA" & Interindividual == 0)
  
  test <- ks.test(df_Inter$Latency, df_Intra$Latency)
  print(test)
  return(data)
}

########### Inhibiton Hypothesis ############

InhibitionHypothesis <- function(df){
  i = 1
  #If Groups Do Not Exist
  if ("Group" %in% colnames(df) == FALSE){
    print("No Groups")  
    for (id in unique(df$ID)){
      
      for (b in (unique(df$CallNr[df$ID == id]))){
        
        #Whale1
        whale1 <- subset(df,ID == id & CallNr == b)
        
        if (length(unique(whale1$CallNr))== 1){ 
          #Whale 2
          whale2_INFO <- df %>% 
            group_by(ID) %>% 
            slice_sample()%>% 
            filter(ID != id) %>%
            ungroup() %>% 
            sample_n(1)
        }
        else{
          #Whale 2
          whale2_INFO <- df %>% 
            group_by(ID, CallNr) %>% 
            slice_sample()%>% 
            filter(ID != id & CallNr != b) %>%
            ungroup() %>% 
            sample_n(1)
        }
        
        whale2 <- subset(df, ID == whale2_INFO$ID & CallNr == whale2_INFO$CallNr)
        
        df_inhib2 <- rbind(whale1,whale2) %>% 
          filter(Latency > 0.1) %>% 
          arrange(CallNr, Onset) %>% 
          mutate(Group = i) %>% 
          mutate(Latency = Onset - lag(Offset, 1))
      
        #Combine
        if (i == 1) df_inhib_final2 <- df_inhib2
        else df_inhib_final2 <- rbind(df_inhib2, df_inhib_final2)
        
        i = i +1
      }
    }
    df <- df %>% #We need to have Group collumn to bind simulated data with original data
      mutate(Group = 1)
  }
  #If Groups Exist
  else{
    print("Groups Exist")
    for (g in unique(df$Group)) {
      
      for (id in unique(df$ID[df$Group == g])){
        
        for (b in (unique(df$CallNr[df$Group == g & df$ID == id]))){
          
          #Whale1
          whale1 <- subset(df, Group == g & ID == id & CallNr == b)
          
          if (length(unique(whale1$CallNr))== 1){
            #Whale 2
            whale2_INFO <- df %>% 
              group_by(ID, Group) %>% 
              slice_sample()%>% 
              filter(Group != g & ID != id) %>%
              ungroup() %>% 
              sample_n(1)
          }
          else{
            #Whale 2
            whale2_INFO <- df %>% 
              group_by(ID, Group, CallNr) %>% 
              slice_sample()%>% 
              filter(Group != g & ID != id & CallNr != b) %>%
              ungroup() %>% 
              sample_n(1)
          }
          
          whale2 <- subset(df, Group == whale2_INFO$Group & ID == whale2_INFO$ID & CallNr == whale2_INFO$CallNr)
          
          df_inhib2 <- rbind(whale1,whale2) %>% 
            filter(Latency > 0.1) %>% 
            arrange(Group, CallNr, Onset) %>% 
            mutate(Group = i) %>% 
            mutate(Latency = Onset - lag(Offset, 1))
          
          
          
          #Combine
          if (i == 1) df_inhib_final2 <- df_inhib2
          else df_inhib_final2 <- rbind(df_inhib2, df_inhib_final2)
          
          i = i + 1
        }
      } 
    }
  }
  #Combine the Shuffled and real data
  df_inhib_final2$Type <-  "Simulated" 
  df$Type <-  "Real"
  
  df_inhib_combined2 <- rbind(df_inhib_final2, df)
  
  #Plot
  Plot_inhib2 <- df_inhib_combined2 %>% 
    ggplot(aes(Latency, color = Type)) + geom_density() + ggtitle("Density Plot of Latencies: Inihibiton Hypothesis")
  print(Plot_inhib2)
  #Test
  Test <- ks.test(df_inhib_final2$Latency, df$Latency)
  print(Test)
  
  return(df_inhib_combined2)
}






######## Rotation  #########


RotateData <- function(d = data, threshold = threshold, delay = delay) {
  
  # We create rotated datasets
  ## for each whale but the first we add a random n of seconds to the timeseries of starting points
  ## we take the shifted timeseries which are now longer than the bout and we cut and paste the sticking out bit to the beginning
  
  for (i in unique(d$bout)){
    
    ## Let's reset the timer within each bout
    BoutReset <- d$StartTime[d$bout==i][1]
    d$BoutStartTime[d$bout==i] <- d$StartTime[d$bout==i] - BoutReset
    d$BoutEndTime[d$bout==i] <- d$EndTime[d$bout==i] - BoutReset
    
    ## identify all callers but the first
    Callers <- unique(d$WhaleID[d$bout==i])
    RotatedCallers <- Callers[-1]
    RotatedDelays <- runif(length(RotatedCallers), min= 1, max= delay)
    
    ## Last call
    LastCall <- nrow(d[d$bout==i,])
    BoutEnd <- d$BoutEndTime[d$bout==i][LastCall]
    
    if (length(Callers)>1){
      
      for (n in seq(length(RotatedCallers))){
        
        rc <- RotatedCallers[n]
        
        d$BoutStartTime[d$bout==i & d$WhaleID == rc] <- d$BoutStartTime[d$bout==i & d$WhaleID == rc] + RotatedDelays[n]
        d$BoutEndTime[d$bout==i & d$WhaleID == rc] <- d$BoutEndTime[d$bout==i & d$WhaleID == rc] + RotatedDelays[n]
        
        d$BoutStartTime[d$bout==i & d$WhaleID == rc & d$BoutStartTime > BoutEnd] <- d$BoutStartTime[d$bout==i & d$WhaleID == rc & d$BoutStartTime > BoutEnd] - BoutEnd
        
        d$BoutEndTime[d$bout==i & d$WhaleID == rc & d$BoutEndTime > BoutEnd] <- d$BoutStartTime[d$bout==i & d$WhaleID == rc & d$BoutEndTime > BoutEnd] + d$Duration[d$bout==i & d$WhaleID == rc & d$BoutEndTime > BoutEnd ]
        
      }
      
    }
    
  }
  
  d <-d[order(d$bout, d$BoutStartTime),]
  
  d <- d %>% mutate(
    BoutLatency = BoutStartTime - lag(BoutEndTime, 1),
  )
  d$BoutLatency[d$BoutStartTime == 0]<-NA
  
  d$Type <- "Rotated"
  
  d <- d[order(d$StartTime),]
  d <- d %>% 
    mutate(
      Latency = StartTime - lag(EndTime, 1),
    )
  
  
  
  return(d)
}






##### CLEAN DATA ##########

CleanData <- function(filename =filename, threshold=threshold){
  
  # Load dataset
  d <- read_delim(filename, "\t") %>% 
    rename( ## More meaningful names to me
      StartTime = Time,
      WhaleID = Source_code,
      WhaleType = Source_type) %>% 
    mutate( # Calculating some additional variables 
      EndTime = StartTime + Duration, 
      Latency = StartTime - l<ag(EndTime, 1),
      DurationPrev = lag(Duration,1),
      CallType = ifelse(Latency < threshold,"ResponseCall","InitiationCall"),
      Age = ifelse(WhaleType == "Juvenile", 1, ifelse(WhaleType == "Indeterminate",2,3))
    )
  
  
  for (n in c(2:nrow(d))){
    
    if (d$Latency[n] > threshold){
      b <- b + 1 # if the distance from previous call is higher than the threshold, change bout
    }
    d$bout[n] <- b # add bout counter value
  }
  
  ## Remove duplicates due to multiple recording devices
  dups =NULL
  for (i in 2:nrow(d)){
    
    if (d$bout[i]==d$bout[i-1] & d$WhaleID[i]==d$WhaleID[i-1] & d$StartTime[i] - d$StartTime[i-1] < 1 & d$Focal[i]!=d$Focal[i-1]){
      dups <- append(dups,i)
    }
    
  }
  
  if (length(dups)){
    d <- d[-dups,]
  }
  
  d <- d %>% 
    mutate(
      Latency = StartTime - lag(EndTime, 1),
    )
  
  d_Real <- d
  
  for (i in unique(d_Real$bout)){
    ## Let's reset the timer within each bout
    BoutReset <- d_Real$StartTime[d_Real$bout==i][1]
    d_Real$BoutStartTime[d_Real$bout==i] <- d_Real$StartTime[d_Real$bout==i] - BoutReset
    d_Real$BoutEndTime[d_Real$bout==i] <- d_Real$EndTime[d_Real$bout==i] - BoutReset
  }
  
  d_Real <- d_Real %>% mutate(
    BoutLatency = BoutStartTime - lag(BoutEndTime, 1),
  )
  
  d_Real$Type <- "Real"
  
  d_Real$BoutLatency[d_Real$BoutStartTime == 0]<-NA
  
  return(d_Real)
}







