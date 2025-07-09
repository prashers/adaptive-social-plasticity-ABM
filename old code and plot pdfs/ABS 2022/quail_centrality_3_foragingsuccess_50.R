# Script for getting metrics of foraging success for agents other than the producer

library(readr)
library(tidyr)
library(dplyr)
library(stringr)

group.sizes = c(3, 6, 10, 15)#, 20)

n.loops = max(group.sizes)
pb = txtProgressBar(min=0, max = n.loops, style=3)
start.time = Sys.time()
for(i in group.sizes) {
  
  if(i==3){
    setwd("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/group_size_3")
  } else if(i==6) {
    setwd("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/group_size_6")
  } else if(i==10) {
    setwd("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/group_size_10")
  } else if(i==15) {
    setwd("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/group_size_15")
  } else if(i==20) {
    setwd("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/group_size_20")
  }
  
  gc()
  
  #q.data = read.csv("q_data_split.csv", header=T)[, -c(1, 14:393)] #exclude proximity columns (goes up to 393 for group size of 20)
  
  q.data = read_csv("q_data_split.csv", col_types = cols()) # including the 'col_types = cols()' argument suppresses the unnecessary column specification messages from read_csv
  q.data = as.data.frame(q.data[, -c(1, 14:ncol(q.data))]) #exclude proximity columns
  
  #FOR EACH PARAMETER COMBO, I NEED:
  # a count of how many time steps each agent or any agent other than the producer successfully foraged 
    #although the number of time steps might not change as much across parameter combos if more individuals are foraging in the same time step
  # average energy level of foragers other than the producer at the end of the model (ticks = 300)
  
  
  #for count of time steps - group by run number, get count, then get average count for each parameter combo, which you can plot
  chr.to.find = as.character(1:(i-1)) #want to find all foragers except the producer (agent 0)
  chr.to.find = paste(chr.to.find, collapse = "|")
  
  #data.to.count = q.data[grep(chr.to.find, q.data$current.succ.foragers),] #this gives all rows with time steps in which any forager other than the producer successfully foraged - the producer also foraged in many of these time steps
  
  for.success = q.data %>% 
    filter(rownames(q.data) %in% grep(chr.to.find, q.data$current.succ.foragers)) %>% #this gives all rows with time steps in which any forager other than the producer successfully foraged - the producer also foraged in many of these time steps
    group_by(run.num, memory, attention, preference, approach.food) %>% 
    summarize(n.timesteps = n()) 
    
  # length(unique(for.success$run.num))
  # for.success is missing some run numbers because the ones where foragers other than the producer never foraged were removed when subsetting 
  # NEED TO FILL IN THE DATA FRAME TO INCLUDE ALL RUN NUMBERS
    
  data.to.add = q.data[!(q.data$run.num %in% for.success$run.num) & q.data$ticks == 0, c(1, 3:6)] #subset of q.data containing one row from each run number that does not appear in for.success and columns 1, and 3:6 (run.num, memory, attention, preference, and approach.food)
  #nrow(data.to.add)
  data.to.add$n.timesteps = rep(0, nrow(data.to.add))
  
  for.success = rbind(for.success, data.to.add)
  #length(unique(for.success$run.num))
  rm(data.to.add)
  for.success = for.success[order(for.success$run.num),]
  
  # for average energy level of foragers, remove all rows where ticks is not 300; separate forager energy levels into separate columns; pivot longer so the energy levels are stacked in the same column; calculate average per run number or combo number
  q.data = q.data[q.data$ticks==300,]
  #nrow(q.data) #should be equal to number of combos
  q.data$energy.list = gsub("[[]", "", q.data$energy.list)
  q.data$energy.list = gsub("[]]", "", q.data$energy.list)
  energy.split = str_split_fixed(q.data$energy.list, " ", i) #split data in energy.list column into i columns
  
  colnames(energy.split) = LETTERS[1:i] #set column names - unique letter for each agent (A = producer)
  
  q.data = cbind(q.data, energy.split)
  
  q.data = q.data %>% 
    pivot_longer(LETTERS[1:i], #pivot_longer is the same as melt in the reshape2 package
                 names_to = "agent", values_to = "energy")
  
  #now calculate average energy level for each run number
  q.data$energy = as.numeric(q.data$energy)
  q.data = q.data[q.data$agent != "A",] %>% #IGNORING ROWS FOR PRODUCER BECAUSE I AM INTERESTED IN THE AVERAGE ENERGY LEVEL OF THE OTHER FORAGERS
    group_by(run.num) %>%
    mutate(mean.run.energy = mean(energy), med.run.energy = median(energy), var.run.energy = var(energy)) #I checked that the mean values were accurate by using mutate() here instead of summarize
  
  q.data = q.data %>%
    group_by(combo.num) %>%
    mutate(mean.combo.energy = mean(energy), med.combo.energy = median(energy), var.combo.energy = var(energy))
  
  q.data = unique(q.data[,c("run.num", "combo.num", "mean.run.energy", "med.run.energy", "var.run.energy", "mean.combo.energy", "med.combo.energy", "var.combo.energy")])
  
  # add average energy level of foragers per run number to for.success data frame and write it to a csv file
  for.success = merge(for.success, q.data, by = "run.num")# I am merging to be safe, but the data should be in the correct order in both data frames, so I could just do for.success$mean.energy = q.data$mean.energy
  write.csv(for.success, "for_success.csv")
  
  setTxtProgressBar(pb,i)#update progress bar
  
}#END OF LOOP
end.time = Sys.time()
run.time = end.time - start.time
run.time #ran in 13 seconds for 5 runs per combo
#ran in 18 seconds for 50 runs per combo group sizes 3 and 6
#ran in 1 minute for 50 runs per combo group sizes 3, 6, 10, 15

#for_success.csv can now be used for plotting, etc.
