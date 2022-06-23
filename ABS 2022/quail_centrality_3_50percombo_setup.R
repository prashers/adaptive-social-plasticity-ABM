

library(stringr)





#can remove prior.affils, unfam.prod, eat.delay, alt-food, and affil.IDs columns before reading in the csv file to reduce file size
#probably don't need memory.succ.foragers

q.data.full = read.csv("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/quail_centrality_3 5percombo.csv", skip = 6, header = T)

head(q.data.full)
nrow(q.data.full)
str(q.data.full)
min(q.data.full$ticks)
max(q.data.full$ticks)

sum(q.data.full$X.step.==q.data.full$ticks) # "X.step." and "ticks" are the same, so I will remove "X.step."

q.data.full = q.data.full[,!(names(q.data.full) %in% c("prior.affils.", "unfam.prod.", "eat.delay.", "alt.food.", "affil.IDs", "reset.food.", "X.step."))]

names(q.data.full)

#Column info:
# "X.run.number." is the model run ID - in this case, there is a unique number for each parameter combination 
# "group.size" is one of the five parameters that can vary between run IDs
  #It sets the number of agents for each model run
# "memory" is one of the five parameters that can vary between run IDs. 
  #It sets the maximum number of time steps that a successful forager can be remembered
#"attention" is one of the five parameters that can vary between run IDs.
  #It sets the probability that agents will enter a successful forager into memory if their memory slot is empty
#"preference" is one of the three parameters that can vary between run IDs.
  #It sets the probability that agents will follow the successful forager in their memory
#"approach.food." is one of the five parameters that can vary between run IDs.
  #It determines whether foragers other than the producer approach food when their energy level gets low
#"ticks", time step within current model run - as appears in netlogo
#"prox.centrality.list", list containing each agent's degree centrality in the proximity network (number of other agents within a certain distance of the individual)
#"proxim.IDs", list of WHO numbers of the agents within proximity of each individual
#"current.succ.foragers", the list containing IDs of agents that successfully foraged in each time step
#"memory.succ.foragers", the memory list, which contains the IDs of foragers that have successfully eaten in the past <memory> time steps - Just for reference. Not used in model behavior 
#"fss.list", list containing first-sf-seen value of each agent
#"foll.centrality.list", list containing each agent's degree centrality in the following network (number of other agents following the individual - count of incoming follow links)
#"foll.IDs", list containing WHO numbers of the agents following each individual
#"current.xycor" list of the xy coordinates for each agent 
#"energy.list" list containing each agent's energy level in the current time step


#renaming a few columns
names(q.data.full)[names(q.data.full) == "X.run.number."] <- "run.num" 
names(q.data.full)[names(q.data.full) == "approach.food."] <- "approach.food" 
names(q.data.full)[names(q.data.full) == "prox.centrality.list"] <- "pr.centrality.list"
names(q.data.full)[names(q.data.full) == "foll.centrality.list"] <- "fo.centrality.list"
names(q.data.full)


#reordering the data frame
q.data.ord = q.data.full[order(q.data.full$run.num, q.data.full$ticks),] #reorder data by run number and time step within run number
head(rownames(q.data.ord))
rownames(q.data.ord) = 1:nrow(q.data.ord)
tail(rownames(q.data.ord))
rm(q.data.full) #remove unordered data frame to save memory

num.combos = nrow(unique(q.data.ord[,2:6])) #number of unique group.size, mem, att, pref, approach.food combinations
# I want to add a column that tells me the combo number (1:1250)

nrow(q.data.ord[q.data.ord$group.size==3 & q.data.ord$memory == 0 & q.data.ord$attention == 0 & q.data.ord$preference == 0 & q.data.ord$approach.food=="false",]) # every combo has 1505 rows
q.data.ord[rownames(q.data.ord) == 3011, colnames(q.data.ord) %in% c("group.size", "memory", "attention", "preference", "approach.food")]

combo = vector()
for (i in 1:num.combos) {
  x = rep(i, 1505)
  combo = append(combo, x, after=length(combo))
}
length(combo)
range(combo)

q.data.ord$combo.num = combo
head(q.data.ord)


# I will need to know which phases each row belongs to 
# (tick 0 is the state of the model when reset button is pressed, 
# ticks 1:100 are the pre-foraging phase, ticks 101:200 are the foraging phase, 
# and ticks 201:300 are the post-foraging phase)
q.data.ord$phase = NA
q.data.ord[q.data.ord$ticks == 0,]$phase = "start"
q.data.ord[q.data.ord$ticks %in% 1:100,]$phase = "pre-forage"
q.data.ord[q.data.ord$ticks %in% 101:200,]$phase = "forage"
q.data.ord[q.data.ord$ticks %in% 201:300,]$phase = "post-forage"


### need to be able to count the number of times an agent was in proximity to, or being followed by, each other agent
### so I need to separate proxim.IDs, foll.IDs and coor.list into different columns
### need to separate data into different dataframes for each group size, because that will influence how many columns that data is split into


for (i in unique(q.data.ord$group.size)) {
  
  loop.data = q.data.ord[q.data.ord$group.size==i,]
  
  ######SPLITTING COORDINATES IS NOT IMPORTANT FOR ABS 2022 presentation, SO I AM SKIPPING IT - SEE "quail_centrality_2_50percombo_setup.R" for relevant code for a group of six######
  
  
  ###separate proxim.IDs into different columns###
  
  # first, a separate column for each agent's list, 
  # then further split into separate columns per agent in proximity proxA1:proxA19 (up to i-1 agents in proximity)
  split.proximIDs = str_split_fixed(loop.data$proxim.IDs, "] ", i) # split the data in the proxim.IDs column at each "] " into i separate columns
  #head(split.proximIDs)
  split.proximIDs[,1:i] = gsub("[[]", "", split.proximIDs[,1:i]) # remove the square brackets from the split data
  split.proximIDs[,1:i] = gsub("[]]", "", split.proximIDs[,1:i])
  #head(split.proximIDs)
  
  split.prox = data.frame(1:nrow(split.proximIDs)) # need same number of rows as nrow(split.proximIDs)
  for(j in 1:ncol(split.proximIDs)){ # loop to separate each foragers proximity IDs into i-1 columns
    x = str_split_fixed(split.proximIDs[,j], " ", i-1)
    x = as.data.frame(x)
    
    for(k in 1:ncol(x)) {
        colnames(x)[k] = paste0("prox", letters[j], k)
    }
     
    
    split.prox = cbind(split.prox, x)
  }
  #colnames(split.prox)[1] = "X"
  #head(split.prox)
  #tail(split.prox)
  

  #unique(split.prox$proxb2) # split.prox has some blanks if there were not 5 values for proximIDs in an agent's split.proximIDs column 
  split.prox[split.prox==""] = NA #replace all blank cells with NA
  split.prox[split.prox=="NA"] = NA #replace all characters "NA" with missing values
  
  split.prox = split.prox[,-1] #remove first column
  #View(data.frame(loop.data[11000:11020,]$proxim.IDs, split.prox[11000:11020,]))# compare split.prox to proxim.IDs column in q.data.ord
  
  ### split.prox is ready to be added to the big data frame
  
  
  
  
  
  ##### add all separated columns to the big dataframe 
  q.data.split = cbind(q.data.ord[, !(names(q.data.ord) %in% c("prox.centrality.list", "proxim.IDs"))], split.prox) #, split.foll) #, split.cor) #add the new columns into the dataframe with affil.IDs, proxim.IDs, foll.IDs, and coor.list columns removed
  #View(q.data.split)
  if(i==3){
    write.csv(q.data.split, "q_data_split_grpsz3.csv")
  } else if(i==6) {
    write.csv(q.data.split, "q_data_split_grpsz6.csv")
  } else if(i==10) {
    write.csv(q.data.split, "q_data_split_grpsz10.csv")
  } else if(i==15) {
    write.csv(q.data.split, "q_data_split_grpsz15.csv")
  } else if(i==20) {
    write.csv(q.data.split, "q_data_split_grpsz20.csv")
  }
  
  
  
  
  
  
}



  
  
# MATRIX WILL CONTAIN COUNTS OF HOW MANY TIME STEPS EACH DYAD WAS IN PROXIMITY (FOR PROXIMITY NETWORK) 
# OR IN HOW MANY TIME STEPS EACH AGENT WAS FOLLOWED BY EACH OTHER AGENT (DIRECTED FOLLOWING NETWORK) 
# WILL HAVE A MATRIX FOR EACH PHASE WITHIN EACH MODEL RUN


# divide proximity dataframe into three -- one for each phase
#qdp.start.pre = qd.prox[qd.prox$phase %in% c("start", "pre-forage"),] 
q.data.pre = q.data.split[q.data.split$phase == "pre-forage",]
write.csv(q.data.pre, "q_data_pre50.csv")

q.data.forage = q.data.split[q.data.split$phase == "forage",]
write.csv(q.data.forage, "q_data_forage50.csv")

q.data.post = q.data.split[q.data.split$phase == "post-forage",]
write.csv(q.data.post, "q_data_post50.csv")
  
  
  
