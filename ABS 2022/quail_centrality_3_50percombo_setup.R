
library(readr) #needed for importing the data file in chunks
library(stringr)

#can remove prior.affils, unfam.prod, eat.delay, alt-food, and affil.IDs columns before reading in the csv file to reduce file size (FILE TOO LARGE TO OPEN IN EXCEL...)
#probably don't need memory.succ.foragers



f = function(x, pos) subset(x, select = -c(`prior-affils?`, `unfam-prod?`, `eat-delay?`, `alt-food?`, `affil-IDs`, `reset-food?`, `[step]`, `memory-succ-foragers`, `foll-centrality-list`, `foll-IDs`, `current-xycor`, `prox-centrality-list`, `fss-list`))#getting rid of all these columns allowed me to read in the file with all group sizes without errors
q.data.full = read_csv_chunked("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/quail_centrality_3_50percombo.csv", callback = DataFrameCallback$new(f), skip = 6)

head(q.data.full)
nrow(q.data.full)
str(q.data.full)
min(q.data.full$ticks)
max(q.data.full$ticks)
unique(q.data.full$`group-size`)

names(q.data.full)

#Remaining column info:
# "X.run.number." is the model run ID - a different number for each run of the model. the first 50 should be for the first combination of parameters 
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


q.data.full = as.data.frame(q.data.full)
#renaming a few columns
names(q.data.full)[names(q.data.full) == "[run number]"] <- "run.num"
names(q.data.full)[names(q.data.full) == "group-size"] <- "group.size"
names(q.data.full)[names(q.data.full) == "approach-food?"] <- "approach.food"
names(q.data.full)[names(q.data.full) == "current-succ-foragers"] <- "current.succ.foragers"
names(q.data.full)[names(q.data.full) == "proxim-IDs"] <- "proxim.IDs"
names(q.data.full)[names(q.data.full) == "energy-list"] <- "energy.list"
names(q.data.full)[names(q.data.full) == "reset-num"] <- "reset.num"
names(q.data.full)

gc() #clear garbage

#reordering the data frame
q.data.ord = q.data.full[order(q.data.full$run.num, q.data.full$ticks),] #reorder data by run number and time step within run number
head(rownames(q.data.ord))
rownames(q.data.ord) = 1:nrow(q.data.ord)
tail(rownames(q.data.ord))

rm(q.data.full, f) #remove unordered data frame to save memory
gc()

#num.combos = nrow(unique(q.data.ord[,2:6])) #number of unique group.size, mem, att, pref, approach.food combinations
num.combos = 1250 # Previous line would not run because memory low, but number of combos should be the same as when I had 5 runs per combo, because the number of parameters/parameter values I am looking at is the same
# I want to add a column that tells me the combo number (1:1250)

nrow(q.data.ord[q.data.ord$group.size==3 & q.data.ord$memory == 0 & q.data.ord$attention == 0 & q.data.ord$preference == 0 & q.data.ord$approach.food==FALSE,]) # every combo has 15050 rows
q.data.ord[rownames(q.data.ord) == 15050, colnames(q.data.ord) %in% c("group.size", "memory", "attention", "preference", "approach.food")] #checking whether combo changes every 15050 rows

combo = vector()
for (i in 1:num.combos) {
  x = rep(i, 15050)
  combo = append(combo, x, after=length(combo))
}
length(combo)
range(combo)

q.data.ord$combo.num = combo
head(q.data.ord)
rm(combo)


# I will need to know which phases each row belongs to 
# (tick 0 is the state of the model when reset button is pressed, 
# ticks 1:100 are the pre-foraging phase, ticks 101:200 are the foraging phase, 
# and ticks 201:300 are the post-foraging phase)
q.data.ord$phase = NA
q.data.ord[q.data.ord$ticks == 0,]$phase = "start"
q.data.ord[q.data.ord$ticks %in% 1:100,]$phase = "pre-forage"
q.data.ord[q.data.ord$ticks %in% 101:200,]$phase = "forage"
q.data.ord[q.data.ord$ticks %in% 201:300,]$phase = "post-forage"


setwd("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022")
write.csv(q.data.ord, "q_data_ord_50percombo.csv") #saving csv here so I can run the big loop after restarting R with clean memory



###########################################
###########################################
#START HERE IF YOU'VE JUST REOPENED R AFTER SAVING q.data.ord IN A CSV 
library(readr) 
library(stringr)

### need to be able to count the number of times an agent was in proximity to, or being followed by, each other agent
### so I need to separate proxim.IDs, foll.IDs and coor.list into different columns
### need to separate data into different dataframes for each group size, because that will influence how many columns that data is split into

group.sizes = c(3,6,10,15,20)

n.loops = 20 # max(unique(q.data.ord$group.size))
pb = txtProgressBar(min=0, max = n.loops, style=3)
start.time = Sys.time()


for (i in group.sizes) {
  
  #loop.data = q.data.ord[q.data.ord$group.size==i,] #subset of data for current group size
  
  gc()
  # I AM GOING TO TRY READING IN DATA FOR EACH GROUP SIZE INSIDE THE LOOP SO I DON'T HAVE THE LARGER DATA FRAME TAKING UP MEMORY
  f = function(x, pos) subset(x, group.size == i, select = -c(1))
  loop.data = read_csv_chunked("C:/Users/sanja/Documents/Sanjay's stuff/QuailCentralityABM/R analyses/quail_centrality_3/ABS 2022/q_data_ord_50percombo.csv", callback = DataFrameCallback$new(f))
  loop.data = as.data.frame(loop.data)
  
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
  
  
  ######SPLITTING FOLLOW IDs IS NOT IMPORTANT FOR ABS 2022 presentation, SO I AM SKIPPING IT - SEE "quail_centrality_2_50percombo_setup.R" for relevant code for a group of six######
  
  
  ##### add all separated columns to the big dataframe 
  q.data.split = cbind(loop.data[, !(names(loop.data) %in% c("proxim.IDs"))], split.prox) #, split.foll) #, split.cor) #add the new columns into the dataframe with affil.IDs, proxim.IDs, foll.IDs, and coor.list columns removed
  #View(q.data.split)
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
  
  write.csv(q.data.split, "q_data_split.csv")
  
  setTxtProgressBar(pb,i)#update progress bar
  
} #end of big loop
end.time = Sys.time()
run.time = end.time - start.time
run.time # ran in 15 minutes for 50 runs per combo, but only got through group sizes 3, 6, and 10, then got error during 15 (cannot allocate vector of size 14.4 Mb)
# NOT ENOUGH MEMORY TO GET q_data_split.csv for group sizes 15 and 20 (I tried running this loop for just those two starting with empty memory and it did not work)


rm(q.data.ord, split.prox, split.proximIDs, x, loop.data)

#START HERE IF RE-RUNNING WITH EMPTY R ENVIRONMENT AFTER SAVING q_data_split files
#group.sizes = c(3, 6, 10, 15, 20)
group.sizes = c(3, 6, 10)

prox.labels = vector() #I use this in the next big loop to fill in the 'prox.key' column
for(i in 1:20) {
  temp.labels = paste0("prox", letters[i])
  
  prox.labels = append(prox.labels, temp.labels, after=length(prox.labels))
}


library(readr)
library(tidyr)
library(dplyr)


n.loops = max(group.sizes)
pb = txtProgressBar(min=0, max = n.loops, style=3)
start.time = Sys.time()

for(i in group.sizes){
  
  gc()
  
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
  
  q.data.split = read_csv("q_data_split.csv", col_types = cols())
  q.data.split = as.data.frame(q.data.split[,-1])
  
  # MATRIX WILL CONTAIN COUNTS OF HOW MANY TIME STEPS EACH DYAD WAS IN PROXIMITY (FOR PROXIMITY NETWORK) 
  # WILL HAVE A MATRIX FOR EACH PHASE WITHIN EACH MODEL RUN
  
  
  # divide proximity dataframe into three -- one for each phase
  #qdp.start.pre = qd.prox[qd.prox$phase %in% c("start", "pre-forage"),] 
  q.data.pre = q.data.split[q.data.split$phase == "pre-forage",]
  write.csv(q.data.pre, "q_data_pre50.csv")
  
  q.data.forage = q.data.split[q.data.split$phase == "forage",]
  write.csv(q.data.forage, "q_data_forage50.csv")
  
  q.data.post = q.data.split[q.data.split$phase == "post-forage",]
  write.csv(q.data.post, "q_data_post50.csv")
  
  rm(q.data.split)# remove q.data.split from environment to save memory
  
  
  
  
  ###PROXIMITY network: PRE-foraging phase###
  prox.count.pre = q.data.pre %>% 
    pivot_longer(starts_with("prox"), #pivot_longer is the same as melt in the reshape2 package
                 names_to = "prox", values_to = "prox.ID") 
  
  
  #nrow(q.data.pre)*(i*(i-1)) == nrow(prox.count.pre)#check that prox.count has the correct number of rows
  
  prox.count.pre = tibble::add_column(prox.count.pre, prox.key = "NA", .after = "prox")
  
  rm(q.data.pre) # remove q.data.pre from environment to save memory
  
  for(j in 1:20){#LOOP TO FILL IN THE 'prox' COLUMN
    if(j > i){break} #end the loop if j > i
    
    prox.letter = unique(prox.count.pre[startsWith(prox.count.pre$prox, prox.labels[j]), ]$prox)
    
    prox.count.pre = prox.count.pre %>% mutate(prox.key=replace(prox.key, prox %in% prox.letter, LETTERS[j]))
    
  }
#  View(prox.count.pre)
#  unique(prox.count.pre$prox.key)# check that all NAs were replaced
  
  
  # use dplyr functions to get counts after grouping by prox.key and proxIDs
  prox.count.pre = prox.count.pre %>% 
    group_by(run.num, memory, attention, preference, approach.food, prox.key, prox.ID) %>% 
    summarize(n = n()) 
  
  #prox.count.pre = prox.count.pre[complete.cases(prox.count.pre$prox.ID),] #using complete.cases like this removes a lot of runs in which agents were never near each other
  #unique(prox.count.pre$prox.ID)
  #range(prox.count.pre$n) #WHY IS THE MAX 200? Max is 200 for a group of 3 because it is counting 100 rows for each proxa/proxb... column
  prox.count.pre$prox.ID = as.numeric(prox.count.pre$prox.ID)
  prox.count.pre[prox.count.pre$n == max(prox.count.pre$n) & is.na(prox.count.pre$prox.ID),]$prox.ID = 111 #replace NAs in the rows you want to keep with 111 - I want to keep these to make sure I have data from all run.numbers (I think especially for small group sizes, the agents were never in proximity and I was removing those runs by accident before)
  prox.count.pre = prox.count.pre[complete.cases(prox.count.pre$prox.ID),]#then remove unnecessary rows
  #length(unique(prox.count.pre$run.num)) #now prox.count.pre contains all run.num values
  
  #View(prox.count.pre)
  ### prox.count.pre contains the counts of how many time steps each agent was in proximity to each other agent during the PRE-FORAGING phase
  ### SINCE I USED q.data.pre, THIS DOES NOT INCLUDE THE STATE OF THE MODEL AT TICK ZERO
  
  #save it as a csv to save space in the R workspace
  write.csv(prox.count.pre, "prox_count_pre50.csv")
  
  
  
  
  
  ###PROXIMITY network: FORAGING phase###
  
  rm(prox.count.pre)# remove prox.count.pre from environment to save memory
  gc()
  
  
#  #For each run, remove rows before the producer first accesses food so we can see effects during period that agents can actually be foraging/following 
#  #this reduces the total number of time steps that can be counted toward the number of time steps that an agent foraged in the foraging phase, but I deal with that by standardizing edge weights by the number of total time steps in each phase (see analysis R script)
   #q.data.forage[q.data.forage$current.succ.foragers==0,] #No instances of just a 0 without brackets, so no need to change anything in the column
  
#  #min(grep("0", q.data.forage$current.succ.foragers, fixed=T)) # shows numerical index of all the rows where current.succ.foragers contains a zero - the minimum value within each model run should be the first time the producer accessed the food patch
  
  
  qdf.fed = data.frame()
#  n.loops = max(unique(q.data.forage$run.num))
#  pb = txtProgressBar(min = min(q.data.forage$run.num), max = n.loops, style=3)
#  start.time = Sys.time()
  
  for (j in unique(q.data.forage$run.num)) {
#    setTxtProgressBar(pb,j)#update progress bar
    
    mod.run = q.data.forage[q.data.forage$run.num==j,] # subset with data from one model run
    first.access = min(grep("0", mod.run$current.succ.foragers, fixed=T)) # index (row number) of the first time step in which the producer ate in the current model run
    
    mod.run.fed = mod.run[first.access:nrow(mod.run),] # subset of mod.run taking only rows from first.access to the end of mod.run (all the time steps after the producer first ate)
    qdf.fed = rbind(qdf.fed, mod.run.fed) # save subset in external dataframe  
  
    }#end of loop
  
#  end.time = Sys.time()
#  run.time = end.time - start.time
#  run.time #ran in 4 minutes for group of 20
  
  write.csv(qdf.fed, "qdf_fed.csv")
  
  
  prox.count.for = qdf.fed %>% 
    pivot_longer(starts_with("prox"), #pivot_longer is the same as melt in the reshape2 package
                 names_to = "prox", values_to = "prox.ID") 


  #nrow(qdf.fed)*(i*(i-1)) == nrow(prox.count.for)#check that prox.count has the correct number of rows

  prox.count.for = tibble::add_column(prox.count.for, prox.key = "NA", .after = "prox")

  rm(q.data.forage,qdf.fed) # remove q.data.forage from environment to save memory

  for(j in 1:20){ #LOOP TO FILL IN THE 'prox' COLUMN
    if(j > i){break} #end the loop if j > i
  
    prox.letter = unique(prox.count.for[startsWith(prox.count.for$prox, prox.labels[j]), ]$prox)
  
    prox.count.for = prox.count.for %>% mutate(prox.key=replace(prox.key, prox %in% prox.letter, LETTERS[j]))
  
  }
#  View(prox.count.for)
#  unique(prox.count.for$prox.key)# check that all NAs were replaced


  # use dplyr functions to get counts after grouping by prox.key and proxIDs
  prox.count.for = prox.count.for %>% 
    group_by(run.num, memory, attention, preference, approach.food, prox.key, prox.ID) %>% 
    summarize(n = n()) 
  #prox.count.for = prox.count.for[complete.cases(prox.count.for$prox.ID),]
  #unique(prox.count.for$prox.ID)
  #range(prox.count.for$n) #max n will not reach (i-1)*100 if rows before producer first forages were removed AND max will be different for each run.num 
  prox.count.for$prox.ID = as.numeric(prox.count.for$prox.ID)
  prox.count.for[prox.count.for$n == max(prox.count.for$n) & is.na(prox.count.for$prox.ID),]$prox.ID = 111 #replace NAs in the rows you want to keep with 111 - I want to keep these to make sure I have data from all run.numbers (I think especially for small group sizes, the agents were never in proximity and I was removing those runs by accident before)
  
  for(j in min(prox.count.for$run.num):max(prox.count.for$run.num)){ #NEED THIS LOOP TO REPLACE NAs IF NO AGENTS WERE IN PROXIMITY TO EACH OTHER DURING THE FORAGING PHASE (AFTER PRODUCER FIRST FORAGED) IN A MODEL RUN
    pcf.loop = prox.count.for[prox.count.for$run.num==j,]
    
    if(nrow(pcf.loop) == i && sum(is.na(pcf.loop$prox.ID)) == i) {
      prox.count.for[prox.count.for$run.num == j,]$prox.ID = 111 #Replace NAs if no agents were in proximity to each other during the foraging phase in this model
    }
    
    prox.count.for[prox.count.for$run.num == j & prox.count.for$n == max(pcf.loop$n) & is.na(prox.count.for$prox.ID),]$prox.ID = 111 #each run number has a different maximum value for 'n' because the producer first foraged at a different time step in each run, so this is to make sure I keep all individual IDs for each run for the next R script
    
  }
  prox.count.for = prox.count.for[complete.cases(prox.count.for$prox.ID),]#then remove unnecessary rows
  #length(unique(prox.count.for$run.num)) #now prox.count.for contains all run.num values

  #View(prox.count.for)
  ### prox.count.for contains the counts of how many time steps each agent was in proximity to each other agent during the FORAGING phase

  #save it as a csv to save space in the R workspace
  write.csv(prox.count.for, "prox_count_for50.csv")
  
  
  


###PROXIMITY network: POST-foraging phase###  
  rm(prox.count.for, pcf.loop)
  gc()
  
  
  prox.count.post = q.data.post %>% 
    pivot_longer(starts_with("prox"), #pivot_longer is the same as melt in the reshape2 package
                 names_to = "prox", values_to = "prox.ID") 
  
  
  #nrow(q.data.post)*(i*(i-1)) == nrow(prox.count.post)#check that prox.count has the correct number of rows
  
  prox.count.post = tibble::add_column(prox.count.post, prox.key = "NA", .after = "prox")
  
  rm(q.data.post) # remove q.data.forage from environment to save memory
  
  for(j in 1:20){ #LOOP TO FILL IN THE 'prox' COLUMN
    if(j > i){break} #end the loop if j > i
    
    prox.letter = unique(prox.count.post[startsWith(prox.count.post$prox, prox.labels[j]), ]$prox)
    
    prox.count.post = prox.count.post %>% mutate(prox.key=replace(prox.key, prox %in% prox.letter, LETTERS[j]))
    
  }
  # View(prox.count.post)
  # unique(prox.count.post$prox.key)# check that all NAs were replaced
  
  
  # use dplyr functions to get counts after grouping by prox.key and proxIDs
  prox.count.post = prox.count.post %>% 
    group_by(run.num, memory, attention, preference, approach.food, prox.key, prox.ID) %>% 
    summarize(n = n()) 
  #prox.count.post = prox.count.post[complete.cases(prox.count.post$prox.ID),]
  #unique(prox.count.post$prox.ID)
  #range(prox.count.post$n)
  prox.count.post$prox.ID = as.numeric(prox.count.post$prox.ID)
  prox.count.post[prox.count.post$n == max(prox.count.post$n) & is.na(prox.count.post$prox.ID),]$prox.ID = 111 #replace NAs in the rows you want to keep with 111 - I want to keep these to make sure I have data from all run.numbers (I think especially for small group sizes, the agents were never in proximity and I was removing those runs by accident before)
  prox.count.post = prox.count.post[complete.cases(prox.count.post$prox.ID),]#then remove unnecessary rows
  #length(unique(prox.count.post$run.num)) #now prox.count.post contains all run.num values
  
  #View(prox.count.post)
  ### prox.count.post contains the counts of how many time steps each agent was in proximity to each other agent during the POST-FORAGING phase
  
  #save it as a csv to save space in the R workspace
  write.csv(prox.count.post, "prox_count_post50.csv")

  rm(prox.count.post)
  
  
  setTxtProgressBar(pb,i)#update progress bar
  
}#end of second big loop

  
end.time = Sys.time()
run.time = end.time - start.time
run.time # ran in 11.5 minutes for 5 runs per combo
#ONLY ENOUGH MEMORY TO GET THROUGH GROUP SIZES 3 AND 6 - TOOK AN HOUR



##### NOW ALL EDGE LISTS CAN BE USED IN THE "quail_centrality_3_analysis50" R SCRIPT