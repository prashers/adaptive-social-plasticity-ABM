### FORMAT QUAIL_CENTRALITY_2 MODEL DATA ###
#This script is for the data from one run of each parameter combo

q.data.full = read.csv("quail_centrality_2 prelim analysis-table.csv", skip = 6, header = T) 
head(q.data.full)
tail(q.data.full)
nrow(q.data.full)
min(q.data.full$ticks)
max(q.data.full$ticks)

names(q.data.full)
#Column info:
# "X.run.number." is the model run ID - in this case, there is a unique number for each parameter combination 
# "memory" is one of the three parameters that can vary between run IDs. 
        #It sets the maximum number of time steps that a successful forager can be remembered
#"attention" is one of the three parameters that can vary between run IDs.
        #It sets the probability that foragers will check their memory for a successful forager
#"preference" is one of the three parameters that can vary between run IDs.
        #It sets the probability that foragers will follow the first successful forager in their memory
#"grouping", slider value controlling the probability with which agents move toward the average coordinates of the group - kept constant for this behaviorspace experiment
#"prior.affils.", switch controlling whether foragers create affiliation links with each other, but not the producer, at beginning of model run - kept OFF for this behaviorspace experiment
#"unfam.prod.", switch controlling whether affiliation links are made between all foragers and are thus used as familiarity links (only the producer is unfamiliar and is less preferred by potential followers)  - kept OFF for this behaviorspace experiment
#"cluster.", switch controlling whether agents move toward the average group coordinates of the group - kept OFF for this behaviorspace experiment
#"eat.delay.", switch controlling whether producer or foragers take their actions first. If ON, the foragers act first and therefore cannot eat food in the same time step as it is accessed by the producer - kept ON for this behaviorspace experiment
#"alt.food.", switch controlling whether there is an alternative food source in the arena that is always accessible by any agent - kept OFF for this behaviorspace experiment
#"reset.food.", switch controlling whether food patch renews itself (and if yellow at the start becomes yellow again) after resource-level of food patches has decreased below a threshold - kept ON for this behaviorspace experiment
#"X.step.", time step within current model run - output from behaviorspace experiment
#"ticks", time step within current model run - as appears in netlogo
#"affil.IDs", list of WHO numbers of each forager's affiliates (outgoing affil-link neighbors) - all zeros in this case, because prior-affils switch was kept off
#"prox.cent.list", list containing each agent's centrality in the proximity network (number of other agents within a certain distance of the individual)
#"proxim.IDs", list of WHO numbers of the agents within proximity of each individual
#"memory.succ.foragers", the memory list, which contains the IDs of foragers that have successfully eaten in the past <memory> time steps - Just for reference. Not used in model behavior 
#"fss.list", list containing first-sf-seen value of each agent
#"foll.cent.list", list containing each agent's centrality in the following network (number of other agents following the individual - count of incoming follow links)
#"foll.IDs", list containing WHO numbers of the agents following each individual
#"current.xycor" list of the xy coordinates for each agent 


# 'grouping' 'prior.affils.' 'unfam.prod.' 'cluster.' 'eat.delay.' 'alt.food.' and 'reset.food.' were the sliders/switches that were all kept contant, so they can be removed 
# 'X.step.' column is the same as 'ticks' column

# remove unnecessary columns:
q.data = q.data.full[,-(5:12)] #remove columns 5 through 12

# rename columns:
names(q.data)
names(q.data) = c("run.num", "memory", "attention", "preference", "ticks", "affil.IDs", "prox.centrality.list", "proxim.IDs", "memory.succ.foragers", "fss.list", "foll.centrality.list", "foll.IDs", "coor.list")
head(q.data)
tail(q.data)

q.data.ord = q.data[order(q.data$run.num, q.data$ticks),] #reorder data by run number and time step within run number
order = 1:nrow(q.data.ord)
q.data.ord = cbind(order, q.data.ord)
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

# separate coordinates into different columns (two columns for each agent):
library(stringr)
split.coordinates = str_split_fixed(q.data.ord$coor.list, "] ", 6) # split the data in the coor.list column at each "] " into 6 separate columns
head(split.coordinates)
nrow(split.coordinates)
split.coordinates[,1:6] = gsub("[[]", "", split.coordinates[,1:6]) # remove the square brackets from the split data
split.coordinates[,1:6] = gsub("[]]", "", split.coordinates[,1:6])
head(split.coordinates)


split.cor = data.frame(1:37625) # need same number of rows as nrow(split.coordinates)
for(i in 1:ncol(split.coordinates)){ # loop to separate each foragers coordinates into two columns
        x = str_split_fixed(split.coordinates[,i], " ", 2)
        split.cor = cbind(split.cor, x)
}
head(split.cor)
names(split.cor) = c("X", "xcorA", "ycorA", "xcorB", "ycorB", "xcorC", "ycorC", "xcorD", "ycorD", "xcorE", "ycorE", "xcorF", "ycorF")

split.cor = split.cor[,-1]

### split.cor is ready to be added to the big dataframe


#separate proxim.IDs into different columns
# first, a separate column for each agent's list, 
# then further split into separate columns per agent in proximity proxA1:proxA5 (up to 5 agents in proximity)
split.proximIDs = str_split_fixed(q.data.ord$proxim.IDs, "] ", 6) # split the data in the proxim.IDs column at each "] " into 6 separate columns
head(split.proximIDs)
split.proximIDs[,1:6] = gsub("[[]", "", split.proximIDs[,1:6]) # remove the square brackets from the split data
split.proximIDs[,1:6] = gsub("[]]", "", split.proximIDs[,1:6])
head(split.proximIDs)

nrow(split.proximIDs)

split.prox = data.frame(1:37625) # need same number of rows as nrow(split.proximIDs)
for(i in 1:ncol(split.proximIDs)){ # loop to separate each foragers coordinates into two columns
        x = str_split_fixed(split.proximIDs[,i], " ", 5)
        split.prox = cbind(split.prox, x)
}
head(split.prox)
tail(split.prox)
names(split.prox) = c("X", "proxA1", "proxA2", "proxA3", "proxA4", "proxA5", 
                      "proxB1", "proxB2", "proxB3", "proxB4", "proxB5",
                      "proxC1", "proxC2", "proxC3", "proxC4", "proxC5",
                      "proxD1", "proxD2", "proxD3", "proxD4", "proxD5",
                      "proxE1", "proxE2", "proxE3", "proxE4", "proxE5",
                      "proxF1", "proxF2", "proxF3", "proxF4", "proxF5")
unique(split.prox$proxB3) # split.prox has some blanks if there were not 5 values for proximIDs in an agent's split.proximIDs column 
split.prox[split.prox==""] = NA #replace all blank cells with NA
split.prox[split.prox=="NA"] = NA #replace all characters "NA" with missing values

split.prox = split.prox[,-1] #remove first column

View(data.frame(q.data.ord$proxim.IDs, split.prox))# compare split.prox to proxim.IDs column in q.data.ord

### split.prox is ready to be added to the big data frame



#separate foll.IDs into different columns
# first, a separate column for each agent's list, 
# then further split into separate columns per agent following follA1:follA5 (up to 5 agents following agent A)
split.follIDs = str_split_fixed(q.data.ord$foll.IDs, "] ", 6) # split the data in the proxim.IDs column at each "] " into 6 separate columns
head(split.follIDs)
split.follIDs[,1:6] = gsub("[[]", "", split.follIDs[,1:6]) # remove the square brackets from the split data
split.follIDs[,1:6] = gsub("[]]", "", split.follIDs[,1:6])
head(split.follIDs)

nrow(split.proximIDs)


split.foll = data.frame(1:37625) # need same number of rows as nrow(split.proximIDs)
for(i in 1:ncol(split.follIDs)){ # loop to separate each foragers coordinates into two columns
        x = str_split_fixed(split.follIDs[,i], " ", 5)
        split.foll = cbind(split.foll, x)
}
head(split.foll)
tail(split.foll)
names(split.foll) = c("X", "follA1", "follA2", "follA3", "follA4", "follA5", 
                      "follB1", "follB2", "follB3", "follB4", "follB5",
                      "follC1", "follC2", "follC3", "follC4", "follC5",
                      "follD1", "follD2", "follD3", "follD4", "follD5",
                      "follE1", "follE2", "follE3", "follE4", "follE5",
                      "follF1", "follF2", "follF3", "follF4", "follF5")

unique(split.foll$follB2) # split.foll has some blanks if there were not 5 values for follIDs in an agent's split.follIDs column 
split.foll[split.foll==""] = NA #replace all blank cells with NA
split.foll[split.foll=="NA"] = NA #replace all characters "NA" with missing values

split.foll = split.foll[,-1] #remove first column

View(data.frame(q.data.ord$foll.IDs, split.foll))# compare split.foll to foll.IDs column in q.data.ord

### split.foll is ready to be added to the big data frame




##### add all separated columns to the big dataframe 
q.data.split = cbind(q.data.ord[, -c(7,9,13,14)], split.prox, split.foll, split.cor) #add the new columns into the dataframe with affil.IDs, proxim.IDs, foll.IDs, and coor.list columns removed
head(q.data.split)
tail(q.data.split)

#need to change a couple column names to make it easier to use the starts_with argument later
names(q.data.split)
colnames(q.data.split)[7] = "pr.centrality.list" #rename prox.centrality.list column
colnames(q.data.split)[10] = "fo.centrality.list" #rename foll.centrality.list column


# divide dataframe into three -- one for each phase
q.data.start.pre = q.data.split[q.data.split$phase %in% c("start", "pre-forage"),]
q.data.pre = q.data.split[q.data.split$phase == "pre-forage",]
q.data.forage = q.data.split[q.data.split$phase == "forage",]
q.data.post = q.data.split[q.data.split$phase == "post-forage",]


# NOW FIGURE OUT HOW TO MAKE AN ADJACENCY MATRIX FOR PROXIMITY AND FOLLOWING
# MATRIX WILL CONTAIN COUNTS OF HOW MANY TIME STEPS EACH DYAD WAS IN PROXIMITY (FOR PROXIMITY NETWORK) OR IN HOW MANY TIME STEPS EACH AGENT WAS FOLLOWED BY EACH OTHER AGENT (DIRECTED FOLLOWING NETWORK)
# WILL HAVE A MATRIX FOR EACH PHASE WITHIN EACH MODEL RUN

library(tidyr)
library(dplyr)

###proximity network: pre-foraging phase###
prox.count.pre = q.data.pre %>% 
        pivot_longer(starts_with("prox"), #pivot_longer is the same as melt in the reshape2 package
        names_to = "prox", values_to = "prox.ID") 


nrow(q.data.pre)*30 == nrow(prox.count.pre)#prox.count has the correct number of rows

#add column with values you can group by
prox.count.pre = tibble::add_column(prox.count.pre, prox.key = "NA", .after = "prox")
prox.count.pre[with(prox.count.pre, grepl("proxA", prox)),]$prox.key = "A"
prox.count.pre[with(prox.count.pre, grepl("proxB", prox)),]$prox.key = "B"
prox.count.pre[with(prox.count.pre, grepl("proxC", prox)),]$prox.key = "C"
prox.count.pre[with(prox.count.pre, grepl("proxD", prox)),]$prox.key = "D"
prox.count.pre[with(prox.count.pre, grepl("proxE", prox)),]$prox.key = "E"
prox.count.pre[with(prox.count.pre, grepl("proxF", prox)),]$prox.key = "F"
unique(prox.count.pre$prox.key)# check that all NAs were replaced

# use dplyr functions to get counts after grouping by prox.key and proxIDs
prox.summ.pre = prox.count.pre %>% 
        group_by(run.num, memory, attention, preference, prox.key, prox.ID) %>% 
        summarize(n = n()) 
prox.summ.pre = prox.summ.pre[complete.cases(prox.summ.pre$prox.ID),]
unique(prox.summ.pre$prox.ID)

View(prox.summ.pre)
### prox.summ.pre contains the counts of how many time steps each agent was in proximity to each other agent
### SINCE I USED q.data.pre, THIS DOES NOT INCLUDE THE STATE OF THE MODEL AT TICK ZERO


###proximity network: foraging phase###
prox.count.for = q.data.forage %>% 
        pivot_longer(starts_with("prox"), #pivot_longer is the same as melt in the reshape2 package
                     names_to = "prox", values_to = "prox.ID") 

nrow(q.data.forage)*30 == nrow(prox.count.for)#if TRUE prox.count.for has the correct number of rows

prox.count.for = tibble::add_column(prox.count.for, prox.key = "NA", .after = "prox")
prox.count.for[with(prox.count.for, grepl("proxA", prox)),]$prox.key = "A"
prox.count.for[with(prox.count.for, grepl("proxB", prox)),]$prox.key = "B"
prox.count.for[with(prox.count.for, grepl("proxC", prox)),]$prox.key = "C"
prox.count.for[with(prox.count.for, grepl("proxD", prox)),]$prox.key = "D"
prox.count.for[with(prox.count.for, grepl("proxE", prox)),]$prox.key = "E"
prox.count.for[with(prox.count.for, grepl("proxF", prox)),]$prox.key = "F"
unique(prox.count.for$prox.key)# check that all NAs were replaced

# use dplyr functions to get counts after grouping by prox.key and proxIDs
prox.summ.for = prox.count.for %>% 
        group_by(run.num, memory, attention, preference, prox.key, prox.ID) %>% 
        summarize(n = n()) 

prox.summ.for = prox.summ.for[complete.cases(prox.summ.for$prox.ID),]
unique(prox.summ.for$prox.ID)

View(prox.summ.for)



###proximity network: post-foraging phase###
prox.count.post = q.data.post %>% 
        pivot_longer(starts_with("prox"), #pivot_longer is the same as melt in the reshape2 package
                     names_to = "prox", values_to = "prox.ID") 

nrow(q.data.post)*30 == nrow(prox.count.post)#if TRUE prox.count.for has the correct number of rows

prox.count.post = tibble::add_column(prox.count.post, prox.key = "NA", .after = "prox")
prox.count.post[with(prox.count.post, grepl("proxA", prox)),]$prox.key = "A"
prox.count.post[with(prox.count.post, grepl("proxB", prox)),]$prox.key = "B"
prox.count.post[with(prox.count.post, grepl("proxC", prox)),]$prox.key = "C"
prox.count.post[with(prox.count.post, grepl("proxD", prox)),]$prox.key = "D"
prox.count.post[with(prox.count.post, grepl("proxE", prox)),]$prox.key = "E"
prox.count.post[with(prox.count.post, grepl("proxF", prox)),]$prox.key = "F"
unique(prox.count.post$prox.key)# check that all NAs were replaced

# use dplyr functions to get counts after grouping by prox.key and proxIDs
prox.summ.post = prox.count.post %>% 
        group_by(run.num, memory, attention, preference, prox.key, prox.ID) %>% 
        summarize(n = n()) 

prox.summ.post = prox.summ.post[complete.cases(prox.summ.post$prox.ID),]
unique(prox.summ.post$prox.ID)

View(prox.summ.post)


##### NOW EDGE LISTS CAN BE USED IN THE "quail_centrality_2 prelim analysis (1 run per combo)" R SCRIPT

########################
########################

##### EVERYTHING BELOW IS NOT NECESSARY (IT WAS USEFUL FOR GETTING AN OVERVIEW OF COUNTS)


##### use dcast from reshape2 to convert edge list to matrix #####
# Function to convert to matrix format using matrix.please function
#matrix.please <- function(x) {
#        m<-as.matrix(x[,-1])
#        rownames(m)<-x[,1]
#        m
#}

###proximity network: pre-foraging phase###
prox.summ.pre.sub = prox.summ.pre[prox.summ.pre$run.num==5,]

prox.pre.matrix = prox.summ.pre.sub %>% reshape2::dcast(prox.key ~ prox.ID) 
prox.pre.matrix = matrix.please(prox.pre.matrix)
prox.pre.matrix


###proximity network: foraging phase###
#prox.summ.for.sub = prox.summ.for[prox.summ.for$run.num==1,]
#prox.for.matrix = prox.summ.for.sub %>% reshape2::dcast(prox.key ~ prox.ID) 
#prox.for.matrix = matrix.please(prox.for.matrix)
#prox.for.matrix


###proximity network: post-foraging phase###
#prox.summ.post.sub = prox.summ.post[prox.summ.post$run.num==1,]
#prox.post.matrix = prox.summ.post.sub %>% reshape2::dcast(prox.key ~ prox.ID) 
#prox.post.matrix = matrix.please(prox.post.matrix)
#prox.post.matrix