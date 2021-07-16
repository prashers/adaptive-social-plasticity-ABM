q.data.full = read.csv("quail_centrality_2 ABSPoster100RunsPerCombo-table.csv", skip = 6, header = T)
nrow(q.data.full)
str(q.data.full)
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
q.data = q.data.full[, -(5:12)]
names(q.data)
names(q.data) = c("run.num", "memory", "attention", "preference", "ticks", "affil.IDs", "prox.centrality.list", "proxim.IDs", "memory.succ.foragers", "fss.list", "foll.centrality.list", "foll.IDs", "coor.list")
head(q.data)


q.data.ord = q.data[order(q.data$run.num, q.data$ticks),] #reorder data by run number and time step within run number
order = 1:nrow(q.data.ord)
q.data.ord = cbind(order, q.data.ord)
head(q.data.ord)


nrow(unique(q.data.ord[,3:5]))
# I want a column that tells me the combo number (1:125)