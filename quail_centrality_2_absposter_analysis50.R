# make loops in which you subset the big edge lists by run number, 
# then use igraph to calculate degree and strength and save in an external data frame
library(tidyr)
library(dplyr)

# Function to convert to matrix format using matrix.please function
matrix.please <- function(x) {
        m<-as.matrix(x[,-1])
        rownames(m)<-x[,1]
        m
}


######PROXIMITY######
prox.summ.pre = read.csv("prox_summ_pre50.csv", header=T) #edge list for PRE-foraging period - contains data from all runs
prox.summ.pre = prox.summ.pre[,-1]

prox.summ.for = read.csv("prox_summ_for50.csv", header=T) #edge list for foraging period - contains data from all runs
prox.summ.for = prox.summ.for[,-1]

prox.summ.post = read.csv("prox_summ_post50.csv", header=T) #edge list for POST-foraging period - contains data from all runs
prox.summ.post = prox.summ.post[,-1]


###PROXIMITY network: PRE-foraging phase###

prox.summ.pre.letters = as.data.frame(ungroup(prox.summ.pre))
for (i in 0:5) { #Loop to replace numbers in prox.ID column with letters 
  j = c("A","B","C","D","E","F")
  prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == i,]$prox.ID = j[i+1]
}
unique(prox.summ.pre.letters$prox.ID) #check if the loop worked



prox.metrics.pre = data.frame(run.num = unique(prox.summ.pre.letters$run.num), A.deg=0, A.str= 0)

timestamp()
for (i in unique(prox.summ.pre.letters$run.num)) {
  current.edge.list = prox.summ.pre.letters[prox.summ.pre.letters$run.num==i, 5:7] #subset of prox.summ.pre giving the edge list for the current run.num
          #eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
  
  current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
  current.matrix = matrix.please(current.matrix)
  current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=0)
  current.matrix.half[is.na(current.matrix.half)] = 0
  eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
  #igraph::E(eg)$weight #weights exist in the igraph object
  
  current.degree = igraph::degree(eg)
  current.strength = igraph::strength(eg) #, weights = current.edge.list$n) #don't need 'weights' argument if igraph object has edge weights attribute already
  
  current.metrics = cbind(current.degree,current.strength)
  
  if ("A" %in% rownames(current.metrics)) {
    prox.metrics.pre[prox.metrics.pre$run.num==i, 2:3] = current.metrics[row.names(current.metrics)=="A",] #save producer metrics in external data.frame
  }
  
}
timestamp()
View(prox.metrics.pre) #check 
summary(prox.metrics.pre$A.deg)#Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)





###PROXIMITY network: foraging phase###

prox.summ.for.letters = as.data.frame(ungroup(prox.summ.for))
for (i in 0:5) { #Loop to replace numbers in prox.ID column with letters 
  j = c("A","B","C","D","E","F")
  prox.summ.for.letters[prox.summ.for.letters$prox.ID == i,]$prox.ID = j[i+1]
}
unique(prox.summ.for.letters$prox.ID) #check if the loop worked



prox.metrics.for = data.frame(run.num = unique(prox.summ.for.letters$run.num), A.deg=0, A.str= 0)

for (i in unique(prox.summ.for.letters$run.num)) {
  current.edge.list = prox.summ.for.letters[prox.summ.for.letters$run.num==i, 5:7] #subset of prox.summ.for giving the edge list for the current run.num
  #eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
  
  current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
  current.matrix = matrix.please(current.matrix)
  current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=0)
  current.matrix.half[is.na(current.matrix.half)] = 0
  eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
  #igraph::E(eg)$weight #weights exist in the igraph object
  
  current.degree = igraph::degree(eg)
  current.strength = igraph::strength(eg) #, weights = current.edge.list$n) #don't need 'weights' argument if igraph object has edge weights attribute already
  
  current.metrics = cbind(current.degree,current.strength)
  
  if ("A" %in% rownames(current.metrics)) {
    prox.metrics.for[prox.metrics.for$run.num==i, 2:3] = current.metrics[row.names(current.metrics)=="A",] #save producer metrics in external data.frame
  }
  
}

View(prox.metrics.for) #check 
summary(prox.metrics.for$A.deg)#Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)





###PROXIMITY network: POST-foraging phase###

prox.summ.post.letters = as.data.frame(prox.summ.post)
for (i in 0:5) { #Loop to replace numbers in prox.ID column with letters 
  j = c("A","B","C","D","E","F")
  prox.summ.post.letters[prox.summ.post.letters$prox.ID == i,]$prox.ID = j[i+1]
}
unique(prox.summ.post.letters$prox.ID) #check if the loop worked



prox.metrics.post = data.frame(run.num = unique(prox.summ.post.letters$run.num), A.deg=0, A.str= 0)

for (i in unique(prox.summ.post.letters$run.num)) {
  current.edge.list = prox.summ.post.letters[prox.summ.post.letters$run.num==i, 5:7] #subset of prox.summ.pre giving the edge list for the current run.num
  #eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
  
  current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
  current.matrix = matrix.please(current.matrix)
  current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=0)
  current.matrix.half[is.na(current.matrix.half)] = 0
  eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
  #igraph::E(eg)$weight #weights exist in the igraph object
  
  current.degree = igraph::degree(eg)
  current.strength = igraph::strength(eg) #, weights = current.edge.list$n) #don't need 'weights' argument if igraph object has edge weights attribute already
  
  current.metrics = cbind(current.degree,current.strength)
  
  if ("A" %in% rownames(current.metrics)) {
    prox.metrics.post[prox.metrics.post$run.num==i, 2:3] = current.metrics[row.names(current.metrics)=="A",] #save producer metrics in external data.frame
  }
  
}
View(prox.metrics.post) #check 
summary(prox.metrics.post$A.deg) #Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)





# want to see if mem, att, pref have an effect on producer's centrality using network metrics from pre-forage phase as a baseline
#####calculate differences between producer's network metrics between phases for each combo of mem/att/pref#####
uniq.qdo = unique(q.data.ord[,c(2:5, 15)])


prox.forXpre = data.frame(run.num = seq(1:nrow(prox.metrics.pre))) # data frame for differences between foraging and pre-foraging phases
prox.forXpre$forXpre.deg = prox.metrics.for$A.deg - prox.metrics.pre$A.deg # differences in degree
prox.forXpre$forXpre.str = prox.metrics.for$A.str - prox.metrics.pre$A.str # differences in strength

prox.forXpre = merge(prox.forXpre, uniq.qdo, by = "run.num") #adds mem, att, pref values to data frame with differences in producer network metrics



prox.postXpre = data.frame(run.num = seq(1:nrow(prox.metrics.pre))) # data frame for differences between post-foraging and pre-foraging phases
prox.postXpre$postXpre.deg = prox.metrics.post$A.deg - prox.metrics.pre$A.deg # differences in degree
prox.postXpre$postXpre.str = prox.metrics.post$A.str - prox.metrics.pre$A.str # differences in strength

prox.postXpre = merge(prox.postXpre, uniq.qdo, by = "run.num") #adds mem, att, pref values to data frame with differences in producer network metrics




#check distribution of network metric differences for each variable combination to see how to proceed with summarizing the data
hist(prox.forXpre[prox.forXpre$combo.num == 7,]$forXpre.deg)
hist(prox.forXpre[prox.forXpre$combo.num == 7,]$forXpre.str)#, breaks = seq(0, 500, 20))

hist(prox.postXpre[prox.postXpre$combo.num == 5,]$postXpre.deg)
hist(prox.postXpre[prox.postXpre$combo.num == 5,]$postXpre.str)#, breaks = seq(0, 500, 20))








# colorbrewer package, spectral color gradient for the heat maps



# For proximity, the question is:
# Does foraging success increase the producer's centrality from a baseline value (when movement is random)?



















######FOLLOWING######
foll.summ.for = read.csv("foll_summ_forage50.csv", header=T) #edge list for foraging period - contains data from all runs
foll.summ.for = foll.summ.for[,-1]

foll.summ.post = read.csv("foll_summ_post50.csv", header=T) #edge list for POST-foraging period - contains data from all runs
foll.summ.post = foll.summ.post[,-1]


###FOLLOWING network: POST-foraging phase###

foll.summ.post.letters = as.data.frame(foll.summ.post)
for (i in 1:5) { #Loop to replace numbers in foll.ID column with letters - doing i in 1:5, because producer didn't follow anyone in the POST-foraging phase (no zeros in the foll.ID column)
  j = c("A","B","C","D","E","F")
  foll.summ.post.letters[foll.summ.post.letters$foll.ID == i,]$foll.ID = j[i+1]
}
unique(foll.summ.post.letters$foll.ID) #check if the loop worked



foll.metrics.post = data.frame(run.num = unique(foll.summ.post.letters$run.num), A.deg=0, A.str= 0)

for (i in unique(foll.summ.post.letters$run.num)) {
  current.edge.list = foll.summ.post.letters[foll.summ.post.letters$run.num==i, 5:7] #subset of foll.summ.pre giving the edge list for the current run.num
  #eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
  
  current.matrix = current.edge.list %>% reshape2::dcast(foll.key ~ foll.ID) 
  current.matrix = matrix.please(current.matrix)
  current.matrix[is.na(current.matrix)] = 0
  eg = igraph::graph_from_incidence_matrix(current.matrix, directed=TRUE, mode="in", weighted=TRUE)#, weighted = TRUE)
  #igraph::E(eg)$weight #weights exist in the igraph object
  
  current.degree = igraph::degree(eg, mode = "in")
  current.degree = current.degree[current.degree != 0]
  
  current.strength = igraph::strength(eg, mode = "in") #, weights = current.edge.list$n) #don't need 'weights' argument if igraph object has edge weights attribute already
  current.strength = current.strength[current.strength != 0]
  
  current.metrics = cbind(current.degree,current.strength)
  
  if ("A" %in% rownames(current.metrics)) {
    foll.metrics.post[foll.metrics.post$run.num==i, 2:3] = current.metrics[row.names(current.metrics)=="A",] #save producer metrics in external data.frame
  }
  
}

View(foll.metrics.post) #check 
summary(foll.metrics.post$A.deg)#Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)















foll.postxfor = data.frame(run.num = seq(1:nrow(foll.metrics.for))) # data frame for differences between post-foraging and pre-foraging phases
foll.postXfor$postXfor.deg = foll.metrics.post$A.deg - foll.metrics.for$A.deg # differences in degree
foll.postXfor$postXfor.str = foll.metrics.post$A.str - foll.metrics.for$A.str # differences in strength

foll.postXfor = merge(foll.postXfor, uniq.qdo, by = "run.num") #adds mem, att, pref values to data frame with differences in producer network metrics





#for following network metrics, either just look for effects of the three variables on following of producer in the post-foraging phase
# AND/OR compare how variables influence difference in following in post-foraging vs. foraging phases
# WHAT IS YOUR QUESTION, DO WHICHEVER OPTION ANSWERS YOUR QUESTION
  # How do variables influence following of producer when food is no longer available?
  # what combination of variables allows producers to experience sustained following once food is gone?
  # Is there a threshold of variable values over which following of producer is stable?
  # Which variable plays the biggest role in increasing the producer's following centrality?
