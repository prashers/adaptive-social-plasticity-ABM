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
View(prox.metrics.pre) #check #Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)



#check distribution of network metric differences to see how to proceed with summarizing for each variable combination
# colorbrewer package, spectral color gradient for the heat maps