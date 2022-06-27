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

group.sizes = c(3, 6, 10, 15, 20)

for(i in group.sizes){ #Proximity network
  
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
 
  
#  q.data.pre = read.csv("q_data_pre50.csv", header=T)
#  q.data.for = read.csv("q_data_forage50.csv", header=T)
#  #qdf.fed = read.csv("qdf_fed.csv", header=T)
#  q.data.post = read.csv("q_data_post50.csv", header=T)
  
  prox.count.pre = read.csv("prox_count_pre50.csv", header=T) #edge list for PRE-foraging period - contains data from all runs
  prox.count.pre = prox.count.pre[,-1]
  
  prox.count.for = read.csv("prox_count_for50.csv", header=T) #edge list for foraging period - contains data from all runs
  #prox.count.for = read.csv("prox_count_for50_fed.csv", header=T) #edge list for foraging period, counting only time steps after producer has first fed - contains data from all runs
  prox.count.for = prox.count.for[,-1]
  
  prox.count.post = read.csv("prox_count_post50.csv", header=T) #edge list for POST-foraging period - contains data from all runs
  prox.count.post = prox.count.post[,-1]
  
  
  ###PROXIMITY network: PRE-foraging phase###
  
  prox.count.pre = as.data.frame(prox.count.pre)
  for (j in 0:(i-1)) { #Loop to replace numbers in prox.ID column with letters 
    prox.count.pre[prox.count.pre$prox.ID == j,]$prox.ID = LETTERS[j+1]
  }
  unique(prox.count.pre$prox.ID) #check if the loop worked
  
  
  prox.metrics.pre = data.frame(run.num = unique(prox.count.pre$run.num), A.deg=0, A.str= 0)
  
  
  for (j in unique(prox.count.pre$run.num)) {
    current.edge.list = prox.count.pre[prox.count.pre$run.num==j, names(prox.count.pre) %in% c("prox.key", "prox.ID", "n")] #subset of prox.summ.pre giving the edge list for the current run.num
    #eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
    
    current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
    current.matrix = matrix.please(current.matrix)
    current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=0)
    current.matrix.half[is.na(current.matrix.half)] = 0
    
    if (sum(!(LETTERS[1:i] %in% rownames(current.matrix.half))) > 0) { #if there is at least one individual missing from current.matrix.half
      
      missing.indiv = LETTERS[which(!(LETTERS[1:i] %in% rownames(current.matrix.half)))] # determine which individual(s) is/are missing
      
      for(k in missing.indiv) { #add a row and column to the matrix for each missing individual
        ind.vec = rep(0, nrow(current.matrix.half))
        current.matrix.half = cbind(current.matrix.half, ind.vec)
        colnames(current.matrix.half)[colnames(current.matrix.half) == "ind.vec"] = k
        
        ind.vec = rep(0, ncol(current.matrix.half))
        current.matrix.half = rbind(current.matrix.half, ind.vec)
        rownames(current.matrix.half)[rownames(current.matrix.half) == "ind.vec"] = k
      }
    }
    
    eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
    igraph::E(eg)$norm.weight = igraph::E(eg)$weight/300 #make another igraph attribute that stores normalized weights (number of time steps in proximity divided by number of time steps within the phase)
    
    current.degree = igraph::degree(eg)
    current.strength = igraph::strength(eg, weights = igraph::E(eg)$norm.weight) #, weights = current.edge.list$n) #don't need 'weights' argument if igraph object has edge weights attribute already
    
    current.metrics = cbind(current.degree,current.strength)
    
    prox.metrics.pre[prox.metrics.pre$run.num==j, 2:3] = current.metrics[row.names(current.metrics)=="A",] #save producer metrics in external data.frame
    
  }
  
  View(prox.metrics.pre) #check 
  length(prox.metrics.pre$run.num) #should have all run.numbers present 
  summary(prox.metrics.pre$A.deg)#Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)
  summary(prox.metrics.pre$A.str)
   
}