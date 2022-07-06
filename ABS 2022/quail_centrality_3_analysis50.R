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


#FUNCTION TO BE USED INSIDE FIRST BIG LOOP TO GET PRODUCER'S NETWORK METRICS FOR EACH PHASE
metrics.func = function(func.df) {
  prox.count = as.data.frame(func.df)
  for (j in 0:(i-1)) { #Loop to replace numbers in prox.ID column with letters 
    prox.count[prox.count$prox.ID == j,]$prox.ID = LETTERS[j+1]
  }
  #unique(prox.count$prox.ID) #check if the loop worked
  
  
  prox.metrics = data.frame(run.num = unique(prox.count$run.num), A.deg=0, A.str= 0)
  
  
  for (j in unique(prox.count$run.num)) {
    #print(paste(i, j, sep = "-"))
    current.edge.list = prox.count[prox.count$run.num==j, names(prox.count) %in% c("prox.key", "prox.ID", "n")] #subset of prox.summ.pre giving the edge list for the current run.num
    #eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
    
    current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID, value.var = "n") 
    current.matrix = matrix.please(current.matrix)
    current.matrix = current.matrix[,!(colnames(current.matrix) %in% c("111", ""))]
    
    
    if (sum(!(LETTERS[1:i] %in% colnames(current.matrix))) > 0) { #if there is at least one individual missing from current.matrix
      
      missing.indiv = LETTERS[which(!(LETTERS[1:i] %in% colnames(current.matrix)))] # determine which individual(s) is/are missing
      
      for(k in missing.indiv) { #add a column to the matrix for each missing individual
        ind.vec = rep(0, nrow(current.matrix))
        current.matrix = cbind(current.matrix, ind.vec)
        colnames(current.matrix)[colnames(current.matrix) == "ind.vec"] = k
        
        #if (!(nrow(current.matrix) == i)){ # add a row for the missing individual if - I think some runs lost an individual when I removed rows with NAs in the prox.ID column (see setup script - the part where I make the prox_count_pre/for/post files)
        #  ind.vec = rep(0, ncol(current.matrix))
        #  current.matrix = rbind(current.matrix, ind.vec)
        #  rownames(current.matrix)[colnames(current.matrix) == "ind.vec"] = k
        #}
      }
    }
    
    current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=0)
    current.matrix.half[is.na(current.matrix.half)] = 0
    
    eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
    igraph::E(eg)$norm.weight = igraph::E(eg)$weight/nrow(q.data[q.data$run.num == j,]) #make another igraph attribute that stores normalized weights (number of time steps in proximity divided by number of time steps within the phase - THIS IS ONLY NECESSARY IF YOU REMOVE TIME STEPS IN FORAGING PHASE BEFORE THE PRODUCER FIRST FORAGED)
    
    current.degree = igraph::degree(eg)
    current.strength = igraph::strength(eg, weights = igraph::E(eg)$norm.weight) #, weights = current.edge.list$n) #don't need 'weights' argument if igraph object has edge weights attribute already
    
    current.metrics = cbind(current.degree,current.strength)
    
    prox.metrics[prox.metrics$run.num==j, 2:3] = current.metrics[row.names(current.metrics)=="A",] #save producer metrics in external data.frame
    
  }
  prox.metrics.out <<- prox.metrics #need this to save object inside function to external object
}





group.sizes = c(3, 6, 10, 15, 20)

n.loops = max(group.sizes)
pb = txtProgressBar(min=0, max = n.loops, style=3)
start.time = Sys.time()

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
 
  
  prox.count.pre = read_csv("prox_count_pre50.csv", col_types = cols()) #edge list for PRE-foraging period - contains data from all runs
  prox.count.pre = as.data.frame(prox.count.pre[,-1])
  
  prox.count.for = read_csv("prox_count_for50.csv", col_types = cols()) #edge list for foraging period - contains data from all runs
  #prox.count.for = read.csv("prox_count_for50_fed.csv", header=T) #edge list for foraging period, counting only time steps after producer has first fed - contains data from all runs
  prox.count.for = as.data.frame(prox.count.for[,-1])
  
  prox.count.post = read_csv("prox_count_post50.csv", col_types = cols()) #edge list for POST-foraging period - contains data from all runs
  prox.count.post = as.data.frame(prox.count.post[,-1])
  
  
  q.data = read_csv("q_data_pre50.csv", col_types = cols())
  q.data = as.data.frame(q.data)
  prox.metrics.out = data.frame()
  metrics.func(prox.count.pre) # see function defined at beginning of script
  write.csv(prox.metrics.out, "prox_metrics_pre.csv")
  rm(q.data)
  
  #  q.data.for = read.csv("q_data_forage50.csv", header=T)
  q.data = read_csv("qdf_fed.csv", col_types = cols())
  q.data = as.data.frame(q.data)
  prox.metrics.out = data.frame()
  metrics.func(prox.count.for) # see function defined at beginning of script
  write.csv(prox.metrics.out, "prox_metrics_for.csv")
  rm(q.data)
  
  q.data = read_csv("q_data_post50.csv", col_types = cols())
  q.data = as.data.frame(q.data)
  prox.metrics.out = data.frame()
  metrics.func(prox.count.post) # see function defined at beginning of script
  write.csv(prox.metrics.out, "prox_metrics_post.csv")
  rm(q.data)
  
  
  setTxtProgressBar(pb,i)#update progress bar
  
} # END OF BIG LOOP

end.time = Sys.time()
run.time = end.time - start.time
run.time # ran in 4 minutes for 5 runs per combo


rm(pb, prox.count.pre, prox.count.for, prox.count.post, prox.metrics.out)


n.loops = max(group.sizes)
pb = txtProgressBar(min=0, max = n.loops, style=3)
start.time = Sys.time()

for(i in group.sizes){ #calculating differences in proximity network metrics between phases
  
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
 
  
  #to.read = list.files()[10] # 10th file in the working directory should be q_data_split for that group size (make sure files written in this loop are not already in the working directory)
  q.data.split = read_csv("q_data_split.csv", col_types = cols())
  q.data.split = as.data.frame(q.data.split[, -c(1, 14:ncol(q.data.split))]) #exclude proximity columns (goes up to 393 for group size of 20)
  
  prox.metrics.pre = read_csv("prox_metrics_pre.csv", col_types = cols())
  prox.metrics.pre = as.data.frame(prox.metrics.pre)
  
  prox.metrics.for = read_csv("prox_metrics_for.csv", col_types = cols())
  prox.metrics.for = as.data.frame(prox.metrics.for)
  
  prox.metrics.post = read_csv("prox_metrics_post.csv", col_types = cols())
  prox.metrics.post = as.data.frame(prox.metrics.post)

  
  # want to see if mem, att, pref have an effect on producer's centrality using network metrics from pre-forage phase as a baseline
  #####calculate differences between producer's network metrics between phases for each combo of mem/att/pref#####
  uniq.qds = unique(q.data.split[,c("run.num", "group.size", "memory", "attention", "preference", "approach.food", "combo.num")]) #data frame containing unique combinations of model parameters 
  
  
  prox.forXpre = data.frame(run.num = uniq.qds$run.num) # data frame for differences between foraging and pre-foraging phases
  prox.forXpre$forXpre.deg = prox.metrics.for$A.deg - prox.metrics.pre$A.deg # differences in degree
  prox.forXpre$forXpre.str = prox.metrics.for$A.str - prox.metrics.pre$A.str # differences in strength
  
  prox.forXpre = merge(prox.forXpre, uniq.qds, by = "run.num") #adds parameter values to data frame containing differences in producer network metrics
  #nrow(prox.forXpre)
  #View(prox.forXpre)
  
  write.csv(prox.forXpre, "prox_forxpre.csv")
  
  
  prox.postXfor = data.frame(run.num = uniq.qds$run.num) # data frame for differences between post-foraging and foraging phases
  prox.postXfor$postXfor.deg = prox.metrics.post$A.deg - prox.metrics.for$A.deg # differences in degree
  prox.postXfor$postXfor.str = prox.metrics.post$A.str - prox.metrics.for$A.str # differences in strength
  
  prox.postXfor = merge(prox.postXfor, uniq.qds, by = "run.num") #adds parameter values to data frame with differences in producer network metrics
  
  write.csv(prox.postXfor, "prox_postxfor.csv")
  
  
  prox.postXpre = data.frame(run.num = uniq.qds$run.num) # data frame for differences between post-foraging and pre-foraging phases
  prox.postXpre$postXpre.deg = prox.metrics.post$A.deg - prox.metrics.pre$A.deg # differences in degree
  prox.postXpre$postXpre.str = prox.metrics.post$A.str - prox.metrics.pre$A.str # differences in strength
  
  prox.postXpre = merge(prox.postXpre, uniq.qds, by = "run.num") #adds parameter values to data frame with differences in producer network metrics
  
  write.csv(prox.postXpre, "prox_postxpre.csv")
  
  setTxtProgressBar(pb,i)#update progress bar
}

end.time = Sys.time()
run.time = end.time - start.time
run.time #36 seconds for 5 runs per combo

