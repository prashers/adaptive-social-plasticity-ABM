# make loops in which you subset the big edge lists by run number, 
# then use igraph to calculate degree and strength and save in an external data frame

prox.summ.pre #edge list for pre-foraging period - contains data from all runs
prox.summ.for #edge list for foraging period - contains data from all runs
prox.summ.post #edge list for post-foraging period - contains data from all runs


###proximity network: pre-foraging phase###
prox.summ.pre.letters = as.data.frame(ungroup(prox.summ.pre))
prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == 0,]$prox.ID = "A"
prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == 1,]$prox.ID = "B"
prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == 2,]$prox.ID = "C"
prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == 3,]$prox.ID = "D"
prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == 4,]$prox.ID = "E"
prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == 5,]$prox.ID = "F"



prox.metrics.pre = data.frame(run.num = seq(1:125), A.deg=NA, A.str= NA)
for (i in unique(prox.summ.pre$run.num)) {
        current.edge.list = prox.summ.pre.letters[prox.summ.pre.letters$run.num==i, 5:7] #subset of prox.summ.pre giving the edge list for the current run.num
#        eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
        
        current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
        current.matrix = matrix.please(current.matrix)
        current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=NA)
        eg = igraph::graph_from_adjacency_matrix(current.matrix, mode="upper", weighted = TRUE, diag = FALSE)
        #igraph::E(eg)$weight #weights exist in the igraph object
        
        
        current.degree = igraph::degree(eg)
        current.strength = igraph::strength(eg) #this is returning same values as degree...
        
        current.metrics = cbind(current.degree,current.strength)
        prox.metrics.pre[prox.metrics.pre$run.num==i, 2:3] = current.metrics[1,] #save producer metrics in external data.frame
}
prox.metrics.pre #check


uniq.q.data.pre = unique(q.data.pre[,2:5])
pre.prox = merge(prox.metrics.pre, uniq.q.data.pre, by = "run.num") #adds mem, att, pref values to data frame with network metrics




###proximity network: foraging phase###
prox.summ.for.letters = as.data.frame(ungroup(prox.summ.for))
for (i in 0:5) { #Loop to replace numbers in prox.ID column with letters 
        j = c("A","B","C","D","E","F")
        prox.summ.for.letters[prox.summ.for.letters$prox.ID == i,]$prox.ID = j[i+1]
}
unique(prox.summ.for.letters$prox.ID) #check if the loop worked


prox.metrics.for = data.frame(run.num = seq(1:125), A.deg=NA, A.str= NA)
for (i in unique(prox.summ.for$run.num)) {
        current.edge.list = prox.summ.for.letters[prox.summ.for.letters$run.num==i, 5:7] #subset of prox.summ.for giving the edge list for the current run.num
#        eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
        
        current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
        current.matrix = matrix.please(current.matrix)
        current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=NA)
        eg = igraph::graph_from_adjacency_matrix(current.matrix, mode="upper", weighted = TRUE, diag = FALSE)
       #igraph::E(eg)$weight #weights exist in the igraph object
        
        current.degree = igraph::degree(eg)
        current.strength = igraph::strength(eg) 
        
        current.metrics = cbind(current.degree,current.strength)
        prox.metrics.for[prox.metrics.for$run.num==i, 2:3] = current.metrics[1,] #save producer metrics in external data.frame
}
prox.metrics.for #check

uniq.q.data.for = unique(q.data.forage[,2:5])
for.prox = merge(prox.metrics.for, uniq.q.data.for, by = "run.num") #adds mem, att, pref values to data frame with network metrics



###proximity network: post-foraging phase###




# want to see if mem, att, pref have an effect on producer's centrality using network metrics from pre-forage phase as a reference point/baseline
# calculate differences between producer's network metrics between phases for each combo of mem/att/pref and 
# make a heat map for mem x att (use only differences for preference = 0, or average over all preference values IF there are little differences between attxmem heatmaps across preference values), mem x pref, att x pref where red shows the biggest differences

# use ggplot for making heat maps #could also check the heatmap2 package
