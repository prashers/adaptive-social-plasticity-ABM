# make loops in which you subset the big edge lists by run number, 
# then use igraph to calculate degree and strength and save in an external data frame
library(tidyr)
library(dplyr)

prox.summ.pre #edge list for pre-foraging period - contains data from all runs
prox.summ.for #edge list for foraging period - contains data from all runs
prox.summ.post #edge list for post-foraging period - contains data from all runs


###proximity network: pre-foraging phase###
prox.summ.pre.letters = as.data.frame(ungroup(prox.summ.pre))
for (i in 0:5) { #Loop to replace numbers in prox.ID column with letters 
        j = c("A","B","C","D","E","F")
        prox.summ.pre.letters[prox.summ.pre.letters$prox.ID == i,]$prox.ID = j[i+1]
}
unique(prox.summ.pre.letters$prox.ID) #check if the loop worked



prox.metrics.pre = data.frame(run.num = seq(1:125), A.deg=NA, A.str= NA)
for (i in unique(prox.summ.pre$run.num)) {
        current.edge.list = prox.summ.pre.letters[prox.summ.pre.letters$run.num==i, 5:7] #subset of prox.summ.pre giving the edge list for the current run.num
#        eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
        
        current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
        current.matrix = matrix.please(current.matrix)
        current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=NA)
        current.matrix.half[is.na(current.matrix.half)] = 0
        eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
        #igraph::E(eg)$weight #weights exist in the igraph object
        
        current.degree = igraph::degree(eg)
        current.strength = igraph::strength(eg) #don't need 'weights' argument if igraph object has edge weights attribute already
        
        current.metrics = cbind(current.degree,current.strength)
        prox.metrics.pre[prox.metrics.pre$run.num==i, 2:3] = current.metrics[1,] #save producer metrics in external data.frame
}
prox.metrics.pre #check #Max degree should be 5, max strength should be 500 because each individual can be in proximity with each of the other 5 agents for all 100 time steps in each phase (it's ok if these maximums are not reached though)


#uniq.q.data.pre = unique(q.data.pre[,2:5])
#pre.prox = merge(prox.metrics.pre, uniq.q.data.pre, by = "run.num") #adds mem, att, pref values to data frame with network metrics




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
        current.matrix.half[is.na(current.matrix.half)] = 0
        eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
       #igraph::E(eg)$weight #weights exist in the igraph object
        
        current.degree = igraph::degree(eg)
        current.strength = igraph::strength(eg) 
        
        current.metrics = cbind(current.degree,current.strength)
        prox.metrics.for[prox.metrics.for$run.num==i, 2:3] = current.metrics[1,] #save producer metrics in external data.frame
}
prox.metrics.for #check

#uniq.q.data.for = unique(q.data.forage[,2:5])
#for.prox = merge(prox.metrics.for, uniq.q.data.for, by = "run.num") #adds mem, att, pref values to data frame with network metrics



###proximity network: post-foraging phase###

prox.summ.post.letters = as.data.frame(ungroup(prox.summ.post))
for (i in 0:5) { #Loop to replace numbers in prox.ID column with letters 
        j = c("A","B","C","D","E","F")
        prox.summ.post.letters[prox.summ.post.letters$prox.ID == i,]$prox.ID = j[i+1]
}
unique(prox.summ.post.letters$prox.ID) #check if the loop worked


prox.metrics.post = data.frame(run.num = seq(1:125), A.deg=NA, A.str= NA)
for (i in unique(prox.summ.post$run.num)) {
        current.edge.list = prox.summ.post.letters[prox.summ.post.letters$run.num==i, 5:7] #subset of prox.summ.for giving the edge list for the current run.num
        #        eg = igraph::graph_from_data_frame(current.edge.list, directed = FALSE) # this way was doubling the degrees
        
        current.matrix = current.edge.list %>% reshape2::dcast(prox.key ~ prox.ID) 
        current.matrix = matrix.please(current.matrix)
        current.matrix.half = sna::lower.tri.remove(current.matrix, remove.val=NA)
        current.matrix.half[is.na(current.matrix.half)] = 0
        eg = igraph::graph_from_adjacency_matrix(current.matrix.half, mode="upper", weighted = TRUE, diag = FALSE)
        #igraph::E(eg)$weight #weights exist in the igraph object
        
        current.degree = igraph::degree(eg)
        current.strength = igraph::strength(eg) 
        
        current.metrics = cbind(current.degree,current.strength)
        prox.metrics.post[prox.metrics.post$run.num==i, 2:3] = current.metrics[1,] #save producer metrics in external data.frame
}
prox.metrics.post #check

#uniq.q.data.post = unique(q.data.post[,2:5])
#post.prox = merge(prox.metrics.post, uniq.q.data.post, by = "run.num") #adds mem, att, pref values to data frame with network metrics




# want to see if mem, att, pref have an effect on producer's centrality using network metrics from pre-forage phase as a baseline
# calculate differences between producer's network metrics between phases for each combo of mem/att/pref:

uniq.q.data.ord = unique(q.data.ord[,2:5])

prox.forXpre = data.frame(run.num = seq(1:nrow(prox.metrics.pre))) # data frame for differences between foraging and pre-foraging phases
prox.forXpre$forXpre.deg = prox.metrics.for$A.deg - prox.metrics.pre$A.deg # differences in degree
prox.forXpre$forXpre.str = prox.metrics.for$A.str - prox.metrics.pre$A.str # differences in strength

prox.forXpre = merge(prox.forXpre, uniq.q.data.ord, by = "run.num") #adds mem, att, pref values to data frame with differences in producer network metrics


prox.postXpre = data.frame(run.num = seq(1:nrow(prox.metrics.pre))) # data frame for differences between foraging and pre-foraging phases
prox.postXpre$postXpre.deg = prox.metrics.post$A.deg - prox.metrics.pre$A.deg # differences in degree
prox.postXpre$postXpre.str = prox.metrics.post$A.str - prox.metrics.pre$A.str # differences in strength

prox.postXpre = merge(prox.postXpre, uniq.q.data.ord, by = "run.num") #adds mem, att, pref values to data frame with differences in producer network metrics



# use ggplot2 or heatmap2 package for making heat maps
# make a heat map for mem x att (use only differences for preference = 0, or average over all preference values IF there are little differences between attxmem heatmaps across preference values), mem x pref, att x pref where red shows the biggest differences

plot(prox.forXpre$forXpre.str ~ prox.forXpre$run.num)
plot(prox.postXpre$postXpre.str ~ prox.postXpre$run.num)

library(ggplot2)

prox.forXpre.deg.plots = list()
prox.forXpre.str.plots = list()
counter = 1
for(i in unique(prox.forXpre$preference)) { #loop to plot heat maps of Attention X Memory for each unique value of preference
        run.data = prox.forXpre[prox.forXpre$preference == i,]
                
        Memory = as.factor(run.data$memory) 
        Attention = as.factor(run.data$attention) 
        Degree = run.data$forXpre.deg
        Strength = run.data$forXpre.str

        #Make dataframes containing the necessary info:
        deg.data = data.frame(Memory, Attention, Degree)
        str.data = data.frame(Memory, Attention, Strength)

        #Make and temporarily store plots:
        plot.deg = ggplot(deg.data, aes(Memory, Attention, fill = Degree)) +
                        geom_tile() +
                        scale_fill_gradient(low="white", high="red") +
                        theme_minimal()
        plot.str = ggplot(str.data, aes(Memory, Attention, fill = Strength)) +
                        geom_tile() +
                        scale_fill_gradient(low="white", high="red") +
                        theme_minimal()
        
        #save plots in external lists:
        prox.forXpre.deg.plots[[counter]] = plot.deg 
        prox.forXpre.str.plots[[counter]] = plot.str
        counter = counter + 1
}


#par(mfrow=c(2,3))
#for (i in 1:length(prox.forXpre.deg.plots)) {
#        prox.forXpre.deg.plots[[i]]
#}#trying to plot 5 heat maps in one panel



prox.postXpre.deg.plots = list()
prox.postXpre.str.plots = list()
counter = 1
for(i in unique(prox.postXpre$preference)) { #loop to plot heat maps of Attention X Memory for each unique value of preference
        run.data = prox.postXpre[prox.postXpre$preference == i,]
        
        Memory = as.factor(run.data$memory) 
        Attention = as.factor(run.data$attention) 
        Degree = run.data$postXpre.deg
        Strength = run.data$postXpre.str
        
        #Make dataframes containing the necessary info:
        deg.data = data.frame(Memory, Attention, Degree)
        str.data = data.frame(Memory, Attention, Strength)
        
        #Make and temporarily store plots:
        plot.deg = ggplot(deg.data, aes(Memory, Attention, fill = Degree)) +
                geom_tile() +
                scale_fill_gradient(low="white", high="red") +
                theme_minimal()
        plot.str = ggplot(str.data, aes(Memory, Attention, fill = Strength)) +
                geom_tile() +
                scale_fill_gradient(low="white", high="red") +
                theme_minimal()
        
        #save plots in external lists:
        prox.postXpre.deg.plots[[counter]] = plot.deg 
        prox.postXpre.str.plots[[counter]] = plot.str
        counter = counter + 1
}
