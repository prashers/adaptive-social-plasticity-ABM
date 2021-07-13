### FORMAT QUAIL_CENTRALITY_2 MODEL DATA ###
#This script is for the data from one run of each parameter combo

q.data = read.csv("quail_centrality_2 prelim analysis-table.csv", skip = 6, header = T) 
head(q.data[,1:3])
tail(q.data)
nrow(q.data)

# rename columns
names(q.data)

# 'grouping' 'prior.affils.' 'unfam.prod.' 'cluster.' 'eat.delay.' 'alt.food.' and 'reset.food.' were the sliders/switches that were all kept contant, so they can be removed 
# 'X.step.' column is the same as 'ticks' column
names(q.data) = c("run.number", "memory", "attention", "preference", "grouping", "prior.affils", "unfam.prod", "cluster", "eat.delay", "alt.food", "reset.food", "step", "ticks", "affil.IDs", "prox.cent.list", "proxim.IDs", "memory.succ.foragers", "fss.list", "foll.cent.list", "foll.IDs", "coor.list")
head(q.data)
tail(q.data)
