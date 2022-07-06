#Plots for ABS 2022
library(ggplot2)
library(RColorBrewer)

group.sizes = c(3, 6, 10, 15, 20)

str.range = vector() #VECTOR TO HOLD MIN AND MAX VALUES FOR MEDIAN STRENGTH ACROSS GROUP SIZES
mean.energy.range = vector() #VECTOR TO HOLD MIN AND MAX VALUES FOR MEAN ENERGY LEVEL OF FORAGERS ACROSS GROUP SIZES
median.energy.range = vector() #VECTOR TO HOLD MIN AND MAX VALUES FOR MEDIAN ENERGY LEVEL OF FORAGERS ACROSS GROUP SIZES
var.energy.range = vector() #VECTOR TO HOLD MIN AND MAX VALUES FOR VARIANCE IN ENERGY LEVEL OF FORAGERS ACROSS GROUP SIZES


for(i in group.sizes){ #GETTING RANGE OF MEDIAN STRENGTH IN PROXIMITY NETWORK, MEAN ENERGY, AND VARIANCE ENERGY TO USE FOR THE COLOR SCALE OF PLOTS (USE THIS IF YOU WANT THE SAME SCALE ACROSS GROUP SIZES)
  
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
 
  
  prox.forXpre = read_csv("prox_forxpre.csv", col_types = cols()) # including the 'col_types = cols()' argument suppresses the unnecessary column specification messages from read_csv
  prox.forXpre = as.data.frame(prox.forXpre[,-1])
  prox.postXfor = read_csv("prox_postxfor.csv", col_types = cols())
  prox.postXfor = as.data.frame(prox.postXfor[,-1])
  prox.postXpre = read_csv("prox_postxpre.csv", col_types = cols())
  prox.postXpre = as.data.frame(prox.postXpre[,-1])
  
  #summarize data (median) for each variable combination before making heat maps
  #check distribution of network metric differences for each variable combination to see how to proceed with summarizing the data
  #If the data are skewed it is better to summarize it using the median
#  par(mfrow=c(2,2))
  
#  hist(prox.forXpre[prox.forXpre$combo.num == 1001,]$forXpre.deg)
#  hist(prox.forXpre[prox.forXpre$combo.num == 1001,]$forXpre.str)#, breaks = seq(0, 500, 20))
#  
#  hist(prox.postXpre[prox.postXpre$combo.num == 1001,]$postXpre.deg)
#  hist(prox.postXpre[prox.postXpre$combo.num == 1001,]$postXpre.str)#, breaks = seq(0, 500, 20))
  
  
  #finding medians of network metric differences between foraging and pre-foraging phases for each combo:
  pfxp.med = prox.forXpre %>% 
    group_by(combo.num) %>% 
    summarize(n=n(), med.deg=median(forXpre.deg), med.str = median(forXpre.str))
  
  #finding medians of network metric differences between post- and foraging phases for each combo:
  ppxf.med = prox.postXfor %>% 
    group_by(combo.num) %>% 
    summarize(n=n(), med.deg = median(postXfor.deg), med.str = median(postXfor.str))
  
  #finding medians of network metric differences between post- and pre-foraging phases for each combo:
  ppxp.med = prox.postXpre %>% 
    group_by(combo.num) %>% 
    summarize(n=n(), med.deg=median(postXpre.deg), med.str = median(postXpre.str))
  
  #add ranges of median strengths to the empty vector
  str.range = append(str.range, c(range(pfxp.med$med.str), range(ppxf.med$med.str), range(ppxp.med$med.str)), after = length(str.range))
  
  
  
  ###################################################################
  for.success = read_csv("for_success.csv", col_types = cols())
  for.success = as.data.frame(for.success[,-1])
  
  mean.energy.range = append(mean.energy.range, range(for.success$mean.combo.energy), after = length(mean.energy.range))
  median.energy.range = append(median.energy.range, range(for.success$med.combo.energy), after = length(median.energy.range))
  var.energy.range = append(var.energy.range, range(for.success$var.combo.energy), after = length(var.energy.range))
  
  gc()
} #END OF LOOP

rm(pfxp.med, ppxf.med, ppxp.med, prox.forXpre, prox.postXfor, prox.postXpre)




#PDFs become corrupted when I use this loop, but not when I run all the code after changing the value of i manually
for(i in group.sizes){ #GETTING RANGE OF MEDIAN STRENGTH IN PROXIMITY NETWORK TO USE FOR THE COLOR SCALE OF PLOTS
  
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
  
  
  dir.create("plots") #create in a new folder in the current working directory that will hold all the plots from this loop
  
  prox.forXpre = read_csv("prox_forxpre.csv", col_types = cols())
  prox.forXpre = as.data.frame(prox.forXpre[,-1])
  prox.postXfor = read_csv("prox_postxfor.csv", col_types = cols())
  prox.postXfor = as.data.frame(prox.postXfor[,-1])
  prox.postXpre = read_csv("prox_postxpre.csv", col_types = cols())
  prox.postXpre = as.data.frame(prox.postXpre[,-1])
  
  
  ############################################################
  ############################################################
  #finding medians of network metric differences between foraging and pre-foraging phases for each combo:
  pfxp.med = prox.forXpre %>% 
    group_by(combo.num) %>% 
    summarize(n=n(), med.deg=median(forXpre.deg), med.str = median(forXpre.str))
  
  pfxp.med = merge(pfxp.med, unique(prox.forXpre[,c("group.size", "memory", "attention", "preference", "approach.food", "combo.num")]), by = "combo.num")
  rm(prox.forXpre)
  
  #plot of median difference in producer degree between foraging and pre-foraging phases
  #ggplot(pfxp.med, aes(as.factor(preference), as.factor(attention), fill = med.deg)) +
  #  ggtitle("Difference in producer's proximity degree between foraging and pre-foraging phases") +
  #  labs(y = "Attention", x = "Preference", fill = "Median Difference in Degree") +
  #  facet_grid(rows=vars(memory)) +
  #  geom_tile() +
  #  scale_fill_gradient(low="white", high="red") +
  #  theme_minimal()
  
  
  pal <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
#  pdf("./plots/StrengthForxPre_scaled.pdf", width=7, height=13)
  
  #plot of median difference in producer strength between foraging and pre-foraging phases 
  ggplot(pfxp.med, aes(as.factor(preference), as.factor(attention), fill = med.str)) +
    ggtitle("Difference in producer's proximity strength between foraging and pre-foraging phases") +
    labs(y = "Attention", x = "Preference", fill = "Median Difference in Strength") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(str.range), 0, max(str.range)), limits=c(min(str.range), max(str.range))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
   ggsave("./plots/StrengthForxPre_scaled.pdf", width=7, height=13)
  
  #rm(pfxp.med) #not removing this, because I will use it for foraging success plots as well
  
  ############################################################
  ############################################################
  #finding medians of network metric differences between post- and foraging phases for each combo:
  ppxf.med = prox.postXfor %>% 
    group_by(combo.num) %>% 
    summarize(n=n(), med.deg = median(postXfor.deg), med.str = median(postXfor.str))
  
  ppxf.med = merge(ppxf.med, unique(prox.postXfor[,c("group.size", "memory", "attention", "preference", "approach.food", "combo.num")]), by = "combo.num")
  rm(prox.postXfor)
  
  
#  pdf("./plots/StrengthPostxFor_scaled.pdf", width=7, height=13)
  
  #plot of median difference in producer strength between post- and foraging phases WHEN APPROACH.FOOD SWITCH WAS ON
  ggplot(ppxf.med, aes(as.factor(preference), as.factor(attention), fill = med.str)) +
    ggtitle("Difference in producer's proximity strength between post- and foraging phases") +
    labs(y = "Attention", x = "Preference", fill = "Median Difference in Strength") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(str.range), 0, max(str.range)), limits=c(min(str.range), max(str.range))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
   ggsave("./plots/StrengthPostxFor_scaled.pdf", width=7, height=13)
  
  rm(ppxf.med)
  
  ############################################################
  ############################################################
  #finding medians of network metric differences between post- and pre-foraging phases for each combo:
  ppxp.med = prox.postXpre %>% 
    group_by(combo.num) %>% 
    summarize(n=n(), med.deg=median(postXpre.deg), med.str = median(postXpre.str))
  
  ppxp.med = merge(ppxp.med, unique(prox.postXpre[,c("group.size", "memory", "attention", "preference", "approach.food", "combo.num")]), by = "combo.num")
  rm(prox.postXpre)
  
#  pdf("./plots/StrengthPostxPre_scaled.pdf", width=7, height=13)
  
  #plot of median difference in producer strength between post and pre-foraging phases WHEN APPROACH.FOOD SWITCH WAS OFF
  ggplot(ppxp.med, aes(as.factor(preference), as.factor(attention), fill = med.str)) +
    ggtitle("Difference in producer's proximity strength between post- and pre-foraging phases") +
    labs(y = "Attention", x = "Preference", fill = "Median Difference in Strength") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(str.range), 0, max(str.range)), limits=c(min(str.range), max(str.range))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
   ggsave("./plots/StrengthPostxPre_scaled.pdf", width=7, height=13)
  
  rm(ppxp.med)
  
  
  ############################################################
  ############################################################
  #PLOTS FOR FORAGING SUCCESS OF NON-PRODUCERS
  for.success = read_csv("for_success.csv", col_types = cols())
  for.success = as.data.frame(for.success[,-1])
  
  
  #plot mean energy level per combo like you did for differences in producer strength
#  pdf("./plots/meanenergy.pdf", width=7, height=13)
  
  #plot of mean energy level WHEN APPROACH.FOOD SWITCH WAS ON
  ggplot(for.success, aes(as.factor(preference), as.factor(attention), fill = mean.combo.energy)) +
    ggtitle("Mean energy of foragers") +
    labs(y = "Attention", x = "Preference", fill = "Mean energy level") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(mean.energy.range), 0, max(mean.energy.range)), limits=c(min(mean.energy.range), max(mean.energy.range))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
   ggsave("./plots/meanenergy.pdf", width=7, height=13)
  
  
  #plot median energy level per combo
#  pdf("./plots/medianenergy.pdf", width=7, height=13)
  
  #plot of median energy level
  ggplot(for.success, aes(as.factor(preference), as.factor(attention), fill = med.combo.energy)) +
    ggtitle("Median energy of foragers") +
    labs(y = "Attention", x = "Preference", fill = "Median energy level") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(median.energy.range), 0, max(median.energy.range)), limits=c(min(median.energy.range), max(median.energy.range))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
  ggsave("./plots/medianenergy.pdf", width=7, height=13)
  
  
  #plot variance in energy level per combo the same way
#  pdf("./plots/varenergy.pdf", width=7, height=13)
  
  #plot of variance in energy levels
  ggplot(for.success, aes(as.factor(preference), as.factor(attention), fill = var.combo.energy)) +
    ggtitle("Variance in energy of foragers") +
    labs(y = "Attention", x = "Preference", fill = "Variance in energy level") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(var.energy.range), 0, max(var.energy.range)), limits=c(min(var.energy.range), max(var.energy.range))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
   ggsave("./plots/varenergy.pdf", width=7, height=13)
  
  
  #calculate mean or median number of time steps that foragers ate for each combo number and plot in the same way
  #hist(for.success$n.timesteps)
  ticks.per.combo = for.success %>% 
    group_by(combo.num) %>% 
    mutate(n=n(), med.ticks=median(n.timesteps), mean.ticks = mean(n.timesteps))
  
  mid.break.mean = min(ticks.per.combo$mean.ticks) + ((max(ticks.per.combo$mean.ticks) - min(ticks.per.combo$mean.ticks))/2)
  mid.break.median = min(ticks.per.combo$med.ticks) + ((max(ticks.per.combo$med.ticks) - min(ticks.per.combo$med.ticks))/2)
  
  
#  pdf("./plots/meanticksforaged.pdf", width=7, height=13)
  
  #plot of mean number of time steps foragers ate 
  ggplot(ticks.per.combo, aes(as.factor(preference), as.factor(attention), fill = mean.ticks)) +
    ggtitle("Mean # time steps foragers ate with approach.food ON") +
    labs(y = "Attention", x = "Preference", fill = "Mean # time steps") +
    facet_grid(rows=vars(memory), cols=vars(approach.food)) +
    geom_tile() +
    scale_fill_gradientn(colours = pal(100), breaks=c(min(ticks.per.combo$mean.ticks), mid.break.mean, max(ticks.per.combo$mean.ticks)), limits=c(min(ticks.per.combo$mean.ticks), max(ticks.per.combo$mean.ticks))) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
#  dev.off()
  ggsave("./plots/meanticksforaged.pdf", width=7, height=13)
  
  
  #plot foraging success metrics by median change in producer's strength between foraging and pre-foraging phases
  
  ticks.mrg = merge(unique(ticks.per.combo[,!names(ticks.per.combo) %in% c("run.num", "n.timesteps", "mean.run.energy", "med.run.energy", "var.run.energy")]), pfxp.med[,names(pfxp.med) %in% c("combo.num", "med.str")], by = "combo.num")
  
#  pdf("./plots/meanticksforagedXstrength.pdf", width=7, height=13)
  
  #plot of mean number of time steps foragers ate WHEN APPROACH.FOOD SWITCH WAS OFF
  ggplot(ticks.mrg, aes(med.str, mean.ticks, color = approach.food)) +
    ggtitle("Mean # time steps foragers ate by change in producer's strength") +
    labs(y = "Mean # time steps", x = "Median change in producer's strength") +
   # scale_x_continuous(name = "Median change in producer's strength", breaks = seq(floor(min(str.range)), ceiling(max(str.range)))) +
  #  scale_y_continuous(name = "Mean # time steps", breaks = seq(floor(min(ticks.mrg$mean.ticks)), ceiling(max(ticks.mrg$mean.ticks)))) +
   # facet_grid(rows=vars(approach.food)) +
    geom_point() 
    
#  dev.off()
  ggsave("./plots/meanticksforagedXstrength.pdf", width=14, height=7)
  
  
#  pdf("./plots/meanenergyXstrength.pdf", width=7, height=13)
  
  #plot of mean number of time steps foragers ate WHEN APPROACH.FOOD SWITCH WAS OFF
  ggplot(ticks.mrg, aes(med.str, mean.combo.energy, color = approach.food)) +
    ggtitle("Mean energy level of foragers by change in producer's strength") +
    labs(y = "Mean energy level", x = "Median change in producer's strength") +
    # facet_grid(rows=vars(approach.food)) +
    geom_point() 
  
#  dev.off()
  ggsave("./plots/meanenergyXstrength.pdf", width=14, height=7)
  
  gc()
  
} # END OF PLOTTING LOOP






