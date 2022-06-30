#Plots for ABS 2022
library(ggplot2)

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
  
  prox.forXpre = read.csv("prox_forxpre.csv", header = T)
  prox.postXfor = read.csv("prox_postxfor.csv", header = T)
  prox.postXpre = read.csv("prox_postxpre.csv", header = T)
  
  
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
  
  pfxp.med = merge(pfxp.med, unique(prox.forXpre[,c("group.size", "memory", "attention", "preference", "approach.food", "combo.num")]), by = "combo.num")
  
  
  #plot of median difference in producer degree between foraging and pre-foraging phases
  #ggplot(pfxp.med, aes(as.factor(preference), as.factor(attention), fill = med.deg)) +
  #  ggtitle("Difference in producer's proximity degree between foraging and pre-foraging phases") +
  #  labs(y = "Attention", x = "Preference", fill = "Median Difference in Degree") +
  #  facet_grid(rows=vars(memory)) +
  #  geom_tile() +
  #  scale_fill_gradient(low="white", high="red") +
  #  theme_minimal()
  
  
  pal <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
  pdf("StrengthForxPre_scaled.pdf", width=7, height=13)
  
  #plot of median difference in producer strength between foraging and pre-foraging phases 
  ggplot(pfxp.med, aes(as.factor(preference), as.factor(attention), fill = med.str)) +
    ggtitle("Difference in producer's proximity strength between foraging and pre-foraging phases") +
    labs(y = "Attention", x = "Preference", fill = "Median Difference in Strength") +
    facet_grid(rows=vars(memory)) +
    geom_tile() +
    #  scale_fill_gradient(low="white", high="blue", breaks=c(-125, 0, 400), limits=c(-125, 500)) + #including these 'breaks' and 'limits' arguments allows for same scale across the plots
    scale_fill_gradientn(colours = pal(100), breaks=c(-2.75, 0, 4.3), limits=c(-2.75, 4.3)) +
    theme_minimal() +
    theme(aspect.ratio=1, text=element_text(size=15))
  
  dev.off()
  
}
















#finding medians of network metric differences between post- and foraging phases for each combo:
ppxf.med = prox.postXfor %>% 
  group_by(combo.num) %>% 
  summarize(n=n(), med.deg = median(postXfor.deg), med.str = median(postXfor.str))

ppxf.med = merge(ppxf.med, unique(prox.postXfor[,4:7]), by = "combo.num")


#plot of median difference in producer degree between post- and foraging phases
ggplot(ppxf.med, aes(as.factor(preference), as.factor(attention), fill = med.deg)) +
  ggtitle("Difference in producer's proximity degree between post- and foraging phases") +
  labs(y = "Attention", x = "Preference", fill = "Median Difference in Degree") +
  facet_grid(rows=vars(memory)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  theme_minimal()


pdf("StrengthPostxFor_scaled.pdf", width=7, height=13)

#plot of median difference in producer strength between post- and foraging phases 
ggplot(ppxf.med, aes(as.factor(preference), as.factor(attention), fill = med.str)) +
  ggtitle("Difference in producer's proximity strength between post- and foraging phases") +
  labs(y = "Attention", x = "Preference", fill = "Median Difference in Strength") +
  facet_grid(rows=vars(memory)) +
  geom_tile() +
  #  scale_fill_gradient(low="white", high="blue", breaks=c(-125, 0, 400), limits=c(-125, 500)) + #including these 'breaks' and 'limits' arguments allows for same scale across the plots
  scale_fill_gradientn(colours = pal(100), breaks=c(-2.75, 0, 4.3), limits=c(-2.75, 4.3)) +
  theme_minimal() +
  theme(aspect.ratio=1, text=element_text(size=15))

dev.off()





#finding medians of network metric differences between post- and pre-foraging phases for each combo:
ppxp.med = prox.postXpre %>% 
  group_by(combo.num) %>% 
  summarize(n=n(), med.deg=median(postXpre.deg), med.str = median(postXpre.str))

ppxp.med = merge(ppxp.med, unique(prox.postXpre[,4:7]), by = "combo.num")


#plot of median difference in producer degree between post- and pre-foraging phases
ggplot(ppxp.med, aes(as.factor(preference), as.factor(attention), fill = med.deg)) +
  ggtitle("Difference in producer's proximity degree between post- and pre-foraging phases") +
  labs(y = "Attention", x = "Preference", fill = "Median Difference in Degree") +
  facet_grid(rows=vars(memory)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  theme_minimal()


pdf("StrengthPostxPre_scaled.pdf", width=7, height=13)

#plot of median difference in producer strength between post and pre-foraging phases 
ggplot(ppxp.med, aes(as.factor(preference), as.factor(attention), fill = med.str)) +
  ggtitle("Difference in producer's proximity strength between post- and pre-foraging phases") +
  labs(y = "Attention", x = "Preference", fill = "Median Difference in Strength") +
  facet_grid(rows=vars(memory)) +
  geom_tile() +
  #  scale_fill_gradient(low="white", high="blue", breaks=c(-125, 0, 400), limits=c(-125, 500)) + #including these 'breaks' and 'limits' arguments allows for same scale across the plots
  scale_fill_gradientn(colours = pal(100), breaks=c(-2.75, 0, 4.3), limits=c(-2.75, 4.3)) +
  theme_minimal() +
  theme(aspect.ratio=1, text=element_text(size=15))

dev.off()



# For proximity, the question is:
# Does foraging success increase the producer's centrality from a baseline value (when movement is random)?



# I THINK I NEED SCALE OF COLOR GRADIENT TO BE CONSISTENT ACROSS THE THREE PLOTS, SO IT IS POSSIBLE TO COMPARE THE RELATIVE DIFFERENCES BETWEEN PHASES
# LOWEST MEDIAN STRENGTH DIFFERENCE ACROSS ALL PHASE PAIRS IS -121 AND HIGHEST IS 440
# MAKE THE LEGEND GO FROM -125 TO 500