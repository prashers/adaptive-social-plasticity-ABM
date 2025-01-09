# plots for manuscript


library(ggplot2)
library(RColorBrewer)
library(ggpubr) #for ggarrange function
library(cowplot)

full.outputs = read.csv("all_outputs.csv")

#color scale for heatmaps
pal <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

#data for heatmaps
full.outputs.combo = full.outputs %>% 
  group_by(groupsize,
           mem, 
           attention, 
           preference, 
           approachfood, 
           combo.num) %>% 
  summarize(n=n(),
            med.forXpre.str = median(scld.forXpre.str), #median difference in scaled prod str phase2v1
            med.postXfor.str = median(scld.postXfor.str),#median difference in scaled prod str phase3v2
            med.postXpre.str = median(scld.postXpre.str),#median difference in scaled prod str phase3v1
            med.ntimesteps = median(n.timesteps),#median num timesteps non-prods ate
            med.combo.energy = median(med.run.energy),#median energy of non-prods
            med.props.total = median(props.total),#median proportion of memories == prod
            med.props.pre = median(props.pre),#median proportion of phase1 memories == prod
            med.props.for = median(props.for),#median proportion of phase2 memories == prod
            med.props.post = median(props.post)) %>% #median proportion of phase3 mems == prod
  filter(groupsize == 15)

#function to get data summaries for plotting
# data : a data frame
# varname : the name of a column containing the variable to be summarized
# groupnames : vector of column names to be used as grouping variables
data_summary <- function(data, varname, groupnames){
  #require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      median = median(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])), #standard error == sd/sqrt(n)
      lowquant = quantile(x[[col]], na.rm=TRUE)[["25%"]],
      highquant = quantile(x[[col]], na.rm=TRUE)[["75%"]]
    )
  }
  data_sum<-plyr::ddply(data, 
                        groupnames, 
                        .fun=summary_func,
                        varname)
  
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  
  return(data_sum)
}



####Figure 1A ####
#heatmap: median diff in producer's strength (scaled by group size) btwn phase 1 and 2 for groupsize 15
fig1a = ggplot(full.outputs.combo, aes(as.factor(preference), as.factor(attention), fill = med.forXpre.str)) +
  #ggtitle("Median change in producer's strength between phases 1 and 2") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Scrounging \nDisabled", "TRUE" = "Scrounging \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 11)
        #text=element_text(size=15)
        )
# ggsave("./ms_plots/Figure1A_Median change in producer's strength between phases 1 and 2.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)


####Figure 1B: ####
#median and interquartile range of change in producer's strength btwn phase 1 and 2 when varying attention
fig1b = full.outputs %>% 
        filter(preference == 1,
               mem == 100) %>% # data keeping preference and memory constant
        data_summary(varname = "scld.forXpre.str", 
                     groupnames = c("attention", "groupsize", "approachfood")) %>%
        filter(approachfood == TRUE) %>% #only looking at when scrounging is enabled
        ggplot(aes(x=attention, y=median, group=groupsize)) + 
        geom_line(aes(color = factor(groupsize))) +
        geom_point(aes(color = factor(groupsize)))+
        #facet_grid(cols = vars(approachfood))+
        geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
        labs(#title=paste0("Effect of Attention on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 1 and 2 \n- Scrounging Enabled"), 
             x = "Attention", 
             y = "Change in strength",
             color = "Group size")+
        theme_classic() +
        scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1B_Effect of Attention on Median Change in Producer's Strength.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)


####Figure 1C: ####
#median and interquartile range of change in producer's strength btwn phase 1 and 2 when varying preference
fig1c = full.outputs %>% 
  filter(attention == 1,
         mem == 100) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.forXpre.str", 
               groupnames = c("preference", "groupsize", "approachfood")) %>%
  filter(approachfood == TRUE) %>% #only looking at when scrounging is enabled
  ggplot(aes(x=preference, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  #facet_grid(cols = vars(approachfood))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Preference on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 1 and 2 \n- Scrounging Enabled"), 
       x = "Preference", 
       y = "Change in strength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1C_Effect of Preference on Median Change in Producer's Strength.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 1D: #### 
#median and interquartile range of change in producer's strength btwn phase 1 and 2 when varying memory
fig1d = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.forXpre.str", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Scrounging Disabled", "TRUE" = "Scrounging Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
  labs(#title=paste0("Effect of Memory on Median Change in Producer's Strength (scaled by groupsize) between phases 1 and 2"), 
       x = "Memory", 
       y = "Change in strength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1D_Effect of Memory on Median Change in Producer's Strength.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)


####Figure 1E: ####
#median and interquartile range of proportion of times within phase 2 that producer is being remembered as successful forager
fig1e = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "props.for", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
    geom_line(aes(color = factor(groupsize))) +
    geom_point(aes(color = factor(groupsize)))+
    facet_grid(cols = vars(approachfood),
               labeller = labeller(approachfood = c("FALSE" = "Scrounging Disabled", "TRUE" = "Scrounging Enabled")))+
    geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
    labs(#title=paste0("Effect of Memory on Median Proportion of Times Correct Producer Remembered during Phase 2"), 
         x = "Memory", 
         y = "Proportion",
         color = "Group size")+
    theme_classic() +
    scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1E_Effect of Memory on Median Proportion Phase2.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)


####save composite figure 2####
legend1BCD = get_legend(fig1b)
ggdraw(xlim = c(0, 1.2), ylim = c(0, 3)) + #initialize empty canvas
  draw_plot(fig1b + theme(legend.position = "none"), x=0, y=2, width = 0.5, height =  1) +
  draw_plot(fig1c + theme(legend.position = "none"), x=0.5, y=2, width = 0.5, height =  1) +
  draw_plot(fig1d + theme(legend.position = "none"), x=0, y=1, width = 1, height =  1) +
  draw_plot(fig1e + theme(legend.position = "none"), x=0, y=0, width = 1, height =  1) +
  draw_plot(legend1BCD, x=0.6, y=1, scale = 1.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), 
                  size = 15,
                  x = c(0, 0.5, 0, 0),
                  y = c(3.0, 3.0, 2.0, 1.0))
ggsave("./ms_plots/Figure1.pdf", 
       width = 8,
       height = 10,
       dpi = 300)


#### Figure 2A ####
#heatmap: median diff in producer's strength (scaled by group size) btwn phase 2 and 3 for groupsize 15
fig2a = ggplot(full.outputs.combo, aes(as.factor(preference), as.factor(attention), fill = med.postXfor.str)) +
  #ggtitle("Median change in producer's strength between phases 2 and 3") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Scrounging \nDisabled", "TRUE" = "Scrounging \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 11)
        #text=element_text(size=15)
        )
# ggsave("./ms_plots/Figure2A_Median change in producer's strength between phases 2 and 3.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)

####Figure 2B: #### 
#median and interquartile range of change in producer's strength btwn phase 2 and 3 when varying attention
fig2b = full.outputs %>% 
  filter(preference == 1,
         mem == 100) %>% # data keeping preference and memory constant
  data_summary(varname = "scld.postXfor.str", 
               groupnames = c("attention", "groupsize", "approachfood")) %>%
  filter(approachfood == TRUE) %>% #only looking at when scrounging is enabled
  ggplot(aes(x=attention, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  #facet_grid(cols = vars(approachfood))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Attention on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 2 and 3 \n- Scrounging Enabled"), 
       x = "Attention", 
       y = "Change in \nstrength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure2B_Effect of Attention on Median Change in Producer's Strength.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 2C: ####
#median and interquartile range of change in producer's strength btwn phase 2 and 3 when varying preference
fig2c = full.outputs %>% 
  filter(attention == 1,
         mem == 100) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.postXfor.str", 
               groupnames = c("preference", "groupsize", "approachfood")) %>%
  filter(approachfood == TRUE) %>% #only looking at when scrounging is enabled
  ggplot(aes(x=preference, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  #facet_grid(cols = vars(approachfood))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Preference on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 2 and 3 \n- Scrounging Enabled"), 
       x = "Preference", 
       y = "Change in \nstrength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure2C_Effect of Preference on Median Change in Producer's Strength.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 2D: ####
#median and interquartile range of change in producer's strength btwn phase 2 and 3 when varying memory
fig2d = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.postXfor.str", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Scrounging Disabled", "TRUE" = "Scrounging Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
  labs(#title=paste0("Effect of Memory on Median Change \nin Producer's Strength (scaled by groupsize) from  \nbetween phases 2 and 3 \n- Scrounging Enabled"), 
       x = "Memory", 
       y = "Change in \nstrength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure2D_Effect of Memory on Median Change in Producer's Strength.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)

#### Figure 2E: ####
#median and interquartile range of proportion of times within phase 3 that producer is being remembered as successful forager
fig2e = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "props.post", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Scrounging Disabled", "TRUE" = "Scrounging Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
  labs(#title=paste0("Effect of Memory on Median Proportion of Times Correct Producer Remembered during Phase 3"), 
       x = "Memory", 
       y = "Proportion",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure2E_Effect of Memory on Median Proportion Phase3.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)


####save composite figure 2####
legend2A = get_legend(fig2a)
legend2BCD = get_legend(fig2b)
ggdraw(xlim = c(0, 2), ylim = c(0, 4.5)) + #initialize empty canvas
  draw_plot(fig2a + theme(legend.position = "none", axis.text.x = element_text(angle=90, hjust=1)), x=0, y=3.3, width = 1.5, height =  1, scale = 1.3) +
  draw_plot(fig2b + theme(legend.position = "none"), x=0, y=2, width = 0.75, height =  1) +
  draw_plot(fig2c + theme(legend.position = "none"), x=0.75, y=2, width = 0.75, height =  1) +
  draw_plot(fig2d + theme(legend.position = "none"), x=0, y=1, width = 1.5, height =  1) +
  draw_plot(fig2e + theme(legend.position = "none"), x=0, y=0, width = 1.5, height =  1) +
  draw_plot(legend2A, x=1.2, y=3.3, scale = 1.5) +
  draw_plot(legend2BCD, x=1.1, y=1, scale = 1.5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E"), 
                  size = 15,
                  x = c(0, 0, 0.75, 0, 0),
                  y = c(4.5, 3.05, 3.05, 2.05, 1.05))
ggsave("./ms_plots/Figure2.pdf", 
       width = 8,
       height = 10,
       dpi = 300)


####Figure 3A ####
#heatmap: Median energy achieved by scroungers at end of simulation for groupsize 15
fig3a = ggplot(full.outputs.combo, aes(as.factor(preference), as.factor(attention), fill = med.combo.energy)) +
  #ggtitle("Median Scrounger Energy") +
  labs(title = "Memory", 
       y = "Attention", 
       x = "Preference", 
       fill = "Scrounger Energy") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Scrounging \nDisabled", "TRUE" = "Scrounging \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 11)
        #text=element_text(size=15)
        )
# ggsave("./ms_plots/Figure3A_Median Scrounger Energy.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)

####Figure 3B: ####
#median and interquartile range of median energy when varying attention
fig3b = full.outputs %>% 
  filter(preference == 1,
         mem == 100) %>% # data keeping preference and memory constant
  data_summary(varname = "med.run.energy", 
               groupnames = c("attention", "groupsize", "approachfood")) %>%
  filter(approachfood == TRUE) %>% #only looking at when scrounging is enabled
  ggplot(aes(x=attention, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  #facet_grid(cols = vars(approachfood))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Attention on Median Scrounger Energy \n- Scrounging Enabled"), 
       x = "Attention", 
       y = "Change in strength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure3B_Effect of Attention on Median Scrounger Energy.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 3C: ####
#median and interquartile range of median energy when varying preference
fig3c = full.outputs %>% 
  filter(attention == 1,
         mem == 100) %>% # data keeping attention and memory constant
  data_summary(varname = "med.run.energy", 
               groupnames = c("preference", "groupsize", "approachfood")) %>%
  filter(approachfood == TRUE) %>% #only looking at when scrounging is enabled
  ggplot(aes(x=preference, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  #facet_grid(cols = vars(approachfood))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Preference on Median Scrounger energy \n- Scrounging Enabled"), 
       x = "Preference", 
       y = "Change in strength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure3C_Effect of Preference on Median Scrounger Energy.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 3D: ####
#median and interquartile range of median energy when varying memory
fig3d = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "med.run.energy", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  filter(approachfood == TRUE) %>% # only looking at scrounging enabled
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  #facet_grid(cols = vars(approachfood))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
  labs(#title=paste0("Effect of Memory on Median Scrounger energy \n- Scrounging Enabled"), 
       x = "Memory", 
       y = "Change in strength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure3D_Effect of Memory on Median Scrounger Energy.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)


####save composite figure 3####
legend3A = get_legend(fig3a)
legend3BCD = get_legend(fig3b)
ggdraw(xlim = c(0, 1.5), ylim = c(0, 3.5)) + #initialize empty canvas
  draw_plot(fig3a + theme(legend.position = "none"), x=0, y=2.3, width = 1.5, height =  1, scale = 1.5) +
  draw_plot(fig3b + theme(legend.position = "none"), x=0, y=1, width = 0.75, height =  1) +
  draw_plot(fig3c + theme(legend.position = "none"), x=0.75, y=1, width = 0.75, height =  1) +
  draw_plot(fig3d + theme(legend.position = "none"), x=0, y=0, width = 0.75, height =  1) +
  draw_plot(legend3A, x=0.9, y=2.3, scale = 1.5) +
  draw_plot(legend3BCD, x=0.5, y=0, scale = 1.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), 
                  size = 15,
                  x = c(0, 0, 0.75, 0),
                  y = c(3.5, 2.05, 2.05, 1.05))
ggsave("./ms_plots/Figure3.tif", 
       width = 11,
       height = 8,
       dpi = 300)



#HEATMAPS ONLY####
# ggdraw(xlim = c(0, 1.5), ylim = c(0, 9)) + #initialize empty canvas
#   draw_plot(fig1a , x=0, y=6, width = 1.5, height =  1.5, scale = 1.5) +
#   draw_plot(fig2a , x=0, y=3, width = 1.5, height =  1.5, scale = 1.5) +
#   draw_plot(fig3a , x=0, y=0, width = 1.5, height =  1.5, scale = 1.5) +
#   draw_plot_label(label = c("A", "B", "C"), 
#                   size = 15,
#                   x = c(0, 0, 0),
#                   y = c(3.5, 2.25, 1))
plot_grid(fig1a + theme(axis.text.x = element_text(angle=90, hjust=1)), 
          fig2a + theme(axis.text.x = element_text(angle=90, hjust=1)), 
          fig3a + theme(axis.text.x = element_text(angle=90, hjust=1)), 
          nrow = 3, rel_widths = c(1.5,1.5,1.5)) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0.02, 0.02, 0.02),
                  y = c(0.99, 0.66, 0.33))
ggsave("./ms_plots/heatmaps.pdf", 
       width = 8,
       height = 9,
       dpi = 300)
