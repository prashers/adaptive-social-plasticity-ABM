# plots for manuscript

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr) #for ggarrange function
library(cowplot)

full.outputs = read.csv("all_outputs.csv")

#color scale for heatmaps
pal <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

#data for heatmaps####
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
            med.props.post = median(props.post))#median proportion of phase3 mems == prod

full.outputs.combo.15 = full.outputs.combo %>% 
                        filter(groupsize == 15)

#function to get data summaries for plotting ####
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



####Figure 1 ####
#heatmap: median diff in producer's strength (scaled by group size) btwn phase 1 and 2 for groupsize 15
fig1 = ggplot(full.outputs.combo.15, aes(as.factor(preference), as.factor(attention), fill = med.forXpre.str)) +
  #ggtitle("Median change in producer's strength between phases 1 and 2") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(0, 
                                    max(full.outputs.combo.15$med.forXpre.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = c(0, 15, 30, 45, round(max(full.outputs.combo.15$med.forXpre.str)))) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        legend.position = "bottom"
        )
ggsave("./ms_plots/Figure1_Median change in strength between phases 1 and 2.tif",
       width = 180,
       height = 88,
       units = "mm",
       dpi = 300)
#ICB: "width of a single (88 mm) or at most a double (180 mm) column width"


####Figure 2A: ####
#median and interquartile range of change in producer's strength btwn phase 1 and 2 when varying attention
fig2a = full.outputs %>% 
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
             y = "Change in \nstrength",
             color = "Group size")+
        theme_classic() +
        scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1B_Effect of Attention on Median Change in Producer's Strength.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)


####Figure 2B: ####
#median and interquartile range of change in producer's strength btwn phase 1 and 2 when varying preference
fig2b = full.outputs %>% 
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
       y = "Change in \nstrength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1C_Effect of Preference on Median Change in Producer's Strength.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 2c: #### 
#median and interquartile range of change in producer's strength btwn phase 1 and 2 when varying memory
fig2c = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.forXpre.str", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
  labs(#title=paste0("Effect of Memory on Median Change in Producer's Strength (scaled by groupsize) between phases 1 and 2"), 
       x = "Memory", 
       y = "Change in \nstrength",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure1D_Effect of Memory on Median Change in Producer's Strength.tif", 
#        width = 15,
#        height = 7,
#        dpi = 300)


####Figure 2d: ####
#median and interquartile range of proportion of times within phase 2 that producer is being remembered as successful forager
fig2d = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "props.for", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
    geom_line(aes(color = factor(groupsize))) +
    geom_point(aes(color = factor(groupsize)))+
    facet_grid(cols = vars(approachfood),
               labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
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
legend2 = cowplot::get_plot_component(fig2a + theme(legend.position = "bottom"), 'guide-box-bottom', return_all = TRUE)
ggdraw(xlim = c(0, 1), ylim = c(0, 3.5)) + #initialize empty canvas
  draw_plot(fig2a + theme(legend.position = "none"), x=0, y=2.5, width = 0.5, height =  1) +
  draw_plot(fig2b + theme(legend.position = "none"), x=0.5, y=2.5, width = 0.5, height =  1) +
  draw_plot(fig2c + theme(legend.position = "none"), x=0, y=1.5, width = 1, height =  1) +
  draw_plot(fig2d + theme(legend.position = "none"), x=0, y=0.5, width = 1, height =  1) +
  draw_plot(legend2, x=0.05, y=0.3, width = 1, height = 0.2) +
  draw_plot_label(label = c("A", "B", "C", "D"), 
                  size = 15,
                  x = c(0, 0.5, 0, 0),
                  y = c(3.5, 3.5, 2.5, 1.5))
ggsave("./ms_plots/Figure2.tif", 
       width = 180,
       height = 180,
       units = "mm",
       dpi = 300)


#### Figure 3 ####
#heatmap: median diff in producer's strength (scaled by group size) btwn phase 2 and 3 for groupsize 15
fig3 = ggplot(full.outputs.combo.15, aes(as.factor(preference), as.factor(attention), fill = med.postXfor.str)) +
  #ggtitle("Median change in producer's strength between phases 2 and 3") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(full.outputs.combo.15$med.postXfor.str), 
                                    max(full.outputs.combo.15$med.postXfor.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(full.outputs.combo.15$med.postXfor.str), 
                                          max(full.outputs.combo.15$med.postXfor.str), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        legend.position = "bottom"
  )
ggsave("./ms_plots/Figure3_Median change in strength between phases 2 and 3.tif",
       width = 180,
       height = 88,
       units = "mm",
       dpi = 300)

####Figure 4a: #### 
#median and interquartile range of change in producer's strength btwn phase 2 and 3 when varying attention
fig4a = full.outputs %>% 
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

####Figure 4b: ####
#median and interquartile range of change in producer's strength btwn phase 2 and 3 when varying preference
fig4b = full.outputs %>% 
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

####Figure 4c: ####
#median and interquartile range of change in producer's strength btwn phase 2 and 3 when varying memory
fig4c = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.postXfor.str", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
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

#### Figure 4d: ####
#median and interquartile range of proportion of times within phase 3 that producer is being remembered as successful forager
fig4d = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "props.post", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
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


####save composite figure 4####
legend4 = cowplot::get_plot_component(fig4a + theme(legend.position = "bottom"), 'guide-box-bottom', return_all = TRUE)
ggdraw(xlim = c(0, 1), ylim = c(0, 3.5)) + #initialize empty canvas
  draw_plot(fig4a + theme(legend.position = "none"), x=0, y=2.5, width = 0.5, height =  1) +
  draw_plot(fig4b + theme(legend.position = "none"), x=0.5, y=2.5, width = 0.5, height =  1) +
  draw_plot(fig4c + theme(legend.position = "none"), x=0, y=1.5, width = 1, height =  1) +
  draw_plot(fig4d + theme(legend.position = "none"), x=0, y=0.5, width = 1, height =  1) +
  draw_plot(legend4, x=0.05, y=0.30, width = 1, height = 0.2) +
  draw_plot_label(label = c("A", "B", "C", "D"), 
                  size = 15,
                  x = c(0, 0.5, 0, 0),
                  y = c(3.5, 3.5, 2.5, 1.5))
ggsave("./ms_plots/Figure4.tif", 
       width = 180,
       height = 180,
       units = "mm",
       dpi = 300)


####Figure 5 ####
#heatmap: Median energy achieved by scroungers at end of simulation for groupsize 15
fig5 = ggplot(full.outputs.combo.15, aes(as.factor(preference), as.factor(attention), fill = med.combo.energy)) +
  #ggtitle("Median energy level of scroungers") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Scrounger \nenergy") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(full.outputs.combo.15$med.combo.energy), 
                                    max(full.outputs.combo.15$med.combo.energy), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(full.outputs.combo.15$med.combo.energy), 
                                          max(full.outputs.combo.15$med.combo.energy), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        legend.position = "bottom"
  )
ggsave("./ms_plots/Figure5_Median scrounger energy.tif",
       width = 180,
       height = 88,
       units = "mm",
       dpi = 300)

####Figure 6a: ####
#median and interquartile range of median energy when varying attention
fig6a = full.outputs %>% 
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
  labs(#title=paste0("Effect of Attention on Median Scrounger Energy \n- Asocial-information Enabled"), 
       x = "Attention", 
       y = "Scrounger \nenergy",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure3B_Effect of Attention on Median Scrounger Energy.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 6b: ####
#median and interquartile range of median energy when varying preference
fig6b = full.outputs %>% 
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
  labs(#title=paste0("Effect of Preference on Median Scrounger energy \n- Asocial-information Enabled"), 
       x = "Preference", 
       y = "Scrounger \nenergy",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure3C_Effect of Preference on Median Scrounger Energy.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)

####Figure 6c: ####
#median and interquartile range of median energy when varying memory
fig6c = full.outputs %>% 
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
  labs(#title=paste0("Effect of Memory on Median Scrounger energy \n- Asocial-information Enabled"), 
       x = "Memory", 
       y = "Scrounger \nenergy",
       color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))
# ggsave("./ms_plots/Figure3D_Effect of Memory on Median Scrounger Energy.tif", 
#        width = 7,
#        height = 7,
#        dpi = 300)


####save composite figure 6####
legend6 = cowplot::get_plot_component(fig6a + theme(legend.position = "bottom"), 'guide-box-bottom', return_all = TRUE)
ggdraw(xlim = c(0, 1), ylim = c(0, 3.5)) + #initialize empty canvas
  draw_plot(fig6a + theme(legend.position = "none"), x=0, y=2.5, width = 0.5, height =  1) +
  draw_plot(fig6b + theme(legend.position = "none"), x=0, y=1.5, width = 0.5, height =  1) +
  draw_plot(fig6c + theme(legend.position = "none"), x=0, y=0.5, width = 0.5, height =  1) +
  draw_plot(legend6, x=-0.25, y=0.34, width = 1, height=0.2) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0, 0, 0),
                  y = c(3.5, 2.5, 1.5))
ggsave("./ms_plots/Figure6.tif", 
       width = 180,
       height = 180,
       units = "mm",
       dpi = 300)




###HEATMAPS ONLY###
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



####SUPPLEMENTAL FIGURES####
{
####heatmap dif in prod strength phase1to3 ### 
ggplot(full.outputs.combo.15, aes(as.factor(preference), as.factor(attention), fill = med.postXpre.str)) +
  #ggtitle("Median change in producer's strength between phases 1 and 3") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(full.outputs.combo.15$med.postXpre.str), 
                                    max(full.outputs.combo.15$med.postXpre.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(full.outputs.combo.15$med.postXpre.str), 
                                          max(full.outputs.combo.15$med.postXpre.str), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        legend.position = "bottom"
  )
ggsave("./ms_plots/supplemental/Median change in producer's strength between phases 1 and 3.pdf",
       width = 180,
       height = 88,
       units = "mm",
       dpi = 300)
}

#### heatmaps for groups 3, 6, and 10 ####
#prepare data
data.3 = full.outputs.combo %>% filter(groupsize==3)
data.6 = full.outputs.combo %>% filter(groupsize==6)
data.10 = full.outputs.combo %>% filter(groupsize==10)

#Fig 1 equivalent: change in strength from phase 1 to 2
fig1.gsize3 = ggplot(data.3, aes(as.factor(preference), as.factor(attention), fill = med.forXpre.str)) +
  #ggtitle("Median change in producer's strength between phases 1 and 2") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.3$med.forXpre.str), 
                                    max(data.3$med.forXpre.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.3$med.forXpre.str),
                                          max(data.3$med.forXpre.str),
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )


fig1.gsize6 = ggplot(data.6, aes(as.factor(preference), as.factor(attention), fill = med.forXpre.str)) +
  #ggtitle("Median change in producer's strength between phases 1 and 2") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.6$med.forXpre.str), 
                                    max(data.6$med.forXpre.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.6$med.forXpre.str),
                                          max(data.6$med.forXpre.str),
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )


fig1.gsize10 = ggplot(data.10, aes(as.factor(preference), as.factor(attention), fill = med.forXpre.str)) +
  #ggtitle("Median change in producer's strength between phases 1 and 2") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.10$med.forXpre.str), 
                                    max(data.10$med.forXpre.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.10$med.forXpre.str),
                                          max(data.10$med.forXpre.str),
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )

plot_grid(fig1.gsize3, 
          fig1.gsize6, 
          fig1.gsize10, 
          nrow = 3, rel_widths = c(1.5, 1.5, 1.5)) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0.02, 0.02, 0.02),
                  y = c(0.99, 0.66, 0.33))
ggsave("./ms_plots/supplemental/Fig1_other_group_sizes.tif", 
       width = 8,
       height = 9,
       dpi = 300)


#Fig 3 equivalent: change in strength from phase 2 to 3
fig3.gsize3 = ggplot(data.3, aes(as.factor(preference), as.factor(attention), fill = med.postXfor.str)) +
  #ggtitle("Median change in producer's strength between phases 2 and 3") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.3$med.postXfor.str), 
                                    max(data.3$med.postXfor.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.3$med.postXfor.str), 
                                          max(data.3$med.postXfor.str), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )

fig3.gsize6 = ggplot(data.6, aes(as.factor(preference), as.factor(attention), fill = med.postXfor.str)) +
  #ggtitle("Median change in producer's strength between phases 2 and 3") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.6$med.postXfor.str), 
                                    max(data.6$med.postXfor.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.6$med.postXfor.str), 
                                          max(data.6$med.postXfor.str), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )

fig3.gsize10 = ggplot(data.10, aes(as.factor(preference), as.factor(attention), fill = med.postXfor.str)) +
  #ggtitle("Median change in producer's strength between phases 2 and 3") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Change in \nproducer's strength") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.10$med.postXfor.str), 
                                    max(data.10$med.postXfor.str), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.10$med.postXfor.str), 
                                          max(data.10$med.postXfor.str), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )


plot_grid(fig3.gsize3, 
          fig3.gsize6, 
          fig3.gsize10, 
          nrow = 3, rel_widths = c(1.5, 1.5, 1.5)) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0.02, 0.02, 0.02),
                  y = c(0.99, 0.66, 0.33))
ggsave("./ms_plots/supplemental/Fig3_other_group_sizes.tif", 
       width = 8,
       height = 9,
       dpi = 300)


#Fig 5 equivalent: Median scrounger energy
fig5.gsize3 = ggplot(data.3, aes(as.factor(preference), as.factor(attention), fill = med.combo.energy)) +
  #ggtitle("Median energy level of scroungers") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Scrounger energy") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.3$med.combo.energy), 
                                    max(data.3$med.combo.energy), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.3$med.combo.energy), 
                                          max(data.3$med.combo.energy), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )


fig5.gsize6 = ggplot(data.6, aes(as.factor(preference), as.factor(attention), fill = med.combo.energy)) +
  #ggtitle("Median energy level of scroungers") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Scrounger energy") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.6$med.combo.energy), 
                                    max(data.6$med.combo.energy), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.6$med.combo.energy), 
                                          max(data.6$med.combo.energy), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )


fig5.gsize10 = ggplot(data.10, aes(as.factor(preference), as.factor(attention), fill = med.combo.energy)) +
  #ggtitle("Median energy level of scroungers") +
  labs(title = "Memory",
       y = "Attention", 
       x = "Preference", 
       fill = "Scrounger energy") +
  facet_grid(rows=vars(approachfood), 
             cols=vars(mem),
             labeller = labeller(approachfood = c("FALSE" = "Asocial \ninformation \nDisabled", "TRUE" = "Asocial \ninformation \nEnabled"))) +
  geom_tile() +
  scale_fill_gradientn(colours = pal(100),
                       breaks = seq(min(data.10$med.combo.energy), 
                                    max(data.10$med.combo.energy), 
                                    length.out = 5), # Adjust length.out as needed
                       labels = round(seq(min(data.10$med.combo.energy), 
                                          max(data.10$med.combo.energy), 
                                          length.out = 5), 0)) +
  theme_minimal() +
  theme(aspect.ratio=1, 
        plot.title = element_text(margin = margin(t=0, b=0, unit = "pt"), 
                                  hjust = 0.5, 
                                  vjust = 0, 
                                  size = 12),
        text=element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )


plot_grid(fig5.gsize3, 
          fig5.gsize6, 
          fig5.gsize10, 
          nrow = 3, rel_widths = c(1.5, 1.5, 1.5)) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0.02, 0.02, 0.02),
                  y = c(0.99, 0.66, 0.33))
ggsave("./ms_plots/supplemental/Fig5_other_group_sizes.tif", 
       width = 8,
       height = 9,
       dpi = 300)


#### full lineplots ####

#fig2 full
fig2a_full = full.outputs %>% 
  filter(preference == 1,
         mem == 100) %>% # data keeping preference and memory constant
  data_summary(varname = "scld.forXpre.str", 
               groupnames = c("attention", "groupsize", "approachfood")) %>%
  ggplot(aes(x=attention, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Attention on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 1 and 2 \n- Scrounging Enabled"), 
    x = "Attention", 
    y = "Change in \nstrength",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))

fig2b_full = full.outputs %>% 
  filter(attention == 1,
         mem == 100) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.forXpre.str", 
               groupnames = c("preference", "groupsize", "approachfood")) %>%
  ggplot(aes(x=preference, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Preference on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 1 and 2 \n- Scrounging Enabled"), 
    x = "Preference", 
    y = "Change in \nstrength",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))


ggdraw(xlim = c(0, 1), ylim = c(0, 2.5)) + #initialize empty canvas
  draw_plot(fig2a_full + theme(legend.position = "none"), x=0, y=1.5, width = 1, height =  1) +
  draw_plot(fig2b_full + theme(legend.position = "none"), x=0, y=0.5, width = 1, height =  1) +
  draw_plot(legend2, x=0.05, y=0.3, width = 1, height = 0.2) +
  draw_plot_label(label = c("A", "B"), 
                  size = 15,
                  x = c(0, 0),
                  y = c(2.5, 1.5))
ggsave("./ms_plots/supplemental/Figure2_full.tif", 
       width = 180,
       height = 180,
       units = "mm",
       dpi = 300)


#Fig4 full
fig4a_full = full.outputs %>% 
  filter(preference == 1,
         mem == 100) %>% # data keeping preference and memory constant
  data_summary(varname = "scld.postXfor.str", 
               groupnames = c("attention", "groupsize", "approachfood")) %>%
  ggplot(aes(x=attention, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Attention on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 2 and 3 \n- Scrounging Enabled"), 
    x = "Attention", 
    y = "Change in \nstrength",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))


fig4b_full = full.outputs %>% 
  filter(attention == 1,
         mem == 100) %>% # data keeping attention and memory constant
  data_summary(varname = "scld.postXfor.str", 
               groupnames = c("preference", "groupsize", "approachfood")) %>%
  ggplot(aes(x=preference, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Preference on Median Change \nin Producer's Strength (scaled by groupsize) \nbetween phases 2 and 3 \n- Scrounging Enabled"), 
    x = "Preference", 
    y = "Change in \nstrength",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))

ggdraw(xlim = c(0, 1), ylim = c(0, 2.5)) + #initialize empty canvas
  draw_plot(fig4a_full + theme(legend.position = "none"), x=0, y=1.5, width = 1, height =  1) +
  draw_plot(fig4b_full + theme(legend.position = "none"), x=0, y=0.5, width = 1, height =  1) +
  draw_plot(legend4, x=0.05, y=0.3, width = 1, height = 0.2) +
  draw_plot_label(label = c("A", "B"), 
                  size = 15,
                  x = c(0, 0),
                  y = c(2.5, 1.5))
ggsave("./ms_plots/supplemental/Figure4_full.tif", 
       width = 180,
       height = 180,
       units = "mm",
       dpi = 300)


#Fig6 full
fig6a_full = full.outputs %>% 
  filter(preference == 1,
         mem == 100) %>% # data keeping preference and memory constant
  data_summary(varname = "med.run.energy", 
               groupnames = c("attention", "groupsize", "approachfood")) %>%
  ggplot(aes(x=attention, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Attention on Median Scrounger Energy \n- Scrounging Enabled"), 
    x = "Attention", 
    y = "Scrounger energy",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))

fig6b_full = full.outputs %>% 
  filter(attention == 1,
         mem == 100) %>% # data keeping attention and memory constant
  data_summary(varname = "med.run.energy", 
               groupnames = c("preference", "groupsize", "approachfood")) %>%
  ggplot(aes(x=preference, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=.05) +
  labs(#title=paste0("Effect of Preference on Median Scrounger energy \n- Scrounging Enabled"), 
    x = "Preference", 
    y = "Scrounger energy",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))

fig6c_full = full.outputs %>% 
  filter(attention == 1,
         preference == 1) %>% # data keeping attention and memory constant
  data_summary(varname = "med.run.energy", 
               groupnames = c("mem", "groupsize", "approachfood")) %>%
  ggplot(aes(x=mem, y=median, group=groupsize)) + 
  geom_line(aes(color = factor(groupsize))) +
  geom_point(aes(color = factor(groupsize)))+
  facet_grid(cols = vars(approachfood),
             labeller = labeller(approachfood = c("FALSE" = "Asocial-information Disabled", "TRUE" = "Asocial-information Enabled")))+
  geom_errorbar(aes(ymin=lowquant, ymax=highquant, color = factor(groupsize)), width=10) +
  labs(#title=paste0("Effect of Memory on Median Scrounger energy \n- Scrounging Enabled"), 
    x = "Memory", 
    y = "Scrounger energy",
    color = "Group size")+
  theme_classic() +
  scale_color_manual(values = c("3" = "gold2", "6" = "darkorange", "10" = "red1", "15" = "red4"))


ggdraw(xlim = c(0, 1), ylim = c(0, 3.5)) + #initialize empty canvas
  draw_plot(fig6a_full + theme(legend.position = "none"), x=0, y=2.5, width = 1, height =  1) +
  draw_plot(fig6b_full + theme(legend.position = "none"), x=0, y=1.5, width = 1, height =  1) +
  draw_plot(fig6c_full + theme(legend.position = "none"), x=0, y=0.5, width = 1, height =  1) +
  draw_plot(legend6, x=0.05, y=0.3, width = 1, height = 0.2) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 15,
                  x = c(0, 0, 0),
                  y = c(3.5, 2.5, 1.5))
ggsave("./ms_plots/supplemental/Figure6_full.tif", 
       width = 180,
       height = 180,
       units = "mm",
       dpi = 300)



####resets and energy by group size ####

#THESE PLOTS COME FROM THE "quail_centrality_4_50_analysis.Rmd" script
legend_scrounging = cowplot::get_plot_component(resets_by_gsize, 'guide-box-bottom', return_all = TRUE)
ggdraw(xlim = c(0, 1), ylim = c(0, 1.5)) + #initialize empty canvas
  draw_plot(resets_by_gsize + theme(legend.position = "none"), x=0, y=0.5, width = 0.5, height =  1) +
  draw_plot(med.energy.by.gsize + theme(legend.position = "none"), x=0.5, y=0.5, width = 0.5, height = 1) +
  draw_plot(legend_scrounging, x=0.05, y=0.45, width = 1, height = 0.2, scale = 1) +
  draw_plot_label(label = c("A", "B"), 
                  size = 15,
                  x = c(0, 0.5),
                  y = c(1.5, 1.5))
ggsave("./ms_plots/supplemental/resets_and_energy_by_group_size.tif", 
       width=180, 
       height=160,
       units = "mm",
       dpi = 300)

