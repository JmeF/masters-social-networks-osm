options(scipen = 10)


library(ggplot2)
library(scales)
library(lubridate)
library(gridExtra)
library(rBDA)

# library(dplyr)


#### READ DATA ####

all_df = read.csv("final_network_output.csv",stringsAsFactors = FALSE, header = TRUE)

#names
names(all_df)[names(all_df) == 'X'] <- 'start_date'

#make some dates
all_df$start_date <- as.POSIXct(all_df$start_date)
all_df$end_date <- all_df$start_date + duration(30, 'days')

###### PLOTTING FUNCTIONS #######
#function for plotting variable
plot.time <- function(var1,var1_name,yseq,ylim,data = all_df, date = all_df$end_date,
                      smooth_num = 15, colour = 'red'){
  
  #smooth_line
  smooth = data.frame(time = all_df$end_date,
                      y = forecast::ma(x = var1, order = smooth_num))
  #actual plot
  time_plot <- ggplot() +
    geom_line(data = data,
              aes(x = date,
                  y = var1),
              color = colour,
              size = 0.75,
              alpha = 0.25) +
    geom_line(data = smooth,
              aes(x = time,
                  y = y),
              color = colour,
              size = 1) + 
    # scale_color_viridis_d(name = "Year",option = "A",begin = 0.2,end = 1) +
    scale_x_datetime("Year",date_breaks = "1 year",date_labels = "%Y-%m") +
    # ylab(var1_name) +
    scale_y_continuous(var1_name,breaks = yseq,limits = ylim) +
    ggtitle(paste(var1_name,"between 2008 and 2019")) +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          # axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  time_plot
  return(time_plot)
}

#function to plot proportions
plot.per.time <- function(var1,var1_name, var2, var2_name,ylabel = "Proportion", data = all_df, date = all_df$end_date,smooth_num = 15){
  
  #smooth_line
  var1_smooth = data.frame(time = all_df$end_date,
                      y = forecast::ma(x = var1, order = smooth_num))
  var2_smooth = data.frame(time = all_df$end_date,
                           y = forecast::ma(x = var2, order = smooth_num))
  #actual plot
  time_plot <- ggplot() +
    geom_line(data = data,
              aes(x = date,
                  y = var1),
              color = 'lightblue',
              size = 0.5,
              alpha = 0.3) +
    geom_line(data = data,
              aes(x = date,
                  y = var2),
              color = 'lightgreen',
              size = 0.5,
              alpha = 0.3) +
    geom_line(data = var1_smooth,
              aes(x = time,
                  y = y,
                  color = 'one'), #
              # color = 'blue',
              size = 1) +
    geom_line(data = var2_smooth,
              aes(x = time,
                  y = y,
                  color = 'two'), #var2_name
              # color = 'green',
              size = 1) + 
    scale_color_manual(name="Legend",labels = c(var1_name,var2_name),values = c('one' = 'blue','two' = 'green')) +
    # scale_color_viridis_d(name = "Year",option = "A",begin = 0.2,end = 1) +
    scale_x_datetime("Year",date_breaks = "1 year",date_labels = "%Y-%m") +
    ylab(ylabel) +
    # scale_y_continuous("Proportion of topics created",breaks = seq(0,0.08,0.01),limits = c(0,0.08)) +
    ggtitle(paste(ylabel,"of",var1_name,"and",var2_name,"between 2008 and 2019")) +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          # axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  time_plot
  return(time_plot)
}

#plot 3 jaccard
plot.jaccard <- function(var1,var1_name,
                          var2, var2_name,
                          var3, var3_name,
                          ylabel = "Proportion",
                          data = all_df, date = all_df$end_date,smooth_num = 15,
                         ylim = c(0.35,0.8)){
  
  #smooth_line
  var1_smooth = data.frame(time = all_df$end_date,
                           y = forecast::ma(x = var1, order = smooth_num))
  var2_smooth = data.frame(time = all_df$end_date,
                           y = forecast::ma(x = var2, order = smooth_num))
  var3_smooth = data.frame(time = all_df$end_date,
                           y = forecast::ma(x = var3, order = smooth_num))
  #actual plot
  time_plot <- ggplot() +
    geom_line(data = data,
              aes(x = date,
                  y = var1),
              color = 'lightblue',
              size = 0.5,
              alpha = 0.3) +
    geom_line(data = data,
              aes(x = date,
                  y = var2),
              color = 'lightgreen',
              size = 0.5,
              alpha = 0.3) +
    geom_line(data = data,
              aes(x = date,
                  y = var3),
              color = 'lightpink',
              size = 0.5,
              alpha = 0.3) +
    geom_line(data = var1_smooth,
              aes(x = time,
                  y = y,
                  color = 'one'), #
              # color = 'blue',
              size = 1) +
    geom_line(data = var2_smooth,
              aes(x = time,
                  y = y,
                  color = 'two'), #var2_name
              # color = 'green',
              size = 1) + 
    geom_line(data = var3_smooth,
              aes(x = time,
                  y = y,
                  color = 'three'), #var3_name
              # color = 'red',
              size = 1) + 
    scale_color_manual(name="Legend",labels = c('one' = var1_name, 'two' = var2_name, 'three'= var3_name),
                       values = c('one' = 'blue','two' = 'green','three'='red')) +
    # scale_color_viridis_d(name = "Year",option = "A",begin = 0.2,end = 1) +
    scale_x_datetime("Year",date_breaks = "1 year",date_labels = "%Y-%m") +
    ylab(ylabel) +
    scale_y_continuous("Jaccard similarity",breaks = seq(0,1,0.1),limits = ylim) +
    ggtitle(paste("Jaccard similarity score for",paste0(var2_name,","),"the",var3_name,"and the",var1_name,"between 2008 and 2019")) +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          # axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  time_plot
  return(time_plot)
}


#plot 3 stack
plot.stack <- function(var1,var1_name,
                         var2, var2_name,
                         var3, var3_name,
                         ylabel = "Proportion",
                         data = all_df, date = all_df$end_date,smooth_num = 15,
                         ylim = c(0,1)){

  #actual plot
  time_plot <- ggplot() +
    geom_ribbon(data = data,
              aes(x = date,
                  ymax = var1,
                  ymin = 0,
              fill = 'one'),
              size = 0.5,
              alpha = 0.8) +
    geom_ribbon(data = data,
              aes(x = date,
                  ymax = (var1 + var2),
                  ymin = var1,
              fill = 'two'),
              size = 0.5,
              alpha = 0.8) +
    geom_ribbon(data = data,
              aes(x = date,
                  ymax = (var1+var2+var3),
                  ymin = (var1 + var2),
              fill = 'three'),
              size = 0.5,
              alpha = 0.8) +
    scale_fill_manual(name="Legend",labels = c('one' = var1_name, 'two' = var2_name, 'three'= var3_name),
                       values = c('one' = 'blue','two' = 'green','three'='orange')) +
    # scale_color_viridis_d(name = "Year",option = "A",begin = 0.2,end = 1) +
    scale_x_datetime("Year",date_breaks = "1 year",date_labels = "%Y-%m") +
    ylab(ylabel) +
    scale_y_continuous("Proportion of participants",breaks = seq(0,1,0.1),limits = ylim) +
    ggtitle(paste("Proportion of participants in",var1_name,"vs",var2_name,"vs",var3_name,"between 2008 and 2019")) +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          # axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  time_plot
  return(time_plot)
}

plot.stack.smooth <- function(var1,var1_name,
                       var2, var2_name,
                       var3, var3_name,
                       ylabel = "Proportion",
                       data = all_df, date = all_df$end_date,smooth_num = 15,
                       ylim = c(0,1)){
  #smooth_line
  var1_smooth = data.frame(time1 = all_df$end_date,
                           var1_y = forecast::ma(x = all_df$X1_per_giant, order = smooth_num))
  var2_smooth = data.frame(time2 = all_df$end_date,
                           var2_y = forecast::ma(x = all_df$X1_per_other, order = smooth_num))
  var3_smooth = data.frame(time3 = all_df$end_date,
                           var3_y = forecast::ma(x = all_df$X1_per_iso, order = smooth_num))
  smooth_df<- cbind(var1_smooth,var2_smooth,var3_smooth)
  #actual plot
  time_plot <- ggplot() +
    geom_ribbon(data = smooth_df,
                aes(x = date,
                    ymax = var1_y,
                    ymin = 0),
                fill = 'blue',
                size = 0.5,
                alpha = 0.7) +
    geom_ribbon(data = smooth_df,
                aes(x = date,
                    ymax = (var1_y + var2_y),
                    ymin = var1_y),
                fill = 'green',
                size = 0.5,
                alpha = 0.7) +
    geom_ribbon(data = smooth_df,
                aes(x = date,
                    ymax = (var1_y+var2_y+var3_y),
                    ymin = (var1_y + var2_y)),
                fill = 'purple',
                size = 0.5,
                alpha = 0.7) +
    scale_x_datetime("Year",date_breaks = "1 year",date_labels = "%Y-%m") +
    ylab(ylabel) +
    scale_y_continuous("Jaccard similarity",breaks = seq(0,1,0.1),limits = ylim) +
    ggtitle(paste("Jaccard similarity score for",var1_name,", the",var2_name,"and the",var3_name,"between 2008 and 2019")) +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          # axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  time_plot
  return(time_plot)
}


# make summary

mean_summary <- function(var1,name){ 
  df = stats::aggregate(var1,by = list(year(all_df$end_date)),FUN = mean)
  colnames(df) = c("Year",name)
  return(df)
}

jac_mean_summary <- function(var1,name,num){ 
  df = stats::aggregate(na.omit(var1),by = list(year(all_df$end_date)[num:271]),FUN = mean)
  colnames(df) = c("Year",name)
  return(df)
}


##### Make some plots ######

### 1 -- Participation

#Number of nodes
node_plot = plot.time(all_df$X1_num_nodes ,"Number of participants",yseq=seq(0,900,100),
                      ylim = c(0,900))
node_plot
node_summary_df = mean_summary(all_df$X1_num_nodes ,"Number of participants")


# percentage giant
prop_LCC_plot = plot.time(all_df$X1_per_giant ,"Proportion in LCC",
                          yseq=seq(0,1,0.1),ylim = c(0.5,1),colour = "blue")
prop_LCC_plot

giant_summary_df = mean_summary(all_df$X1_per_giant ,"Proportion of participants in LCC")


# Those involved and ignored in decision making processes
#Other
all_df$X1_per_other = 1 - (all_df$X1_per_giant + all_df$X1_per_iso)
other_summary_df = mean_summary(all_df$X1_per_other ,"Proportion of participants not isolated or LCC")


# #small but steady decline in giant componenet and rise in isolate
# prop_iso_plot = plot.time(all_df$X1_per_iso ,"Proportion of isolated participants",yseq=seq(0,0.15,0.05),
#                           ylim = c(0,0.15))
# prop_iso_plot
iso_summary_df = mean_summary(all_df$X1_per_iso,"Proportion of isolated participants")

#Plot percentage isolate and giant 
# LCC_iso_plot = plot.per.time(all_df$X1_per_giant,"participants in LCC",all_df$X1_per_iso ,"isolated participants",smooth_num = 10)
# LCC_iso_plot

#Plot a stacked version
stack_plot = plot.stack(var1 = all_df$X1_per_giant,var1_name = "LCC",
           var2 = all_df$X1_per_other, var2_name = "Other",
            var3 = all_df$X1_per_iso , var3_name = "Isolates",
            ylim = c(0,1))

particip_summary_df = merge(node_summary_df,giant_summary_df)
particip_summary_df = merge(particip_summary_df,iso_summary_df)
particip_summary_df = merge(particip_summary_df,other_summary_df)
write.csv(particip_summary_df,file = "figs/1_participation.csv")

### Saving
figure1 = grid.arrange(grid.arrange(node_plot + theme(axis.text.x = element_text(angle = 45,hjust = 1)),
                                    prop_LCC_plot+ theme(axis.text.x = element_text(angle = 45,hjust = 1)),ncol = 2)
             ,stack_plot,heights = c(16,10), ncol = 1)


ggsave("figs/figure1.png",figure1, units = "cm",width = 18, height = 12)


##2.2

# Gini coefficient - all + unique
gini_cont_plot = plot.per.time(all_df$X2_giant_gini,"All interactions",all_df$X2_giant_gini_unweight,"Unique interactions",
                               ylabel= "Gini coefficient of interactions in LCC",smooth_num = 10) + 
  ggtitle("Gini coefficient of all interactions and unique interactions in LCC") +
  theme(legend.position = 'bottom')
gini_cont_plot

gini_all_summary_df = mean_summary(all_df$X2_giant_gini,"Gini coefficient of all interactions in LCC")
gini_uni_summary_df = mean_summary(all_df$X2_giant_gini_unweight,"Gini coefficient of unique interactions in LCC")

# per 10 -- all + unique
per10_cont_plot = plot.per.time(all_df$X2_top10_per_contrib,"All interactions",all_df$X2_top10_per_contrib_unweigh,"Unique interactions",
                               ylabel= "Proportion of interactions in LCC by top 10%",smooth_num = 10) + 
  ggtitle("Proportion of all interactions and unique interactions in LCC by top 10%") +
  theme(legend.position = 'bottom')
per10_cont_plot

per10_all_summary_df = mean_summary(all_df$X2_top10_per_contrib,"Proportion of all interactions by top 10% contributors")
per10_uni_summary_df = mean_summary(all_df$X2_top10_per_contrib_unweigh,"Proportion of unique interactions by top 10% contributors")


ineq_summary2_df = merge(gini_all_summary_df,gini_uni_summary_df)
ineq_summary2_df = merge(ineq_summary2_df,per10_all_summary_df)
ineq_summary2_df = merge(ineq_summary2_df,per10_uni_summary_df)
write.csv(ineq_summary2_df,file = "figs/2_2_inequality2.csv")

figure2_22 = grid.arrange(gini_cont_plot +theme(legend.position = 'None'), #axis.text.x = element_text(angle = 45,hjust = 1
                         per10_cont_plot,heights = c(10,12), ncol = 1) #,heights = c(16,10)

ggsave("figs/figure2_22.png",figure2_22, units = "cm",width = 14, height = 12)


### 3 -- Meritocratic homophily

#degree assortativity
degree_assort_plot = plot.time(all_df$X3_giant_weightDA,"Assortativity by weighted degree",
          yseq=seq(-1,1,0.1),ylim = c(-.5,.1),colour = "red") + geom_hline(yintercept = 0)
degree_assort_plot
degree_assort_summary_df = mean_summary(all_df$X3_giant_weightDA,"Assortativity by weighted degree")


#edit assortativity
edit_assort_plot = plot.time(all_df$X3_num_edits_cat_assort_unfilt,"Assortativity by edit contributions",
          yseq=seq(-1,1,0.05),ylim = c(-.2,.05),colour = "blue") + geom_hline(yintercept = 0)
edit_assort_plot
edit_assort_summary_df = mean_summary(all_df$X3_num_edits_cat_assort_unfilt,"Assortativity by edit contributions")


#edit modularity
edit_mod_plot = plot.time(all_df$X3_edit_modularity,"Modularity by edit contributions",
                yseq=seq(-1,1,0.05),ylim = c(-.1,.05),colour = "green")  + geom_hline(yintercept = 0)
edit_mod_summary_df = mean_summary(all_df$X3_edit_modularity,"Modularity by edit contributions")

merit_summary_df = merge(degree_assort_summary_df,edit_assort_summary_df)
merit_summary_df = merge(merit_summary_df,edit_mod_summary_df)
write.csv(merit_summary_df,file = "figs/3_merit.csv")

figure3 = grid.arrange(grid.arrange(degree_assort_plot + 
                         theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
                         ggtitle("Assortativity by weighted degree"),
                       edit_assort_plot + 
                         theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
                         ggtitle("Assortativity by edit contribution"),ncol = 2),
                       edit_mod_plot,heights = c(16,10)) #+ 
                         # theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
                         # ggtitle("Modularity by edit contribution"),
                        #,heights = c(16,10)

ggsave("figs/figure3.png",figure3, units = "cm",width = 16, height = 12)

### 4 -- Decentralisation

# best partition
best_mod_plot = plot.time(all_df$X4_comm_modularity,"Modularity of best community partition (Louvain)",
                yseq=seq(-1,1,0.1),ylim = c(0.15,0.75),colour = "red")  #+ geom_hline(yintercept = 0)
best_mod_plot 
best_mod_summary_df = mean_summary(all_df$X4_comm_modularity,"Modularity of Louvain best community partition")


# Number of communities 
num_community_plot = plot.time(all_df$X4_num_communities,"Number of communities",
                      yseq=seq(0,15,3),ylim = c(0,15),colour = "blue")
num_community_plot
num_community_summary_df = mean_summary(all_df$X4_num_communities,"Number of communities identified by Louvain")

#community size inequality
comm_size_ineq_plot = plot.time(all_df$X4_comm_size_gini,"Gini inequality",
                      yseq=seq(-1,1,0.1),ylim = c(0.15,0.75),colour = "green")
comm_size_ineq_plot
comm_size_ineq_summary_df = mean_summary(all_df$X4_comm_size_gini,"Gini inequality between community sizes")

comm_summary_df = merge(num_community_summary_df,best_mod_summary_df)
comm_summary_df = merge(comm_summary_df,comm_size_ineq_summary_df)
write.csv(comm_summary_df,file = "figs/4_communities.csv")

figure4 = grid.arrange(best_mod_plot,
                       grid.arrange(num_community_plot+ 
                                      theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
                                      ggtitle("Number of communities"),
                                    comm_size_ineq_plot+ 
                                      theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
                                      ggtitle("Gini inequality in community size"),ncol=2),
                       nrow = 2,heights = c(16,10)) #,heights = c(16,10)

ggsave("figs/figure4.png",figure4, units = "cm",width = 16, height = 12)


### 5 - Persistence

#do plot with all three
jaccard_plot = plot.jaccard(var1 = all_df$X5_top10_nodes_jac_sim2,var1_name = "top 10%",
                            var2 = all_df$X5_all_nodes_jac_sim2, var2_name = "all contributors",
                            var3 = all_df$X5_giant_nodes_jac_sim2, var3_name = "LCC",
                            ylim = c(0.1,0.7))
jaccard_plot

ggsave("figs/figure5.png",jaccard_plot, units = "cm",width = 16, height = 10)


# #Summaries
jac_all_summary_df = jac_mean_summary(all_df$X5_all_nodes_jac_sim2,"Jaccard similarity of all contributors",3)
jac_LCC_summary_df = jac_mean_summary(all_df$X5_giant_nodes_jac_sim2,"Jaccard similarity of LCC",3)
jac_top10_summary_df = jac_mean_summary(all_df$X5_top10_nodes_jac_sim2,"Jaccard similarity of 10%",3)

jac_summary_df = merge(jac_all_summary_df,jac_LCC_summary_df)
jac_summary_df = merge(jac_summary_df,jac_top10_summary_df)
write.csv(jac_summary_df,file = "figs/5_similarity.csv")

