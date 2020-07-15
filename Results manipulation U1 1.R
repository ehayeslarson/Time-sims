# Program name: Results manipulation.R
# Created: 10/6/18
# Last edit: 10/12/18
# Author: Eleanor Hayes-Larson

##The purpose of this program is to load the results of the simulations from the Run Simulations.R program
  # and use datasets to develop figures and tables of results. 


#Install packages needed for code to run
library(ggplot2)
library(wesanderson)
library(RColorBrewer)

##################################################################################################
# Step 1
# Load all results files into program and combine into one data frame
##################################################################################################

  #load where U2 has no change
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangenone_U2changenone.RData")
    Results_Rchangenone_U2changenone<-Results2
    
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelinup_U2changenone.RData")
    Results_Rchangelinup_U2changenone<-Results2
    
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelindown_U2changenone.RData")
    Results_Rchangelindown_U2changenone<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeaccelup_U2changenone.RData")
    Results_Rchangeaccelup_U2changenone<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeacceldown_U2changenone.RData")
    Results_Rchangeacceldown_U2changenone<-Results2
    
  #load where U2 has linear increase
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangenone_U2changelinup.RData")
    Results_Rchangenone_U2changelinup<-Results2
 
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelinup_U2changelinup.RData")
    Results_Rchangelinup_U2changelinup<-Results2
    
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelindown_U2changelinup.RData")
    Results_Rchangelindown_U2changelinup<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeaccelup_U2changelinup.RData")
    Results_Rchangeaccelup_U2changelinup<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeacceldown_U2changelinup.RData")
    Results_Rchangeacceldown_U2changelinup<-Results2
    
  #load where U2 has linear decrease     
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangenone_U2changelindown.RData")
    Results_Rchangenone_U2changelindown<-Results2
    
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelinup_U2changelindown.RData")
    Results_Rchangelinup_U2changelindown<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelindown_U2changelindown.RData")
    Results_Rchangelindown_U2changelindown<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeaccelup_U2changelindown.RData")
    Results_Rchangeaccelup_U2changelindown<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeacceldown_U2changelindown.RData")
    Results_Rchangeacceldown_U2changelindown<-Results2

    
  #load where U2 has exponential increase
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangenone_U2changeaccelup.RData")
    Results_Rchangenone_U2changeaccelup<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelinup_U2changeaccelup.RData")
    Results_Rchangelinup_U2changeaccelup<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelindown_U2changeaccelup.RData")
    Results_Rchangelindown_U2changeaccelup<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeaccelup_U2changeaccelup.RData")
    Results_Rchangeaccelup_U2changeaccelup<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeacceldown_U2changeaccelup.RData")
    Results_Rchangeacceldown_U2changeaccelup<-Results2

    
  #load where U2 has exponential decrease  
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangenone_U2changeacceldown.RData")
    Results_Rchangenone_U2changeacceldown<-Results2
  
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelinup_U2changeacceldown.RData")
    Results_Rchangelinup_U2changeacceldown<-Results2
    
    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangelindown_U2changeacceldown.RData")
    Results_Rchangelindown_U2changeacceldown<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeaccelup_U2changeacceldown.RData")
    Results_Rchangeaccelup_U2changeacceldown<-Results2

    load("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Aim 2/Results/U1 1/Results_Rchangeacceldown_U2changeacceldown.RData")
    Results_Rchangeacceldown_U2changeacceldown<-Results2

    #Combine all results datasets
    Results_all<-do.call(rbind, lapply(ls(pattern = "^Results_Rch"), get))
    
    #Keep sim labels distinct
    Results_all$simid_all<-c(rep(seq(1,25),5), rep(seq(26,50),5), rep(seq(51,75),5), rep(seq(76,100),5), rep(seq(101,125),5),
                           rep(seq(126,150),5), rep(seq(151,175),5), rep(seq(176,200),5), rep(seq(201,225),5), rep(seq(226,250),5),
                           rep(seq(251,275),5), rep(seq(276,300),5), rep(seq(301,325),5), rep(seq(326,350),5), rep(seq(351,375),5),
                           rep(seq(376,400),5), rep(seq(401,425),5), rep(seq(426,450),5), rep(seq(451,475),5), rep(seq(476,500),5),
                           rep(seq(501,525),5), rep(seq(526,550),5), rep(seq(551,575),5), rep(seq(576,600),5), rep(seq(601,625),5))

    #substitute actual time instead of "t1, t2" etc.
    Results_all$months<-ifelse(Results_all$time=="t1",0,
                                ifelse(Results_all$time=="t2",12,
                                       ifelse(Results_all$time=="t3", 60,
                                              ifelse(Results_all$time=="t4",120,
                                                     ifelse(Results_all$time=="t5",240,"N/A")))))
    
    
  


##################################################################################################
# Graph results for absolute bias
##################################################################################################
    letters<-LETTERS[seq( from = 1, to = 25 )]
    labelset<- data.frame(expand.grid(Rprev_base=c(0.01, 0.2, 0.5,0.8,.99),U2prev_base=c(0.01, 0.2, 0.5,0.8,.99)))
    labelset$letters<-letters
    labelset$months<-15
    labelset$RDavg<-0.85
    labelset$Rchangefunc<-"Rchangenone"
    
    ##################################################################################################
    # Graph holding U2 change function constant
    ##################################################################################################

    #Set color palette for R change functions
    display.brewer.pal(9,"YlGnBu")
    p<-brewer.pal(9,"YlGnBu")
    p2<-p[c(3:7,9)]
    
    #Graph for no change in U2 over time
    Results_noU2change<-Results_all[Results_all$U2changefunc=="U2changenone",]
    
    noU2change<-ggplot(Results_noU2change, 
              aes(x=as.numeric(months), y=unlist(biasabs), color=factor(Rchangefunc, levels=c("Rchangeaccelup", "Rchangelinup", "Rchangenone", "Rchangelindown", "Rchangeacceldown")))) + 
      scale_color_manual(name="R change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=p2) + 
      geom_point()+geom_line(aes(group=Rchangefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
      facet_grid(as.numeric(U2prev_base)~.~as.numeric(Rprev_base), labeller=label_value)  +
      geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
      theme_bw()+
      theme(
        legend.position="bottom", 
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(1.25, "lines"))
    
      
      noU2change
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/noU2change.jpg", 
                plot=noU2change, width = 30, height = 30, units = "cm", dpi="retina")
      
    #Graph for linear increase in U2 over time
    Results_linupU2change<-Results_all[Results_all$U2changefunc=="U2changelinup",]
    
    linupU2change<-ggplot(Results_linupU2change, 
                      aes(x=as.numeric(months), y=unlist(biasabs), color=factor(Rchangefunc, levels=c("Rchangeaccelup", "Rchangelinup", "Rchangenone", "Rchangelindown", "Rchangeacceldown")))) + 
      scale_color_manual(name="R change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=p2) + 
      geom_point()+geom_line(aes(group=Rchangefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
      facet_grid(as.numeric(U2prev_base)~.~as.numeric(Rprev_base), labeller=label_value)  +
      geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
      theme_bw()+
      theme(
        legend.position="bottom", 
        axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(1.25, "lines"))
      linupU2change
    
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/linupU2change.jpg", 
             plot=linupU2change, width = 30, height = 30, units = "cm", dpi="retina")
      
      
      #Graph for exponential increase in U2 over time
      Results_accelupU2change<-Results_all[Results_all$U2changefunc=="U2changeaccelup",]
      
      accelupU2change<-ggplot(Results_accelupU2change, 
                            aes(x=as.numeric(months), y=unlist(biasabs), color=factor(Rchangefunc, levels=c("Rchangeaccelup", "Rchangelinup", "Rchangenone", "Rchangelindown", "Rchangeacceldown")))) + 
        scale_color_manual(name="R change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=p2) + 
        geom_point()+geom_line(aes(group=Rchangefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(as.numeric(U2prev_base)~.~as.numeric(Rprev_base), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      accelupU2change
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/accelupU2change.jpg", 
             plot=accelupU2change, width = 30, height = 30, units = "cm", dpi="retina")
      

      #Graph for linear decrease in U2 over time
      Results_lindownU2change<-Results_all[Results_all$U2changefunc=="U2changelindown",]
      
      lindownU2change<-ggplot(Results_lindownU2change, 
                            aes(x=as.numeric(months), y=unlist(biasabs), color=factor(Rchangefunc, levels=c("Rchangeaccelup", "Rchangelinup", "Rchangenone", "Rchangelindown", "Rchangeacceldown")))) + 
        scale_color_manual(name="R change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=p2) + 
        geom_point()+geom_line(aes(group=Rchangefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(as.numeric(U2prev_base)~.~as.numeric(Rprev_base), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      lindownU2change
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/lindownU2change.jpg", 
             plot=lindownU2change, width = 30, height = 30, units = "cm", dpi="retina")
      
      
      #Graph for exponential decrease in U2 over time
      Results_acceldownU2change<-Results_all[Results_all$U2changefunc=="U2changeacceldown",]
      
      acceldownU2change<-ggplot(Results_acceldownU2change, 
                              aes(x=as.numeric(months), y=unlist(biasabs), color=factor(Rchangefunc, levels=c("Rchangeaccelup", "Rchangelinup", "Rchangenone", "Rchangelindown", "Rchangeacceldown")))) + 
        scale_color_manual(name="R change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=p2) + 
        geom_point()+geom_line(aes(group=Rchangefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(as.numeric(U2prev_base)~.~as.numeric(Rprev_base), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      acceldownU2change
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/acceldownU2change.jpg", 
             plot=acceldownU2change, width = 30, height = 30, units = "cm", dpi="retina")

      
      ##################################################################################################
      # Graph holding R change function constant
      ##################################################################################################
      
      #Set color palette for U2 change functions
      display.brewer.pal(9,"YlOrRd")
      U2p<-brewer.pal(9,"YlOrRd")
      U2p2<-U2p[c(3:7,9)]
      
      
      #Graph for no change in R over time
      Results_noRchange<-Results_all[Results_all$Rchangefunc=="Rchangenone",]
      
      noRchange<-ggplot(Results_noRchange, 
                        aes(x=as.numeric(months), y=unlist(biasabs), color=factor(U2changefunc, levels=c("U2changeaccelup", "U2changelinup", "U2changenone", "U2changelindown", "U2changeacceldown")))) + 
        scale_color_manual(name="U2 change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=U2p2) + 
        geom_point()+geom_line(aes(group=U2changefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(as.numeric(U2prev_base)~.~as.numeric(Rprev_base), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      noRchange
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/noRchange.jpg", 
             plot=noRchange, width = 30, height = 30, units = "cm", dpi="retina")
      
      
      #Graph for linear increase in R over time
      Results_linupRchange<-Results_all[Results_all$Rchangefunc=="Rchangelinup",]
      
      linupRchange<-ggplot(Results_linupRchange, 
                        aes(x=as.numeric(months), y=unlist(biasabs), color=factor(U2changefunc, levels=c("U2changeaccelup", "U2changelinup", "U2changenone", "U2changelindown", "U2changeacceldown")))) + 
        scale_color_manual(name="U2 change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=U2p2) + 
        geom_point()+geom_line(aes(group=U2changefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(rows=vars(as.numeric(U2prev_base)), cols=vars(as.numeric(Rprev_base)), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      linupRchange
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/linupRchange.jpg", 
             plot=linupRchange, width = 30, height = 30, units = "cm", dpi="retina")
      
      
      #Graph for exponential increase in R over time
      Results_accelupRchange<-Results_all[Results_all$Rchangefunc=="Rchangeaccelup",]
      
      accelupRchange<-ggplot(Results_accelupRchange, 
                        aes(x=as.numeric(months), y=unlist(biasabs), color=factor(U2changefunc, levels=c("U2changeaccelup", "U2changelinup", "U2changenone", "U2changelindown", "U2changeacceldown")))) + 
        scale_color_manual(name="U2 change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=U2p2) + 
        geom_point()+geom_line(aes(group=U2changefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(rows=vars(as.numeric(U2prev_base)), cols=vars(as.numeric(Rprev_base)), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      
      accelupRchange
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/accelupRchange.jpg", 
             plot=accelupRchange, width = 30, height = 30, units = "cm", dpi="retina")
      
      
      #Graph for linear decrease in R over time
      Results_lindownRchange<-Results_all[Results_all$Rchangefunc=="Rchangelindown",]
      
      lindownRchange<-ggplot(Results_noRchange, 
                        aes(x=as.numeric(months), y=unlist(biasabs), color=factor(U2changefunc, levels=c("U2changeaccelup", "U2changelinup", "U2changenone", "U2changelindown", "U2changeacceldown")))) + 
        scale_color_manual(name="U2 change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=U2p2) + 
        geom_point()+geom_line(aes(group=U2changefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(rows=vars(as.numeric(U2prev_base)), cols=vars(as.numeric(Rprev_base)), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      
      lindownRchange
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/lindownRchange.jpg", 
             plot=lindownRchange, width = 30, height = 30, units = "cm", dpi="retina")
      
      
      #Graph for expoential decrease in R over time
      Results_acceldownRchange<-Results_all[Results_all$Rchangefunc=="Rchangeacceldown",]
      
      acceldownRchange<-ggplot(Results_acceldownRchange, 
                        aes(x=as.numeric(months), y=unlist(biasabs), color=factor(U2changefunc, levels=c("U2changeaccelup", "U2changelinup", "U2changenone", "U2changelindown", "U2changeacceldown")))) + 
        scale_color_manual(name="U2 change function", labels = c("Exponential increase", "Linear increase", "No change", "Linear decrease", "Exponential decrease"), values=U2p2) + 
        geom_point()+geom_line(aes(group=U2changefunc), size=1.5) + xlab("Time (months)") + ylab("Absolute bias in target intervention effect estimate") + ylim(-1, 1) +
        facet_grid(rows=vars(as.numeric(U2prev_base)), cols=vars(as.numeric(Rprev_base)), labeller=label_value)  +
        geom_text(aes(x=as.numeric(months), y=unlist(RDavg), label=letters), data=labelset, size=6,color="black", inherit.aes=T)+
        theme_bw()+
        theme(
          legend.position="bottom", 
          axis.text.x = element_text(size=14), 
          axis.text.y = element_text(size=14), 
          axis.title.x = element_text(size=20), 
          axis.title.y = element_text(size=20), 
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          panel.grid.minor = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(1.25, "lines"))
      
      
      acceldownRchange
      
      ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/acceldownRchange.jpg", 
             plot=acceldownRchange, width = 30, height = 30, units = "cm", dpi="retina")
      

      
    
      
    