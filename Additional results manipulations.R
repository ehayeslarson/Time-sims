# Program name: Results manipulation.R
# Created: 11/7/18
# Last edit: 11/7/18
# Author: Eleanor Hayes-Larson

##The purpose of this program is to load the results of the simulations from the Run Simulations.R program
# and use datasets to supplement existing results


#Install packages needed for code to run
library(ggplot2)
library(RColorBrewer)
library(dplyr)





#Run a set of simulations and keep individual-level data so that I can show response types changing over time

#Call simulation function code. 
source("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Simulation function.R")

Rlist<-list(Rchangeaccelup,Rchangelinup,Rchangenone,Rchangelindown,Rchangeacceldown) 
Rlistchar<-c("Rchangeaccelup","Rchangelinup","Rchangenone","Rchangelindown","Rchangeacceldown") 

U2list<-list(U2changeaccelup, U2changelinup, U2changenone, U2changelindown, U2changeacceldown) 
U2listchar<-c( "U2changeaccelup", "U2changelinup","U2changenone","U2changelindown", "U2changeacceldown") #, 



#Loop over values of Rchangefunc and save response types
RTs<-list()
for(i in 1:length(Rlist)){
    
    #only need first dataset to get response types, so nIterations=1
    indiv_sims <- simrun(Rprev=0.8, U1prev=1, U2prev=0.5, Rchangefunc=Rlist[[i]], U2changefunc=U2changenone, 
                   t2=12, t3=60, t4=120, t5=240, sampsize=100000, nIterations=5)$samp_dataset
    
    #Assign resposne types at each time point
        indiv_sims$RT_t1<-ifelse(indiv_sims$Y1==1 & indiv_sims$Y0==1,"Doomed",
                                ifelse(indiv_sims$Y1==1 & indiv_sims$Y0==0, "Causal",
                                       ifelse(indiv_sims$Y1==0 & indiv_sims$Y0==1, "Preventive",
                                              ifelse(indiv_sims$Y1==0 & indiv_sims$Y0==0,"Immune","Error"))))
        
        indiv_sims$RT_t2<-ifelse(indiv_sims$Y1_t2==1 & indiv_sims$Y0_t2==1,"Doomed",
                                 ifelse(indiv_sims$Y1_t2==1 & indiv_sims$Y0_t2==0, "Causal",
                                        ifelse(indiv_sims$Y1_t2==0 & indiv_sims$Y0_t2==1, "Preventive",
                                               ifelse(indiv_sims$Y1_t2==0 & indiv_sims$Y0_t2==0,"Immune","Error"))))
        
        indiv_sims$RT_t3<-ifelse(indiv_sims$Y1_t3==1 & indiv_sims$Y0_t3==1,"Doomed",
                                 ifelse(indiv_sims$Y1_t3==1 & indiv_sims$Y0_t3==0, "Causal",
                                        ifelse(indiv_sims$Y1_t3==0 & indiv_sims$Y0_t3==1, "Preventive",
                                               ifelse(indiv_sims$Y1_t3==0 & indiv_sims$Y0_t3==0,"Immune","Error"))))
        
        indiv_sims$RT_t4<-ifelse(indiv_sims$Y1_t4==1 & indiv_sims$Y0_t4==1,"Doomed",
                                 ifelse(indiv_sims$Y1_t4==1 & indiv_sims$Y0_t4==0, "Causal",
                                        ifelse(indiv_sims$Y1_t4==0 & indiv_sims$Y0_t4==1, "Preventive",
                                               ifelse(indiv_sims$Y1_t4==0 & indiv_sims$Y0_t4==0,"Immune","Error"))))
        
        indiv_sims$RT_t5<-ifelse(indiv_sims$Y1_t5==1 & indiv_sims$Y0_t5==1,"Doomed",
                                 ifelse(indiv_sims$Y1_t5==1 & indiv_sims$Y0_t5==0, "Causal",
                                        ifelse(indiv_sims$Y1_t5==0 & indiv_sims$Y0_t5==1, "Preventive",
                                               ifelse(indiv_sims$Y1_t5==0 & indiv_sims$Y0_t5==0,"Immune","Error"))))
    #Summarize response types
    response_types<-c("Doomed","Causal","Immune")
      response_types<-data.frame(response_types)

      response_types$t_1[1]<-sum(indiv_sims$RT_t1 == "Doomed")
      response_types$t_1[2]<-sum(indiv_sims$RT_t1 == "Causal")
      response_types$t_1[3]<-sum(indiv_sims$RT_t1 == "Immune")
      
      response_types$t_2[1]<-sum(indiv_sims$RT_t2 == "Doomed")
      response_types$t_2[2]<-sum(indiv_sims$RT_t2 == "Causal")
      response_types$t_2[3]<-sum(indiv_sims$RT_t2 == "Immune")
      
      response_types$t_3[1]<-sum(indiv_sims$RT_t3 == "Doomed")
      response_types$t_3[2]<-sum(indiv_sims$RT_t3 == "Causal")
      response_types$t_3[3]<-sum(indiv_sims$RT_t3 == "Immune")
      
      response_types$t_4[1]<-sum(indiv_sims$RT_t4 == "Doomed")
      response_types$t_4[2]<-sum(indiv_sims$RT_t4 == "Causal")
      response_types$t_4[3]<-sum(indiv_sims$RT_t4 == "Immune")
      
      response_types$t_5[1]<-sum(indiv_sims$RT_t5 == "Doomed")
      response_types$t_5[2]<-sum(indiv_sims$RT_t5 == "Causal")
      response_types$t_5[3]<-sum(indiv_sims$RT_t5 == "Immune")
      
    
      response_types2<-reshape(as.data.frame(response_types), varying=c(2:6), v.names="Frequency",direction="long", sep="_")
      response_types2$months<-ifelse(response_types2$time==1,0,
                                 ifelse(response_types2$time==2,12,
                                        ifelse(response_types2$time==3, 60,
                                               ifelse(response_types2$time==4,120,
                                                      ifelse(response_types2$time==5,240,"N/A")))))
    
      response_types2$sim<-paste(Rlistchar[i], "U2changenone", sep = "_")
    
      RTs[[i]]<-response_types2
  
}
      
big_data = do.call(rbind, RTs)
big_data$sim_f = factor(big_data$sim, levels=c('Rchangeaccelup_U2changenone',
                                               'Rchangelinup_U2changenone','Rchangenone_U2changenone',
                                               'Rchangelindown_U2changenone','Rchangeacceldown_U2changenone'))

big_data$percent<-(big_data$Frequency/100000)

big_data$response_types<-relevel(big_data$response_types, ref="Doomed")

labeltest <- c(
  Rchangeaccelup_U2changenone = "A) Exponential increase in R",
  Rchangelinup_U2changenone = "B) Linear increase in R",
  Rchangenone_U2changenone= "C) No change in R",
  Rchangelindown_U2changenone = "D) Linear decrease in R",
  Rchangeacceldown_U2changenone = "E) Exponential decrease in R"
)

#Create labels for panels in faceted graphs below
letters<-LETTERS[seq( from = 1, to = 5 )]
labelset<- data.frame(Rprev_base=rep(0.8,5))
labelset$sim_f<-factor(c('Rchangeaccelup_U2changenone',
                         'Rchangelinup_U2changenone','Rchangenone_U2changenone',
                         'Rchangelindown_U2changenone','Rchangeacceldown_U2changenone'))
labelset$letters<-letters
labelset$months<-0
labelset$percent<-.7


  plot1<-ggplot(big_data)+ aes(x=as.numeric(months), y=percent, color = response_types, ) + geom_point(size=2) + geom_line(size=1.25)+
    xlab("Time (months)") +labs(color = "Response type")+
    theme_bw()+  facet_wrap( ~ sim_f, ncol=1, labeller=labeller(sim_f=labeltest))+
    scale_y_continuous(name="Distribution of response types", labels = scales::percent_format(accuracy=1), limits=c(0,0.6))+
    theme(
      axis.text.x = element_text(size=12), 
      axis.text.y = element_text(size=12), 
      axis.title.x = element_text(size=16), 
      axis.title.y = element_text(size=16),
      strip.text.x = element_text(size=10),
      legend.position = "right")#+
  #geom_text(aes(x=months, y=percent, label=letters), data=labelset, inherit.aes=FALSE) 
    
  plot1

  ggsave("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Figures/RTs_U2changenone.jpg", 
         plot=plot1, dpi="retina")

  
#  #Repeat all above for U2changeaccelup
#   RT2s<-list()
#   for(i in 1:length(Rlist)){
# 
#     #only need first dataset to get response types, so nIterations=1
#     indiv_sims <- simrun(Rprev=0.5, U1prev=1, U2prev=0.01, Rchangefunc=Rlist[[i]], U2changefunc=U2changeaccelup, 
#                          t2=12, t3=60, t4=120, t5=240, sampsize=1000, nIterations=5)$samp_dataset
#     
#     #Assign resposne types at each time point
#     indiv_sims$RT_t1<-ifelse(indiv_sims$Y1==1 & indiv_sims$Y0==1,"Doomed",
#                              ifelse(indiv_sims$Y1==1 & indiv_sims$Y0==0, "Causal",
#                                     ifelse(indiv_sims$Y1==0 & indiv_sims$Y0==1, "Preventive",
#                                            ifelse(indiv_sims$Y1==0 & indiv_sims$Y0==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t2<-ifelse(indiv_sims$Y1_t2==1 & indiv_sims$Y0_t2==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t2==1 & indiv_sims$Y0_t2==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t2==0 & indiv_sims$Y0_t2==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t2==0 & indiv_sims$Y0_t2==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t3<-ifelse(indiv_sims$Y1_t3==1 & indiv_sims$Y0_t3==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t3==1 & indiv_sims$Y0_t3==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t3==0 & indiv_sims$Y0_t3==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t3==0 & indiv_sims$Y0_t3==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t4<-ifelse(indiv_sims$Y1_t4==1 & indiv_sims$Y0_t4==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t4==1 & indiv_sims$Y0_t4==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t4==0 & indiv_sims$Y0_t4==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t4==0 & indiv_sims$Y0_t4==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t5<-ifelse(indiv_sims$Y1_t5==1 & indiv_sims$Y0_t5==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t5==1 & indiv_sims$Y0_t5==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t5==0 & indiv_sims$Y0_t5==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t5==0 & indiv_sims$Y0_t5==0,"Immune","Error"))))
#     #Summarize response types
#     response_types<-c("Doomed","Causal","Immune")
#     response_types<-data.frame(response_types)
#     
#     response_types$t_1[1]<-sum(indiv_sims$RT_t1 == "Doomed")
#     response_types$t_1[2]<-sum(indiv_sims$RT_t1 == "Causal")
#     response_types$t_1[3]<-sum(indiv_sims$RT_t1 == "Immune")
#     
#     response_types$t_2[1]<-sum(indiv_sims$RT_t2 == "Doomed")
#     response_types$t_2[2]<-sum(indiv_sims$RT_t2 == "Causal")
#     response_types$t_2[3]<-sum(indiv_sims$RT_t2 == "Immune")
#     
#     response_types$t_3[1]<-sum(indiv_sims$RT_t3 == "Doomed")
#     response_types$t_3[2]<-sum(indiv_sims$RT_t3 == "Causal")
#     response_types$t_3[3]<-sum(indiv_sims$RT_t3 == "Immune")
#     
#     response_types$t_4[1]<-sum(indiv_sims$RT_t4 == "Doomed")
#     response_types$t_4[2]<-sum(indiv_sims$RT_t4 == "Causal")
#     response_types$t_4[3]<-sum(indiv_sims$RT_t4 == "Immune")
#     
#     response_types$t_5[1]<-sum(indiv_sims$RT_t5 == "Doomed")
#     response_types$t_5[2]<-sum(indiv_sims$RT_t5 == "Causal")
#     response_types$t_5[3]<-sum(indiv_sims$RT_t5 == "Immune")
#     
#     
#     response_types2<-reshape(as.data.frame(response_types), varying=c(2:6), v.names="Frequency",direction="long", sep="_")
#     response_types2$months<-ifelse(response_types2$time==1,0,
#                                    ifelse(response_types2$time==2,12,
#                                           ifelse(response_types2$time==3, 60,
#                                                  ifelse(response_types2$time==4,120,
#                                                         ifelse(response_types2$time==5,240,"N/A")))))
#     
#     response_types2$sim<-paste(Rlistchar[i], "U2changeaccelup", sep = "_")
#     
#     RT2s[[i]]<-response_types2
#     
#   }
#   
#   big_data2 = do.call(rbind, RT2s)
#   big_data2$sim_f = factor(big_data2$sim, levels=c('Rchangeaccelup_U2changeaccelup',
#                                                  'Rchangelinup_U2changeaccelup','Rchangenone_U2changeaccelup',
#                                                  'Rchangelindown_U2changeaccelup','Rchangeacceldown_U2changeaccelup'))
# 
#   
#   labeltest2 <- c(
#     Rchangeaccelup_U2changeaccelup = "Exponential increase in R",
#     Rchangelinup_U2changeaccelup = "Linear increase in R",
#     Rchangenone_U2changeaccelup= "No change in R",
#     Rchangelindown_U2changeaccelup = "Linear decrease in R",
#     Rchangeacceldown_U2changeaccelup = "Exponential decrease in R"
#   )
#   
#   #Create labels for panels in faceted graphs below
#   labelset2<- data.frame(Rprev_base=rep(0.8,5))
#   labelset2$sim_f<-factor(c('Rchangeaccelup_U2changeaccelup',
#                            'Rchangelinup_U2changeaccelup','Rchangenone_U2changeaccelup',
#                            'Rchangelindown_U2changeaccelup','Rchangeacceldown_U2changeaccelup'))
#   labelset2$letters<-letters
#   labelset2$months<-0
#   labelset2$Frequency<-900
#   
#   
#   plot2<-ggplot(big_data2)+ aes(x=as.numeric(months), y=Frequency, fill = response_types) + geom_col(position = "stack") +
#     xlab("Time (months)") + ylab("Frequency of Response Types")+  scale_fill_brewer(name="Response Types", palette="Dark2") +
#     theme_minimal()+facet_grid(sim_f~., labeller=labeller(sim_f=labeltest2))+
#     theme(
#       axis.text.x = element_text(size=12), 
#       axis.text.y = element_text(size=12), 
#       axis.title.x = element_text(size=16), 
#       axis.title.y = element_text(size=16),
#       strip.text.x = element_text(size=16))+
#     geom_text(aes(x=months, y=Frequency, label=letters), data=labelset2, inherit.aes=FALSE) 
#   
#   
#   plot2
#   
#   ggsave("/Users/Eleanor/Documents/Columbia - Dissertation/Aim 2/Results/Results plots/RTs_U2changeacceldown.jpg", 
#          plot=plot2, width = 30, height = 30, units = "cm", dpi="retina")
#   
#   
#   
# #Repeat all above for R constant, U2 change.
#   RT3s<-list()
#   for(i in 1:length(Rlist)){
#     
#     #only need first dataset to get response types, so nIterations=1
#     indiv_sims <- simrun(Rprev=0.8, U1prev=1, U2prev=0.5, Rchangefunc=Rchangenone, U2changefunc=U2list[[i]], 
#                          t2=12, t3=60, t4=120, t5=240, sampsize=1000, nIterations=5)$samp_dataset
#     
#     #Assign resposne types at each time point
#     indiv_sims$RT_t1<-ifelse(indiv_sims$Y1==1 & indiv_sims$Y0==1,"Doomed",
#                              ifelse(indiv_sims$Y1==1 & indiv_sims$Y0==0, "Causal",
#                                     ifelse(indiv_sims$Y1==0 & indiv_sims$Y0==1, "Preventive",
#                                            ifelse(indiv_sims$Y1==0 & indiv_sims$Y0==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t2<-ifelse(indiv_sims$Y1_t2==1 & indiv_sims$Y0_t2==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t2==1 & indiv_sims$Y0_t2==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t2==0 & indiv_sims$Y0_t2==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t2==0 & indiv_sims$Y0_t2==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t3<-ifelse(indiv_sims$Y1_t3==1 & indiv_sims$Y0_t3==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t3==1 & indiv_sims$Y0_t3==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t3==0 & indiv_sims$Y0_t3==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t3==0 & indiv_sims$Y0_t3==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t4<-ifelse(indiv_sims$Y1_t4==1 & indiv_sims$Y0_t4==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t4==1 & indiv_sims$Y0_t4==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t4==0 & indiv_sims$Y0_t4==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t4==0 & indiv_sims$Y0_t4==0,"Immune","Error"))))
#     
#     indiv_sims$RT_t5<-ifelse(indiv_sims$Y1_t5==1 & indiv_sims$Y0_t5==1,"Doomed",
#                              ifelse(indiv_sims$Y1_t5==1 & indiv_sims$Y0_t5==0, "Causal",
#                                     ifelse(indiv_sims$Y1_t5==0 & indiv_sims$Y0_t5==1, "Preventive",
#                                            ifelse(indiv_sims$Y1_t5==0 & indiv_sims$Y0_t5==0,"Immune","Error"))))
#     #Summarize response types
#     response_types<-c("Doomed","Causal","Immune")
#     response_types<-data.frame(response_types)
#     
#     response_types$t_1[1]<-sum(indiv_sims$RT_t1 == "Doomed")
#     response_types$t_1[2]<-sum(indiv_sims$RT_t1 == "Causal")
#     response_types$t_1[3]<-sum(indiv_sims$RT_t1 == "Immune")
#     
#     response_types$t_2[1]<-sum(indiv_sims$RT_t2 == "Doomed")
#     response_types$t_2[2]<-sum(indiv_sims$RT_t2 == "Causal")
#     response_types$t_2[3]<-sum(indiv_sims$RT_t2 == "Immune")
#     
#     response_types$t_3[1]<-sum(indiv_sims$RT_t3 == "Doomed")
#     response_types$t_3[2]<-sum(indiv_sims$RT_t3 == "Causal")
#     response_types$t_3[3]<-sum(indiv_sims$RT_t3 == "Immune")
#     
#     response_types$t_4[1]<-sum(indiv_sims$RT_t4 == "Doomed")
#     response_types$t_4[2]<-sum(indiv_sims$RT_t4 == "Causal")
#     response_types$t_4[3]<-sum(indiv_sims$RT_t4 == "Immune")
#     
#     response_types$t_5[1]<-sum(indiv_sims$RT_t5 == "Doomed")
#     response_types$t_5[2]<-sum(indiv_sims$RT_t5 == "Causal")
#     response_types$t_5[3]<-sum(indiv_sims$RT_t5 == "Immune")
#     
#     
#     response_types2<-reshape(as.data.frame(response_types), varying=c(2:6), v.names="Frequency",direction="long", sep="_")
#     response_types2$months<-ifelse(response_types2$time==1,0,
#                                    ifelse(response_types2$time==2,12,
#                                           ifelse(response_types2$time==3, 60,
#                                                  ifelse(response_types2$time==4,120,
#                                                         ifelse(response_types2$time==5,240,"N/A")))))
#     
#     response_types2$sim<-paste("Rchangenone", U2listchar[i], sep = "_")
#     
#     RT3s[[i]]<-response_types2
#     
#   }
#   
#   big_data3 = do.call(rbind, RT3s)
#   big_data3$sim_f = factor(big_data3$sim, levels=c('Rchangenone_U2changeaccelup',
#                                                    'Rchangenone_U2changelinup','Rchangenone_U2changenone',
#                                                    'Rchangenone_U2changelindown','Rchangenone_U2changeacceldown'))
#   
#   
#   labeltest3 <- c(
#     Rchangenone_U2changeaccelup = "Exponential increase in U2",
#     Rchangenone_U2changelinup = "Linear increase in U2",
#     Rchangenone_U2changenone= "No change in U2",
#     Rchangenone_U2changelindown = "Linear decrease in U2",
#     Rchangenone_U2changeacceldown = "Exponential decrease in U2"
#   )
#   
#   #Create labels for panels in faceted graphs below
#   labelset3<- data.frame(Rprev_base=rep(0.8,5))
#   labelset3$sim_f<-factor(c('Rchangenone_U2changeaccelup',
#                             'Rchangenone_U2changelinup','Rchangenone_U2changenone',
#                             'Rchangenone_U2changelindown','Rchangenone_U2changeacceldown'))
#   labelset3$letters<-letters
#   labelset3$months<-0
#   labelset3$Frequency<-900
#   
#   plot3<-ggplot(big_data3)+ aes(x=as.numeric(months), y=Frequency, fill = response_types) + geom_col(position = "stack") +
#     xlab("Time (months)") + ylab("Frequency of Response Types")+  scale_fill_brewer(name="Response Types", palette="Dark2") +
#     theme_minimal()+facet_grid(sim_f~., labeller=labeller(sim_f=labeltest3))+
#     
#     theme(
#       axis.text.x = element_text(size=12), 
#       axis.text.y = element_text(size=12), 
#       axis.title.x = element_text(size=16), 
#       axis.title.y = element_text(size=16),
#       strip.text.x = element_text(size=16))+
#     geom_text(aes(x=months, y=Frequency, label=letters), data=labelset3, inherit.aes=FALSE) 
#   
#   
#   plot3
#   
#   ggsave("/Users/Eleanor/Documents/Columbia - Dissertation/Aim 2/Results/Results plots/RTs_Rchangenone.jpg", 
#          plot=plot3, width = 30, height = 30, units = "cm", dpi="retina")
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   #if i want to have the error reduced later. 
#   response_types$t_1[1]<-round(sum(indiv_sims$RT_t1 == "Doomed")/10,-1)
#   response_types$t_1[2]<-round(sum(indiv_sims$RT_t1 == "Causal")/10,-1)
#   response_types$t_1[3]<-round(sum(indiv_sims$RT_t1 == "Immune")/10,-1)
#   
#   response_types$t_2[1]<-round(sum(indiv_sims$RT_t2 == "Doomed")/10,-1)
#   response_types$t_2[2]<-round(sum(indiv_sims$RT_t2 == "Causal")/10,-1)
#   response_types$t_2[3]<-round(sum(indiv_sims$RT_t2 == "Immune")/10,-1)
#   
#   response_types$t_3[1]<-round(sum(indiv_sims$RT_t3 == "Doomed")/10,-1)
#   response_types$t_3[2]<-round(sum(indiv_sims$RT_t3 == "Causal")/10,-1)
#   response_types$t_3[3]<-round(sum(indiv_sims$RT_t3 == "Immune")/10,-1)
#   
#   response_types$t_4[1]<-round(sum(indiv_sims$RT_t4 == "Doomed")/10,-1)
#   response_types$t_4[2]<-round(sum(indiv_sims$RT_t4 == "Causal")/10,-1)
#   response_types$t_4[3]<-round(sum(indiv_sims$RT_t4 == "Immune")/10,-1)
#   
#   response_types$t_5[1]<-round(sum(indiv_sims$RT_t5 == "Doomed")/10,-1)
#   response_types$t_5[2]<-round(sum(indiv_sims$RT_t5 == "Causal")/10,-1)
#   response_types$t_5[3]<-round(sum(indiv_sims$RT_t5 == "Immune")/10,-1)
  