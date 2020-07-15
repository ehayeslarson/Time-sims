# Program name: Simulation function.R
# Created: 10/6/18
# Last edit: 11/8/18
# Author: Eleanor Hayes-Larson

##The purpose of this program is to create and test the function that will run the simulations
  # The program calls the source code for the change functions program. 


#Install packages needed for code to run
library(dplyr)
library(reshape2)


#Need to run code for change functions, stored in another R script
source("/Users/Eleanor/MHL Dropbox/Eleanor Hayes-Larson/Columbia - Dissertation/Publication versions/Matter of time/Code/Change functions.R")


#create a function that will simulate a dataset and calculate the causal effect in that cohort.
simrun <- function(Rprev, U1prev, U2prev, Rchangefunc, U2changefunc, t2, t3, t4, t5, sampsize,
                    nIterations){
  #Set a seed. Because the seed is outside the loop, each set of parameters will start with the 
    #same seed but each simulation within a set of parameters will be distinct
  set.seed(12345)
  
  #create empty objects to store results of each simulation run
  RD_t1<-rep(NA,nIterations)
  RD_LCI_t1<-rep(NA,nIterations)
  RD_UCI_t1<-rep(NA,nIterations)
  RD_t2<-rep(NA,nIterations)
  RD_LCI_t2<-rep(NA,nIterations)
  RD_UCI_t2<-rep(NA,nIterations)
  RD_t3<-rep(NA,nIterations)
  RD_LCI_t3<-rep(NA,nIterations)
  RD_UCI_t3<-rep(NA,nIterations)
  RD_t4<-rep(NA,nIterations)
  RD_LCI_t4<-rep(NA,nIterations)
  RD_UCI_t4<-rep(NA,nIterations)
  RD_t5<-rep(NA,nIterations)
  RD_LCI_t5<-rep(NA,nIterations)
  RD_UCI_t5<-rep(NA,nIterations)
  
  #loop to run simulation over and over again (n) times
  for(i in 1:nIterations){
    n <- c(1:sampsize)
    dataset <- data.frame(n)
    
    #Simulate population by drawing from distributions according to prevalence of each component cause
       #Simulate population by drawing from distributions according to prevalence of each component cause
        dataset$I1 <- 1
        dataset$I0 <- 0
        dataset$R <-rbinom(sampsize,1,Rprev)
        dataset$U1 <-rbinom(sampsize,1,U1prev)
        dataset$U2 <-rbinom(sampsize,1,U2prev)
        dataset$Y1 <- ifelse(dataset$R==1 & dataset$U1==1 & dataset$I1==1 | dataset$U2==1, 1, 0)
        dataset$Y0 <- ifelse(dataset$R==1 & dataset$U1==1 & dataset$I0==1 | dataset$U2==1, 1, 0)
        
    #Calculate and store risk difference
        #Calculate risks,risk difference and 95% CI
        means <- dataset %>% summarize(risk1=mean(Y1),risk0=mean(Y0),A=sum(Y1), C=sum(Y0)) 
        means$B <- 1000-means$A
        means$D <- 1000-means$C
        means$rd <-means$risk1-means$risk0
        means$rd
        means$rdse <- sqrt((means$risk1*(1-means$risk1)/(means$A+means$B))+(means$risk0*(1-means$risk0)/(means$C+means$D)))
        means$rdse
        means$rdLci <-means$rd-1.96*means$rdse
        means$rdUci <-means$rd+1.96*means$rdse
        means$rdLci
        means$rdUci
    
        #Store risk difference and CI
        RD_t1[i]<-rename(select(means,rd),riskdiff = rd)
        RD_LCI_t1[i]<- rename(select(means,rdLci),rd_lci = rdLci)
        RD_UCI_t1[i]<- rename(select(means,rdUci),rd_uci = rdUci)
        
        
        #Simulate changes in R and U2 over time
        
        #Timepoint 2
          #Set prevalences
          Rprev_t2<-Rchangefunc(t2, Rprev)
          dataset$R_t2 <-rbinom(sampsize,1,Rprev_t2)
          U2prev_t2<-U2changefunc(t2, U2prev)
          dataset$U2_t2 <-rbinom(sampsize,1,U2prev_t2)
          
          #Calculate outcomes
          dataset$Y1_t2 <- ifelse(dataset$R_t2==1 & dataset$U1==1 & dataset$I1==1 | dataset$U2_t2==1, 1, 0)
          dataset$Y0_t2 <- ifelse(dataset$R_t2==1 & dataset$U1==1 & dataset$I0==1 | dataset$U2_t2==1, 1, 0)
        
          #Get RD and CI  
          means_t2 <- dataset %>% summarize(risk1=mean(Y1_t2),risk0=mean(Y0_t2),A=sum(Y1_t2), C=sum(Y0_t2)) 
          means_t2$B <- 1000-means_t2$A
          means_t2$D <- 1000-means_t2$C
          means_t2$rd <-means_t2$risk1-means_t2$risk0
          means_t2$rd
          means_t2$rdse <- sqrt((means_t2$risk1*(1-means_t2$risk1)/(means_t2$A+means_t2$B))+(means_t2$risk0*(1-means_t2$risk0)/(means_t2$C+means_t2$D)))
          means_t2$rdse
          means_t2$rdLci <-means_t2$rd-1.96*means_t2$rdse
          means_t2$rdUci <-means_t2$rd+1.96*means_t2$rdse
          means_t2$rdLci
          means_t2$rdUci
        
          #Store risk difference and CI
          RD_t2[i]<-rename(select(means_t2,rd),riskdiff_t2 = rd)
          RD_LCI_t2[i]<- rename(select(means_t2,rdLci),rd_lci_t2 = rdLci)
          RD_UCI_t2[i]<- rename(select(means_t2,rdUci),rd_uci_t2 = rdUci)
          
       #Timepoint 3
          #Set prevalences
          Rprev_t3<-Rchangefunc(t3, Rprev)
          dataset$R_t3 <-rbinom(sampsize,1,Rprev_t3)
          U2prev_t3<-U2changefunc(t3, U2prev)
          dataset$U2_t3 <-rbinom(sampsize,1,U2prev_t3)
          
          #Calculate outcomes
          dataset$Y1_t3 <- ifelse(dataset$R_t3==1 & dataset$U1==1 & dataset$I1==1 | dataset$U2_t3==1, 1, 0)
          dataset$Y0_t3 <- ifelse(dataset$R_t3==1 & dataset$U1==1 & dataset$I0==1 | dataset$U2_t3==1, 1, 0)
          
          #Get RD and CI  
          means_t3 <- dataset %>% summarize(risk1=mean(Y1_t3),risk0=mean(Y0_t3),A=sum(Y1_t3), C=sum(Y0_t3)) 
          means_t3$B <- 1000-means_t3$A
          means_t3$D <- 1000-means_t3$C
          means_t3$rd <-means_t3$risk1-means_t3$risk0
          means_t3$rd
          means_t3$rdse <- sqrt((means_t3$risk1*(1-means_t3$risk1)/(means_t3$A+means_t3$B))+(means_t3$risk0*(1-means_t3$risk0)/(means_t3$C+means_t3$D)))
          means_t3$rdse
          means_t3$rdLci <-means_t3$rd-1.96*means_t3$rdse
          means_t3$rdUci <-means_t3$rd+1.96*means_t3$rdse
          means_t3$rdLci
          means_t3$rdUci
          
          #Store risk difference and CI
          RD_t3[i]<-rename(select(means_t3,rd),riskdiff_t3 = rd)
          RD_LCI_t3[i]<- rename(select(means_t3,rdLci),rd_lci_t3 = rdLci)
          RD_UCI_t3[i]<- rename(select(means_t3,rdUci),rd_uci_t3 = rdUci)
          
      #Timepoint 4
          #Set prevalences
          Rprev_t4<-Rchangefunc(t4, Rprev)
          dataset$R_t4 <-rbinom(sampsize,1,Rprev_t4)
          U2prev_t4<-U2changefunc(t4, U2prev)
          dataset$U2_t4 <-rbinom(sampsize,1,U2prev_t4)
          
          #Calculate outcomes
          dataset$Y1_t4 <- ifelse(dataset$R_t4==1 & dataset$U1==1 & dataset$I1==1 | dataset$U2_t4==1, 1, 0)
          dataset$Y0_t4 <- ifelse(dataset$R_t4==1 & dataset$U1==1 & dataset$I0==1 | dataset$U2_t4==1, 1, 0)
          
          #Get RD and CI  
          means_t4 <- dataset %>% summarize(risk1=mean(Y1_t4),risk0=mean(Y0_t4),A=sum(Y1_t4), C=sum(Y0_t4)) 
          means_t4$B <- 1000-means_t4$A
          means_t4$D <- 1000-means_t4$C
          means_t4$rd <-means_t4$risk1-means_t4$risk0
          means_t4$rd
          means_t4$rdse <- sqrt((means_t4$risk1*(1-means_t4$risk1)/(means_t4$A+means_t4$B))+(means_t4$risk0*(1-means_t4$risk0)/(means_t4$C+means_t4$D)))
          means_t4$rdse
          means_t4$rdLci <-means_t4$rd-1.96*means_t4$rdse
          means_t4$rdUci <-means_t4$rd+1.96*means_t4$rdse
          means_t4$rdLci
          means_t4$rdUci
          
          #Store risk difference and CI
          RD_t4[i]<-rename(select(means_t4,rd),riskdiff_t4 = rd)
          RD_LCI_t4[i]<- rename(select(means_t4,rdLci),rd_lci_t4 = rdLci)
          RD_UCI_t4[i]<- rename(select(means_t4,rdUci),rd_uci_t4 = rdUci)
       
        #Timepoint 5
          #Set prevalences
          Rprev_t5<-Rchangefunc(t5, Rprev)
          dataset$R_t5 <-rbinom(sampsize,1,Rprev_t5)
          U2prev_t5<-U2changefunc(t5, U2prev)
          dataset$U2_t5 <-rbinom(sampsize,1,U2prev_t5)
          
          #Calculate outcomes
          dataset$Y1_t5 <- ifelse(dataset$R_t5==1 & dataset$U1==1 & dataset$I1==1 | dataset$U2_t5==1, 1, 0)
          dataset$Y0_t5 <- ifelse(dataset$R_t5==1 & dataset$U1==1 & dataset$I0==1 | dataset$U2_t5==1, 1, 0)
          
          #Get RD and CI  
          means_t5 <- dataset %>% summarize(risk1=mean(Y1_t5),risk0=mean(Y0_t5),A=sum(Y1_t5), C=sum(Y0_t5)) 
          means_t5$B <- 1000-means_t5$A
          means_t5$D <- 1000-means_t5$C
          means_t5$rd <-means_t5$risk1-means_t5$risk0
          means_t5$rd
          means_t5$rdse <- sqrt((means_t5$risk1*(1-means_t5$risk1)/(means_t5$A+means_t5$B))+(means_t5$risk0*(1-means_t5$risk0)/(means_t5$C+means_t5$D)))
          means_t5$rdse
          means_t5$rdLci <-means_t5$rd-1.96*means_t5$rdse
          means_t5$rdUci <-means_t5$rd+1.96*means_t5$rdse
          means_t5$rdLci
          means_t5$rdUci
          
          #Store risk difference and CI
          RD_t5[i]<-rename(select(means_t5,rd),riskdiff_t5 = rd)
          RD_LCI_t5[i]<- rename(select(means_t5,rdLci),rd_lci_t5 = rdLci)
          RD_UCI_t5[i]<- rename(select(means_t5,rdUci),rd_uci_t5 = rdUci)
  #end of loop        
  }
  
  RDavg_t1<-mean(as.numeric(RD_t1))
  RDLCIavg_t1<-mean(as.numeric(RD_LCI_t1))
  RDUCIavg_t1<-mean(as.numeric(RD_UCI_t1))
  biasabs_t1<-RDavg_t1-RDavg_t1
  biasrel_t1<-(RDavg_t1-RDavg_t1)/RDavg_t1
  
  RDavg_t2<-mean(as.numeric(RD_t2))
  RDLCIavg_t2<-mean(as.numeric(RD_LCI_t2))
  RDUCIavg_t2<-mean(as.numeric(RD_UCI_t2))
  biasabs_t2<-RDavg_t1-RDavg_t2
  biasrel_t2<-(RDavg_t1-RDavg_t2)/RDavg_t1
  
  RDavg_t3<-mean(as.numeric(RD_t3))
  RDLCIavg_t3<-mean(as.numeric(RD_LCI_t3))
  RDUCIavg_t3<-mean(as.numeric(RD_UCI_t3))
  biasabs_t3<-RDavg_t1-RDavg_t3
  biasrel_t3<-(RDavg_t1-RDavg_t3)/RDavg_t1
  
  RDavg_t4<-mean(as.numeric(RD_t4))
  RDLCIavg_t4<-mean(as.numeric(RD_LCI_t4))
  RDUCIavg_t4<-mean(as.numeric(RD_UCI_t4))
  biasabs_t4<-RDavg_t1-RDavg_t4
  biasrel_t4<-(RDavg_t1-RDavg_t4)/RDavg_t1
  
  RDavg_t5<-mean(as.numeric(RD_t5))
  RDLCIavg_t5<-mean(as.numeric(RD_LCI_t5))
  RDUCIavg_t5<-mean(as.numeric(RD_UCI_t5))
  biasabs_t5<-RDavg_t1-RDavg_t5
  biasrel_t5<-(RDavg_t1-RDavg_t5)/RDavg_t1
  
  return(list(Rprev_t1=Rprev, U1prev_t1=U1prev, U2prev_t1=U2prev, Rchangefunc=Rchangefunc, 
              Rprev_t2=Rprev_t2, U2prev_t2=U2prev_t2,
              Rprev_t3=Rprev_t3, U2prev_t3=U2prev_t3,
              Rprev_t4=Rprev_t4, U2prev_t4=U2prev_t4,
              Rprev_t5=Rprev_t5, U2prev_t5=U2prev_t5,
              RD_t1=RD_t1, RDavg_t1=RDavg_t1, RD_LCI_t1=RD_LCI_t1, RDLCIavg_t1=RDLCIavg_t1, RD_UCI_t1=RD_UCI_t1, RDUCIavg_t1=RDUCIavg_t1, biasabs_t1=biasabs_t1, biasrel_t1=biasrel_t1,
              RD_t2=RD_t2, RDavg_t2=RDavg_t2, RD_LCI_t2=RD_LCI_t2, RDLCIavg_t2=RDLCIavg_t2, RD_UCI_t2=RD_UCI_t2, RDUCIavg_t2=RDUCIavg_t2, biasabs_t2=biasabs_t2, biasrel_t2=biasrel_t2, 
              RD_t3=RD_t3, RDavg_t3=RDavg_t3, RD_LCI_t3=RD_LCI_t3, RDLCIavg_t3=RDLCIavg_t3, RD_UCI_t3=RD_UCI_t3, RDUCIavg_t3=RDUCIavg_t3, biasabs_t3=biasabs_t3, biasrel_t3=biasrel_t3,
              RD_t4=RD_t4, RDavg_t4=RDavg_t4, RD_LCI_t4=RD_LCI_t4, RDLCIavg_t4=RDLCIavg_t4, RD_UCI_t4=RD_UCI_t4, RDUCIavg_t4=RDUCIavg_t4, biasabs_t4=biasabs_t4, biasrel_t4=biasrel_t4,
              RD_t5=RD_t5, RDavg_t5=RDavg_t5, RD_LCI_t5=RD_LCI_t5, RDLCIavg_t5=RDLCIavg_t5, RD_UCI_t5=RD_UCI_t5, RDUCIavg_t5=RDUCIavg_t5, biasabs_t5=biasabs_t5, biasrel_t5=biasrel_t5,
              samp_dataset=dataset))
  }


#Test simrun with simple parameters to make sure the function works as expected
  #Run function
  test <- simrun(Rprev=0.5, U1prev=0.5, U2prev=0.05, Rchangefunc=Rchangelinup, U2changefunc=U2changenone, 
                 t2=12, t3=60, t4=120, t5=240, sampsize=100, nIterations=50)
 
   #Check output
    #RDs at each time point
    test$RDavg_t1
    test$RDLCIavg_t1
    test$RDUCIavg_t1
    test$Rprev_t1
    test$biasabs_t1
    test$biasrel_t1
    
    test$RDavg_t2
    test$RDLCIavg_t2
    test$RDUCIavg_t2
    test$Rprev_t2
    test$biasabs_t2
    test$biasrel_t2
    
    test$RDavg_t3
    test$RDLCIavg_t3
    test$RDUCIavg_t3
    test$Rprev_t3
    test$biasabs_t3
    
    test$biasabs_t4
    test$biasrel_t5
    
    #Check histogram of all RDs in simulation
    hist(as.numeric(test$RD_t1))
    hist(as.numeric(test$RD_LCI_t1))
    hist(as.numeric(test$RD_UCI_t1))
    
    #If want to check min/max of RDs in simulation
    test2<-t(data.frame(test$RD_t1))
    rownames(test2) <- c(1:50)
    colnames(test2) <- "RD"
    head(test2)
    min(test2[,1])


    
    
  

  
