#Install packages needed for code to run
library(ggplot2)

#Define R and U2 change functions for simulation function inputs later on
  #this is a generic function to reverse the log odds that is used in each of the R change functions
  logtrans<-function(x){logtransout <- exp(x)/(1+exp(x));logtransout}

      #no change in log odds of R
      Rchangenone<-function(t, Rprev2){
        #calculate baseline log odds
        odds<-Rprev2/(1-Rprev2)
        base<-log(odds)
        #calculate current log odds using logtrans function defined above and formula
        output<-logtrans(0*t+base)
        ; output}
      
      #positive linear change in log odds of R
      Rchangelinup<-function(t, Rprev2){
        #calculate baseline log odds
        odds<-Rprev2/(1-Rprev2)
        base<-log(odds)
        #calculate current log odds using logtrans function defined above and formula
        output<-logtrans(.01*t+base)
        ; output}
      
      #negative linear change in log odds of R using same format as previous
      Rchangelindown<-function(t, Rprev2){
        odds<-Rprev2/(1-Rprev2)
        base<-log(odds)
        output<-logtrans(-.01*t+base)
        ; output}
      
      #positive quadratic (accelerating) change in log odds of R using same format as previous
      Rchangeaccelup<-function(t, Rprev2){
        odds<-Rprev2/(1-Rprev2)
        base<-log(odds)
        output<-logtrans(.0025*t^(3/2)+base)
        ; output}
      
      #negative quadratic (accelerating) change in log odds of R using same format as previous
      Rchangeacceldown<-function(t, Rprev2){
        odds<-Rprev2/(1-Rprev2)
        base<-log(odds)
        output<-logtrans(-.0025*t^(3/2)+base)
        ; output}
      
      #U2 change functions 
      #no change in log odds of R
      U2changenone<-function(t, U2prev2){
        #calculate baseline log odds
        odds<-U2prev2/(1-U2prev2)
        base<-log(odds)
        #calculate current log odds using logtrans function defined above and formula
        output<-logtrans(0*t+base)
        ; output}
      
      #positive linear change in log odds of U2 
      U2changelinup<-function(t, U2prev2){
        #calculate baseline log odds
        odds<-U2prev2/(1-U2prev2)
        base<-log(odds)
        #calculate current log odds using logtrans function defined above and formula
        output<-logtrans(.01*t+base)
        ; output}
      
      #negative linear change in log odds of U2 using same format as previous
      U2changelindown<-function(t, U2prev2){
        odds<-U2prev2/(1-U2prev2)
        base<-log(odds)
        output<-logtrans(-.01*t+base)
        ; output}
      
      #positive quadratic (accelerating) change in log odds of U2 using same format as previous
      U2changeaccelup<-function(t, U2prev2){
        odds<-U2prev2/(1-U2prev2)
        base<-log(odds)
        output<-logtrans(.0025*t^(3/2)+base)
        ; output}
      
      #negative quadratic (accelerating) change in log odds of U2 using same format as previous
      U2changeacceldown<-function(t, U2prev2){
        odds<-U2prev2/(1-U2prev2)
        base<-log(odds)
        output<-logtrans(-.0025*t^(3/2)+base)
        ; output}
      
      
      #Check if functions are reasonable changes in R
    argscheckR <- expand.grid(Rprev2=c(.01, .2, .5, .8, .99),  t=c(0, 12, 60, 120, 240))
    
     test_Rchangenone<- mapply(FUN = Rchangenone,  t=argscheckR$t,Rprev2=argscheckR$Rprev2)
     check_Rchangenone<-cbind(argscheckR,test_Rchangenone)
     ggplot(check_Rchangenone, aes(x=unlist(t), y=unlist(test_Rchangenone), color=as.factor(unlist((Rprev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab("p(R)") + ggtitle("Change in p(R) over time") + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_Rchangelinup<- mapply(FUN = Rchangelinup,  t=argscheckR$t,Rprev2=argscheckR$Rprev2)
     check_Rchangelinup<-cbind(argscheckR,test_Rchangelinup)
     ggplot(check_Rchangelinup, aes(x=unlist(t), y=unlist(test_Rchangelinup), color=as.factor(unlist((Rprev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab("p(R)") + ggtitle("Change in p(R) over time") + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_Rchangelindown<- mapply(FUN = Rchangelindown,  t=argscheckR$t,Rprev2=argscheckR$Rprev2)
     check_Rchangelindown<-cbind(argscheckR,test_Rchangelindown)
     ggplot(check_Rchangelindown, aes(x=unlist(t), y=unlist(test_Rchangelindown), color=as.factor(unlist((Rprev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab("p(R)") + ggtitle("Change in p(R) over time") + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_Rchangeaccelup<- mapply(FUN = Rchangeaccelup,  t=argscheckR$t,Rprev2=argscheckR$Rprev2)
     check_Rchangeaccelup<-cbind(argscheckR,test_Rchangeaccelup)
     ggplot(check_Rchangeaccelup, aes(x=unlist(t), y=unlist(test_Rchangeaccelup), color=as.factor(unlist((Rprev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab("p(R)") + ggtitle("Change in p(R) over time") + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_Rchangeacceldown<- mapply(FUN = Rchangeacceldown,  t=argscheckR$t,Rprev2=argscheckR$Rprev2)
     check_Rchangeacceldown<-cbind(argscheckR,test_Rchangeacceldown)
     ggplot(check_Rchangeacceldown, aes(x=unlist(t), y=unlist(test_Rchangeacceldown), color=as.factor(unlist((Rprev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab("p(R)") + ggtitle("Change in p(R) over time") + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     #Check if functions are reasonable changes in U2
     argscheckU2 <- expand.grid(U2prev2=c(.01, .2, .5, .8, .99),  t=c(0, 12, 60, 120, 240))
  
     test_U2changenone<- mapply(FUN = U2changenone,  t=argscheckU2$t,U2prev2=argscheckU2$U2prev2)
     check_U2changenone<-cbind(argscheckU2,test_U2changenone)
     ggplot(check_U2changenone, aes(x=unlist(t), y=unlist(test_U2changenone), color=as.factor(unlist((U2prev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab(expression(p(U[2]))) + ggtitle(expression(paste("Change in ",p(U[2])," over time"))) + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_U2changelinup<- mapply(FUN = U2changelinup,  t=argscheckU2$t,U2prev2=argscheckU2$U2prev2)
     check_U2changelinup<-cbind(argscheckU2,test_U2changelinup)
     ggplot(check_U2changelinup, aes(x=unlist(t), y=unlist(test_U2changelinup), color=as.factor(unlist((U2prev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab(expression(p(U[2]))) + ggtitle(expression(paste("Change in ",p(U[2])," over time"))) + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_U2changelindown<- mapply(FUN = U2changelindown,  t=argscheckU2$t,U2prev2=argscheckU2$U2prev2)
     check_U2changelindown<-cbind(argscheckU2,test_U2changelindown)
     ggplot(check_U2changelindown, aes(x=unlist(t), y=unlist(test_U2changelindown), color=as.factor(unlist((U2prev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab(expression(p(U[2]))) + ggtitle(expression(paste("Change in ",p(U[2])," over time"))) + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_U2changeaccelup<- mapply(FUN = U2changeaccelup,  t=argscheckU2$t,U2prev2=argscheckU2$U2prev2)
     check_U2changeaccelup<-cbind(argscheckU2,test_U2changeaccelup)
     ggplot(check_U2changeaccelup, aes(x=unlist(t), y=unlist(test_U2changeaccelup), color=as.factor(unlist((U2prev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab(expression(p(U[2]))) + ggtitle(expression(paste("Change in ",p(U[2])," over time"))) + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     test_U2changeacceldown<- mapply(FUN = U2changeacceldown,  t=argscheckU2$t,U2prev2=argscheckU2$U2prev2)
     check_U2changeacceldown<-cbind(argscheckU2,test_U2changeacceldown)
     ggplot(check_U2changeacceldown, aes(x=unlist(t), y=unlist(test_U2changeacceldown), color=as.factor(unlist((U2prev2))))) +
       geom_point()+geom_line() + xlab("Time (months)") + ylab(expression(p(U[2]))) + ggtitle(expression(paste("Change in ",p(U[2])," over time"))) + 
       theme(legend.position="none", plot.title = element_text(hjust = 0.5)) 
     
     
     
     
     
     
     
     
     
     
     
     
     
       