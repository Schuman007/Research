# Impact of different initial conditions 
# in the absence of migratory birds
# initialisation
# Mosquito Population (First Test Case)
# This section is to check the impact of mortality rate of mosquitoes on the infected bird population
# So, we shall choose different mortality rates  of mosquito population

library(stats)
library(graphics)
library(grDevices)
library(gdata)
#library(xlsx)
library(readxl)
library(ggplot2)
#require(xlsx)
library(plot3D)
library(reshape2)
#mydata1 = read_excel("Summary.xlsx", sheet =1)
library(readr)
MV2007_2017 <- read_delim("~/Dropbox/Local_Dynamics_R_Codes/Sensivity_R0/MV2007_2017.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE,locale = locale(decimal_mark = ","))
MV2007_2017$Termin <- as.Date(MV2007_2017$Termin,'%d.%m.%Y')
#MV2007_2017 <- MV2007_2017[1:98487,c(1,2,3,5)]
MV2007_2017 <- MV2007_2017[1:98487,c(1,2,5)]

library(tidyr)
data_wide <- spread(MV2007_2017,Station,'Temperatur-Mittel' )
#mydata <- data_wide
mydata <- read.csv('MyData1.csv', header = TRUE)
#T1=mydata[,i];
#results <- data.frame(IM=numeric(), IBSC=numeric(), Date=as.POSIXct(character()), Group=character(),station=character())
results <- data.frame(IM=numeric(), IBC=numeric(), Date=as.POSIXct(character()),station=character())
#results <- data.frame(IM=numeric(), SB=numeric(), Date=as.POSIXct(character()),station=character())
#results <- data.frame(IM=NA,IBSC=NA,Date=NA,group=NA,station=NA)
for (h in 4:length(mydata)){
      T1=unlist(mydata[,h])
      sname <- colnames(mydata)[h]
      l=length(T1)
      n1=length(T1)-1
      t1=seq(0,n1)
      # Mosquito Population Parameters 
      k1=seq(0, n1); 
      k1=0.344/(1+1.231*exp(-0.184*(T1-20)))
      bM1=seq(0, n1); 
      bM1=2.325*k1/10; 
      mM1=seq(0, n1); 
      mM1=0.0025*T1^2-0.094*T1+1.0257; 
      mM1=mM1/30;
      KM=1000000;
       gM=seq(0, 0, length=n1+1)
       for (i in 1:n1)
       {
         if(T1[i] > 15)
               gM[i]=0.0093*T1[i]-.1352
        # print(gM)
       }
      #gM=.012;#newdata
      # Transmission Parameters for local birds
      c2=k1*.99;
      c1=k1*.89;
      # Transmission Parameters for Migratory birds
      c3=k1*98;
      # Local Bird Population Parameters
      KB=10000;
      # Migratory Bird Population Parameters
      KBm=10000;
      # Birth Rate for local birds population
      bB1=0.0074;
      # Birth Rate for migratory birds population
      bBm=0.0034;
      # Mortality Rate for local brids
      mB1=.0012;
      # Mortality Rate for migratory brids
      mBm=.0012;
      # Recruitment Rate From Susceptible local Birds
      b3=.23*k1;
      b4=.12*k1;
      # Recruitment Rate From migratory local Birds
      b5=.20*k1;
      # Incubation Period for local birds
      gBC=.67;
      gBSC=.56;
      # Incubation Period for migratory birds
      gBm=.50;
      # Recovery Rate for local birds
      a3=.122;
      a4=.182;
      # Recovery Rate for migratory birds
      a9=.126;
      # Recruitment Rate From Recovered Birds
      g3=.12;
      # Calculating R_0 and then to plot against the birth and mortality rates of the mosquitoes
      R0=seq(0,n1);
      #R0=sqrt((KM*gM)/(KB*mM1*(gM+mM1))*(((c2*gBC*b4)/(a4+mB1)*(gBC+mB1))+(c1*gBSC*b3)/(a3+mB1)*(gBSC+mB1)));
      #R0=sqrt(((KM*gM*c2*gBC*b4)/(KB*mM1*(gM+mM1)*(a4+mB1)*(gBC+mB1)))+((KM*gM*c1*gBSC*b3)/(KB*mM1*(gM+mM1)*(a3+mB1)*(gBSC+mB1))))
      
      # initialisation
      # Mosquito Population First Population
      SM1=seq(1000, 1000, length=n1+1)
      EM1=seq(100, 100, length=n1+1)
      IM1=seq(10, 10, length=n1+1)
      NM1=seq(1110, 1110, length=n1+1)
      
      
      
      
      #  Local Bird Population (First population)
      SB1=   seq(500, 500, length=n1+1)
      EBC1=   seq(1, 1, length=n1+1)
      EBSC1=  seq(1, 1, length=n1+1)
      IBC1=   seq(1, 1, length=n1+1)
      IBSC1=  seq(1, 1, length=n1+1)
      RBC1=   seq(0, 0, length=n1+1)
      RBSC1=  seq(0, 0, length=n1+1)
      NB1=   seq(504, 504, length=n1+1)
      
      
     
      
      
      h1=1;
      NMmin=110.0;
      # ODE Model Loop (Cauchy Euler Method)
      PhiB=30;
      
      
      for (i in 1:n1)
        
      {
        
        # Mosquito Population Loop First Population
        
        SM1[i+1]=SM1[i]+h1*(((bM1[i]*NM1[i]-mM1[i]*SM1[i])*(1-SM1[i]/KB))-(c2[i]*SM1[i]*IBC1[i])/KB-(c1[i]*SM1[i]*IBSC1[i])/KB)
        EM1[i+1]=EM1[i] + h1*((c2[i]*SM1[i]*IBC1[i])/KB+(c1[i]*SM1[i]*IBSC1[i])/KB-gM[i]*EM1[i]-mM1[i]*EM1[i])
        IM1[i+1]=IM1[i] + h1*(gM[i]*EM1[i]-mM1[i]*IM1[i])
        NM1[i+1]=SM1[i+1]+EM1[i+1]+IM1[i+1]
        if (SM1[i+1]<0) {SM1[i+1]=0}
        if (NM1[i+1]<NMmin) 
        {SM1[i+1]=SM1[i]; EM1[i+1]=EM1[i]; IM1[i+1]=IM1[i]}
        # Local Bird Population Loop
        SB1[i+1]=SB1[i] + h1*((bB1-(bB1-mB1)*NB1[i]/KB)*NB1[i]-mB1*SB1[i]-PhiB*(b3[i]+b4[i])*IM1[i]*SB1[i]/KM)
        EBC1[i+1]=EBC1[i] + h1*(PhiB*b4[i]*IM1[i]*SB1[i]/KM-mB1*EBC1[i]-gBC*EBC1[i])
        EBSC1[i+1]=EBSC1[i] + h1*(PhiB*b3[i]*IM1[i]*SB1[i]/KM-mB1*EBSC1[i]-gBSC*EBSC1[i])
        IBC1[i+1]=IBC1[i] + h1*(gBC*EBC1[i]-mB1*IBC1[i]-a4*IBC1[i])
        IBSC1[i+1]=IBSC1[i] + h1*(gBSC*EBSC1[i]-mB1*IBSC1[i]-a3*IBSC1[i]+g3*RBSC1[i])
        RBC1[i+1]=RBC1[i] + h1*(a4*IBC1[i]-mB1*RBC1[i])
        RBSC1[i+1]=RBSC1[i] + h1*(a3*IBSC1[i]-mB1*RBSC1[i]-g3*RBSC1[i])
        NB1[i+1]=SB1[i+1]+EBC1[i+1]+IBC1[i+1]+RBC1[i+1]+EBSC1[i+1]+IBSC1[i+1]+RBSC1[i+1] 
       
      }
  


#results1 <- data.frame(IM1,IBSC1,mydata$Date)
results1 <- data.frame(IM1,IBC1,mydata$Termin)

names(results1) <- c("IM","IBC","Date")
#names(results1) <- c("IM","SB","Date")
results1$station <-sname



results <- rbind(results,results1)
}



#setEPS()
 ggplot(results, aes(x=results$Date,y=results$IM))+
   theme_bw()+
   geom_line(aes(colour=as.factor(station)))






