# An endeavour to perform bubble plot:
# X axis- Infected Birds
# Y axis- Infected Mosquitoes

library(stats)
library(graphics)
library(grDevices)
library(gdata)
library(readxl)
library(ggplot2)
library(plot3D)
library(reshape2)
library(readr)
mydata = read_excel("11Summary.xlsx", sheet =1)
results <- data.frame(SM = numeric(),
                      IM = numeric(),
                      IBC = numeric(),
                      IBSC = numeric(),
                      NB = numeric(),
                      NM = numeric(),
                      Date=as.POSIXct(character()), 
                      group=character(),Station=character())

for (h in 2:length(mydata)){
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
  mM2=mM1/35;
  mM3=mM1/40;
  KM=100000;
  gM=seq(0, 0, length=n1+1)
  for (i in 1:n1)
  {
    if(T1[i]>15)
      gM[i]=0.0093*T1[i]-.1352
  }
  #gM=.012;
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
  R0=sqrt(((KM*gM*c2*gBC*b4)/(KB*mM1*(gM+mM1)*(a4+mB1)*(gBC+mB1)))+((KM*gM*c1*gBSC*b3)/(KB*mM1*(gM+mM1)*(a3+mB1)*(gBSC+mB1))))
  
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
  NMmin=100.0;
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
  results1 <- data.frame(IM1,IBC1,IBSC1,NB1,NM1, mydata$Date)
  #results1$group <-"Mortality group1: mu1" 
  #results1$t <- 0:3653
  names(results1) <- c("IM","IBC","IBSC","NB","NM","Date")
  results1$Station <-sname
  results <- rbind(results,results1)
  
}


####################################################################################
###Saving all the final values from the simulation of different weather stations####
####################################################################################
## Station Name
#######
Mos1Stat <-results$Station[[3654*1]]
Mos2Stat <-results$Station[[3654*2]]
Mos3Stat <-results$Station[[3654*3]]
Mos4Stat <-results$Station[[3654*4]]
Mos5Stat <-results$Station[[3654*5]]
Mos6Stat <-results$Station[[3654*6]]
Mos7Stat <-results$Station[[3654*7]]
Mos8Stat <-results$Station[[3654*8]]
Mos9Stat <-results$Station[[3654*9]]
Mos10Stat <-results$Station[[3654*10]]
Mos11Stat <-results$Station[[3654*11]]
StationData <-rbind(Mos1Stat, Mos2Stat, Mos3Stat, Mos4Stat, Mos5Stat, Mos6Stat, Mos7Stat, Mos8Stat, Mos9Stat, Mos10Stat, Mos11Stat )
####################################################################################

## Final Infected Clinical bird population
#######
IBC1Stat <-results$IBC[[3654*1]]
IBC2Stat <-results$IBC[[3654*2]]
IBC3Stat <-results$IBC[[3654*3]]
IBC4Stat <-results$IBC[[3654*4]]
IBC5Stat <-results$IBC[[3654*5]]
IBC6Stat <-results$IBC[[3654*6]]
IBC7Stat <-results$IBC[[3654*7]]
IBC8Stat <-results$IBC[[3654*8]]
IBC9Stat <-results$IBC[[3654*9]]
IBC10Stat <-results$IBC[[3654*10]]
IBC11Stat <-results$IBC[[3654*11]]
IBCData <- rbind(IBC1Stat, IBC2Stat, IBC3Stat, IBC4Stat, IBC5Stat, IBC6Stat, IBC7Stat, IBC8Stat, IBC9Stat, IBC10Stat, IBC11Stat)
####################################################################################

## Final Infected Sub Clinical bird population
#######
IBSC1Stat <-results$IBSC[[3654*1]]
IBSC2Stat <-results$IBSC[[3654*2]]
IBSC3Stat <-results$IBSC[[3654*3]]
IBSC4Stat <-results$IBSC[[3654*4]]
IBSC5Stat <-results$IBSC[[3654*5]]
IBSC6Stat <-results$IBSC[[3654*6]]
IBSC7Stat <-results$IBSC[[3654*7]]
IBSC8Stat <-results$IBSC[[3654*8]]
IBSC9Stat <-results$IBSC[[3654*9]]
IBSC10Stat <-results$IBSC[[3654*10]]
IBSC11Stat <-results$IBSC[[3654*11]]
IBSCData <- rbind(IBSC1Stat, IBSC2Stat, IBSC3Stat, IBSC4Stat, IBSC5Stat, IBSC6Stat, IBSC7Stat, IBSC8Stat, IBSC9Stat, IBSC10Stat, IBSC11Stat)
####################################################################################


## Final Total bird population
#######

NB1Stat <-results$NB[[3654*1]]
NB2Stat <-results$NB[[3654*2]]
NB3Stat <-results$NB[[3654*3]]
NB4Stat <-results$NB[[3654*4]]
NB5Stat <-results$NB[[3654*5]]
NB6Stat <-results$NB[[3654*6]]
NB7Stat <-results$NB[[3654*7]]
NB8Stat <-results$NB[[3654*8]]
NB9Stat <-results$NB[[3654*9]]
NB10Stat <-results$NB[[3654*10]]
NB11Stat <-results$NB[[3654*11]]
NBData <- rbind(NB1Stat, NB2Stat, NB3Stat, NB4Stat, NB5Stat, NB6Stat, NB7Stat, NB8Stat, NB9Stat, NB10Stat, NB11Stat)
####################################################################################


## Final Total mosquito population
#######

NM1Stat <-results$NM[[3654*1]]
NM2Stat <-results$NM[[3654*2]]
NM3Stat <-results$NM[[3654*3]]
NM4Stat <-results$NM[[3654*4]]
NM5Stat <-results$NM[[3654*5]]
NM6Stat <-results$NM[[3654*6]]
NM7Stat <-results$NM[[3654*7]]
NM8Stat <-results$NM[[3654*8]]
NM9Stat <-results$NM[[3654*9]]
NM10Stat <-results$NM[[3654*10]]
NM11Stat <-results$NM[[3654*11]]
NMData <- rbind(NM1Stat, NM2Stat, NM3Stat, NM4Stat, NM5Stat, NM6Stat, NM7Stat, NM8Stat, NM9Stat, NM10Stat, NM11Stat)
####################################################################################

## Final Infected mosquito population
#######

IM1Stat <-results$IM[[3654*1]]
IM2Stat <-results$IM[[3654*2]]
IM3Stat <-results$IM[[3654*3]]
IM4Stat <-results$IM[[3654*4]]
IM5Stat <-results$IM[[3654*5]]
IM6Stat <-results$IM[[3654*6]]
IM7Stat <-results$IM[[3654*7]]
IM8Stat <-results$IM[[3654*8]]
IM9Stat <-results$IM[[3654*9]]
IM10Stat <-results$IM[[3654*10]]
IM11Stat <-results$IM[[3654*11]]
IMData <- rbind(IM1Stat, IM2Stat, IM3Stat, IM4Stat, IM5Stat, IM6Stat, IM7Stat, IM8Stat, IM9Stat, IM10Stat, IM11Stat)
####################################################################################
#Finaldata1 <- cbind(StationData,IBCData, IBSCData, NBData, NMData, IMData)
Finaldata1 <- cbind(StationData, IBCData, IBSCData, NBData, NMData, IMData)
write.csv(Finaldata1, file='Final2.csv', row.names=TRUE)
#setEPS()
#postscript("bub2.eps")
#png(file="animals45.png",width=800,height=850,res=120)
InfectionTable <- read.csv(file='Final2.csv')
InfecIndex <- InfectionTable$TotalMosquitoPopulation/max(InfectionTable$TotalMosquitoPopulation)
InfecIndex <- InfectionTable$V5/max(InfectionTable$V5)
InfectionTableNew <- cbind(InfectionTable, InfecIndex)



ggplot(InfectionTable, aes(x=InfectionTable$V3,y= InfectionTable$V6, label=V1))+
 # geom_point(colour='white', fill='#E69F00', shape=21)+ 
  geom_point(aes(size=InfectionTable$V3, fill=InfecIndex), shape=21)+
  scale_size_area(max_size=20)+
  scale_x_continuous(name="Local Sub-Clinical Birds", limits=c(0, 3200))+
  scale_y_continuous(name="Infected Mosquitoes", limits=c(0, 1400))+
  scale_fill_gradient(trans="reverse")+
  theme_bw()+
  geom_text(size=2)+
  scale_colour_gradient2()+
  labs(x = "Local Sub-Clinical Bird", y= "Infected Mosquito")+
 # theme(axis.ticks = element_line(size = 5))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=18))
  

##############################New One in the paper

Final2 <- read_delim("~/Dropbox/Local_Dynamics_R_Codes/CODESPaper/Final2.csv", ";", escape_double = FALSE, trim_ws = TRUE)

InfInd<- max(Final2$`Infected Mosquito`)/(Final2$`Total Mosquito`)

InfectionTabl1 <- cbind(Final2, InfInd)

library(ggrepel)


p6 <- ggplot(InfectionTabl1, aes(x = (InfectionTabl1$IBC+InfectionTabl1$IBSC), 
                                 y = InfectionTabl1$`Infected Mosquito`, 
                                 fill=(InfectionTabl1$IBC+InfectionTabl1$IBSC)/10,label=InfectionTabl1$Station)) +
  geom_point(shape = 21)+ scale_size_area(max_size = 35)+
  geom_text_repel(aes((InfectionTabl1$IBC+InfectionTabl1$IBSC), 
                      InfectionTabl1$`Infected Mosquito`, label = InfectionTabl1$Station), size = 9)+
  scale_fill_continuous(low = "gray", high = "#CC6666")+
  scale_x_log10(limits = c(47, 4000))+scale_y_log10(limits = c(90, 1400))+
  theme_bw()+
  theme(axis.ticks = element_line(size = 1))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=22)) +labs(fill="Infected Birds",size ='Total Mosquito')+
  labs(x = "Infected Bird Population", y = "Infected Mosquito Population")

p6

ggsave("BublleAll.eps", width = 30, height = 30, units = "cm")

labs(x = "Time (Year)", y= "Temperature (C)", size = 15)+
  theme(axis.ticks = element_line(size = 1))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=22))+
  scale_x_continuous(breaks = round(seq(0, 4000, by =365)),
                     label = c("0", "1", "2","3","4","5","6","7","8","9","10")) 

ggsave("Gottin.eps", width = 80, height = 80, units = "cm")






#symbols(InfectionTable$V3, InfectionTable$V6, circles=InfectionTable$V5)

#radius <- sqrt( InfectionTable$V5/ pi )


#symbols(InfectionTable$V3, InfectionTable$V6, circles=radius)

#pdf("mygraph.pdf")

#symbols(InfectionTable$V3, InfectionTable$V6, circles=radius, inches=0.5, fg="black", bg="white", xlab="Subclinical Local Bird", ylab="Infected Mosquito", cex.axis=1.5, ces.lab=1.5, font.lab=2)


#text(InfectionTable$V3, InfectionTable$V6, InfectionTable$V1, cex=0.8, font.lab=2)

#dev.off()