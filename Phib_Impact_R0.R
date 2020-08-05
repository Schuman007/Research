# Plots to show the impact of \phi i.e. the mosquito to the birds ratio 
# We will take different values and its inflluence in the infected population
# In this case we shall not consider the migratory birds
library(dplyr)
library(tidyr)
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
mydata = read_excel("Summary.xlsx", sheet =1)
T1=mydata$Temp_Greifswald;
l=length(T1);
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
# Mosquito to bird raio
Phi1B=10;
Phi2B=20;
Phi3B=30;
# Calculating R_0 and then to plot against the birth and mortality rates of the mosquitoes
R10=seq(0,n1);
R20=seq(0,n1);
R30=seq(0,n1);
R10=sqrt(((KM*gM*c2*gBC*b4*Phi1B)/(KB*mM1*(gM+mM1)*(a4+mB1)*(gBC+mB1)))+((KM*gM*c1*gBSC*b3*Phi1B)/(KB*mM1*(gM+mM1)*(a3+mB1)*(gBSC+mB1))))
R20=sqrt(((KM*gM*c2*gBC*b4*Phi2B)/(KB*mM1*(gM+mM1)*(a4+mB1)*(gBC+mB1)))+((KM*gM*c1*gBSC*b3*Phi2B)/(KB*mM1*(gM+mM1)*(a3+mB1)*(gBSC+mB1))))
R30=sqrt(((KM*gM*c2*gBC*b4*Phi3B)/(KB*mM1*(gM+mM1)*(a4+mB1)*(gBC+mB1)))+((KM*gM*c1*gBSC*b3*Phi3B)/(KB*mM1*(gM+mM1)*(a3+mB1)*(gBSC+mB1))))

# initialisation
# Mosquito Population First Population
SM1=seq(10000, 10000, length=n1+1)
EM1=seq(1, 1, length=n1+1)
IM1=seq(1, 1, length=n1+1)
NM1=seq(10002, 10002, length=n1+1)

# Mosquito Population Second Population
SM2=seq(10000, 10000, length=n1+1)
EM2=seq(1, 1, length=n1+1)
IM2=seq(1, 1, length=n1+1)
NM2=seq(10002, 10002, length=n1+1)


# Mosquito Population Third Population
SM3=seq(10000, 10000, length=n1+1)
EM3=seq(1, 1, length=n1+1)
IM3=seq(1, 1, length=n1+1)
NM3=seq(10002, 10002, length=n1+1)


#  Local Bird Population (First population)
SB1=   seq(500, 500, length=n1+1)
EBC1=   seq(1, 1, length=n1+1)
EBSC1=  seq(1, 1, length=n1+1)
IBC1=   seq(1, 1, length=n1+1)
IBSC1=  seq(1, 1, length=n1+1)
RBC1=   seq(0, 0, length=n1+1)
RBSC1=  seq(0, 0, length=n1+1)
NB1=   seq(504, 504, length=n1+1)


#  Local Bird Population (First population)
SB2=   seq(500, 500, length=n1+1)
EBC2=   seq(1, 1, length=n1+1)
EBSC2=  seq(1, 1, length=n1+1)
IBC2=   seq(1, 1, length=n1+1)
IBSC2=  seq(1, 1, length=n1+1)
RBC2=   seq(0, 0, length=n1+1)
RBSC2=  seq(0, 0, length=n1+1)
NB2=   seq(504, 504, length=n1+1)

#  Local Bird Population (First population)
SB3=   seq(500, 500, length=n1+1)
EBC3=   seq(1, 1, length=n1+1)
EBSC3=  seq(1, 1, length=n1+1)
IBC3=   seq(1, 1, length=n1+1)
IBSC3=  seq(1, 1, length=n1+1)
RBC3=   seq(0, 0, length=n1+1)
RBSC3=  seq(0, 0, length=n1+1)
NB3=   seq(504, 504, length=n1+1)


h1=1;
NMmin=100.0;
# ODE Model Loop (Cauchy Euler Method)
Phi1B=10;
Phi2B=20;
Phi3B=30;

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
  
  # Mosquito Population Loop Second Population
  
  SM2[i+1]=SM2[i]+h1*(((bM1[i]*NM2[i]-mM1[i]*SM2[i])*(1-SM2[i]/KB))-(c2[i]*SM2[i]*IBC2[i])/KB-(c1[i]*SM2[i]*IBSC2[i])/KB)
  EM2[i+1]=EM2[i] + h1*((c2[i]*SM2[i]*IBC2[i])/KB+(c1[i]*SM2[i]*IBSC2[i])/KB-gM[i]*EM2[i]-mM1[i]*EM2[i])
  IM2[i+1]=IM2[i] + h1*(gM[i]*EM2[i]-mM1[i]*IM2[i])
  NM2[i+1]=SM2[i+1]+EM2[i+1]+IM2[i+1]
  if (SM2[i+1]<0) {SM2[i+1]=0}
  if (NM2[i+1]<NMmin) 
  {SM2[i+1]=SM2[i]; EM2[i+1]=EM2[i]; IM2[i+1]=IM2[i]}
  
  
  # Mosquito Population Loop Third Population
  
  SM3[i+1]=SM3[i]+h1*(((bM1[i]*NM3[i]-mM1[i]*SM3[i])*(1-SM3[i]/KB))-(c2[i]*SM3[i]*IBC3[i])/KB-(c1[i]*SM3[i]*IBSC3[i])/KB)
  EM3[i+1]=EM3[i] + h1*((c2[i]*SM3[i]*IBC3[i])/KB+(c1[i]*SM3[i]*IBSC3[i])/KB-gM[i]*EM3[i]-mM1[i]*EM3[i])
  IM3[i+1]=IM3[i] + h1*(gM[i]*EM3[i]-mM1[i]*IM3[i])
  NM3[i+1]=SM3[i+1]+EM3[i+1]+IM3[i+1]
  if (SM3[i+1]<0) {SM3[i+1]=0}
  if (NM3[i+1]<NMmin) 
  {SM3[i+1]=SM3[i]; EM3[i+1]=EM3[i]; IM3[i+1]=IM3[i]}
  
  
  
  
  # Local Bird Population Loop
  SB1[i+1]=SB1[i] + h1*((bB1-(bB1-mB1)*NB1[i]/KB)*NB1[i]-mB1*SB1[i]-Phi1B*(b3[i]+b4[i])*IM1[i]*SB1[i]/KM)
  EBC1[i+1]=EBC1[i] + h1*(Phi1B*b4[i]*IM1[i]*SB1[i]/KM-mB1*EBC1[i]-gBC*EBC1[i])
  EBSC1[i+1]=EBSC1[i] + h1*(Phi1B*b3[i]*IM1[i]*SB1[i]/KM-mB1*EBSC1[i]-gBSC*EBSC1[i])
  IBC1[i+1]=IBC1[i] + h1*(gBC*EBC1[i]-mB1*IBC1[i]-a4*IBC1[i])
  IBSC1[i+1]=IBSC1[i] + h1*(gBSC*EBSC1[i]-mB1*IBSC1[i]-a3*IBSC1[i]+g3*RBSC1[i])
  RBC1[i+1]=RBC1[i] + h1*(a4*IBC1[i]-mB1*RBC1[i])
  RBSC1[i+1]=RBSC1[i] + h1*(a3*IBSC1[i]-mB1*RBSC1[i]-g3*RBSC1[i])
  NB1[i+1]=SB1[i+1]+EBC1[i+1]+IBC1[i+1]+RBC1[i+1]+EBSC1[i+1]+IBSC1[i+1]+RBSC1[i+1] 
  
  
  # Local Bird Population Loop Second Population
 
  SB2[i+1]=SB2[i] + h1*((bB1-(bB1-mB1)*NB2[i]/KB)*NB2[i]-mB1*SB2[i]-Phi2B*(b3[i]+b4[i])*IM2[i]*SB2[i]/KM)
  EBC2[i+1]=EBC2[i] + h1*(Phi2B*b4[i]*IM2[i]*SB2[i]/KM-mB1*EBC2[i]-gBC*EBC2[i])
  EBSC2[i+1]=EBSC2[i] + h1*(Phi2B*b3[i]*IM2[i]*SB2[i]/KM-mB1*EBSC2[i]-gBSC*EBSC2[i])
  IBC2[i+1]=IBC2[i] + h1*(gBC*EBC2[i]-mB1*IBC2[i]-a4*IBC2[i])
  IBSC2[i+1]=IBSC2[i] + h1*(gBSC*EBSC2[i]-mB1*IBSC2[i]-a3*IBSC2[i]+g3*RBSC2[i])
  RBC2[i+1]=RBC2[i] + h1*(a4*IBC2[i]-mB1*RBC2[i])
  RBSC2[i+1]=RBSC2[i] + h1*(a3*IBSC2[i]-mB1*RBSC2[i]-g3*RBSC2[i])
  NB2[i+1]=SB2[i+1]+EBC2[i+1]+IBC2[i+1]+RBC2[i+1]+EBSC2[i+1]+IBSC2[i+1]+RBSC2[i+1] 
  
  
  
  # Local Bird Population Loop Third Population
  
  SB3[i+1]=SB3[i] + h1*((bB1-(bB1-mB1)*NB3[i]/KB)*NB3[i]-mB1*SB3[i]-Phi3B*(b3[i]+b4[i])*IM3[i]*SB3[i]/KM)
  EBC3[i+1]=EBC3[i] + h1*(Phi3B*b4[i]*IM3[i]*SB3[i]/KM-mB1*EBC3[i]-gBC*EBC3[i])
  EBSC3[i+1]=EBSC3[i] + h1*(Phi3B*b3[i]*IM3[i]*SB3[i]/KM-mB1*EBSC3[i]-gBSC*EBSC3[i])
  IBC3[i+1]=IBC3[i] + h1*(gBC*EBC3[i]-mB1*IBC3[i]-a4*IBC3[i])
  IBSC3[i+1]=IBSC3[i] + h1*(gBSC*EBSC3[i]-mB1*IBSC3[i]-a3*IBSC3[i]+g3*RBSC3[i])
  RBC3[i+1]=RBC3[i] + h1*(a4*IBC3[i]-mB1*RBC3[i])
  RBSC3[i+1]=RBSC3[i] + h1*(a3*IBSC3[i]-mB1*RBSC3[i]-g3*RBSC3[i])
  NB3[i+1]=SB3[i+1]+EBC3[i+1]+IBC3[i+1]+RBC3[i+1]+EBSC3[i+1]+IBSC3[i+1]+RBSC3[i+1] 

}
#attach(mtcars)
#par(mfrow=c(1,3))
#plot(t1/365, IBSC1, type="p", ylab = 'Population KM', xlab = 'Time (years)', lwd = .5, main="InfMosPop",col="black")
#plot(t1/365, IBSC2, type="p", ylab = 'Population KB', xlab = 'Time (years)',lwd = .5, main="InfeCliBirdPop",col="black")
#plot(t1/365, IBSC3, type="p",ylab = 'Population KB', xlab = 'Time (years)',lwd = .5, main="InfeSubCliBirPop",col="black")

#plot( t1/365, IBSC1, type="l", col="red" )
#par(new=TRUE)
#plot( t1/365, IBSC2, type="l", col="green" )

#matplot(t1/365, cbind(IM1,IM2, IM3),type="l",col=c("red","green", "black"),lty=c(1,1,1))

 #value <- data.frame(RBSC1,RBSC2, RBSC3)
 #df <- data.frame(t1/365, value)
# 
# 
# 
# ggplot(df, aes(t1/365)) +  
#   theme_gray()+
#   geom_line(aes(y=RBSC1), colour="red") +  
#   geom_line(aes(y=RBSC2), colour="green") +
#   geom_line(aes(y=RBSC3), colour="black") 


#ggplot(df, aes(t1/365, fill=RBSC1)) + geom_area(aes(group = RBSC1, colour = RBSC1)) 
# ggplot() +
#   geom_density(data=df, 
#                aes(x=t1, colour=value, fill=value),
#                alpha=.3)
#  

#x <- seq(-pi, pi, len = 65)
#x<-1:10; y1=x*x; y2=2*y1
#plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y")
#setEPS()
#postscript("PhiImpactR0.eps")
#postscript("PhiImpactIBC.eps")
#postscript("PhiImpactIBSC.eps")
R10[R10==0]<- NA;
R20[R20==0]<- NA;
R30[R30==0]<- NA;
df1 <- data.frame(R10, IM1, IBSC1, IBC1)
df1$Group <-"R1"
df2 <- data.frame(R20, IM2, IBSC2, IBC2)
df2$Group <-"R2"
df3 <- data.frame(R30, IM3, IBSC3, IBC3)
df3$Group <-"R3"
results <- data.frame(df1, df2, df3)
R0<-data.frame(R10, R20, R30)
# ggplot(data=results, aes(x=t1/365, y=R30/5,colour=Group)) +
#   theme_gray()+
#   geom_point(size=2, colour="blue")+
#   labs(x = "Time", y= expression(paste("", R[0])), size = 15)+
#   theme(axis.ticks = element_line(size = 2))+
#   theme(axis.text = element_text(colour = "black", size = 15))+
#   theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=32, hjust=0)) +
#   theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=32))

#################
ggplot(results)+
  geom_point(aes(x=t1/365, y=R10/5, color = "red"),size=2)+
  geom_point(aes(x=t1/365, y=R20/5, color = "blue"),size=2)+
  geom_point(aes(x=t1/365, y=R30/5, color = "black"),size=2)+
  theme(axis.title = element_text(color="black", face="bold", size=12))+
  theme(legend.title=element_blank())+
  labs(x = "Time", y= expression(paste("", R[0])), size = 60)+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(legend.background = element_rect())+
  theme(legend.text = element_text( size = 20, face = "bold"))+
  theme(axis.title = element_text( face="bold", size=22))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c('red' = 'red','blue' = 'blue', 'black'='black'),
                      name = '', labels = expression(paste(R1[0]),R2[0], R3[0]))
ggsave("phiImpactR0.eps", width = 17, height = 17, units = "cm")
# ggplot(AllData)+
#   geom_point(aes(x=AllData$Time, y=AllData$R0, color = "red"),size=2)+
#   geom_point(aes(x=AllData$Time, y=AllData$R0.1, color = "blue"),size=2)+
#   geom_point(aes(x=AllData$Time, y=AllData$R0.2, color = "black"),size=2)+
#   theme(axis.title = element_text(color="black", face="bold", size=12))+
#   theme(legend.title=element_blank())+
#   labs(x = "Time", y= expression(paste("", R[0])), size = 60)+
#   theme(axis.text = element_text(colour = "black", size = 15))+
#   theme(legend.background = element_rect())+
#   theme(legend.text = element_text( size = 20, face = "bold"))+
#   theme(axis.title = element_text( face="bold", size=22))+
#   theme(legend.title=element_blank())+
#   scale_colour_manual(values = c('red' = 'red','blue' = 'blue', 'black'='black'),
#                       name = '', labels = expression(paste(R1[0]),R2[0], R3[0]))



ggsave("mMImpactR0.eps", width = 17, height = 17, units = "cm")

##scale_fill_discrete(breaks=c(expression(paste("", R1[0])),expression(paste("", R2[0])),expression(paste("", R3[0]))))+
                      
##scale_colour_manual(values = c('red' = 'red','blue' = 'blue', 'black'='black'),
  #name = '', labels = expression(P[M1](tilde(z)>0),P[M0](tilde(z)>0), P[M2](tilde(z)>0)))



#matplot(t1/365, R10/5, type="p", lwd=1, pch=1, lty=1.5, col="red", xlab= "Time (Years)", ylab=expression(paste("",R[0])),cex.axis=1.4,cex.lab=1.4,xlim=c(0, 10), ylim=c(2, 13))
# Add a line
#lines(t1/365, R20/5, pch=1, col="blue", type="p", lwd=1.5, lty=1)
#lines(t1/365, R30/5, pch=1, col="black", type="p",lwd=1.5  ,lty=1)
# Add a legend 
#legend("topr",inset=.0001,90, 90, legend=c(expression(paste("", phi[1])), expression(paste("", phi[2])),expression(paste("", phi[3]))),
 #      col=c("red", "blue", "black"), lty=2:2, cex=2,text.font=4,bty = "n")

 
#title(main= expression(paste("Impact of different values of  ", phi)),cex.main=2)


#dev.off()
