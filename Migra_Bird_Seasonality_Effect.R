# Temperature dependence in b3 and b4 implemented
# Temperature dependent gM aka incubation period is being implemented
# Here we have included the Migratory birds 
# At the first step we shall keep the Migratory Birds present for the whole simulations 
# then we shall make them to appear periodically (It isdone)
#library(stats)
library(graphics)
library(grDevices)
library(gdata)
#library(xlsx)
library(readxl)
library(ggplot2)
#require(xlsx)
library(plot3D)
################################
# multiplot <- function(..., plotlist=p1,p2, file, cols=2, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
######################################################


mydata = read_excel("Summary-4.xlsx", sheet =1)
T1=mydata$Temp_Greifswald
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
mM1=mM1/32;
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
#R0=sqrt(((KM*gM*c2*gBC*b4)/(KB*mM1*(gM+mM1)*(a4+mB1)*(gBC+mB1)))+((KM*gM*c1*gBSC*b3)/(KB*mM1*(gM+mM1)*(a3+mB1)*(gBSC+mB1))))

# initialisation
# Mosquito Population
SM1=seq(10000, 10000, length=n1+1)
EM1=seq(1, 1, length=n1+1)
IM1=seq(1, 1, length=n1+1)
NM1=seq(10002, 10002, length=n1+1)


#  Local Bird Population
SB1=   seq(500, 500, length=n1+1)
EBC=   seq(1, 1, length=n1+1)
EBSC=  seq(1, 1, length=n1+1)
IBC=   seq(1, 1, length=n1+1)
IBSC=  seq(1, 1, length=n1+1)
RBC=   seq(0, 0, length=n1+1)
RBSC=  seq(0, 0, length=n1+1)
NB1=   seq(504, 504, length=n1+1)


# Migratory Bird Population

SBm=   seq(500, 500, length=n1+1)
EBm=   seq(0, 0, length=n1+1)
IBm=   seq(10, 10, length=n1+1)
RBm=   seq(0, 0, length=n1+1)
NBm=   seq(510, 510, length=n1+1)

h1=1;
NMmin=100.0;
# ODE Model Loop (Cauchy Euler Method)
PhiB=30;
for (i in 1:n1)
  
{
  
  # Mosquito Population Loop
  
  SM1[i+1]=SM1[i]+h1*(((bM1[i]*NM1[i]-mM1[i]*SM1[i])*(1-SM1[i]/KB))-(c2[i]*SM1[i]*IBC[i])/KB-(c1[i]*SM1[i]*IBSC[i])/KB-(c3[i]*SM1[i]*IBm[i])/KBm)
  EM1[i+1]=EM1[i] + h1*((c2[i]*SM1[i]*IBC[i])/KB+(c1[i]*SM1[i]*IBSC[i])/KB+(c3[i]*SM1[i]*IBm[i])/KBm-gM[i]*EM1[i]-mM1[i]*EM1[i])
  IM1[i+1]=IM1[i] + h1*(gM[i]*EM1[i]-mM1[i]*IM1[i])
  NM1[i+1]=SM1[i+1]+EM1[i+1]+IM1[i+1]
  if (SM1[i+1]<0) {SM1[i+1]=0}
  if (NM1[i+1]<NMmin) 
  {SM1[i+1]=SM1[i]; EM1[i+1]=EM1[i]; IM1[i+1]=IM1[i]}
  # Local Bird Population Loop
  SB1[i+1]=SB1[i] + h1*((bB1-(bB1-mB1)*NB1[i]/KB)*NB1[i]-mB1*SB1[i]-PhiB*(b3[i]+b4[i])*IM1[i]*SB1[i]/KM)
  EBC[i+1]=EBC[i] + h1*(PhiB*b4[i]*IM1[i]*SB1[i]/KM-mB1*EBC[i]-gBC*EBC[i])
  EBSC[i+1]=EBSC[i] + h1*(PhiB*b3[i]*IM1[i]*SB1[i]/KM-mB1*EBSC[i]-gBSC*EBSC[i])
  IBC[i+1]=IBC[i] + h1*(gBC*EBC[i]-mB1*IBC[i]-a4*IBC[i])
  IBSC[i+1]=IBSC[i] + h1*(gBSC*EBSC[i]-mB1*IBSC[i]-a3*IBSC[i]+g3*RBSC[i])
  RBC[i+1]=RBC[i] + h1*(a4*IBC[i]-mB1*RBC[i])
  RBSC[i+1]=RBSC[i] + h1*(a3*IBSC[i]-mB1*RBSC[i]-g3*RBSC[i])
  NB1[i+1]=SB1[i+1]+EBC[i+1]+IBC[i+1]+RBC[i+1]+EBSC[i+1]++IBSC[i+1]+RBSC[i+1] 
  
  # Migratory Bird Population Loop
  
  SBm[i+1]=SBm[i] + h1*((bBm-(bBm-mBm)*NBm[i]/KBm)*NBm[i]-mBm*SBm[i]-(PhiB*b5[i]*IM1[i]*SBm[i])/KM)
  EBm[i+1]=EBm[i] + h1*((PhiB*b5[i]*IM1[i]*SBm[i])/KM-mBm*EBm[i]-gBm*EBm[i])
  IBm[i+1]=IBm[i] + h1*(gBm*EBm[i]-mBm*IBm[i]-a9*IBm[i])
  RBm[i+1]=RBm[i] + h1*(a9*IBm[i]-mBm*RBm[i])
  (NBm[i+1]=SBm[i+1]+EBm[i+1]+IBm[i+1]+RBm[i+1])
  if (i>180 & i<361) 
  {(NBm[i+1]=0)}
  else if (i>542 & i< 723)
  {(NBm[i+1]=0)} 
  else if (i>904 & i< 1084)
  {(NBm[i+1]=0)} 
  else if (i>1266 & i< 1446)
  {(NBm[i+1]=0)} 
  else if (i>1627 & i< 1808)
  {(NBm[i+1]=0)} 
  else if (i>1990 & i< 2170)
  {(NBm[i+1]=0)} 
  else if (i>2352 & i< 2532)
  {(NBm[i+1]=0)} 
  else if (i>2714 & i< 2894)
  {(NBm[i+1]=0)} 
  else if (i>3076 & i< 3256)
  {(NBm[i+1]=0)} 
  else if (i>3438 & i< 3618)
  {(NBm[i+1]=0)} 
  
}

#setEPS()
#postscript("infect.eps")
#attach(mtcars)
#par(mfrow=c(2,1))
#(SBm[i+1]=0) & (RBm[i+1]=0) & (IBm[i+1]=0) & (EBm[i+1]=0) & 
#plot(t1/365,SM1,  type="p", ylab = expression("Population KM"), xlab = 'Time (years)',lwd = .5, main="SusMosPop",col="black")
#plot(t1/365, EM1, type="p",ylab = 'Population KM', xlab = 'Time (years)', lwd = .5, main="ExpMosPop",col="black")
#plot(t1/365, IM1, type="l", ylab = 'Population KM', xlab = 'Time (years)', lwd = 2.5, main="Infected Mosquito Population",col="black")
#plot(t1/365, SB1, type="p",ylab = 'Population KB', xlab = 'Time (years)', lwd = .5, main="SusBirPop",col="black")
#plot(t1/365, EBC, type="p", ylab = 'Population (K_B', xlab = 'Time (years)',lwd = .5, main="ExpCliBirPop",col="black")
#plot(t1/365, EBSC, type="p", ylab = 'Population KB', xlab = 'Time (years)',lwd = .5, main="ExpSubCliBirPop",col="black")
#plot(t1/365, IBC, type="l", ylab = 'Population KB', xlab = 'Time (years)',lwd = 2.5, main="Infected Clinical Bird Population.",col="black")
#plot(t1/365, IBSC, type="p",ylab = 'Population KB', xlab = 'Time (years)',lwd = .5, main="InfeSubCliBirPop",col="black")
#plot(t1/365, RBC, type="p", ylab = 'Population KB', xlab = 'Time (years)',lwd = .5, main="RecCliBirdPop",col="black")
#plot(t1/365, RBSC, type="p",ylab = 'Population KB', xlab = 'Time (years)',lwd = .5, main="RecSubCliBirPop",col="black")
#plot(t1/365, SBm, type="l",ylab = 'Population KB', xlab = 'Time (years)', lwd = .5, main="SuscMigBirPop",col="black")
#plot(t1/365, EBm, type="p",ylab = 'Population KB', xlab = 'Time (years)', lwd = .5, main="ExpMigBirPop",col="black")
#plot(t1/365, IBm, type="p",ylab = 'Population KB', xlab = 'Time (years)', lwd = .5, main="InfMigBirPop",col="black")
#plot (t1/365,T1,xlab = 'Time (years)', ylab = 'Temperature',type="p",lwd = .5, main="Temperature Vs Time",col="black")
#dev.off()

library(cowplot)
library(ggpubr)
plotdata1 <- data.frame(cbind(t1/365,IM1))
plotdata2 <- data.frame(cbind(t1/365,IBSC))
plotdata3 <- data.frame(cbind(t1/365,IBC))
plotdata4 <- data.frame(cbind(t1/365,mydata$Temp_Greifswald))
p1<-ggplot(plotdata1, aes(x=t1/365, y=IM1)) +
  geom_line(size=.9) +
  theme_cowplot(35)+
  labs(x="Time [Year]",y="Infected Mosquito", size = 200)+
  theme(plot.title = element_text(hjust = 0.9))+
  theme(axis.text.x = element_text(face="bold",size=25, angle=0),axis.text.y = element_text(face="bold",size=20, angle=0))+
  theme(axis.title.y = element_text(face="bold", size=25))+
  theme(axis.title.x = element_text(face="bold", size=25))+
  theme(plot.title = element_text(size = 35, face = "bold"))
  




p2<-ggplot(plotdata2, aes(x=t1/365, y=IBSC)) +
  geom_line(size=.9) +
  theme_cowplot(35)+
  labs(x="Time [Year]",y=" Subclinical Bird ", size = 200)+
  theme(plot.title = element_text(hjust = 0.9))+
  theme(axis.text.x = element_text(face="bold",size=25, angle=0),axis.text.y = element_text(face="bold",size=20, angle=0))+
  theme(axis.title.y = element_text(face="bold", size=25))+
  theme(axis.title.x = element_text(face="bold", size=20))+
  theme(plot.title = element_text(size = 35, face = "bold"))




p3<-ggplot(plotdata3, aes(x=t1/365, y=IBC)) +
  geom_line(size=.9) +
  theme_cowplot(35)+
  labs(x="Time [Year]",y=" Clinical Bird ", size = 200)+
  theme(plot.title = element_text(hjust = 0.9))+
  theme(axis.text.x = element_text(face="bold",size=25, angle=0),axis.text.y = element_text(face="bold",size=20, angle=0))+
  theme(axis.title.y = element_text(face="bold", size=25))+
  theme(axis.title.x = element_text(face="bold", size=25))+
  theme(plot.title = element_text(size = 35, face = "bold"))




p4 <-ggplot(mydata, aes(x=t1/365, y=mydata$Temp_Greifswald)) +
  geom_line(size=.9) +
  theme_cowplot(35)+
  labs(x="Time [Year]",y="Temperature", size = 200)+
  theme(plot.title = element_text(hjust = 0.9))+
  theme(axis.text.x = element_text(face="bold",size=25, angle=0),axis.text.y = element_text(face="bold",size=20, angle=0))+
  theme(axis.title.y = element_text(face="bold", size=25))+
  theme(axis.title.x = element_text(face="bold", size=25))+
  theme(plot.title = element_text(size = 35, face = "bold"))
p4
#multiplot(p1, p2, cols=2)
#ggsave(file="Infection.eps")
#dev.off()
gridplot<-plot_grid(p1, p2, p3, ncol = 1, nrow = 3)
#plot_grid(p1, p2, ncol = 1, nrow = 2)
ggsave(file="GreifInfCur.eps", width = 30, height = 40, units = "cm")
ggsave(file="LahrInfCur.eps", width = 30, height = 40, units = "cm")
#dev.off()
save_plot("gridplot.png", gridplot,
          base_aspect_ratio = 1.1 # make room for figure legend
)

