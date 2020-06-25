# Including time dependencies

library(stats)
library(graphics)
library(grDevices)
library(gdata)
#library(xlsx)
library(readxl)
library(ggplot2)
#require(xlsx)
library(plot3D)
#library(plotly)
library(scatterplot3d)
#n1=36500;
n1=7300
t1=seq(0,n1)
T1=10.665-6.729*cos((2*pi*t1)/365-.4)    # Greifswald
#T1=9.365-3.218*sin((2*t1*pi)/365)-8.114*cos((2*pi*t1)/365)
###########################
##T1=11.162-8.871*cos((2*pi*t1)/365-.38)#11.018-2.513*sin((2*t1*pi)/365)-8.489*cos((2*pi*t1)/365);   
##T1<- 11.018-2.513*sin((2*t1*pi)/365)-8.489*cos((2*pi*t1)/365)# Lahr                             
#T1=9.594-8.398*cos((2*pi*t1)/365-.38) #9.594-2.777*sin((2*t1*pi)/365)-7.934*cos((2*pi*t1)/365);  # G??ttingen
## 10.665 +.24 C
##########################################
T2 <- 11.16275-8.91589*cos(2*(pi/365)*t1-0.29) ## Lahr
#T1<-  9.35903-8.64518*cos(2*(pi/365.25)*t1-0.5) ## Greifswald in .2C
# Mosquito Population Parameters Greifswald #####
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
  if(T1[i]>15)
    gM[i]=0.0093*T1[i]-.1352
}
#gM=.012;
# Transmission Parameters for local birds
c2=k1*.92;
c1=k1*.89;
# Transmission Parameters for Migratory birds
c3=k1*.69;

# Mosquito Population Parameters Lahr #####
k11=seq(0, n1); 
k11=0.344/(1+1.231*exp(-0.184*(T2-20)))
bM11=seq(0, n1); 
bM11=2.325*k11/10; 
mM11=seq(0, n1); 
mM11=0.0025*T2^2-0.094*T2+1.0257; 
mM11=mM11/30;
KM=1000000;
gM1=seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  if(T2[i]>15)
    gM1[i]=0.0093*T2[i]-.1352
}
# Transmission Parameters for local birds
c21=k11*.92;
c11=k11*.89;
# Transmission Parameters for Migratory birds
c31=k11*.69
# Recruitment Rate From Susceptible local Birds Lahr
b31=.23*k11;
b41=.29*k11;
# Recruitment Rate From migratory local Birds Lahr
b51=.30*k11;
# Local Bird Population Parameters
KB=10000;
# Migratory Bird Population Parameters
KBm=10000;
# Birth Rate for local birds population
bB1=0.0074;
# Birth Rate for migratory birds population
bBm=0.0054;
# Mortality Rate for local brids
mB1=.0022;
# Mortality Rate for migratory brids
mBm=.0012;
# Recruitment Rate From Susceptible local Birds Greifswald
b3=.23*k1;
b4=.29*k1;
# Recruitment Rate From migratory local Birds Greifswald
b5=.30*k1;
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
g3=.62;


# initialisation
# First Mosquito Population
SM1=seq(100, 100, length=n1+1)
EM1=seq(0, 0, length=n1+1)
IM1=seq(1, 1, length=n1+1)
NM1=seq(101, 101, length=n1+1)


#  Local Bird Population 
#First Bird Population
SB1=   seq(500, 500, length=n1+1)
EBC1=   seq(0, 0, length=n1+1)
EBSC1=  seq(0, 0, length=n1+1)
IBC1=   seq(0, 0, length=n1+1)
IBSC1=  seq(0, 0, length=n1+1)
RBC1=   seq(0, 0, length=n1+1)
RBSC1=  seq(0, 0, length=n1+1)
NB1=   seq(500, 500, length=n1+1)


# Second Mosquito Population
SM2=seq(100,100, length=n1+1)
EM2=seq(0, 0, length=n1+1)
IM2=seq(1, 1, length=n1+1)
NM2=seq(101, 101, length=n1+1)


#  Local Bird Population 
#First Bird Population
SB2=   seq(500, 500, length=n1+1)
EBC2=   seq(0, 0, length=n1+1)
EBSC2=  seq(0, 0, length=n1+1)
IBC2=   seq(0, 0, length=n1+1)
IBSC2=  seq(0, 0, length=n1+1)
RBC2=   seq(0, 0, length=n1+1)
RBSC2=  seq(0, 0, length=n1+1)
NB2=   seq(500, 500, length=n1+1)




h1=1;
NMmin=100.0;
# Calculating R_0 and then to plot against the birth and mortality rates of the mosquitoes
R0=seq(0, 0, length=n1+1)
R10=seq(0, 0, length=n1+1)
# ODE Model Loop (Cauchy Euler Method)
PhiB=25;
for (i in 1:n1)
  
{
  
  # Mosquito Population Loop
  # First Population
  SM1[i+1]=SM1[i]+h1*(((bM1[i]*NM1[i]-mM1[i]*SM1[i])*(1-SM1[i]/KB))-(c2[i]*SM1[i]*IBC1[i])/KB-(c1[i]*SM1[i]*IBSC1[i])/KB)
  EM1[i+1]=EM1[i] + h1*((c2[i]*SM1[i]*IBC1[i])/KB+(c1[i]*SM1[i]*IBSC1[i])/KB-gM[i]*EM1[i]-mM1[i]*EM1[i])
  IM1[i+1]=IM1[i] + h1*(gM[i]*EM1[i]-mM1[i]*IM1[i])
  NM1[i+1]=SM1[i+1]+EM1[i+1]+IM1[i+1]
  #write.csv(SM1,file = "SM1.csv")
  if (SM1[i+1]<0) {SM1[i+1]=0}
  if (NM1[i+1]<NMmin) 
  {SM1[i+1]=SM1[i]; EM1[i+1]=EM1[i]; IM1[i+1]=IM1[i]}
  
  
  
  # Local Bird Population Loop
  #First Population
  SB1[i+1]=SB1[i] + h1*((bB1-(bB1-mB1)*NB1[i]/KB)*NB1[i]-mB1*SB1[i]-PhiB*(b3[i]+b4[i])*IM1[i]*SB1[i]/KM)
  EBC1[i+1]=EBC1[i] + h1*(PhiB*b4[i]*IM1[i]*SB1[i]/KM-mB1*EBC1[i]-gBC*EBC1[i])
  EBSC1[i+1]=EBSC1[i] + h1*(PhiB*b3[i]*IM1[i]*SB1[i]/KM-mB1*EBSC1[i]-gBSC*EBSC1[i])
  IBC1[i+1]=IBC1[i] + h1*(gBC*EBC1[i]-mB1*IBC1[i]-a4*IBC1[i])
  IBSC1[i+1]=IBSC1[i] + h1*(gBSC*EBSC1[i]-mB1*IBSC1[i]-a3*IBSC1[i]+g3*RBSC1[i])
  RBC1[i+1]=RBC1[i] + h1*(a4*IBC1[i]-mB1*RBC1[i])
  RBSC1[i+1]=RBSC1[i] + h1*(a3*IBSC1[i]-mB1*RBSC1[i]-g3*RBSC1[i])
  NB1[i+1]=SB1[i+1]+EBC1[i+1]+IBC1[i+1]+RBC1[i+1]+EBSC1[i+1]+IBSC1[i+1]+RBSC1[i+1] 
  
  
  # Mosquito Population Loop
  # Second Population
  SM2[i+1]=SM2[i]+h1*(((bM11[i]*NM2[i]-mM11[i]*SM2[i])*(1-SM2[i]/KB))-(c21[i]*SM2[i]*IBC2[i])/KB-(c11[i]*SM2[i]*IBSC2[i])/KB)
  EM2[i+1]=EM2[i] + h1*((c21[i]*SM2[i]*IBC2[i])/KB+(c11[i]*SM2[i]*IBSC2[i])/KB-gM1[i]*EM2[i]-mM11[i]*EM2[i])
  IM2[i+1]=IM2[i] + h1*(gM1[i]*EM2[i]-mM11[i]*IM2[i])
  NM2[i+1]=SM2[i+1]+EM2[i+1]+IM2[i+1]
  if (SM2[i+1]<0) {SM2[i+1]=0}
  if (NM2[i+1]<NMmin) 
  {SM2[i+1]=SM2[i]; EM2[i+1]=EM2[i]; IM2[i+1]=IM2[i]}
  
  
  
  # Local Bird Population Loop
  #Second Population
  SB2[i+1]=SB2[i] + h1*((bB1-(bB1-mB1)*NB2[i]/KB)*NB2[i]-mB1*SB2[i]-PhiB*(b31[i]+b41[i])*IM2[i]*SB2[i]/KM)
  EBC2[i+1]=EBC2[i] + h1*(PhiB*b41[i]*IM2[i]*SB2[i]/KM-mB1*EBC2[i]-gBC*EBC2[i])
  EBSC2[i+1]=EBSC2[i] + h1*(PhiB*b31[i]*IM2[i]*SB2[i]/KM-mB1*EBSC2[i]-gBSC*EBSC2[i])
  IBC2[i+1]=IBC2[i] + h1*(gBC*EBC2[i]-mB1*IBC2[i]-a4*IBC2[i])
  IBSC2[i+1]=IBSC2[i] + h1*(gBSC*EBSC2[i]-mB1*IBSC2[i]-a3*IBSC2[i]+g3*RBSC2[i])
  RBC2[i+1]=RBC2[i] + h1*(a4*IBC2[i]-mB1*RBC2[i])
  RBSC2[i+1]=RBSC2[i] + h1*(a3*IBSC2[i]-mB1*RBSC2[i]-g3*RBSC2[i])
  NB2[i+1]=SB2[i+1]+EBC2[i+1]+IBC2[i+1]+RBC2[i+1]+EBSC2[i+1]+IBSC2[i+1]+RBSC2[i+1] 
  
  
  
  
}

# resultIBSC1<-data.frame(IBSC1, t1)
# resultIBC1<-data.frame(IBC1, t1)
# resultSB1<-data.frame(SB1, t1)
# resultIM1<-data.frame(IM1, t1)
# resultSM1<-data.frame(SM1, t1)
# resultEM1<-data.frame(EM1, t1)
# resultPhaseSMIM<-data.frame(SM1, IM1)
# resultPhaseSBIBC<-data.frame(SB1, IBC1)
# resultPhaseSBIBSC<-data.frame(SB1, IBSC1)

#plot(SM1,IM1, type="l",lwd = 1, main="Phase Plot",col="red")
#lines(SM2, IM2, type="l", lwd = 1,col="blue")
#lines(SM3, IM3, type="l", lwd = 1,col="green")
#setEPS()
#postscript("Greif2C1Mos.eps", width = 12.0, height = 10.0)
#postscript("Greif2C1Bir.eps", width = 12.0, height = 10.0)
#postscript("TimeDependenceBirdLahr.eps")
#postscript("TimeDependenceBirdGreifswald.eps")
#postscript("TimeDependenceMosLahr.eps")
#postscript("TimeDependenceMosGreifswald.eps")
#postscript("TimeSeriesSubCliniLahr.eps")
#postscript("TimeSeriesSubCliniGreif.eps")
#xstart<-t1[3500];
#xend<-t1[10000];
#plot(t1, SB3/NB3)
#plot(T1, gM)
#ggplot(resultIBSC1, aes(x=t1,y=IBSC1))+
 # geom_line(size = 2)+
  #theme_bw()+
  #labs(x="Time",y="Sub Clinical", face="bold",size = (30))+
  #theme(axis.text=element_text(size=18),
   #     axis.title=element_text(size=18,face="bold"))
#+xlim(c(xstart,xend))+
#plot(t1/365, IBSC1, type="l",lwd = 3, col="blue",xlab= "Time (Year)",  ylab="Subclinical Bird",cex.main=2,cex.axis=1.2,cex.lab=1.5 )
#lines(t1/365, IBSC2, type="p", lwd = 1,col="blue")
#lines(t1/365, IBSC3, type="p", lwd = 1,col="green") 
#main="Phase Plot", 
#x=IBC1;
#y=IBC1;
#z=SB1;  xlim=c(0, 10)
#plot_ly(x=x,y=y,z=z, type="surface")

#scatterplot3d(SB1,IBC1,IBSC1, type="g", color = 'red')
#scatter3D(SB1, IBC1, IBSC1, bty = "u", pch = 18, col = gg.col(200))
#cairo_ps("TimeDependenceBirdLahr.eps")
#scatter3D(SB1, IBC1, IBSC1, phi = 0,bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible Bird",
 #         ylab ="Clinical Bird", zlab = "Sub-Clinical Bird", type = "l",ticktype = "detailed",colvar = NULL,cex.main=2,cex.axis=1.2,cex.lab=1.5 )


scatter3D(SM1, EM1, IM1, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
       ylab ="Exposed", zlab = "Infected", type = "l", ticktype = "detailed", colvar = NULL,cex.main=2,cex.axis=1.5,cex.lab=1.5 )

scatter3D(SM2, EM2, IM2, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Exposed", zlab = "Infected", type = "l", ticktype = "detailed", colvar = NULL,cex.main=2,cex.axis=1.2,cex.lab=1.5 )
#scatterplot3d(SB1,IBC1,IBSC1, lwd = 3, type="l", xlab = "Susceptible",
 #             ylab ="Exposed", zlab = "Infected")

#plot(t1, IM1, type="p") ###Lahr

#plot(t1, SB1, type = "p") ### Greifswald

#dev.off()

#lines(IBSC1,IBC1)
#plot(IBSC2,IBC2)
scatter3D(SM2, EM2, IM2, phi = 0, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Exposed", zlab = "Infected", type = "l", colvar = NULL,
          ticktype = "detailed",cex.main=2,cex.axis=1.2,cex.lab=1.5)


scatter3D(SM1, EM1, IM1, phi = 0, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Exposed", zlab = "Infected", type = "l", colvar = NULL,ticktype = "detailed",cex.main=2,cex.axis=1.2,cex.lab=1.5)

layout(matrix(c(1, 2), 1))
scatterplot3d(SB1,IBC1,IBSC1, type="l", color = 'red')
scatterplot3d(SB2,IBC2,IBSC2, type="l", color = 'red')
scatter3D(SM1, EM1, IM1, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Exposed", zlab = "Infected", type = "l", ticktype = "detailed",
          cex.main=2,cex.axis=1.2,cex.lab=1.5,colvar = NULL )
lines3D(SM2, EM2, IM2, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
             ylab ="Exposed", zlab = "Infected", type = "l", ticktype = "detailed",axis.scales=TRUE,
          cex.main=2,cex.axis=1.2,cex.lab=1.5,colvar = NULL,col="black" )
       

lines3D((SB2), (IBC2), (IBSC2), phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2, xlab = "Susceptible",
        ylab ="Clinical Bird", zlab = "Subclinical Bird", type = "l", ticktype = "detailed",
        grid=TRUE, grid.lines=26,
        cex.main=2,cex.axis=2.5,cex.lab=3,colvar = NULL,col="black" )

lines3D((SB1), 200*(IBC1),25*(IBSC1), phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2, xlab = "Susceptible",
        ylab ="Clinical Bird", zlab = "Subclinical Bird", type = "l", ticktype = "detailed",
        grid=TRUE, grid.lines=26,
        cex.main=2,cex.axis=2.5,cex.lab=3,colvar = NULL,col="black" )
cairo_ps("TimeDependenceBirdGreig.eps", width = 15, height = 13, pointsize = 12)
cairo_ps("TimeDependenceBirdGreig2C.eps", width = 15, height = 13, pointsize = 12)
cairo_ps("TimeDependenceBirdLahr.eps", width = 15, height = 13, pointsize = 12)

scatter3D((SB1), IBC1, IBSC1, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Clinical Bird", zlab = "Subclinical Bird", type = "l", ticktype = "detailed",axis.scales=TRUE,
          cex.main=2,cex.axis=1.2,cex.lab=1.5,colvar = NULL )
cairo_ps("TimeDependenceMosGreig.eps")
cairo_ps("TimeDependenceMosGreig2C.eps", width = 15, height = 13, pointsize = 12)
scatter3D((SM1), EM1, IM1, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Exposed", zlab = "Infected", type = "l", ticktype = "detailed",
          grid=TRUE, grid.lines=26,
          cex.main=2,cex.axis=2.5,cex.lab=3,colvar = NULL )
dev.off()
scatter3D((SB2), IBC2, IBSC2, phi = 0, bty = "g", pch = 20, cex = 2.5, lwd=2.5, xlab = "Susceptible",
          ylab ="Clinical Bird", zlab = "Subclinical Bird", type = "l", ticktype = "detailed",
          cex.main=2,cex.axis=1.2,cex.lab=1.5,colvar = NULL )


