# R script to plot the birth and death rates of the mosquitoes vs Time and Temperature
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
library(cowplot)
mydata = read_excel("Summary-4.xlsx", sheet =1)
T1=mydata$Temp_Lahr;
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
gM=seq(0, 0, length=n1+1)
for (i in 1:n1)
{
  if(T1[i]>15)
    gM[i]=0.0093*T1[i]-.1352
}


# Transmission Parameters for local birds
c2=k1*.99;
c1=k1*.89;
gM[gM==0] <-NA
df <- data.frame(T1,t1,bM1, mM1, c1, c2, gM,k1)

#library(cowplot)
#setEPS()
 #postscript("Mosquito_Params1.eps")
 #postscript("Mosquito_Params2.eps")
 #postscript("Mosquito_Params3.eps")
#postscript("Mosquito_Params4.eps")
#ggplot(data=df, aes(x=T1, y=1/(5*gM))) +
ggplot(data=df, aes(x=T1, y=k1)) +
#ggplot(data=df, aes(x=T1, y= bM1)) +
 #ggplot(data=df, aes(x=T1, y= mM1)) +
  theme_cowplot()+
 geom_line(size=2, colour="blue")+
  #labs(x =  expression(paste('Temperature T [', ~degree,'C]')), y= expression(paste(g[M]  , ' [1/Days]')), size = 15)+
   # labs(x =  expression(paste('Temperature T [', ~degree,'C]')), y= expression(paste(1/g[M]  , ' [Days]')), size = 15)+
 # labs(x =  expression(paste('Temperature T [', ~degree,'C]')), y= expression('Birth Rate [1/Days]'), size = 15)+
   labs(x =  expression(paste('Temperature T [', ~degree,'C]')), y= expression('Death Rate [1/Days]'), size = 15)+
   
   theme(axis.ticks = element_line(size = .5))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=22))+
  scale_x_continuous(breaks = round(seq(-10, 30, by =5), 1))
#ggsave("MosbM.eps", width = 20, height = 20, units = "cm")  
ggsave("MosmM.eps", width = 20, height = 20, units = "cm")  

 # theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0)) +
  #theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

 # 
 # scatterplot3d(T1, bM1, mM1,
 #               main="3-D Scatterplot Example 1")

#scatter3D(T1, bM1, mM1, phi = 0, bty = "g", pch = 10, cex = 0.5, xlab = "Temperature",
 #         ylab ="Mortality", zlab = "Birth Rate", ticktype = "detailed",colvar = NULL,cex.main=2,cex.axis=1.2,cex.lab=1.5 )

 #dev.off()
