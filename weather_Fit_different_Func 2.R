#Interpolating weather data with two different kinds of sine and cosine functions
library(stats)
library(graphics)
library(grDevices)
library(gdata)
#library(xlsx)
library(readxl)
library(ggplot2)
library(cowplot)

#require(xlsx)
library(plot3D)
mydata = read_excel("Summary-4.xlsx", sheet =1)
T1=mydata$Temp_Lahr
T2=mydata$Temp_Greifswald
T2 <- T2+1.2
l1=length(T1)-1;
n1=seq(0,l1)
#plot(n,T1)
train=seq(1,3654)
l=lm(T1~sin(2*pi/365*train)+cos(2*pi/365.25*train))
l1=lm(T1~cos(2*(pi/365.25)*train-.38))
l2=lm(T1~sin(2*pi/365.25*train))
#summary(l)
#setEPS()
l_Greif=lm(T2~cos(2*(pi/365.25)*train-.5))

#postscript("Gottin.eps")
df<- data.frame(T1,n1,l1$fitted.values)
## Lahr###
ggplot(df)+
   theme_bw()+
  geom_line(aes(x=n1, y=T1), size=1, colour="red")+
 geom_line(aes(x=n1, y=l1$fitted.values), size=1, colour="blue")+
labs(x = "Time (Days)", y= "Temperature (C)", size = 15)+
 theme(axis.ticks = element_line(size = 1))+
theme(axis.text = element_text(colour = "black", size = 15))+
theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
theme(axis.title = element_text( face="bold", size=22))+
scale_x_continuous(breaks = round(seq(0, 4000, by =365)))

### Greif ###
df1<- data.frame(T2,n1,l_Greif$fitted.values)
ggplot(df1)+
  theme_bw()+
  geom_line(aes(x=n1, y=T2), size=1, colour="red")+
  geom_line(aes(x=n1, y=l_Greif$fitted.values), size=1, colour="black")+
  labs(x = "Time (Days)", y= "Temperature (C)", size = 15)+
  theme(axis.ticks = element_line(size = 1))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=22))+
  scale_x_continuous(breaks = round(seq(0, 4000, by =365)))

plot.ts(l_Greif$fitted.values)

summary(l_Greif)

curve((9.36439-8.66448*cos(2*(pi/365)*x - 0.5)), from=1, to=365*10)
curve((9.736439-8.66448*cos(2*(pi/365)*x - 0.5)), from=1, to=365*10)

x <- seq(0,365*40)
y1 <- (9.36439-8.66448*cos(2*(pi/365)*x - 0.5))
y2 <- (9.736439-8.66448*cos(2*(pi/365)*x - 0.5))
   
plot(y1,y2, type = 'l')    
lines(x,y1)      
       
0.05783133
## Greifswald
curve((0.00005783133*x -8.64518*cos(2*(pi/365)*x - 0.5)), from=1, to=365*80)
###########################
eq1 = function(x) {0.00005783133*x -8.64518*cos(2*(pi/365)*x - 0.5)}

ggplot(data.frame(x=c(1, 365*80)), aes(x=x)) + stat_function(fun=eq1, geom="line") + 
  xlab("Time in Days") + 
  ylab("Temperature (°C)")
########################
library("ggplot2")
eq = function(x){x*x}
ggplot(data.frame(x=c(1, 50)), aes(x=x)) + 
  stat_function(fun=eq, geom="line") + 
  xlab("x") + ylab("y")
# # +
# #scale_y_continuous(breaks = round(seq(0, 3654, by =5), 1))
# plot(T2,type="l",col="red",ylab = 'Temparature(C)', xlab = 'Time (Days)',lwd = 1,main="Time Series: Temperature",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# lines(l$fitted.values,col="blue",lwd = 2.5, 
#       cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5)
# lines(l1$fitted.values,col="green",lwd = .5, 
#       cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5,type="p")
# lines(l2$fitted.values,col="black",lwd = 2.5, 
#       cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5)
# 
# 
# lines(l_Greif$fitted.values,col="black",lwd = 2.5, 
#       cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5)
# #dev.off()