#Interpolating weather data with two different kinds of sine and cosine functions
library(stats)
library(graphics)
library(grDevices)
library(gdata)
#library(xlsx)
library(readxl)
library(ggplot2)
library(forecast)
library(fpp2)
#plot(rnorm(100), main="Hey Some Data")

#require(xlsx)
library(plot3D)
mydata = read_excel("Summary-4.xlsx", sheet =1)
T1=mydata$Temp_Gottingen
T2=mydata$Temp_Greifswald
l1=length(T1)-1;
n1=seq(0,l1)
#plot(n,T1)
train=seq(1,3654)
l=lm(T1~sin(2*pi/365*train)+cos(2*pi/365*train))
l1=lm(T2~cos(2*(pi/365)*train-.38))
l2=lm(T1~sin(2*pi/365*train))
l3=lm(T1~log(train))
#summary(l)
#setEPS()
l_Greif=lm(T2~cos(2*(pi/365)*train-.5))

#postscript("Gottin.eps")
df<- data.frame(T1,n1,l1$fitted.values)
df1<- data.frame(T2,n1,l_Greif$fitted.values)
ggplot(data=df, aes(x= n1, y= T2))+ geom_line(size=1,colour="red")+ 
  geom_line(aes(x=n1, y=l_Greif$fitted.values), size=1, colour="blue")+
 geom_line(aes(x=n1, y=l3$fitted.values), size=1, colour="green")+
  labs(x = "Time (Days)", y= "Temperature (C)", size = 15)+
  theme(axis.ticks = element_line(size = 1))+ 
  theme(axis.text =element_text(colour = "black", size = 15))+ 
  theme(plot.title = element_text(face="bold", size=32, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=22))+ 
    scale_x_continuous(breaks = round(seq(0, 4000, by =365)),
                       label = c("0", "1", "2","3","4","5","6","7","8","9","10")) 
  

  ggsave("Greifswald.eps", width = 17, height = 17, units = "cm")
  
ggplot(df1)+
  geom_line(aes(x=n1, y=T2), size=1, colour="red")+
 geom_line(aes(x=n1, y=l1$fitted.values), size=1, colour="blue")+
  theme_classic()+
labs(x = "Time (Year)", y= "Temperature (°C)", size = 15)+
 theme(axis.ticks = element_line(size = 1))+
theme(axis.text = element_text(colour = "black", size = 15))+
theme(plot.title = element_text( face="bold", size=35, hjust=0)) +
theme(axis.title = element_text( face="bold", size=22))+
scale_x_continuous(breaks = round(seq(0, 4000, by =365)),
                   label = c("0", "1", "2","3","4","5","6","7","8","9","10")) 

ggsave("Greif.eps", width = 17, height = 17, units = "cm")


ggplot(df1)+
  geom_line(aes(x=n1, y=T2), size=1, colour="red")+
  labs(x = "Time (Year)", y= "Temperature (C)", size = 15)+
  theme(axis.ticks = element_line(size = 1))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=22))+
  scale_x_continuous(breaks = round(seq(0, 4000, by =365)),
                     label = c("0", "1", "2","3","4","5","6","7","8","9","10")) 

ggsave("Gottin.eps", width = 17, height = 17, units = "cm")

# +breaks=c(5,7.5, 20, 25)
#scale_y_continuous(breaks = round(seq(0, 3654, by =5), 1))
plot(T2,type="l",col="red",ylab = 'Temparature(C)', xlab = 'Time (Days)',lwd = 1,main="Time Series: Temperature",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(l$fitted.values,col="blue",lwd = 2.5, 
      cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5)
lines(l1$fitted.values,col="green",lwd = .5, 
      cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5,type="p")
lines(l2$fitted.values,col="black",lwd = 2.5, 
      cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5)


lines(l_Greif$fitted.values,col="black",lwd = 2.5, 
      cex.lab=15.5, cex.axis=15.5, cex.main=15.5, cex.sub=15.5)
#dev.off()
############ Toying for the Trend ##########
library(forecast)
library(fpp2)
library(lattice)
library(zoo)
z <- read.zoo(mydata,header = TRUE)
plot(z)
u<-as.ts(z)
class(u)
autoplot(u)
w1 <- data.frame(mydata$Date,mydata$Temp_Greifswald)
w <- read.zoo(w1,header = TRUE)
xyplot(w)
plot(w)
abline(reg=lm(w~time(w)))



w <- ts(w, start=2007, frequency=365)
plot(w)
xyplot(w~seq_along(w), aspect = 0.3, type = "l")
autoplot(w)
autoplot(w) +
  geom_smooth() 
abline(lm(w~time(w)+time(w)))

library("TTR")
kingstimeseriesSMA3 <- SMA(w,n=120)
plot.ts(kingstimeseriesSMA3)
birthstimeseriescomponents <- decompose(w)
plot(birthstimeseriescomponents)

birthstimeseriesseasonallyadjusted <- w - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
rainseriesforecasts <- HoltWinters(w, beta=FALSE, gamma=FALSE)
plot(rainseriesforecasts)
rainseriesforecasts$fitted
rainseriesforecasts
