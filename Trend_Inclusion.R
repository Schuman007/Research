## Inclusion of linear Trend

library(readxl)
library(ggplot2)
library(plot3D)
mydata = read_excel("Summary-4.xlsx", sheet =1)

########## Plotting Temperature Data (Daily) #######

TempData <- cbind.data.frame(mydata$Date, mydata$Temp_Lahr,mydata$Temp_Greifswald)
names(TempData) <- c("Time","Lahr","Greifswald")
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
  labs(x = "Time (Days)", y= "Temperature (°C)", size = 15)+
  theme(axis.ticks = element_line(size = 1))+
  theme(axis.text = element_text(colour = "black", size = 15))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=22))+
  scale_x_continuous(breaks = round(seq(0, 4000, by =365)))

## Greifswald
curve((0.00005783133*x -17.64518*cos(2*(pi/365)*x - 0.5)), from=1, to=365*80)
###########################
eq1 = function(x) {0.00005783133*x -18.64518*cos(2*(pi/365)*x - 0.5)}

ggplot(data.frame(x=c(1, 365*80)), aes(x=x)) + stat_function(fun=eq1, geom="line", size=1, colour="red") + 
  labs(x = "Time (Days)", y= "Temperature (°C)", size = 35)+
  theme(axis.ticks = element_line(size = 1))+
  theme(axis.text = element_text(colour = "black", size = 16))+
  theme(plot.title = element_text( face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text( face="bold", size=29))
ggsave("Trend.eps", width = 27, height = 30, units = "cm")



