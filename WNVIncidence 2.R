library(readr)
# install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
Data1 <- read_delim("Data3.csv",";", escape_double = FALSE, trim_ws = TRUE)

library(ggplot2)
ggplot(data=Data1, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(colour="black", width=.6,stat="identity", position=position_dodge())
###########################
cbbPalette1 <- c("midnightblue", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                "#CC79A7","brown4")
Data1$Country <- as.factor(Data1$Country)


ggplot(data=Data1, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(aes(color=Country),colour="black", width=.6,stat="identity", 
           position=position_dodge())+ 
  scale_fill_manual(values=cbbPalette1)+ 
  xlab("Year") + ylab("Number of Human cases")+
  theme(axis.text.x = element_text(face="bold", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0))+
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  )
ggsave("HumanCase.pdf", width = 17, height = 17, units = "cm")
###########
p4 <- ggplot() + geom_bar(aes(y = Cases, x = Year, fill = Country), 
                          data=Data1,
                          stat="identity",colour="grey", width=.6,position = "stack")+ 
  scale_fill_manual(values=cbbPalette1)+
  xlab("Year") + ylab("Number of Human cases")+
  theme(axis.text.x = element_text(face="bold", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0))+
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  ) 
p4
ggsave("HumanCase.pdf", width = 17, height = 17, units = "cm")
##########
###############

## ggplot2 xlabel stuffs nice:
###theme(axis.text.x = element_text(face="bold", color="#993333", size=14, angle=45),
####   axis.text.y = element_text(face="bold", color="#993333", size=14, angle=45))
####
#theme(
 # plot.title = element_text(color="red", size=14, face="bold.italic"),
  #axis.title.x = element_text(color="black", size=14, face="bold"),
  #axis.title.y = element_text(color="black", size=14, face="bold")
#)
############################

Data2 <- read_delim("Animal_Case.csv",";", escape_double = FALSE, trim_ws = TRUE)

ggplot(data=Data2, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(stat="identity", position=position_dodge())


ggplot(data=Data2, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(colour="black", width=.6,stat="identity", position=position_dodge())+
  xlab("Year") + ylab("Number of Equine Cases")
############################

cbbPalette <- c("midnightblue", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                "#CC79A7","brown4","blueviolet","cadetblue4","cornsilk2","coral1")
Data2$Country <- as.factor(Data2$Country)


ggplot(data=Data2, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(aes(color=Country),colour="black", width=.6,stat="identity", position=position_dodge())+ 
  scale_fill_manual(values=cbbPalette)+ 
  xlab("Year") + ylab("Number of Equine cases")+
  theme(axis.text.x = element_text(face="bold", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0))+
   theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  )

ggsave("AnimalCase.pdf", width = 17, height = 17, units = "cm")
 ##########################################
########################################
p5 <- ggplot() + geom_bar(aes(y = Cases, x = Year, fill = Country), 
                          data=Data2,
                          stat="identity",colour="grey", width=.5,position = "stack")+ 
  scale_fill_manual(values=cbbPalette)+
  xlab("Year") + ylab("Number of Equine cases")+
  theme(axis.text.x = element_text(face="bold", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0))+
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  )

p5
ggsave("AnimalCase.pdf", width = 17, height = 17, units = "cm")
##########################
############# Radial ############
#plot the stacked bar plot with polar coordinates
ggplot(data=Data2, aes(x = Year)) + 
  geom_bar(aes(weight=Cases, fill = Country), position = 'fill') +
  scale_fill_manual(values=cbbPalette) + coord_polar()

#########################
############################
 # Data
 library("ggplot2")
 mycars <- mtcars
 mycars$cyl <- as.factor(mycars$cyl)
 
 # Custom theme
 mytheme <- theme(panel.grid.major = element_line(size = 2))
 mycolors <- c("burlywood4", "cadetblue", "midnightblue","cadetblue1","chocolate4","bisque4","cornsilk3",
               "blanchedalmond","darkgoldenrod1","brown4","blueviolet","cadetblue4","cornsilk2","coral1")
 # put the elements in a list
 mytheme2 <- list(mytheme, scale_color_manual(values = mycolors))
 
 # plot 
 ggplot(mycars, aes(x = wt, y = mpg)) +
   geom_point(aes(color = cyl)) +
   mytheme2
 
 