library(readr)
Data1 <- read_delim("Data3.csv",";", escape_double = FALSE, trim_ws = TRUE)

library(ggplot2)
ggplot(data=Data1, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(colour="black", width=.6,stat="identity", position=position_dodge())
###########################
cbbPalette1 <- c("midnightblue", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                "#CC79A7","brown4")
Data1$Country <- as.factor(Data1$Country)


ggplot(data=Data1, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(aes(color=Country),colour="black", width=.6,stat="identity", position=position_dodge())+ 
  scale_fill_manual(values=cbbPalette1)+ 
  xlab("Year") + ylab("Number of Human cases")+
  theme(axis.text.x = element_text(face="bold", size=30, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=30, angle=0))+
  theme(
    axis.title.x = element_text(color="black", size=24, face="bold"),
    axis.title.y = element_text(color="black", size=24, face="bold")
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+ theme(
  legend.title = element_text(color = "black", size = 20),
  legend.text = element_text(color = "black", size = 18)
)
ggsave("HumanCase.pdf", width = 17, height = 17, units = "cm")
ggsave( "tr_tst1.pdf",  bg = "transparent")
####

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
  xlab("Year") + ylab("Number of Cases")
############################

cbbPalette <- c("midnightblue", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                "#CC79A7","brown4","blueviolet","cadetblue4","cornsilk2","coral1")
Data2$Country <- as.factor(Data2$Country)


ggplot(data=Data2, aes(x=Year, y=Cases, fill=Country)) +
  geom_bar(aes(color=Country),colour="black", width=.6,stat="identity", position=position_dodge())+ 
  scale_fill_manual(values=cbbPalette)+ 
  xlab("Year") + ylab("Number of animal cases")+
  theme(axis.text.x = element_text(face="bold", size=30, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=30, angle=0))+
   theme(
    axis.title.x = element_text(color="black", size=24, face="bold"),
    axis.title.y = element_text(color="black", size=24, face="bold")
  )+
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+ theme(
    legend.title = element_text(color = "black", size = 20),
    legend.text = element_text(color = "black", size = 18)
  )

ggsave("AnimalCase.pdf", width = 17, height = 17, units = "cm")
ggsave( "tr_tst2.pdf",  bg = "transparent")
 ##########################################
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
 