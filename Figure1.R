library(readr)
library(ochRe)
WNV_Cases <- read_delim("WNV_Cases.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
library(ggplot2)
g <- ggplot(data=WNV_Cases, aes(x=interaction(Species,Year), y=Cases, fill=Country)) +
  geom_bar(colour="black", width=.6,stat="identity", position=position_stack())

ggplot(data=WNV_Cases, aes(x=interaction(Species,Year), y=Cases, fill=Country)) +
  geom_bar(colour="black", width=.6,stat="identity", position=position_stack())

f <- ggplot(data=WNV_Cases, aes(x=Species, y=Cases, fill=Country)) +
  geom_bar(colour=NA, width=.6,stat="identity", position=position_stack())+
  facet_grid(~Year)+
  theme_bw()+
  scale_fill_ochre()

pal <- colorRampPalette(ochre_palettes[["winmar"]])
f1 <- ggplot(data=WNV_Cases, aes(x=Species, y=Cases, fill=Country)) +
  geom_bar(colour=NA, width=.6,stat="identity", position=position_stack())+
  facet_grid(~Year)+
  theme_bw()+
  scale_fill_ochre(values=pal)+ 
  xlab("Year") + ylab("Number of cases")+
  theme(axis.text.x = element_text(face="bold", size=15, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=15, angle=0))+
  theme(
    axis.title.x = element_text(color="black", size=15, face="bold"),
    axis.title.y = element_text(color="black", size=15, face="bold")
  )+My_Theme+
  theme(axis.text.x=element_text(angle=0), 
        legend.text=element_text(size=20),
        legend.key.height=unit(.3, "cm"))
f1
ggsave("Case.eps", width = 25, height = 20, units = "cm")

ggplot(data=WNV_Cases, aes(x=Species, y=Cases, color=Country)) +
  geom_point(size=4,stat="identity", position=position_stack())+
  facet_grid(~Year)+
  theme_bw()

ggplot(data=WNV_Cases, aes(x=Year, y=Cases, color=interaction(Species,Country))) +
  geom_line(size=1,stat="identity", position=position_stack())+
#  facet_grid(~Year)+
  theme_bw()

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 15),
  axis.title.y = element_text(size = 20))
