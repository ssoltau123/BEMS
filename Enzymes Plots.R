Enzymes <- read.csv("~/Desktop/Enzymes.csv")

#Converting factor to numeric

Enzymes$BG.corr<-as.numeric(Enzymes$BG.corr)
Enzymes$NAG.corr<-as.numeric(Enzymes$NAG.corr)
Enzymes$AP.corr<-as.numeric(Enzymes$AP.corr)

#Subsetting with removed outliers

EnzymesNO<-subset(Enzymes, outlier=="n")

library(tidyverse)
EnzymesSum <- EnzymesNO %>% # the names of the new data frame and the data frame to be summarised
  group_by(Days, Meadow, Treatment) %>%   # the grouping variable
  summarise(BG = mean(BG.corr), NAG = mean(NAG.corr), AP = mean(AP.corr), 
            SE_BG = sd(BG.corr)/sqrt(n()), SE_NAG = sd(NAG.corr)/sqrt(n()), SE_AP = sd(AP.corr)/sqrt(n()))

#Reordering Treatments so Legends can Reflect Correct Order of Treatments

EnzymesSum$Treatment <- ordered(EnzymesSum$Treatment,
                              levels = c("AD", "FC","SAT","4","12"))


levels(EnzymesSum$Treatment)

#Subsetting by Soil

EUL<-subset(EnzymesSum, Meadow=="UL")
EDM<-subset(EnzymesSum, Meadow=="DM")
EBF<-subset(EnzymesSum, Meadow=="BF")
ECF<-subset(EnzymesSum, Meadow=="CF")

#Setting up Plots
require(ggplot2)

ULEBG <- ggplot(EUL, aes(x = Days, y = BG, col = Treatment)) +
  geom_point(data = EUL, stat = "identity", size = 1) +
  geom_line(data = EUL, aes(x = Days, y = BG, col = Treatment)) +
  geom_errorbar(aes(ymin = BG - SE_BG, ymax = BG + SE_BG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("BG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULEBG


DMEBG <- ggplot(EDM, aes(x = Days, y = BG, col = Treatment)) +
  geom_point(data = EDM, stat = "identity", size = 1) +
  geom_line(data = EDM, aes(x = Days, y = BG, col = Treatment)) +
  geom_errorbar(aes(ymin = BG - SE_BG, ymax = BG + SE_BG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("BG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMEBG

BFEBG <- ggplot(EBF, aes(x = Days, y = BG, col = Treatment)) +
  geom_point(data = EBF, stat = "identity", size = 1) +
  geom_line(data = EBF, aes(x = Days, y = BG, col = Treatment)) +
  geom_errorbar(aes(ymin = BG - SE_BG, ymax = BG + SE_BG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("BG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFEBG

CFEBG <- ggplot(ECF, aes(x = Days, y = BG, col = Treatment)) +
  geom_point(data = ECF, stat = "identity", size = 1) +
  geom_line(data = ECF, aes(x = Days, y = BG, col = Treatment)) +
  geom_errorbar(aes(ymin = BG - SE_BG, ymax = BG + SE_BG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("BG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFEBG

#______________NAG

ULENAG <- ggplot(EUL, aes(x = Days, y = NAG, col = Treatment)) +
  geom_point(data = EUL, stat = "identity", size = 1) +
  geom_line(data = EUL, aes(x = Days, y = NAG, col = Treatment)) +
  geom_errorbar(aes(ymin = NAG - SE_NAG, ymax = NAG + SE_NAG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("NAG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULENAG


DMENAG <- ggplot(EDM, aes(x = Days, y = NAG, col = Treatment)) +
  geom_point(data = EDM, stat = "identity", size = 1) +
  geom_line(data = EDM, aes(x = Days, y = NAG, col = Treatment)) +
  geom_errorbar(aes(ymin = NAG - SE_NAG, ymax = NAG + SE_NAG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("NAG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMENAG

BFENAG <- ggplot(EBF, aes(x = Days, y = NAG, col = Treatment)) +
  geom_point(data = EBF, stat = "identity", size = 1) +
  geom_line(data = EBF, aes(x = Days, y = NAG, col = Treatment)) +
  geom_errorbar(aes(ymin = NAG - SE_NAG, ymax = NAG + SE_NAG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("NAG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFENAG

CFENAG <- ggplot(ECF, aes(x = Days, y = NAG, col = Treatment)) +
  geom_point(data = ECF, stat = "identity", size = 1) +
  geom_line(data = ECF, aes(x = Days, y = NAG, col = Treatment)) +
  geom_errorbar(aes(ymin = NAG - SE_NAG, ymax = NAG + SE_NAG), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("NAG Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFENAG

#________________AP________

ULEAP <- ggplot(EUL, aes(x = Days, y = AP, col = Treatment)) +
  geom_point(data = EUL, stat = "identity", size = 1) +
  geom_line(data = EUL, aes(x = Days, y = AP, col = Treatment)) +
  geom_errorbar(aes(ymin = AP - SE_AP, ymax = AP + SE_AP), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("AP Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULEAP


DMEAP <- ggplot(EDM, aes(x = Days, y = AP, col = Treatment)) +
  geom_point(data = EDM, stat = "identity", size = 1) +
  geom_line(data = EDM, aes(x = Days, y = AP, col = Treatment)) +
  geom_errorbar(aes(ymin = AP - SE_AP, ymax = AP + SE_AP), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("AP Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMEAP

BFEAP <- ggplot(EBF, aes(x = Days, y = AP, col = Treatment)) +
  geom_point(data = EBF, stat = "identity", size = 1) +
  geom_line(data = EBF, aes(x = Days, y = AP, col = Treatment)) +
  geom_errorbar(aes(ymin = AP - SE_AP, ymax = AP + SE_AP), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("AP Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFEAP

CFEAP <- ggplot(ECF, aes(x = Days, y = AP, col = Treatment)) +
  geom_point(data = ECF, stat = "identity", size = 1) +
  geom_line(data = ECF, aes(x = Days, y = AP, col = Treatment)) +
  geom_errorbar(aes(ymin = AP - SE_AP, ymax = AP + SE_AP), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("AP Activity (mmol"~kg^-1*~h^-1*")"), expand=c(0,0), limits=c(0,250), breaks=seq(0,250, by=50))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFEAP

require(gridExtra)

BG<-grid.arrange(ULEBG, DMEBG, BFEBG, CFEBG, ncol=4)
NAG<-grid.arrange(ULENAG, DMENAG, BFENAG, CFENAG, ncol=4)
AP<-grid.arrange(ULEAP, DMEAP, BFEAP, CFEAP, ncol=4)

All<-grid.arrange(BG, NAG, AP, nrow=3)

