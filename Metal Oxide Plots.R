Metals <- read.csv("~/Desktop/Metals.csv")
 View(Metals)

 #Summarizing Data
 
library(tidyverse)
MetalsSum <- Metals %>% # the names of the new data frame and the data frame to be summarised
   group_by(Days, Soil.Type, Treatment) %>%   # the grouping variable
   summarise(Fe = mean(Fe..mg.kg.OD.soil.), Mn = mean(Mn..mg.kg.OD.soil.), Cu = mean(Cu..mg.kg.OD.soil.), Zn = mean(Zn..mg.kg.OD.soil.),
             SE_Fe = sd(Fe..mg.kg.OD.soil.)/sqrt(n()),  SE_Mn = sd(Mn..mg.kg.OD.soil.)/sqrt(n()),  SE_Cu = sd(Cu..mg.kg.OD.soil.)/sqrt(n()),  SE_Zn = sd(Zn..mg.kg.OD.soil.)/sqrt(n()))
 
#Reordering Treatments

MetalsSum$Treatment <- ordered(MetalsSum$Treatment,
                                levels = c("SAT","4","12", "Initial"))


levels(MetalsSum$Treatment)
 
 #Subsetting by Soil

MUL<-subset(MetalsSum, Soil.Type=="UL")
MDM<-subset(MetalsSum, Soil.Type=="DM")
MBF<-subset(MetalsSum, Soil.Type=="BF")
MCF<-subset(MetalsSum, Soil.Type=="CF")

#Subsetting by Treatment

MULSAT<-subset(MUL, Treatment=="Initial"|Treatment=="SAT")
MUL4<-subset(MUL, Treatment=="Initial"|Treatment=="4")
MUL12<-subset(MUL, Treatment=="Initial"|Treatment=="12")

MDMSAT<-subset(MDM, Treatment=="Initial"|Treatment=="SAT")
MDM4<-subset(MDM, Treatment=="Initial"|Treatment=="4")
MDM12<-subset(MDM, Treatment=="Initial"|Treatment=="12")

MBFSAT<-subset(MBF, Treatment=="Initial"|Treatment=="SAT")
MBF4<-subset(MBF, Treatment=="Initial"|Treatment=="4")
MBF12<-subset(MBF, Treatment=="Initial"|Treatment=="12")

MCFSAT<-subset(MCF, Treatment=="Initial"|Treatment=="SAT")
MCF4<-subset(MCF, Treatment=="Initial"|Treatment=="4")
MCF12<-subset(MCF, Treatment=="Initial"|Treatment=="12")


#Setting Up Plots

ULFM <- ggplot(MULSAT, aes(x = Days, y = Fe)) +
  geom_point(data = MULSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MULSAT, aes(x = Days, y = Fe), col="blue") +
  geom_errorbar(aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="blue") +
  geom_point(data = MUL4, stat = "identity", size = 1, col="red") +
  geom_line(data = MUL4, aes(x = Days, y = Fe), col="red") +
  geom_errorbar(data=MUL4, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="red") +
  geom_point(data = MUL12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MUL12, aes(x = Days, y = Fe), col="purple") +
  geom_errorbar(data=MUL12, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Fe Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0,800, by=200))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULFM

DMFM <- ggplot(MDMSAT, aes(x = Days, y = Fe)) +
  geom_point(data = MDMSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MDMSAT, aes(x = Days, y = Fe), col="blue") +
  geom_errorbar(aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="blue") +
  geom_point(data = MDM4, stat = "identity", size = 1, col="red") +
  geom_line(data = MDM4, aes(x = Days, y = Fe), col="red") +
  geom_errorbar(data=MDM4, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="red") +
  geom_point(data = MDM12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MDM12, aes(x = Days, y = Fe), col="purple") +
  geom_errorbar(data=MDM12, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Fe Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0,800, by=200))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMFM

BFFM <- ggplot(MBFSAT, aes(x = Days, y = Fe)) +
  geom_point(data = MBFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MBFSAT, aes(x = Days, y = Fe), col="blue") +
  geom_errorbar(aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="blue") +
  geom_point(data = MBF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MBF4, aes(x = Days, y = Fe), col="red") +
  geom_errorbar(data=MBF4, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="red") +
  geom_point(data = MBF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MBF12, aes(x = Days, y = Fe), col="purple") +
  geom_errorbar(data=MBF12, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Fe Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0,800, by=200))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFFM

CFFM <- ggplot(MCFSAT, aes(x = Days, y = Fe)) +
  geom_point(data = MCFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MCFSAT, aes(x = Days, y = Fe), col="blue") +
  geom_errorbar(aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="blue") +
  geom_point(data = MCF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MCF4, aes(x = Days, y = Fe), col="red") +
  geom_errorbar(data=MCF4, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="red") +
  geom_point(data = MCF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MCF12, aes(x = Days, y = Fe), col="purple") +
  geom_errorbar(data=MCF12, aes(ymin = Fe - SE_Fe, ymax = Fe + SE_Fe), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Fe Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0,800, by=200))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFFM


ULMM <- ggplot(MULSAT, aes(x = Days, y = Mn)) +
  geom_point(data = MULSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MULSAT, aes(x = Days, y = Mn), col="blue") +
  geom_errorbar(aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="blue") +
  geom_point(data = MUL4, stat = "identity", size = 1, col="red") +
  geom_line(data = MUL4, aes(x = Days, y = Mn), col="red") +
  geom_errorbar(data=MUL4, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="red") +
  geom_point(data = MUL12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MUL12, aes(x = Days, y = Mn), col="purple") +
  geom_errorbar(data=MUL12, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Mn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,80), breaks=seq(0,80, by=20))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULMM

DMMM <- ggplot(MDMSAT, aes(x = Days, y = Mn)) +
  geom_point(data = MDMSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MDMSAT, aes(x = Days, y = Mn), col="blue") +
  geom_errorbar(aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="blue") +
  geom_point(data = MDM4, stat = "identity", size = 1, col="red") +
  geom_line(data = MDM4, aes(x = Days, y = Mn), col="red") +
  geom_errorbar(data=MDM4, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="red") +
  geom_point(data = MDM12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MDM12, aes(x = Days, y = Mn), col="purple") +
  geom_errorbar(data=MDM12, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Mn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,80), breaks=seq(0,80, by=20))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMMM

BFMM <- ggplot(MBFSAT, aes(x = Days, y = Mn)) +
  geom_point(data = MBFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MBFSAT, aes(x = Days, y = Mn), col="blue") +
  geom_errorbar(aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="blue") +
  geom_point(data = MBF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MBF4, aes(x = Days, y = Mn), col="red") +
  geom_errorbar(data=MBF4, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="red") +
  geom_point(data = MBF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MBF12, aes(x = Days, y = Mn), col="purple") +
  geom_errorbar(data=MBF12, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Mn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,80), breaks=seq(0,80, by=20))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFMM

CFMM <- ggplot(MCFSAT, aes(x = Days, y = Mn)) +
  geom_point(data = MCFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MCFSAT, aes(x = Days, y = Mn), col="blue") +
  geom_errorbar(aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="blue") +
  geom_point(data = MCF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MCF4, aes(x = Days, y = Mn), col="red") +
  geom_errorbar(data=MCF4, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="red") +
  geom_point(data = MCF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MCF12, aes(x = Days, y = Mn), col="purple") +
  geom_errorbar(data=MCF12, aes(ymin = Mn - SE_Mn, ymax = Mn + SE_Mn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Mn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,80), breaks=seq(0,80, by=20))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFMM

ULCM <- ggplot(MULSAT, aes(x = Days, y = Cu)) +
  geom_point(data = MULSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MULSAT, aes(x = Days, y = Cu), col="blue") +
  geom_errorbar(aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="blue") +
  geom_point(data = MUL4, stat = "identity", size = 1, col="red") +
  geom_line(data = MUL4, aes(x = Days, y = Cu), col="red") +
  geom_errorbar(data=MUL4, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="red") +
  geom_point(data = MUL12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MUL12, aes(x = Days, y = Cu), col="purple") +
  geom_errorbar(data=MUL12, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Cu Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULCM

DMCM <- ggplot(MDMSAT, aes(x = Days, y = Cu)) +
  geom_point(data = MDMSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MDMSAT, aes(x = Days, y = Cu), col="blue") +
  geom_errorbar(aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="blue") +
  geom_point(data = MDM4, stat = "identity", size = 1, col="red") +
  geom_line(data = MDM4, aes(x = Days, y = Cu), col="red") +
  geom_errorbar(data=MDM4, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="red") +
  geom_point(data = MDM12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MDM12, aes(x = Days, y = Cu), col="purple") +
  geom_errorbar(data=MDM12, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Cu Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMCM

BFCM <- ggplot(MBFSAT, aes(x = Days, y = Cu)) +
  geom_point(data = MBFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MBFSAT, aes(x = Days, y = Cu), col="blue") +
  geom_errorbar(aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="blue") +
  geom_point(data = MBF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MBF4, aes(x = Days, y = Cu), col="red") +
  geom_errorbar(data=MBF4, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="red") +
  geom_point(data = MBF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MBF12, aes(x = Days, y = Cu), col="purple") +
  geom_errorbar(data=MBF12, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Cu Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFCM

CFCM <- ggplot(MCFSAT, aes(x = Days, y = Cu)) +
  geom_point(data = MCFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MCFSAT, aes(x = Days, y = Cu), col="blue") +
  geom_errorbar(aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="blue") +
  geom_point(data = MCF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MCF4, aes(x = Days, y = Cu), col="red") +
  geom_errorbar(data=MCF4, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="red") +
  geom_point(data = MCF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MCF12, aes(x = Days, y = Cu), col="purple") +
  geom_errorbar(data=MCF12, aes(ymin = Cu - SE_Cu, ymax = Cu + SE_Cu), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Cu Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFCM


ULZM <- ggplot(MULSAT, aes(x = Days, y = Zn)) +
  geom_point(data = MULSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MULSAT, aes(x = Days, y = Zn), col="blue") +
  geom_errorbar(aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="blue") +
  geom_point(data = MUL4, stat = "identity", size = 1, col="red") +
  geom_line(data = MUL4, aes(x = Days, y = Zn), col="red") +
  geom_errorbar(data=MUL4, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="red") +
  geom_point(data = MUL12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MUL12, aes(x = Days, y = Zn), col="purple") +
  geom_errorbar(data=MUL12, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Zn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Undegraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

ULZM

DMZM <- ggplot(MDMSAT, aes(x = Days, y = Zn)) +
  geom_point(data = MDMSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MDMSAT, aes(x = Days, y = Zn), col="blue") +
  geom_errorbar(aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="blue") +
  geom_point(data = MDM4, stat = "identity", size = 1, col="red") +
  geom_line(data = MDM4, aes(x = Days, y = Zn), col="red") +
  geom_errorbar(data=MDM4, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="red") +
  geom_point(data = MDM12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MDM12, aes(x = Days, y = Zn), col="purple") +
  geom_errorbar(data=MDM12, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Zn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Degraded") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

DMZM

BFZM <- ggplot(MBFSAT, aes(x = Days, y = Zn)) +
  geom_point(data = MBFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MBFSAT, aes(x = Days, y = Zn), col="blue") +
  geom_errorbar(aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="blue") +
  geom_point(data = MBF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MBF4, aes(x = Days, y = Zn), col="red") +
  geom_errorbar(data=MBF4, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="red") +
  geom_point(data = MBF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MBF12, aes(x = Days, y = Zn), col="purple") +
  geom_errorbar(data=MBF12, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Zn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Restored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

BFZM

CFZM <- ggplot(MCFSAT, aes(x = Days, y = Zn)) +
  geom_point(data = MCFSAT, stat = "identity", size = 1, col="blue") +
  geom_line(data = MCFSAT, aes(x = Days, y = Zn), col="blue") +
  geom_errorbar(aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="blue") +
  geom_point(data = MCF4, stat = "identity", size = 1, col="red") +
  geom_line(data = MCF4, aes(x = Days, y = Zn), col="red") +
  geom_errorbar(data=MCF4, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="red") +
  geom_point(data = MCF12, stat = "identity", size = 1, col="purple") +
  geom_line(data = MCF12, aes(x = Days, y = Zn), col="purple") +
  geom_errorbar(data=MCF12, aes(ymin = Zn - SE_Zn, ymax = Zn + SE_Zn), width = 0.2, col="purple") +
  scale_y_continuous(name = bquote("Zn Oxides (mg"~kg^-1*"OD Soil)"), expand=c(0,0), limits=c(0,8), breaks=seq(0,8, by=2))+
  ggtitle("Unrestored") +
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))

CFZM

 require(gridExtra)

Fe<-grid.arrange(ULFM, DMFM, BFFM, CFFM, ncol=4)
Mn<-grid.arrange(ULMM, DMMM, BFMM, CFMM, ncol=4)
Cu<-grid.arrange(ULCM, DMCM, BFCM, CFCM, ncol=4)
Zn<-grid.arrange(ULZM, DMZM, BFZM, CFZM, ncol=4)

Metals<-grid.arrange(Fe, Mn, Cu, Zn, nrow=4)
