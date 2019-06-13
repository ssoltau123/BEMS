#Read in Data

MBN <- read.csv("~/Desktop/BEMS R Studio/Data/MBN.csv")
View(MBN)

#Subsetting Data and replacing Sample Date values to reflect total days (starting from end of Preincubation Week)

MBNet<-subset(MBN, Analysis=="MBUF")
view(MBNet)

MBNet$Sample.Date[MBNet$Sample.Date == 1] <- 0
MBNet$Sample.Date[MBNet$Sample.Date == 2] <- 21
MBNet$Sample.Date[MBNet$Sample.Date == 3] <- 42
MBNet$Sample.Date[MBNet$Sample.Date == 4] <- 63
MBNet$Sample.Date[MBNet$Sample.Date == 5] <- 84

#Summarizing Net MC Net MN, UF C and N, and IN w/ Means and SE

library(tidyverse)
MBNetSum <- MBNet %>% # the names of the new data frame and the data frame to be summarised
  group_by(Sample.Date, Soil, Treatment) %>%   # the grouping variable
  summarise(MTC = mean(TC), MTN = mean(TN), MFlushC = mean(Flush.TC), MFlushN = mean(Flush.TN), MNO3 = mean(NO3), MSum = mean(Sum),
            SE_TC=sd(TC)/sqrt(n()), SE_TN=sd(TN)/sqrt(n()), SE_FC=sd(Flush.TC)/sqrt(n()), SE_FN=sd(Flush.TN)/sqrt(n()), SE_NO3=sd(NO3)/sqrt(n()), SE_Sum=sd(Sum)/sqrt(n()))
     
#Reordering Treatments so Legends can Reflect Correct Order of Treatments

MBNetSum$Treatment <- ordered(MBNetSum$Treatment,
                           levels = c("AD", "FC","SAT","4","12"))


levels(MBNetSum$Treatment)

#Subsetting by Soil

MBNetUL<-subset(MBNetSum, Soil=="UL")
MBNetDM<-subset(MBNetSum, Soil=="DM")
MBNetBF<-subset(MBNetSum, Soil=="BF")
MBNetCF<-subset(MBNetSum, Soil=="CF")


#Plots for UNFUMIGATED C and N over time

ULMBC <- ggplot(MBNetUL, aes(x = Sample.Date, y = MTC, col = Treatment)) +
  geom_point(data = MBNetUL, stat = "identity", size = 1) +
  geom_line(data = MBNetUL, aes(x = Sample.Date, y = MTC, col = Treatment)) +
  geom_errorbar(aes(ymin = MTC - SE_TC, ymax = MTC + SE_TC), width = 0.2)+
  scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
  scale_y_continuous(name = bquote("Unfumigated C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0,800, by=200))+
  ggtitle("Undegraded")+
  scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
   axis.line = element_line(colour = "black"),
  plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
  axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  

  ULMBC
  
  DMMBC <- ggplot(MBNetDM, aes(x = Sample.Date, y = MTC, col = Treatment)) +
    geom_point(data = MBNetDM, stat = "identity", size = 1) +
    geom_line(data = MBNetDM, aes(x = Sample.Date, y = MTC, col = Treatment)) +
    geom_errorbar(aes(ymin = MTC - SE_TC, ymax = MTC + SE_TC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0,800, by=200)) +
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMMBC
  
  BFMBC <- ggplot(MBNetBF, aes(x = Sample.Date, y = , col = Treatment)) +
    geom_point(data = MBNetBF, stat = "identity", size = 1) +
    geom_line(data = MBNetBF, aes(x = Sample.Date, y = MTC, col = Treatment)) +
    geom_errorbar(aes(ymin = MTC - SE_TC, ymax = MTC + SE_TC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,NA), breaks=seq(0, 800, by=200)) +
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFMBC
  
  CFMBC <- ggplot(MBNetCF, aes(x = Sample.Date, y = MTC, col = Treatment)) +
    geom_point(data = MBNetCF, stat = "identity", size = 1) +
    geom_line(data = MBNetCF, aes(x = Sample.Date, y = MTC, col = Treatment)) +
    geom_errorbar(aes(ymin = MTC - SE_TC, ymax = MTC + SE_TC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,800), breaks=seq(0, 800, by=200)) + 
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
 
  CFMBC 
  
  
  #----------------
  
  ULMBN <- ggplot(MBNetUL, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_point(data = MBNetUL, stat = "identity", size = 1) +
    geom_line(data = MBNetUL, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_errorbar(aes(ymin = MTN - SE_TN, ymax = MTN + SE_TN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,400), breaks=seq(0,400, by=100))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULMBN
  
  DMMBN <- ggplot(MBNetDM, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_point(data = MBNetDM, stat = "identity", size = 1) +
    geom_line(data = MBNetDM, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_errorbar(aes(ymin = MTN - SE_TN, ymax = MTN + SE_TN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,400), breaks=seq(0,400, by=100)) +
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMMBN
  
  BFMBN <- ggplot(MBNetBF, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_point(data = MBNetBF, stat = "identity", size = 1) +
    geom_line(data = MBNetBF, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_errorbar(aes(ymin = MTN - SE_TN, ymax = MTN + SE_TN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,400), breaks=seq(0, 400, by=100)) +
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFMBN
  
  CFMBN <- ggplot(MBNetCF, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_point(data = MBNetCF, stat = "identity", size = 1) +
    geom_line(data = MBNetCF, aes(x = Sample.Date, y = MTN, col = Treatment)) +
    geom_errorbar(aes(ymin = MTN - SE_TN, ymax = MTN + SE_TN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Unfumigated N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0,400), breaks=seq(0, 400, by=100)) + 
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  

  
  CFMBN
  
  require(gridExtra)
  UFC<-grid.arrange(ULMBC, DMMBC, BFMBC, CFMBC, nrow=4)
  UFN<-grid.arrange(ULMBN, DMMBN, BFMBN, CFMBN, nrow=4)
  
  UF<-grid.arrange(UFC, UFN, ncol=2)
  
  #Subsetting for Sample Dates 1,2 and 5 for MB Plots
  
  MBNet125<-subset(MBNetSum, Sample.Date=="0"|Sample.Date=="21"|Sample.Date=="84")
  

  MBUL<-subset(MBNet125, Soil=="UL")
  MBDM<-subset(MBNet125, Soil=="DM")
  MBBF<-subset(MBNet125, Soil=="BF")
  MBCF<-subset(MBNet125, Soil=="CF")
  
  
  #Plots for MICROBIAL C and N over time
  
  ULFlushC <- ggplot(MBUL, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBUL, stat = "identity", size = 1) +
    geom_line(data = MBUL, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULFlushC
  
  DMFlushC <- ggplot(MBDM, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBDM, stat = "identity", size = 1) +
    geom_line(data = MBDM, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMFlushC
  
  BFFlushC <- ggplot(MBBF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBBF, stat = "identity", size = 1) +
    geom_line(data = MBBF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFFlushC
  
  
  CFFlushC <- ggplot(MBCF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBCF, stat = "identity", size = 1) +
    geom_line(data = MBCF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  CFFlushC
  
  #---------------------------
  
  ULFlushN <- ggplot(MBUL, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBUL, stat = "identity", size = 1) +
    geom_line(data = MBUL, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0,400, by=100))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULFlushN
  
  DMFlushN <- ggplot(MBDM, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBDM, stat = "identity", size = 1) +
    geom_line(data = MBDM, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0, 400, by=100))+
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMFlushN
  
  BFFlushN <- ggplot(MBBF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBBF, stat = "identity", size = 1) +
    geom_line(data = MBBF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0,400, by=100))+
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFFlushN
  
  
  CFFlushN <- ggplot(MBCF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBCF, stat = "identity", size = 1) +
    geom_line(data = MBCF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0,400, by=100))+
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  CFFlushN
  
  require(gridExtra)
  FC<-grid.arrange(ULFlushC, DMFlushC, BFFlushC, CFFlushC, nrow=4)
  FN<-grid.arrange(ULFlushN,DMFlushN, BFFlushN, CFFlushN, nrow=4)
  
  Flush<-grid.arrange(FC, FN, ncol=2)
  
  #Plots for MICROBIAL C and N over time
  
  ULFlushC <- ggplot(MBUL, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBUL, stat = "identity", size = 1) +
    geom_line(data = MBUL, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULFlushC
  
  DMFlushC <- ggplot(MBDM, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBDM, stat = "identity", size = 1) +
    geom_line(data = MBDM, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMFlushC
  
  BFFlushC <- ggplot(MBBF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBBF, stat = "identity", size = 1) +
    geom_line(data = MBBF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFFlushC
  
  
  CFFlushC <- ggplot(MBCF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_point(data = MBCF, stat = "identity", size = 1) +
    geom_line(data = MBCF, aes(x = Sample.Date, y = MFlushC, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushC - SE_FC, ymax = MFlushC + SE_FC), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial C (mg C" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 1200), breaks=seq(0,1200, by=300))+
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  CFFlushC
  
  #---------------------------
  
  ULFlushN <- ggplot(MBUL, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBUL, stat = "identity", size = 1) +
    geom_line(data = MBUL, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0,400, by=100))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULFlushN
  
  DMFlushN <- ggplot(MBDM, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBDM, stat = "identity", size = 1) +
    geom_line(data = MBDM, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0, 400, by=100))+
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMFlushN
  
  BFFlushN <- ggplot(MBBF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBBF, stat = "identity", size = 1) +
    geom_line(data = MBBF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0,400, by=100))+
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFFlushN
  
  
  CFFlushN <- ggplot(MBCF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_point(data = MBCF, stat = "identity", size = 1) +
    geom_line(data = MBCF, aes(x = Sample.Date, y = MFlushN, col = Treatment)) +
    geom_errorbar(aes(ymin = MFlushN - SE_FN, ymax = MFlushN + SE_FN), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("Microbial N (mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 400), breaks=seq(0,400, by=100))+
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  CFFlushN
  
  require(gridExtra)
  FC<-grid.arrange(ULFlushC, DMFlushC, BFFlushC, CFFlushC, nrow=4)
  FN<-grid.arrange(ULFlushN,DMFlushN, BFFlushN, CFFlushN, nrow=4)
  
  Flush<-grid.arrange(FC, FN, ncol=2)
  
  #Plots for N Transform
  
  ULNO3 <- ggplot(MBUL, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_point(data = MBUL, stat = "identity", size = 1) +
    geom_line(data = MBUL, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_errorbar(aes(ymin = MNO3 - SE_NO3, ymax = MNO3 + SE_NO3), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULNO3
  
  DMNO3 <- ggplot(MBDM, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_point(data = MBDM, stat = "identity", size = 1) +
    geom_line(data = MBDM, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_errorbar(aes(ymin = MNO3 - SE_NO3, ymax = MNO3 + SE_NO3), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMNO3
  
  BFNO3 <- ggplot(MBBF, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_point(data = MBBF, stat = "identity", size = 1) +
    geom_line(data = MBBF, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_errorbar(aes(ymin = MNO3 - SE_NO3, ymax = MNO3 + SE_NO3), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFNO3
  
  CFNO3 <- ggplot(MBCF, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_point(data = MBCF, stat = "identity", size = 1) +
    geom_line(data = MBCF, aes(x = Sample.Date, y = MNO3, col = Treatment)) +
    geom_errorbar(aes(ymin = MNO3 - SE_NO3, ymax = MNO3 + SE_NO3), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  CFNO3
  
  
  ULSum <-ggplot(MBUL, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_point(data = MBUL, stat = "identity", size = 1) +
    geom_line(data = MBUL, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_errorbar(aes(ymin = MSum - SE_Sum, ymax = MSum + SE_Sum), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"+ NH"[4]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Undegraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  ULSum
  
  DMSum <-ggplot(MBDM, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_point(data = MBDM, stat = "identity", size = 1) +
    geom_line(data = MBDM, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_errorbar(aes(ymin = MSum - SE_Sum, ymax = MSum + SE_Sum), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"+ NH"[4]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Degraded")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  DMSum
  
  BFSum <-ggplot(MBBF, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_point(data = MBBF, stat = "identity", size = 1) +
    geom_line(data = MBBF, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_errorbar(aes(ymin = MSum - SE_Sum, ymax = MSum + SE_Sum), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"+ NH"[4]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Restored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  BFSum
  
  CFSum <-ggplot(MBCF, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_point(data = MBCF, stat = "identity", size = 1) +
    geom_line(data = MBCF, aes(x = Sample.Date, y = MSum, col = Treatment)) +
    geom_errorbar(aes(ymin = MSum - SE_Sum, ymax = MSum + SE_Sum), width = 0.2)+
    scale_color_manual(values=c("orange", "green", "blue", "red", "purple"))+
    scale_y_continuous(name = bquote("NO"[3]*"+ NH"[4]*"(mg N" ~ kg^-1* "OD Soil)"), expand=c(0,0), limits=c(0, 300), breaks=seq(0,300, by=60))+
    ggtitle("Unrestored")+
    scale_x_continuous(name = "Time (Days)", breaks = c(0, 21, 42, 63, 84)) +  
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          plot.title=element_text(face="plain", size=12), legend.position = "none", axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12), axis.title.x = element_text(size=12), axis.title.y = element_text(size=11))
  
  
  CFSum
  
  
  
  require(gridExtra)
  NO3<-grid.arrange(ULNO3, DMNO3, BFNO3, CFNO3, nrow=4)
  Sum<-grid.arrange(ULSum, DMSum,BFSum,CFSum, nrow=4)
  
  NTran<-grid.arrange(NO3, Sum, ncol=2)
  
  