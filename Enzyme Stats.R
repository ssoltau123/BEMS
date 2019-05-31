library(Rmisc)
library(ggplot2)
library(plyr)

library(Rmisc)
library(ggplot2)
library(plyr)

#Reading in the Data

Time1 <- read.csv("~/Desktop/Github_Misc2/Birch_Effect_Enzymes/Data/Processed/Time1.csv")
Time2 <- read.csv("~/Desktop/Github_Misc2/Birch_Effect_Enzymes/Data/Processed/Time2.csv")
Time3 <- read.csv("~/Desktop/Github_Misc2/Birch_Effect_Enzymes/Data/Processed/Time3.csv")
Time4 <- read.csv("~/Desktop/Github_Misc2/Birch_Effect_Enzymes/Data/Processed/Time4.csv")
Time5 <- read.csv("~/Desktop/Github_Misc2/Birch_Effect_Enzymes/Data/Processed/Time5.csv")

#Coercing the Data

Time1$time <- 7
Time2$time <- 27
Time3$time <- 28
Time4$time <-90
Time5$time <- 91

Enzyme <- rbind.fill(Time1, Time2, Time3, Time4, Time5)

View(Enzyme)

#Subsetting by Meadow

BF <- subset(Enzyme, Meadow == "BF")
CF <- subset(Enzyme, Meadow == "CF")
DM <- subset(Enzyme, Meadow == "DM")
UL <- subset(Enzyme, Meadow == "UL")

#Normality Test

shapiro.test(UL$BG.corr)
shapiro.test(UL$NAG.corr)
shapiro.test(UL$AP.corr)

shapiro.test(DM$BG.corr)
shapiro.test(DM$NAG.corr) #**
shapiro.test(DM$AP.corr)

shapiro.test(BF$BG.corr)
shapiro.test(BF$NAG.corr) #** 
shapiro.test(BF$AP.corr)

shapiro.test(CF$BG.corr) #**
shapiro.test(CF$NAG.corr) #**
shapiro.test(CF$AP.corr) #**

#Log Transform

DM$NAGLOG<-log(DM$NAG.corr)
shapiro.test(DM$NAGLOG) 

DM$NAGLOGL<-log(DM$NAGLOG)
shapiro.test(DM$NAGLOGL) #Pass (had to double log)

BF$NAGLOG<-log(BF$NAG.corr)
shapiro.test(BF$NAGLOG) #Pass

CF$BGLOG<-log(CF$BG.corr)
shapiro.test(CF$BGLOG) #Pass

CF$NAGLOG<-log(CF$NAG.corr+30)
shapiro.test(CF$NAGLOG) 

CF$NAGLOGL<-log(CF$NAGLOG)
shapiro.test(CF$NAGLOGL) #No Pass

CF$APLOG<-log(CF$AP.corr)
shapiro.test(CF$APLOG) #No Pass

#Subset by Time

ULTime<-subset(UL, time =="27"|time=="90"|time =="28"|time=="91")

DMTime<-subset(DM, time =="27"|time=="90"|time =="28"|time=="91")

BFTime<-subset(BF, time =="27"|time=="90"|time =="28"|time=="91")

CFTime<-subset(CF, time =="27"|time=="90"|time =="28"|time=="91")


#Subset by Trmt

ULTime12 <-subset(ULTime, Treatment=="12")

ULTime4 <-subset(ULTime, Treatment=="4")

DMTime12 <-subset(DMTime, Treatment=="12")

DMTime4 <-subset(DMTime, Treatment=="4")

BFTime12 <-subset(BFTime, Treatment=="12")

BFTime4 <-subset(BFTime, Treatment=="4")

CFTime12 <-subset(CFTime, Treatment=="12")

CFTime4 <-subset(CFTime, Treatment=="4")


# 1 WAY ANOVA for Pulse (Wet up Effect) for 4 and 12 Cycle

res.aov<- aov(BG.corr~time, data=ULTime12) #no sig
summary(res.aov)

res.aov<- aov(NAG.corr~time, data=ULTime12) #no sig
summary(res.aov)

res.aov<- aov(AP.corr~time, data=ULTime12) #no sig
summary(res.aov)

res.aov<- aov(BG.corr~time, data=ULTime4) #no sig
summary(res.aov)

res.aov<- aov(NAG.corr~time, data=ULTime4) #no sig
summary(res.aov)

res.aov<- aov(AP.corr~time, data=ULTime4) #no sig
summary(res.aov)



res.aov<- aov(BG.corr~time, data=DMTime12) #no sig
summary(res.aov)

res.aov<- aov(NAGLOGL~time, data=DMTime12) #no sig
summary(res.aov)

res.aov<- aov(AP.corr~time, data=DMTime12) #no sig
summary(res.aov)

res.aov<- aov(BG.corr~time, data=DMTime4) #no sig
summary(res.aov)

res.aov<- aov(NAGLOGL~time, data=DMTime4) #no sig
summary(res.aov)

res.aov<- aov(AP.corr~time, data=DMTime4) #no sig
summary(res.aov)



res.aov<- aov(BG.corr~time, data=BFTime12) #p=0.0383
summary(res.aov)

res.aov<- aov(NAGLOG~time, data=BFTime12) #p=0.0486
summary(res.aov)

res.aov<- aov(AP.corr~time, data=BFTime12) #p=0.00592
summary(res.aov)

res.aov<- aov(BG.corr~time, data=BFTime4) #no sig
summary(res.aov)

res.aov<- aov(NAGLOG~time, data=BFTime4) #no sig
summary(res.aov)

res.aov<- aov(AP.corr~time, data=BFTime4) #no sig
summary(res.aov)


res.aov<- aov(BGLOG~time, data=CFTime12) #no sig
summary(res.aov)

kruskal.test(NAG.corr~time, data=CFTime12) #no sig

kruskal.test(AP.corr~time, data=CFTime12)  #no sig


res.aov<- aov(BGLOG~time, data=CFTime4) #p=0.0484
summary(res.aov)

kruskal.test(NAG.corr~time, data=CFTime4) #no sig

kruskal.test(AP.corr~time, data=CFTime4)  #no sig


