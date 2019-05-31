#Read in Data

MBN <- read.csv("~/Desktop/MBN.csv")
View(MBN)

#Subsetting Data for Net Changes in MB and N-T

MBNet<-subset(MBN, Analysis=="MBUF")
MBNet2<-subset(MBNet, Sample.Date=="5")


#Subsetting by Soil

MBUL<-subset(MBNet2, Soil=="UL")
MBDM<-subset(MBNet2, Soil=="DM")
MBBF<-subset(MBNet2, Soil=="BF")
MBCF<-subset(MBNet2, Soil=="CF")

#Subsetting by Static vs. Cycling

MBULCycle <-subset(MBUL, Treatment=="SAT"|Treatment=="4"|Treatment=="12")
MBULCon <-subset(MBUL, Treatment=="SAT"|Treatment=="AD"|Treatment=="FC")
MBDMCycle <-subset(MBDM, Treatment=="SAT"|Treatment=="4"|Treatment=="12")
MBDMCon <-subset(MBDM, Treatment=="SAT"|Treatment=="AD"|Treatment=="FC")
MBBFCycle <-subset(MBBF, Treatment=="SAT"|Treatment=="4"|Treatment=="12")
MBBFCon <-subset(MBBF, Treatment=="SAT"|Treatment=="AD"|Treatment=="FC")
MBCFCycle <-subset(MBCF, Treatment=="SAT"|Treatment=="4"|Treatment=="12")
MBCFCon <-subset(MBCF, Treatment=="SAT"|Treatment=="AD"|Treatment=="FC")

#Normality Testing

shapiro.test(MBULCycle$Net.TC)
shapiro.test(MBULCycle$Net.TN)
shapiro.test(MBULCycle$Net.Min)
shapiro.test(MBULCycle$Net.Nit)

shapiro.test(MBULCon$Net.TC)
shapiro.test(MBULCon$Net.TN)
shapiro.test(MBULCon$Net.Min)
shapiro.test(MBULCon$Net.Nit) #**

shapiro.test(MBDMCycle$Net.TC)
shapiro.test(MBDMCycle$Net.TN)
shapiro.test(MBDMCycle$Net.Min) #**
shapiro.test(MBDMCycle$Net.Nit) #**

shapiro.test(MBDMCon$Net.TC)
shapiro.test(MBDMCon$Net.TN)
shapiro.test(MBDMCon$Net.Min)#**
shapiro.test(MBDMCon$Net.Nit)

shapiro.test(MBBFCycle$Net.TC)
shapiro.test(MBBFCycle$Net.TN)
shapiro.test(MBBFCycle$Net.Min) 
shapiro.test(MBBFCycle$Net.Nit) #**

shapiro.test(MBBFCon$Net.TC)
shapiro.test(MBBFCon$Net.TN)
shapiro.test(MBBFCon$Net.Min) #**
shapiro.test(MBBFCon$Net.Nit)

shapiro.test(MBCFCycle$Net.TC)
shapiro.test(MBCFCycle$Net.TN)
shapiro.test(MBCFCycle$Net.Min)  #**
shapiro.test(MBCFCycle$Net.Nit) #**

shapiro.test(MBCFCon$Net.TC)
shapiro.test(MBCFCon$Net.TN)
shapiro.test(MBCFCon$Net.Min) 
shapiro.test(MBCFCon$Net.Nit) #**

#Temp non paramtetic approach (will go back and do parametric after log T)

kruskal.test(Net.TC~Treatment, MBULCon) #no sig
kruskal.test(Net.TC~Treatment, MBULCycle) #no sig

kruskal.test(Net.TN~Treatment, MBULCon) #no sig
kruskal.test(Net.TN~Treatment, MBULCycle) #no sig

kruskal.test(Net.Min~Treatment, MBULCon) #p-value = 0.02732
dunnTest(Net.Min~Treatment, MBULCon) #Only FC and SAT sig

kruskal.test(Net.Min~Treatment, MBULCycle) #no sig

kruskal.test(Net.Nit~Treatment, MBULCon) #p-value = 0.02732
dunnTest(Net.Nit~Treatment, MBULCon) #Only FC and AD sig

kruskal.test(Net.Nit~Treatment, MBULCycle) #no sig


kruskal.test(Net.TC~Treatment, MBDMCon) #p-value = 0.02732
dunnTest(Net.TC~Treatment, MBDMCon) #Only sig in AD and SAT

kruskal.test(Net.TC~Treatment, MBDMCycle) #no sig

kruskal.test(Net.TN~Treatment, MBDMCon) #no sig
kruskal.test(Net.TN~Treatment, MBDMCycle) #no sig

kruskal.test(Net.Min~Treatment, MBDMCon) #no sig
kruskal.test(Net.Min~Treatment, MBDMCycle) #no sig

kruskal.test(Net.Nit~Treatment, MBDMCon) #no sig
kruskal.test(Net.Nit~Treatment, MBDMCycle) #p-value = 0.03899
dunnTest(Net.Nit~Treatment, MBDMCycle) #Significant between 12 and SAT


kruskal.test(Net.TC~Treatment, MBBFCon) #p-value = 0.02732
dunnTest(Net.TC~Treatment, MBBFCon) #Only sig in AD and SAT

kruskal.test(Net.TC~Treatment, MBBFCycle) #no sig

kruskal.test(Net.TN~Treatment, MBBFCon) #no sig
kruskal.test(Net.TN~Treatment, MBBFCycle) #no sig

kruskal.test(Net.Min~Treatment, MBBFCon) #no sig
kruskal.test(Net.Min~Treatment, MBBFCycle) # p-value = 0.03899
dunnTest(Net.Min~Treatment, MBBFCycle) #Sig between SAT and 4

kruskal.test(Net.Nit~Treatment, MBBFCon) ##p-value = 0.03899
dunnTest(Net.Nit~Treatment, MBBFCon) #Sig between AD and FC

kruskal.test(Net.Nit~Treatment, MBBFCycle) #p-value = 0.02732
dunnTest(Net.Nit~Treatment, MBBFCycle) #Significant between 12 and 4


kruskal.test(Net.TC~Treatment, MBCFCon) ##no sig
kruskal.test(Net.TC~Treatment, MBCFCycle) #no sig

kruskal.test(Net.TN~Treatment, MBCFCon) #p-value = 0.02732
dunnTest(Net.TN~Treatment, MBCFCon) #Sig between AD and FC
kruskal.test(Net.TN~Treatment, MBCFCycle) #no sig

kruskal.test(Net.Min~Treatment, MBCFCon) 
kruskal.test(Net.Min~Treatment, MBCFCycle) #no sig


kruskal.test(Net.Nit~Treatment, MBCFCon) 
kruskal.test(Net.Nit~Treatment, MBCFCycle) #p-value = 0.02732
dunnTest(Net.Nit~Treatment, MBCFCycle) #Sig between 4 and SAT


