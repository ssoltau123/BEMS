#Read in Data

MBN <- read.csv("~/Desktop/BEMS R Studio/Data/MBN.csv")
View(MBN)

#Subsetting Data for Net Changes in MB and N-T

MBNet<-subset(MBN, Analysis=="MBUF")
MBNet5<-subset(MBNet, Sample.Date=="5")

#Setting Level Order of Treatments

MBNet5$Treatment <- ordered(MBNet5$Treatment,
                           levels = c("AD", "FC","SAT","4","12"))

levels(MBNet5$Treatment)

#Subsetting by Soil

MBUL<-subset(MBNet5, Soil=="UL")
MBDM<-subset(MBNet5, Soil=="DM")
MBBF<-subset(MBNet5, Soil=="BF")
MBCF<-subset(MBNet5, Soil=="CF")


#Setting up Model (similar to Cum Gases)

Contrasts = list(ADvsFC         = c(1,  -1, 0, 0,  0),
                 ADvsSAT        = c(1, 0, -1, 0,  0),
                 FCvsSAT       = c(0, 1, -1,  0,  0),
                 SATvs4         = c(0, 0,  1,  -1,  0),
                 SATvs12        = c(0,  0,  1, 0,  -1),
                 Fourvs12        = c(0,  0,  0,  1, -1))

# Stats for each measurement --------Net Microbial C-----------------


describeBy(MBUL$Net.TC, MBUL$Treatment)

model <- aov(Net.TC ~ Treatment, data = MBUL)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TC ~ Treatment, data = MBUL , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBDM$Net.TC, MBDM$Treatment)

model <- aov(Net.TC ~ Treatment, data = MBDM)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TC ~ Treatment, data = MBDM , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below 
leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBBF$Net.TC, MBBF$Treatment)

model <- aov(Net.TC ~ Treatment, data = MBBF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TC ~ Treatment, data = MBBF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBCF$Net.TC, MBCF$Treatment)

model <- aov(Net.TC ~ Treatment, data = MBCF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TC ~ Treatment, data = MBCF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


# Stats for each measurement --------Net Microbial N-----------------


describeBy(MBUL$Net.TN, MBUL$Treatment)

model <- aov(Net.TN ~ Treatment, data = MBUL)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TN ~ Treatment, data = MBUL , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBDM$Net.TN, MBDM$Treatment)

model <- aov(Net.TN ~ Treatment, data = MBDM)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TN ~ Treatment, data = MBDM , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below 
leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBBF$Net.TN, MBBF$Treatment)

model <- aov(Net.TN ~ Treatment, data = MBBF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TN ~ Treatment, data = MBBF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")

describeBy(MBCF$Net.TN, MBCF$Treatment)

model <- aov(Net.TN ~ Treatment, data = MBCF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.TN ~ Treatment, data = MBCF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")

# Stats for each measurement --------Net Mineralization-----------------


describeBy(MBUL$Net.Min, MBUL$Treatment)

model <- aov(Net.Min ~ Treatment, data = MBUL)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Min ~ Treatment, data = MBUL , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBDM$Net.Min, MBDM$Treatment)

model <- aov(Net.Min ~ Treatment, data = MBDM)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Min ~ Treatment, data = MBDM , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below 
leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBBF$Net.Min, MBBF$Treatment)

model <- aov(Net.Min ~ Treatment, data = MBBF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Min ~ Treatment, data = MBBF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")

describeBy(MBCF$Net.Min, MBCF$Treatment)

model <- aov(Net.Min ~ Treatment, data = MBCF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Min ~ Treatment, data = MBCF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


# Stats for each measurement --------Net Nitrification-----------------


describeBy(MBUL$Net.Nit, MBUL$Treatment)

model <- aov(Net.Nit ~ Treatment, data = MBUL)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Nit ~ Treatment, data = MBUL , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBDM$Net.Nit, MBDM$Treatment)

model <- aov(Net.Nit ~ Treatment, data = MBDM)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Nit ~ Treatment, data = MBDM , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below 
leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


describeBy(MBBF$Net.Nit, MBBF$Treatment)

model <- aov(Net.Nit ~ Treatment, data = MBBF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Nit ~ Treatment, data = MBBF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")

describeBy(MBCF$Net.Nit, MBCF$Treatment)

model <- aov(Net.Nit ~ Treatment, data = MBCF)

shapiro.test(residuals(model)) # normality PASS

leveneTest(Net.Nit ~ Treatment, data = MBCF , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) # If p < 0.05, proceed below #Not Sig

leastsquare = lsmeans(model, "Treatment")  
contrast(leastsquare, Contrasts, adjust = "sidak")


##########


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

res.aov<-aov(Net.TC~Treatment, MBUL)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Net.TC~Treatment, MBULCon)
summary(res.aov)
TukeyHSD(res.aov)

res.aov<-aov(Net.TC~Treatment, MBULCycle)
summary(res.aov)
TukeyHSD(res.aov)

#With Contrasts

MBUL$Treatment = factor(MBUL$Treatment, 
                         levels=unique(ULCum$Treatment))

levels(MBUL$Treatment)


model <- aov(Net.TC ~ Treatment, data = MBUL)
summary(model)

TukeyHSD(model, method="sidak")


library(lsmeans)

leastsquare = lsmeans(model, "Treatment")

Contrasts = list(ADvsFC         = c(1,  1, 0, -0,  0),
                 ADvsSAT        = c(1, 0, -1, 0,  0),
                 FCvsSAT       = c(0, 1, -1,  0,  0),
                 SATvs4         = c(0, 0,  1,  -1,  0),
                 SATvs12        = c(0,  0,  1, 0,  -1),
                 Fourvs12        = c(0,  0,  0,  1, -1))



contrast(leastsquare, Contrasts, adjust="sidak")

require(psych)
library(psych)
describeBy(MBUL$Net.TC, MBUL$Treatment, mat=TRUE)

