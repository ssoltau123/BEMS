BemsIsotopes <- read.csv("~/Desktop/BEMS R Studio/Data/ISO.csv")
View(BemsIsotopes)

# Reading in CSV file

#Converting TC and TN to SI units --> g/kg

BemsIsotopes$TC<-BemsIsotopes$X.C*10
BemsIsotopes$TN<-BemsIsotopes$X.N*10

#Subsetting by Soil

BFIso <- subset(BemsIsotopes, Sample.ID == "BF")
CFIso <- subset(BemsIsotopes, Sample.ID == "CF")
ULIso <- subset(BemsIsotopes, Sample.ID == "UL")
DMIso <- subset(BemsIsotopes, Sample.ID == "DM")

#1 WAY ANOVA (setting up model for samples)
ULIso$Treatment = factor(ULIso$Treatment, 
                         levels=unique(ULIso$Treatment))

levels(ULIso$Treatment)

DMIso$Treatment = factor(DMIso$Treatment, 
                         levels=unique(DMIso$Treatment))

levels(DMIso$Treatment)

BFIso$Treatment = factor(BFIso$Treatment, 
                         levels=unique(BFIso$Treatment))

levels(BFIso$Treatment)

CFIso$Treatment = factor(CFIso$Treatment, 
                         levels=unique(CFIso$Treatment))

levels(CFIso$Treatment)

library(psych)

#_________________Model for TC

describeBy(ULIso$TC, ULIso$Treatment)

model <- aov(TC ~ Treatment, data = ULIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TC ~ Treatment, data = ULIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

describeBy(DMIso$TC, DMIso$Treatment)

model <- aov(TC ~ Treatment, data = DMIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TC ~ Treatment, data = DMIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) #No Sig
TukeyHSD(model, method="sidak")


describeBy(BFIso$TC, BFIso$Treatment)

model <- aov(TC ~ Treatment, data = BFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TC ~ Treatment, data = BFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")


describeBy(CFIso$TC, CFIso$Treatment)

model <- aov(TC ~ Treatment, data = CFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TC ~ Treatment, data = CFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

#_________________Model for TN

describeBy(ULIso$TN, ULIso$Treatment)

model <- aov(TN ~ Treatment, data = ULIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TN ~ Treatment, data = ULIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

describeBy(DMIso$TN, DMIso$Treatment)

model <- aov(TN ~ Treatment, data = DMIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TN ~ Treatment, data = DMIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) #No Sig
TukeyHSD(model, method="sidak")


describeBy(BFIso$TN, BFIso$Treatment)

model <- aov(TN ~ Treatment, data = BFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TN ~ Treatment, data = BFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")


describeBy(CFIso$TN, CFIso$Treatment)

model <- aov(TN ~ Treatment, data = CFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(TN ~ Treatment, data = CFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

#_________________Model for 13C

describeBy(ULIso$X13C, ULIso$Treatment)

model <- aov(X13C ~ Treatment, data = ULIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13C ~ Treatment, data = ULIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

describeBy(DMIso$X13C, DMIso$Treatment)

model <- aov(X13C ~ Treatment, data = DMIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13C ~ Treatment, data = DMIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) #No Sig
TukeyHSD(model, method="sidak")


describeBy(BFIso$X13C, BFIso$Treatment)

model <- aov(X13C ~ Treatment, data = BFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13C ~ Treatment, data = BFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")


describeBy(CFIso$X13C, CFIso$Treatment)

model <- aov(X13C ~ Treatment, data = CFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13C ~ Treatment, data = CFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

#_________________Model for 

describeBy(ULIso$X13N, ULIso$Treatment)

model <- aov(X13N ~ Treatment, data = ULIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13N ~ Treatment, data = ULIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")

describeBy(DMIso$X13N, DMIso$Treatment)

model <- aov(X13N ~ Treatment, data = DMIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13N ~ Treatment, data = DMIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model) #No Sig
TukeyHSD(model, method="sidak")


describeBy(BFIso$X13N, BFIso$Treatment)

model <- aov(X13N ~ Treatment, data = BFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13N ~ Treatment, data = BFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")


describeBy(CFIso$X13N, CFIso$Treatment)

model <- aov(X13N ~ Treatment, data = CFIso)

shapiro.test(residuals(model)) # normality PASS

leveneTest(X13N ~ Treatment, data = CFIso , center = mean) # homoskedasticity
plot(model) # further diagnostics

summary(model)
TukeyHSD(model, method="sidak")


#Creating Box Plots for Total C and N between Treatments (Possible supplemental figures)



BFIso$Treatment <- ordered(BFIso$Treatment,
                             levels = c("PI", "AD", "FC","SAT","4","12"))

CFIso$Treatment <- ordered(CFIso$Treatment,
                      levels = c("PI", "AD", "FC","SAT","4","12"))

ULIso$Treatment <- ordered(ULIso$Treatment,
                      levels = c("PI", "AD", "FC","SAT","4","12"))

DMIso$Treatment <- ordered(DMIso$Treatment,
                      levels = c("PI", "AD", "FC","SAT","4","12"))

par(mfrow=c(2,2))
boxplot(X.C~Treatment, data=BFIso, ylab="Wt.% C", xlab="Treatment", main="Total Carbon in Big Flat (Restored)", ylim=c(3.5,11), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))
boxplot(X.C~Treatment, data=CFIso, ylab="Wt.% C", xlab="Treatment", main="Total Carbon in Coyote Flat (Severely Degraded)", ylim=c(3.5,11), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))
boxplot(X.C~Treatment, data=ULIso, ylab="Wt.% C", xlab="Treatment", main="Total Carbon in Upper Loney (Pristine)", ylim=c(4.0,17.5), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))
boxplot(X.C~Treatment, data=DMIso, ylab="Wt.% C", xlab="Treatment", main="Total Carbon in Deer Meadow (Moderately Degraded)", ylim=c(4.0,17.5), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))

boxplot(Wt...N~Treatment, data=BFIso, ylab="Wt.% N", xlab="Treatment", main="Total Nitrogen in Big Flat (Restored)", ylim=c(0.25,1), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))
boxplot(Wt...N~Treatment, data=CFIso, ylab="Wt.% N", xlab="Treatment", main="Total Nitrogen in Coyote Flat (Severely Degraded)", ylim=c(0.25,1), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))
boxplot(Wt...N~Treatment, data=ULIso, ylab="Wt.% N", xlab="Treatment", main="Total Nitrogen in Upper Loney (Pristine)", ylim=c(0.2,1), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))
boxplot(Wt...N~Treatment, data=DMIso, ylab="Wt.% N", xlab="Treatment", main="Total Nitrogen in Deer Meadow (Moderately Degraded)", ylim=c(0.2,1), col=c("orange", "yellow", "light green", "light blue", "gray", "pink"))

par(mfrow=c(2,4))

boxplot(Wt...C13~Treatment, data=BFIso)
boxplot(Wt...C13~Treatment, data=CFIso)
boxplot(Wt...C13~Treatment, data=ULIso)
boxplot(Wt...C13~Treatment, data=DMIso)

boxplot(Wt...N15~Treatment, data=BFIso)
boxplot(Wt...N15~Treatment, data=CFIso)
boxplot(Wt...N15~Treatment, data=ULIso)
boxplot(Wt...N15~Treatment, data=DMIso)
