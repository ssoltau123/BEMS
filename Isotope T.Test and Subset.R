BemsIsotopes <- read.csv("~/Desktop/ISO.csv")
View(BemsIsotopes)

# Reading in CSV file

#Subsetting by Soil

BFIso <- subset(BemsIsotopes, Sample.ID == "BF")
CFIso <- subset(BemsIsotopes, Sample.ID == "CF")
ULIso <- subset(BemsIsotopes, Sample.ID == "UL")
DMIso <- subset(BemsIsotopes, Sample.ID == "DM")


#Testing Normality
shapiro.test(ULIso$X.C) #Pass
shapiro.test(DMIso$X.C) #Pass
shapiro.test(BFIso$X.C) #Pass
shapiro.test(CFIso$X.C) #Pass

shapiro.test(ULIso$X.N) #Pass
shapiro.test(DMIso$X.N) #Pass
shapiro.test(BFIso$X.N) #Pass
shapiro.test(CFIso$X.N) #No Pass

CFIso$logN<-log(CFIso$X.N)
shapiro.test(CFIso$logN) #Still No Pass

shapiro.test(ULIso$X13C) #No Pass
shapiro.test(DMIso$X13C) #No Pass
shapiro.test(BFIso$X13C) #No Pass
shapiro.test(CFIso$X13C) #No Pass

shapiro.test(ULIso$X13N) #Pass
shapiro.test(DMIso$X13N) #Pass
shapiro.test(BFIso$X13N) #Pass
shapiro.test(CFIso$X13N) #Pass

#Running 1-way ANOVA on %C by all Treatments

res.aov <- aov(X.C ~ Treatment, data = ULIso) #p=9.58e-05
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X.C ~ Treatment, data = DMIso) #p=0.72 no sig
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X.C ~ Treatment, data = BFIso) #p=0.00794
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X.C ~ Treatment, data = CFIso) #p-0.00687
summary(res.aov)

TukeyHSD(res.aov)

#What is the best way to assign the subscripts indicating significance to my table using the results from Tukey.  Steve mentioned some tips, but I cannot recall 100%

#Running 1-way ANOVA/Krustal Wallis on %N by all Treatments

res.aov <- aov(X.N ~ Treatment, data = ULIso) #p=0.0115
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X.N ~ Treatment, data = DMIso) #p=0.838 no sig
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X.N ~ Treatment, data = BFIso) #p=0.256 no sig
summary(res.aov)

TukeyHSD(res.aov)

require(FSA)
install.packages("FSA")
library(FSA)
kruskal.test(X.N ~ Treatment, data = CFIso) #p-value = 0.008749
dunnTest(X.N ~ Treatment, data = CFIso, method="bh")

#Running Krustal Wallis and Dunn Test on 13C for all Treatments

kruskal.test(X13C ~ Treatment, data = ULIso) #p-value = 0.763 no sig
dunnTest(X13C ~ Treatment, data = ULIso, method="bh")

kruskal.test(X13C ~ Treatment, data = DMIso) #p-value = 0.022
dunnTest(X13C ~ Treatment, data = ULIso, method="bh")

kruskal.test(X13C ~ Treatment, data = BFIso) #p-value = 0.1436 no sig
dunnTest(X13C ~ Treatment, data = ULIso, method="bh")

kruskal.test(X13C ~ Treatment, data = CFIso) #p-value = 0.1527 no sig
dunnTest(X13C ~ Treatment, data = ULIso, method="bh")

#Running 1-way ANOVA on 15N by all Treatments

res.aov <- aov(X13N ~ Treatment, data = ULIso) # p=0.00014
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X13N ~ Treatment, data = DMIso) #p=0.771 no sig
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X13N ~ Treatment, data = BFIso) #p=0.0147
summary(res.aov)

TukeyHSD(res.aov)

res.aov <- aov(X13N ~ Treatment, data = CFIso) #p-0.00123
summary(res.aov)

TukeyHSD(res.aov)


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
