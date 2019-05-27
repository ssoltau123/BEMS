GasMoist <- read.csv("~/Desktop/GasMoist.csv")
View(GasMoist)

#Read in File


#Subset by Soil

ULRate<-subset(GasMoist, Sample.ID=="UL")
DMRate<-subset(GasMoist, Sample.ID=="DM")
BFRate<-subset(GasMoist, Sample.ID=="BF")
CFRate<-subset(GasMoist, Sample.ID=="CF")

#Subset by cycling and control treatments

ULRate4<-subset(ULRate, Treatment=="4")
ULRate12<-subset(ULRate, Treatment=="12")
ULRateAD<-subset(ULRate, Treatment=="AD")
ULRateFC<-subset(ULRate, Treatment=="FC")
ULRateSAT<-subset(ULRate, Treatment=="SAT")

DMRate4<-subset(DMRate, Treatment=="4")
DMRate12<-subset(DMRate, Treatment=="12")
DMRateAD<-subset(DMRate, Treatment=="AD")
DMRateFC<-subset (DMRate, Treatment=="FC")
DMRateSAT<-subset(DMRate, Treatment=="SAT")

BFRate4<-subset(BFRate, Treatment=="4")
BFRate12<-subset(BFRate, Treatment=="12")
BFRateAD<-subset(BFRate, Treatment=="AD")
BFRateFC<-subset(BFRate, Treatment=="FC")
BFRateSAT<-subset(BFRate, Treatment=="SAT")

CFRate4<-subset(CFRate, Treatment=="4")
CFRate12<-subset(CFRate, Treatment=="12")
CFRateAD<-subset(CFRate, Treatment=="AD")
CFRateFC<-subset(CFRate, Treatment=="FC")
CFRateSAT<-subset(CFRate, Treatment=="SAT")

#Subset by Pre and Post for 4 Cycle

ULRate4Pre<-subset(ULRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
ULRate4Post<-subset(ULRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

ULRate4Pre$Post<-ULRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d


DMRate4Pre<-subset(DMRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
DMRate4Post<-subset(DMRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

DMRate4Pre$Post<-DMRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

BFRate4Pre<-subset(BFRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
BFRate4Post<-subset(BFRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

BFRate4Pre$Post<-BFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

CFRate4Pre<-subset(CFRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
CFRate4Post<-subset(CFRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

CFRate4Pre$Post<-CFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

#Subset Pre and Post for 12 Cycle

ULRate12Pre<-subset(ULRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
ULRate12Post<-subset(ULRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

ULRate12Pre$Post<-ULRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

DMRate12Pre<-subset(DMRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
DMRate12Post<-subset(DMRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

DMRate12Pre$Post<-DMRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

BFRate12Pre<-subset(BFRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
BFRate12Post<-subset(BFRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

BFRate12Pre$Post<-BFRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

CFRate12Pre<-subset(CFRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
CFRate12Post<-subset(CFRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

CFRate12Pre$Post<-CFRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d

#Test for Normality (#** needs Log T) CO2

shapiro.test(ULRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**
 
shapiro.test(ULRate4Pre$Post) 

shapiro.test(DMRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**
  
shapiro.test(DMRate4Pre$Post) 

shapiro.test(BFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) 

shapiro.test(BFRate4Pre$Post) 

shapiro.test(CFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**

shapiro.test(CFRate4Pre$Post) 

shapiro.test(ULRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**

shapiro.test(ULRate12Pre$Post) #**

shapiro.test(DMRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**

shapiro.test(DMRate12Pre$Post) #**

shapiro.test(BFRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**

shapiro.test(BFRate12Pre$Post) #**

shapiro.test(CFRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.) #**

shapiro.test(CFRate12Pre$Post) #**

#Log Transform (Did for all 12 cycle)

ULRate4Pre$logPre<-log10(ULRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.+10)
shapiro.test(ULRate4Pre$logPre)

DMRate4Pre$logPre<-log10(DMRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.+10)
shapiro.test(DMRate4Pre$logPre)

CFRate4Pre$logPre<-log10(CFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d.+10)
shapiro.test(CFRate4Pre$logPre)


#Paired T Test for Cycle (Wilcoxxn non parametric approach) Pre and Post CO2, N2O, CH4
wilcox.test(ULRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., ULRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.0004883
wilcox.test(DMRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., DMRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.002441 
wilcox.test(BFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., BFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.0004883
wilcox.test(CFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., CFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.0004883

wilcox.test(ULRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., ULRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.002628
wilcox.test(DMRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., DMRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.03344
wilcox.test(BFRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., BFRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) # p-value = 0.0308
wilcox.test(CFRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., CFRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d., paired=TRUE) # p-value = 2.297e-06

wilcox.test(ULRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d., ULRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) #p-value = 0.0004883
wilcox.test(DMRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d., DMRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) #p-value = 0.0004883
wilcox.test(BFRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d., BFRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) #p-value = 0.0004883
wilcox.test(CFRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d., CFRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) #p-value = 0.0004883

wilcox.test(ULRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d., ULRate12Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) #p-value = 2.91e-11
wilcox.test(DMRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d., DMRate12Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) #p-value = 1.601e-09
wilcox.test(BFRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d., BFRate12Post$Gas.Flux...µg.N2O.N.kg.OD.d., paired=TRUE) # p-value = 2.91e-11
wilcox.test(CFRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d., CFRate12Post$Gas.Flux...µg.N2O.N.kg.OD.d.,  paired=TRUE) # p-value = 4.881e-07

wilcox.test(ULRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., ULRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) # p-value = 0.009277
wilcox.test(DMRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., DMRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #N.S
wilcox.test(BFRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., BFRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #p-value = 0.0004883
wilcox.test(CFRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., CFRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #N.S

wilcox.test(ULRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., ULRate12Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #N.S
wilcox.test(DMRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., DMRate12Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #N.S
wilcox.test(BFRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., BFRate12Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #N.S
wilcox.test(CFRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d., CFRate12Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d., paired=TRUE) #N.S

#non parametric tests would be preferred (p value way too low-most likely not fixed by log T)

install.packages("onewaytests")

#Krustal Wallis and Dunn Test CO2, N2O, and CH4 (4 Cycle)
install.packages("FSA")
library(FSA)
citation('FSA')
citation('ggplot2')

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = ULRate4) #p-value = 5.582e-05
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = ULRate4, method="bh") #1 and 2 SD were sig.

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = DMRate4) #p-value = 5.817e-05
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = DMRate4, method="bh") #Just 1 SD is sig

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = BFRate4) #p-value = 4.301e-05
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = BFRate4, method="bh") #Just 1 SD is sig

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = CFRate4) #p-value = 0.0002674
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = CFRate4, method="bh") #Just 1 SD is sig, less so than BF or DM

kruskal.test(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = ULRate4) #p-value = 4.05e-05
dunnTest(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = ULRate4, method="bh") #1 and 2 SD were sig.

kruskal.test(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = DMRate4) #p-value = 5.817e-05
dunnTest(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = DMRate4, method="bh") #Just 1 SD is sig

kruskal.test(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = BFRate4) #p-value = 4.301e-05
dunnTest(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = BFRate4, method="bh") #Just 1 SD is sig

kruskal.test(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = CFRate4) #p-value = 0.0002674
dunnTest(Gas.Flux...µg.CH4.C.kg.OD.soil.d. ~ Sample.Date, data = CFRate4, method="bh") #Just 1 SD is sig, less so than BF or DM

#Krustal Wallis and Dunn Test CO2, N2O, and CH4 (12 cycle)

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = ULRate12) #p-value = 2.012e-06
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = ULRate12, method="bh") #no sig

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = DMRate12) #p-value = 9.83e-06
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = DMRate12, method="bh") #no sig

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = BFRate12) #p-value = 3.037e-06
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = BFRate12, method="bh") #Just 6 SD is sig (half way)

kruskal.test(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = CFRate12) # p-value = 2.124e-06
dunnTest(Gas.Flux...mg.CO2.C.kg.OD.soil.d. ~ Sample.Date, data = CFRate12, method="bh") #Just 2 SD is sig









