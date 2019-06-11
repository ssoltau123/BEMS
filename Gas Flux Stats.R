GasMoist <- read.csv("~/Desktop/BEMS R Studio/Data/GasMoist.csv")
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

ULRate4Pre$Post<-ULRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d.
ULRate4Pre$PostN2O<-ULRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
ULRate4Pre$PostCH4<-ULRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.


DMRate4Pre<-subset(DMRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
DMRate4Post<-subset(DMRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

DMRate4Pre$Post<-DMRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
DMRate4Pre$PostN2O<-DMRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
DMRate4Pre$PostCH4<-DMRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

BFRate4Pre<-subset(BFRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
BFRate4Post<-subset(BFRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

BFRate4Pre$Post<-BFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
BFRate4Pre$PostN2O<-BFRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
BFRate4Pre$PostCH4<-BFRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

CFRate4Pre<-subset(CFRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
CFRate4Post<-subset(CFRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")

CFRate4Pre$Post<-CFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
CFRate4Pre$PostN2O<-CFRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
CFRate4Pre$PostCH4<-CFRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

#Subset Pre and Post for 12 Cycle

ULRate12Pre<-subset(ULRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
ULRate12Post<-subset(ULRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

ULRate12Pre$Post<-ULRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d.
ULRate12Pre$PostN2O<-ULRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
ULRate12Pre$PostCH4<-ULRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

DMRate12Pre<-subset(DMRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
DMRate12Post<-subset(DMRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

DMRate12Pre$Post<-DMRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
DMRate12Pre$PostN2O<-DMRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
DMRate12Pre$PostCH4<-DMRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

BFRate12Pre<-subset(BFRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
BFRate12Post<-subset(BFRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

BFRate12Pre$Post<-BFRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
BFRate12Pre$PostN2O<-BFRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
BFRate12Pre$PostCH4<-BFRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

CFRate12Pre<-subset(CFRate12, Sample.Date=="2"|Sample.Date=="4"|Sample.Date=="6"|Sample.Date=="8"|Sample.Date=="10"|Sample.Date=="12"|Sample.Date=="14"|Sample.Date=="16"|Sample.Date=="18"|Sample.Date=="20"|Sample.Date=="22"|Sample.Date=="24")
CFRate12Post<-subset(CFRate12, Sample.Date=="3"|Sample.Date=="5"|Sample.Date=="7"|Sample.Date=="9"|Sample.Date=="11"|Sample.Date=="13"|Sample.Date=="15"|Sample.Date=="17"|Sample.Date=="19"|Sample.Date=="21"|Sample.Date=="23"|Sample.Date=="25")

CFRate12Pre$Post<-CFRate12Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d.
CFRate12Pre$PostN2O<-CFRate4Post$Gas.Flux...µg.N2O.N.kg.OD.d.
CFRate12Pre$PostCH4<-CFRate4Post$Gas.Flux...µg.CH4.C.kg.OD.soil.d.


#Paired T Test for Cycle (Wilcoxxn non parametric approach) Pre and Post CO2, N2O, CH4
#Is the Birch effect happening???  A sig diff between pre and post wet up gas fluxes
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

#Krustal Wallis and Dunn Test CO2, N2O, and CH4 (4 Cycle)
install.packages("FSA")
install.packages ("car")
install.packages ("lsmeans")
install.packages("ggplot2")
library(FSA)

install.packages("dunn.test")

#Difference between pre and post and new column

ULRate4Pre$Dif <- ULRate4Pre$Post - ULRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d
DMRate4Pre$Dif <- DMRate4Pre$Post -DMRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d
BFRate4Pre$Dif <- BFRate4Pre$Post - BFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d
CFRate4Pre$Dif <- CFRate4Pre$Post - CFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d

ULRate4Pre$DifN2O <- ULRate4Pre$Post - ULRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d.
DMRate4Pre$DifN2O <- DMRate4Pre$Post -DMRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d.
BFRate4Pre$DifN2O <- BFRate4Pre$Post - BFRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d.
CFRate4Pre$DifN2O <- CFRate4Pre$Post - CFRate4Pre$Gas.Flux...µg.N2O.N.kg.OD.d.

ULRate4Pre$DifCH4 <- ULRate4Pre$Post - ULRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.
DMRate4Pre$DifCH4 <- DMRate4Pre$Post -DMRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.
BFRate4Pre$DifCH4 <- BFRate4Pre$Post - BFRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.
CFRate4Pre$DifCH4 <- CFRate4Pre$Post - CFRate4Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

ULRate12Pre$Dif <- ULRate12Pre$Post - ULRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d
DMRate12Pre$Dif <- DMRate12Pre$Post -DMRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d
BFRate12Pre$Dif <- BFRate12Pre$Post - BFRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d
CFRate12Pre$Dif <- CFRate12Pre$Post - CFRate12Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d

ULRate12Pre$DifN2O <- ULRate12Pre$Post - ULRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d.
DMRate12Pre$DifN2O <- DMRate12Pre$Post -DMRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d.
BFRate12Pre$DifN2O <- BFRate12Pre$Post - BFRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d.
CFRate12Pre$DifN2O <- CFRate12Pre$Post - CFRate12Pre$Gas.Flux...µg.N2O.N.kg.OD.d.

ULRate12Pre$DifCH12 <- ULRate12Pre$Post - ULRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.
DMRate12Pre$DifCH12 <- DMRate12Pre$Post -DMRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.
BFRate12Pre$DifCH12 <- BFRate12Pre$Post - BFRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.
CFRate12Pre$DifCH12 <- CFRate12Pre$Post - CFRate12Pre$Gas.Flux...µg.CH4.C.kg.OD.soil.d.

View((ULRate12Pre))
#Krustal Wallis and Dunn Test on Difference 
#Is there a change in the magnitude of the Birch effect over time??


# 4 Cycle

kruskal.test(Dif ~ Sample.Date, data = ULRate4Pre) #p-value = 0.01556
dunnTest(Dif ~ Sample.Date, data = ULRate4Pre, method="sidak") #sig between 6 and 18

kruskal.test(Dif ~ Sample.Date, data = DMRate4Pre) #p-value = 0.02374
dunnTest(Dif ~ Sample.Date, data = DMRate4Pre, method="sidak") #sig between 18 and 24

kruskal.test(Dif ~ Sample.Date, data = BFRate4Pre) #N.S
dunnTest(Dif ~ Sample.Date, data = BFRate4Pre)

kruskal.test(Dif ~ Sample.Date, data = CFRate4Pre) #p-value = 0.0232
dunnTest(Dif ~ Sample.Date, data = CFRate4Pre, method="sidak") #sig between 6 and 18



kruskal.test(DifN2O ~ Sample.Date, data = ULRate4Pre) #p-value = 0.01556
dunnTest(DifN2O ~ Sample.Date, data = ULRate4Pre, method="sidak") #sig between 6 and 18

kruskal.test(DifN2O ~ Sample.Date, data = DMRate4Pre) #p-value=0.02374
dunnTest(DifN2O ~ Sample.Date, data = DMRate4Pre,method="sidak") #Sig between 18 and 24

kruskal.test(DifN2O ~ Sample.Date, data = BFRate4Pre) #p-value=0.03781
dunnTest(DifN2O ~ Sample.Date, data = BFRate4Pre, method="sidak") #N.S.

kruskal.test(DifN2O ~ Sample.Date, data = CFRate4Pre) #p-value = 0.01556
dunnTest(DifN2O ~ Sample.Date, data = CFRate4Pre) #sig between 6 and 18



kruskal.test(DifCH4 ~ Sample.Date, data = ULRate4Pre) #p-value = 0.02374
dunnTest(DifCH4 ~ Sample.Date, data = ULRate4Pre, method="sidak") #sig between 12 and 18


kruskal.test(DifCH4 ~ Sample.Date, data = BFRate4Pre) #p-value = 0.03781
dunnTest(DifCH4 ~ Sample.Date, data = BFRate4Pre, method="sidak") #sig between 18 and 24


#12 Cycle

kruskal.test(Dif ~ Sample.Date, data = ULRate12Pre) #p-value = 0.001052
dunnTest(Dif ~ Sample.Date, data = ULRate12Pre, method="sidak") #sig between 8 and 12 and between 4 and 12

kruskal.test(Dif ~ Sample.Date, data = DMRate12Pre) #p-value= 0.002659
dunnTest(Dif ~ Sample.Date, data = DMRate12Pre, method="sidak") #sig between 10 and 16 and between

kruskal.test(Dif ~ Sample.Date, data = BFRate12Pre) #p-value=0.0008179
dunnTest(Dif ~ Sample.Date, data = BFRate12Pre, method="sidak") #sig between 12 and 2 

kruskal.test(Dif ~ Sample.Date, data = CFRate12Pre) #p-value = 0.003694
dunnTest(Dif ~ Sample.Date, data = CFRate12Pre, method="sidak") #sig between 12 and 8 and between 12 and 4



kruskal.test(DifN2O ~ Sample.Date, data = ULRate12Pre) #p-value=0.0005088
dunnTest(DifN2O ~ Sample.Date, data = ULRate12Pre, method="sidak") #sig between 18 and 4

kruskal.test(DifN2O ~ Sample.Date, data = DMRate12Pre) #p-value=0.001464
dunnTest(DifN2O ~ Sample.Date, data = DMRate12Pre,method="sidak") #sig between 18 and 2

kruskal.test(DifN2O ~ Sample.Date, data = BFRate12Pre) #p-value=0.001063
dunnTest(DifN2O ~ Sample.Date, data = BFRate12Pre, method="sidak") #sig between 12 and 2

kruskal.test(DifN2O ~ Sample.Date, data = CFRate12Pre) #0.0005665
dunnTest(DifN2O ~ Sample.Date, data = CFRate12Pre, method="sidak") #sig between 18 and 8 and between 18 and 2 


#CH4 not sig in Wilcox so no follow up for 12 cycle
