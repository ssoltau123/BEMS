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


#Paired T Test for 4 Cycle Pre and Post

t.test(DMRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., DMRate4Pre$Post, paired = T)

t.test(BFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., BFRate4Pre$Post, paired = T)

t.test(CFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., CFRate4Pre$Post, paired = T)


