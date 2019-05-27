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
DMRateCon <-subset(DMRate, Treatment=="FC"|Treatment=="SAT")

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

