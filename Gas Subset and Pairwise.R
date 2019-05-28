CumGas <- read.csv("~/Desktop/CumGas.csv")

View(CumGas)

# Reading in CSV file

Cum25 <- subset(CumGas, Date == "25")

#Subsetting data by cumulative gas amount collected on SD 25

BFCum <- subset(Cum25, Sample == "BF")
CFCum <- subset(Cum25, Sample == "CF")
ULCum <- subset(Cum25, Sample == "UL")
DMCum <- subset(Cum25, Sample == "DM")

#Subsetting data by soils

BFControls <- subset(BFCum, Treatment == "AD" |Treatment == "FC" |Treatment == "SAT" )
BFCycles <- subset(BFCum, Treatment == "4" |Treatment == "12" |Treatment == "SAT" )
CFControls <- subset(CFCum, Treatment == "AD" |Treatment == "FC" |Treatment == "SAT" )
CFCycles <- subset(CFCum, Treatment == "4" |Treatment == "12" |Treatment == "SAT" )
ULControls <- subset(ULCum, Treatment == "AD" |Treatment == "FC" |Treatment == "SAT" )
ULCycles <- subset(ULCum, Treatment == "4" |Treatment == "12" |Treatment == "SAT" )
DMControls <- subset(DMCum, Treatment == "AD" |Treatment == "FC" |Treatment == "SAT" )
DMCycles <- subset(DMCum, Treatment == "4" |Treatment == "12" |Treatment == "SAT" )

#Subsetting data by two Trmt subsets (controls v. cycling)

#Checking for Normality (#** needs log transform)

shapiro.test(ULControls$CO2) #**
shapiro.test(ULControls$N2O) #**
shapiro.test(ULControls$CH4) 

shapiro.test(DMControls$CO2) 
shapiro.test(DMControls$N2O) #**
shapiro.test(DMControls$CH4) 

shapiro.test(BFControls$CO2) #**
shapiro.test(BFControls$N2O)
shapiro.test(BFControls$CH4)

shapiro.test(CFControls$CO2) #**
shapiro.test(CFControls$N2O)
shapiro.test(CFControls$CH4) #**

shapiro.test(ULCycles$CO2) 
shapiro.test(ULCycles$N2O)
shapiro.test(ULCycles$CH4) 

shapiro.test(DMCycles$CO2) #**
shapiro.test(DMCycles$N2O) #**
shapiro.test(DMCycles$CH4) 

shapiro.test(BFCycles$CO2) 
shapiro.test(BFCycles$N2O)
shapiro.test(BFCycles$CH4) #**

shapiro.test(CFCycles$CO2) 
shapiro.test(CFCycles$N2O)
shapiro.test(CFCycles$CH4) 

#Log Transform #**

ULControls$logCO2<-log(ULControls$CO2)
shapiro.test(ULControls$logCO2) #log T made p value even lower (not sure what to do here?? Go to non parametric?)

ULControls$logN2O<-log(ULControls$N2O)
shapiro.test(ULControls$logN2O)
qqnorm(ULControls$logN2O) #No Pass

DMControls$logN2O<-log(DMControls$N2O)
shapiro.test(DMControls$logN2O) #Pass

BFControls$logCO2<-log(BFControls$CO2)
shapiro.test(BFControls$logCO2) #No Pass

CFControls$logCO2<-log(CFControls$CO2+10)
shapiro.test(CFControls$logCO2) #No Pass

CFControls$logCH4<-log(CFControls$CH4+1000)
shapiro.test(CFControls$logCH4) #No Pass

DMCycles$logCO2<-log(DMCycles$CO2+10)
shapiro.test(DMCycles$logCO2) #No Pass

DMCycles$logN2O<-log(DMCycles$N2O+10)
shapiro.test(DMCycles$logN2O) #Pass


#One Way ANOVA and Tukey for Subsetted Data (Controls)

res.aov <- aov(CH4 ~ Treatment, data = ULControls) #p=0.000105 
summary(res.aov)

TukeyHSD(res.aov) #All controls are sig for CH4 in UL

#Pairwise and 1 Way Anova (basic command line)

res.aov <- aov(CO2 ~ Treatment, data = DMControls) #p=9.94e-08 
summary(res.aov)

TukeyHSD(res.aov) #All controls are sig for CO2 in DM

res.aov <- aov(logN2O ~ Treatment, data = DMControls) #p=1.99e-06 
summary(res.aov)

TukeyHSD(res.aov) #All controls are sig for N2O in DM


res.aov <- aov(CH4 ~ Treatment, data = DMControls) #p=0.000288  
summary(res.aov)

TukeyHSD(res.aov) #FC and AD sig from SAT, but not SAT and FC


res.aov <- aov(N2O ~ Treatment, data = BFControls) #p= 0.0238  
summary(res.aov)

TukeyHSD(res.aov) #FC and AD sig only

res.aov <- aov(CH4 ~ Treatment, data = BFControls) #p= 0.00145  
summary(res.aov)

TukeyHSD(res.aov) #All controls are sig for CH4 in BF


res.aov <- aov(N2O ~ Treatment, data = CFControls) #p= 0.776
summary(res.aov)

TukeyHSD(res.aov) #no sig


#One Way ANOVA and Tukey on Subsetted Data (Cycling)

res.aov <- aov(CO2 ~ Treatment, data = ULCycles) #p=0.0279
summary(res.aov)

TukeyHSD(res.aov) #Only Sat and 4 sig

res.aov <- aov(N2O ~ Treatment, data = ULCycles) #p=0.00197
summary(res.aov)

TukeyHSD(res.aov) #Only Sat and 4 not sig

res.aov <- aov(CH4 ~ Treatment, data = ULCycles) #p=0.000101
summary(res.aov)

TukeyHSD(res.aov) #Only Sat and 12 not sig


res.aov <- aov(logN2O ~ Treatment, data = DMCycles) #p=0.00027
summary(res.aov)

TukeyHSD(res.aov) #Only Sat and 12 not sig


res.aov <- aov(CO2 ~ Treatment, data = CFCycles) #p8.61e-06
summary(res.aov)

TukeyHSD(res.aov) #All sig

res.aov <- aov(N2O ~ Treatment, data = CFCycles) #p=0.00104
summary(res.aov) 

TukeyHSD(res.aov) #All sig

res.aov <- aov(CH4 ~ Treatment, data = CFCycles) #p= 0.00467
summary(res.aov)

TukeyHSD(res.aov) #Only Sat and 4 sig








#Plot building for mean cumulative gases across treatments (possible supplemental figure for those who want to visually see the data points in relation to each other)
library(tidyverse)
DMCumAve<-DMCum %>% 
  group_by(Treatment) %>% 
  summarise(CO2 = mean(CO2))

DMCumAve <- as.data.frame(DMCumAve)

#Piping function below to compute averages within specified groups (eg. treatments)


DMCumSummary <- DMCum %>% # the names of the new data frame and the data frame to be summarised
  group_by(Treatment) %>%   # the grouping variable
  summarise(mean_CO2 = mean(CO2),  # calculates the mean of each group
            sd_C = sd(CO2), # calculates the standard deviation of each group
            n_C = n(),  # calculates the sample size per group
            SE_C = sd(CO2)/sqrt(n())) # calculates the standard error of each group

#Summarizing data with basic stats that will help create error bars later

library("ggpubr")


DMCumSummary$Treatment <- as.character(DMCumSummary$Treatment)

DMCumSummary$Treatment <- factor(DMCumSummary$Treatment, levels=c( "AD", "FC","SAT","4","12"))

#Reorder data so X axis labels are in recommended order 

library("ggplot2")
DMCumPlot <- ggplot(DMCumSummary, aes(Treatment, mean_CO2)) + 
  geom_point() +  
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.2) +
  geom_point(shape=c(1,2,3,4,5), fill="NA", color=c("red","dark green","blue", "orange", "purple"), size=2)
  DMCumPlot + labs(y="CO2 Flux (mg C/kg OD soil) ± s.d.", x = "Treatment") + ggtitle("Mean Cumulative CO2 Evolved within Deer Meadow") + theme_classic()
  
 #Basic scatterplot discrete X and continuous Y with SD error bars and different thems for each point 

