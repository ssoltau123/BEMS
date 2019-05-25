GasMoist <- read.csv("~/Desktop/GasMoist.csv")
 View(GasMoist)
 
# Heyyyyyyyy
 
 #Read in File
 
 ULRate4Pre<-subset(ULRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
 ULRate4Post<-subset(ULRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")
 
 ULRate4Pre$Post<-ULRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
 
 t.test(ULRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., ULRate4Pre$Post, paired = T)
 
 
 DMRate4Pre<-subset(DMRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
 DMRate4Post<-subset(DMRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")
 
 DMRate4Pre$Post<-DMRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
 
 t.test(DMRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., DMRate4Pre$Post, paired = T)
 
 
 BFRate4Pre<-subset(BFRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
 BFRate4Post<-subset(BFRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")
 
 BFRate4Pre$Post<-BFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
 
 t.test(BFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., BFRate4Pre$Post, paired = T)
 
 
 CFRate4Pre<-subset(CFRate4, Sample.Date=="6"|Sample.Date=="12"|Sample.Date=="18"|Sample.Date=="24")
 CFRate4Post<-subset(CFRate4, Sample.Date=="7"|Sample.Date=="13"|Sample.Date=="19"|Sample.Date=="25")
 
 CFRate4Pre$Post<-CFRate4Post$Gas.Flux...mg.CO2.C.kg.OD.soil.d
 
 t.test(CFRate4Pre$Gas.Flux...mg.CO2.C.kg.OD.soil.d., CFRate4Pre$Post, paired = T)
 
 library(tidyverse)
 ULRate4PreSum <- ULRate4Pre %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Post))
 
 DMRate4PreSum <- DMRate4Pre %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Post)) 
 
 BFRate4PreSum <- BFRate4Pre %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Post))
 
 CFRate4PreSum <- CFRate4Pre %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Post))
 
 library(tidyverse)
 ULRate12Sum1 <- ULRate12 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(ULRate12$Gas.Flux...mg.CO2.C.kg.OD.soil.d.))

 res.aov <- aov(Gas.Flux...mg.CO2.C.kg.OD.soil.d.~ Treatment, data=DMRateCon)
 summary(res.aov)
 TukeyHSD(res.aov)
 
 ULRate4$Gas.Flux...mg.CO2.C.kg.OD.soil.d.
 
 
install.packages("pgirmess")
 
kruskal.test(ULRate12$Gas.Flux...mg.CO2.C.kg.OD.soil.d~ Sample.Date, data = ULRate12)

require(pgirmess)
kruskalmc(ULRate4$Cum.Gas.Flux...mg.CO2.C.kg.OD.soil., ULRate4$Sample.Date)


TukeyHSD(res.aov)

 library("car")
 qqplot()
 
 wilcox.test(y~A) (Gas.Flux...mg.CO2.C.kg.OD.soil.d.~ Sample.Date,
          data=BFRate4, method = "bh")

shapiro.test(ULRate4Pre$Post)
 
 ULRate4$logCO2<-log10(10*ULRate4$Gas.Flux...mg.CO2.C.kg.OD.soil.d.+10)  
 BFCum$logCO2<-log10(10*BFCum$CO2)  **
   CFCum$logCO2<-log10(10*CFCum$CO2)  
 
 
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
 

 
 
 DMRate4<-subset(DMRate, Treatment=="4")
 DMRate12<-subset(DMRate, Treatment=="12")
 
 
 BFRate4<-subset(BFRate, Treatment=="4")
 BFRate12<-subset(BFRate, Treatment=="12")

 CFRate4<-subset(CFRate, Treatment=="4")
 CFRate12<-subset(CFRate, Treatment=="12")
 
 #Summarize 4 and 12 data with Standard errors
 
 #Summarize Gas Rates
 library(tidyverse)
 ULRate4Sum <- ULRate4 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
            
 
 library(tidyverse)
 ULRate12Sum <- ULRate12 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 ULRateADSum <- ULRateAD %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 ULRateFCSum <- ULRateFC %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 ULRateSATSum <- ULRateSAT %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 DMRate4Sum <- DMRate4 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 DMRate12Sum <- DMRate12 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 DMRateADSum <- DMRateAD %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 DMRateFCSum <- DMRateFC %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 DMRateSATSum <- DMRateSAT %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 BFRate4Sum <- BFRate4 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 BFRate12Sum <- BFRate12 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 BFRateADSum <- BFRateAD %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 BFRateFCSum <- BFRateFC %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 BFRateSATSum <- BFRateSAT %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 CFRate4Sum <- CFRate4 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 CFRate12Sum <- CFRate12 %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 
 library(tidyverse)
 CFRateADSum <- CFRateAD %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 CFRateFCSum <- CFRateFC %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 library(tidyverse)
 CFRateSATSum <- CFRateSAT %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(mean_CO2 = mean(Gas.Flux...mg.CO2.C.kg.OD.soil.d.),
             mean_CH4 = mean(Gas.Flux...µg.CH4.C.kg.OD.soil.d.),
             mean_N2O = mean(Gas.Flux...µg.N2O.N.kg.OD.d.),
             Days=mean(Cum.Days),
             APer.Sat = mean(Per.Sat), 
             sd_M = sd(Per.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Per.Sat)/sqrt(n()), # calculates the mean of each group
             sd_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_C = n(),  # calculates the sample size per group
             SE_C = sd(Gas.Flux...mg.CO2.C.kg.OD.soil.d.)/sqrt(n()),
             sd_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.), # calculates the standard deviation of each group
             n_N = n(),  # calculates the sample size per group
             SE_N = sd(Gas.Flux...µg.N2O.N.kg.OD.d.)/sqrt(n()),
             sd_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.), # calculates the standard deviation of each group
             n_CH = n(),  # calculates the sample size per group
             SE_CH = sd(Gas.Flux...µg.CH4.C.kg.OD.soil.d.)/sqrt(n()))
 
 #Summarize Soil Moisture
 library(tidyverse)
 CFRateSumMoist <- CFRate %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(Per.Sat = mean(Percent.Sat),
             CumDays=mean(Cum.Days),
             Weeks=mean(Weeks),
             sd_M = sd(Percent.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Percent.Sat)/sqrt(n())) 
 
 
 library(tidyverse)
 BFRateSumMoist <- BFRate %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(Per.Sat = mean(Percent.Sat),
             sd_M = sd(Percent.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Percent.Sat)/sqrt(n())) 
 
 library(tidyverse)
 ULRateSumMoist <- ULRate %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(Per.Sat = mean(Percent.Sat),
             sd_M = sd(Percent.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Percent.Sat)/sqrt(n())) 
 
 library(tidyverse)
 DMRateSumMoist <- DMRate %>% # the names of the new data frame and the data frame to be summarised
   group_by(Sample.Date) %>%   # the grouping variable
   summarise(Per.Sat = mean(Percent.Sat),
             sd_M = sd(Percent.Sat), # calculates the standard deviation of each group
             n_M = n(),  # calculates the sample size per group
             SE_M = sd(Percent.Sat)/sqrt(n())) 
 
 
 #Simple Plots (more advanced plots for paper later)
 
 par(mfrow=c(4,2),  mai = c(0.4, 0.8, 0.2, 0.1))
 
 plot(CFRate4Sum$Sample.Date, CFRate4Sum$mean_CO2, type="l", xlab=)
 
 plot(CFRate12Sum$Sample.Date, CFRate12Sum$mean_CO2, type="l", xlab=)
 
 plot(CFRate4Sum$Sample.Date, CFRate4Sum$mean_N2O, type="l", xlab=)
 
 plot(CFRate12Sum$Sample.Date, CFRate12Sum$mean_N2O, type="l", xlab=)
 
 plot(CFRate4Sum$Sample.Date, CFRate4Sum$mean_CH4, type="l", xlab=)
 
 plot(CFRate12Sum$Sample.Date, CFRate12Sum$mean_CH4, type="l", xlab=)
 
 plot(CFRate4Sum$Sample.Date, CFRate4Sum$Per.Sat, type="l")
 
 plot(CFRate12Sum$Sample.Date, CFRate12Sum$Per.Sat, type="l")
 

 
 

 View(ULRate4SumMoist) 
 library(ggplot2)

 ULM4 <-ggplot(data=ULRate4Sum, aes(x=Days, y=APer.Sat)) + theme_bw() + 
   geom_line(color="red")+
   geom_point(color="red", size=1.3) + 
   geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="red")+
   labs(y="Percent Saturation", x = "# Incubation Weeks") +
     theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  +
   scale_x_continuous(breaks= seq(0,91,by=10)) +  scale_y_continuous(limits=c(NA,105), expand = c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
  ULM4 
  
 
 ULC4 <-ggplot(data=ULRate4Sum, aes(x=Days, y=mean_CO2)) +
   geom_line(color="red")+
   geom_point(color="red", size=1.3) + 
   geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="red")+
   labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
 theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(NA,NA), expand = c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 ULC4
 
 ULM12 <-ggplot(data=ULRate12Sum, aes(x=Days, y=APer.Sat)) +
   geom_line(color="purple")+
   geom_point(color="purple", size=1.3) + 
   geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="purple")+
   labs(y="Percent Saturation", x = "# Incubation Days") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) + scale_y_continuous(limits = c(0, 105), expand = c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
  ULM12
 
 ULC12 <-ggplot(data=ULRate12Sum, aes(x=Days, y=mean_CO2)) +
   geom_line(color="purple")+
   geom_point(color="purple", size=1.3) + 
   geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="purple")+
   labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
 theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(0,NA), expand = c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 ULC12
 
 ULMCon <-ggplot(data=ULRateADSum, aes(x=Days, y=APer.Sat)) +
   geom_line(color="orange")+
   geom_line(data=ULRateFCSum, aes(x=Days, y=APer.Sat), color="green")+
   geom_line(data=ULRateSATSum, aes(x=Days, y=APer.Sat), color="blue")+
   geom_point(data=ULRateSATSum, color="blue", size=1.3) + 
   geom_point(data=ULRateADSum, color="orange", size=1.3) +
   geom_point(data=ULRateFCSum, color="green", size=1.3) +
   geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="dark gray")+
   labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days")+
 theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(0,100), expand_limits(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 

  ULMCon
 
 ULCCon <-ggplot(data=ULRateADSum, aes(x=Days, y=mean_CO2)) +
   geom_line(color="orange")+
   geom_line(data=ULRateFCSum, aes(x=Days, y=mean_CO2), color="green")+
   geom_line(data=ULRateSATSum, aes(x=Days, y=mean_CO2), color="blue")+
   geom_point(data=ULRateSATSum, color="blue", size=1.2) + 
   geom_point(data=ULRateADSum, color="orange", size=1.3) +
 geom_point(data=ULRateFCSum, color="green", size=1.2) +
   geom_errorbar(data=ULRateADSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="orange")+
   geom_errorbar(data=ULRateFCSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="green")+
   geom_errorbar(data=ULRateSATSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="blue")+
   labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(breaks= seq(0,100,by=25)) +
   theme(axis.title.y = element_blank(), 
  axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
   axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 
 ULCCon 
 
 
 ULN4 <-ggplot(data=ULRate4Sum, aes(x=Days, y=mean_N2O)) +
   geom_line(color="red")+
   geom_point(color="red", size=1.3) + 
   geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="red")+
   labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(-10,3500), expand=c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 ULN4
 
 ULCH4 <-ggplot(data=ULRate4Sum, aes(x=Days, y=mean_CH4)) +
   geom_line(color="red")+
   geom_hline(yintercept=0) +
   geom_point(color="red", size=1.3) + 
   geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="red")+
   labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(-60,30), expand=c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 ULCH4
 
 ULN12 <-ggplot(data=ULRate12Sum, aes(x=Days, y=mean_N2O)) +
   geom_line(color="purple")+
   geom_point(color="purple", size=1.3) + 
   geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="purple")+
   labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10))  + scale_y_continuous(expand=c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 ULN12
 
 ULCH12 <-ggplot(data=ULRate12Sum, aes(x=Days, y=mean_CH4)) +
   geom_line(color="purple")+
   geom_hline(yintercept=0) +
   geom_point(color="purple", size=1.3) + 
   geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="purple")+
   labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(-55,130), expand=c(0,0)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 
 ULCH12
 
 ULNCon <-ggplot(data=ULRateADSum, aes(x=Days, y=mean_N2O)) +
   geom_line(color="orange")+
   geom_line(data=ULRateFCSum, aes(x=Days, y=mean_N2O), color="green")+
   geom_line(data=ULRateSATSum, aes(x=Days, y=mean_N2O), color="blue")+
   geom_point(data=ULRateSATSum, color="blue", size=1.2) + 
   geom_point(data=ULRateADSum, color="orange", size=1.3) +
   geom_point(data=ULRateFCSum, color="green", size=1.2) +
   geom_errorbar(data=ULRateADSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="orange")+
   geom_errorbar(data=ULRateFCSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="green")+
   geom_errorbar(data=ULRateSATSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="blue")+
   labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(breaks= seq(0,2000,by=400)) +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 
 ULNCon 
 
 ULCHCon <-ggplot(data=ULRateADSum, aes(x=Days, y=mean_CH4)) +
   geom_line(color="orange")+
   geom_line(data=ULRateFCSum, aes(x=Days, y=mean_CH4), color="green")+
   geom_line(data=ULRateSATSum, aes(x=Days, y=mean_CH4), color="blue")+
   geom_point(data=ULRateSATSum, color="blue", size=1.3) + 
   geom_point(data=ULRateADSum, color="orange", size=1.3) +
   geom_point(data=ULRateFCSum, color="green", size=1.3) +
   geom_errorbar(data=ULRateADSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="orange")+
   geom_errorbar(data=ULRateFCSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="green")+
   geom_errorbar(data=ULRateSATSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="blue")+
   labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
   scale_x_continuous(breaks= seq(0,90,by=10))  +
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
         axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))
 
 
 ULCHCon 
 
 
 par(mfrow=c(2,1))
ULM
ULC

require(gridExtra)
ULGas4<-grid.arrange(ULC4, ULN4, ULCH4, ULM4, nrow=4)
ULGas12<-grid.arrange(ULC12, ULN12, ULCH12, ULM12, nrow=4)
ULGasCon<-grid.arrange(ULCCon, ULNCon, ULCHCon, ULMCon, nrow=4)

ULGas<-grid.arrange(ULGas4, ULGas12, ULGasCon, ncol=3)

library(ggplot2)

DMM4 <-ggplot(data=DMRate4Sum, aes(x=Days, y=APer.Sat)) + theme_bw() + 
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="red")+
  labs(y="Percent Saturation", x = "# Incubation Weeks") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  +
  scale_x_continuous(breaks= seq(0,91,by=10)) +  scale_y_continuous(limits=c(NA,105), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMM4 


DMC4 <-ggplot(data=DMRate4Sum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(NA,NA), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMC4

DMM12 <-ggplot(data=DMRate12Sum, aes(x=Days, y=APer.Sat)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="purple")+
  labs(y="Percent Saturation", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) + scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMM12

DMC12 <-ggplot(data=DMRate12Sum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="purple")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMC12

DMMCon <-ggplot(data=DMRateADSum, aes(x=Days, y=APer.Sat)) +
  geom_line(color="orange")+
  geom_line(data=DMRateFCSum, aes(x=Days, y=APer.Sat), color="green")+
  geom_line(data=DMRateSATSum, aes(x=Days, y=APer.Sat), color="blue")+
  geom_point(data=DMRateSATSum, color="blue", size=1.3) + 
  geom_point(data=DMRateADSum, color="orange", size=1.3) +
  geom_point(data=DMRateFCSum, color="green", size=1.3) +
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="dark gray")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(0,100), expand_limits(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


DMMCon

DMCCon <-ggplot(data=DMRateADSum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="orange")+
  geom_line(data=DMRateFCSum, aes(x=Days, y=mean_CO2), color="green")+
  geom_line(data=DMRateSATSum, aes(x=Days, y=mean_CO2), color="blue")+
  geom_point(data=DMRateSATSum, color="blue", size=1.2) + 
  geom_point(data=DMRateADSum, color="orange", size=1.3) +
  geom_point(data=DMRateFCSum, color="green", size=1.2) +
  geom_errorbar(data=DMRateADSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="orange")+
  geom_errorbar(data=DMRateFCSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="green")+
  geom_errorbar(data=DMRateSATSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(breaks= seq(0,30, by=5)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


DMCCon 


DMN4 <-ggplot(data=DMRate4Sum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(-10,2600), expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMN4

DMCH4 <-ggplot(data=DMRate4Sum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="red")+
  geom_hline(yintercept=0) +
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(-40,30), expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMCH4

DMN12 <-ggplot(data=DMRate12Sum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="purple")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  + scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

DMN12

DMCH12 <-ggplot(data=DMRate12Sum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="purple")+
  geom_hline(yintercept=0) +
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="purple")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(-55,60), expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


DMCH12

DMNCon <-ggplot(data=DMRateADSum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="orange")+
  geom_line(data=DMRateFCSum, aes(x=Days, y=mean_N2O), color="green")+
  geom_line(data=DMRateSATSum, aes(x=Days, y=mean_N2O), color="blue")+
  geom_point(data=DMRateSATSum, color="blue", size=1.2) + 
  geom_point(data=DMRateADSum, color="orange", size=1.3) +
  geom_point(data=DMRateFCSum, color="green", size=1.2) +
  geom_errorbar(data=DMRateADSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="orange")+
  geom_errorbar(data=DMRateFCSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="green")+
  geom_errorbar(data=DMRateSATSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(breaks= seq(0,2000,by=40)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


DMNCon 

DMCHCon <-ggplot(data=DMRateADSum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="orange")+
  geom_line(data=DMRateFCSum, aes(x=Days, y=mean_CH4), color="green")+
  geom_line(data=DMRateSATSum, aes(x=Days, y=mean_CH4), color="blue")+
  geom_point(data=DMRateSATSum, color="blue", size=1.3) + 
  geom_point(data=DMRateADSum, color="orange", size=1.3) +
  geom_point(data=DMRateFCSum, color="green", size=1.3) +
  geom_errorbar(data=DMRateADSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="orange")+
  geom_errorbar(data=DMRateFCSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="green")+
  geom_errorbar(data=DMRateSATSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


DMCHCon 


par(mfrow=c(2,1))
ULM
ULC

require(gridExtra)
DMGas4<-grid.arrange(DMC4, DMN4, DMCH4, DMM4, nrow=4)
DMGas12<-grid.arrange(DMC12, DMN12, DMCH12, DMM12, nrow=4)
DMGasCon<-grid.arrange(DMCCon, DMNCon, DMCHCon, DMMCon, nrow=4)

DMGas<-grid.arrange(DMGas4, DMGas12, DMGasCon, ncol=3)

library(ggplot2)

BFM4 <-ggplot(data=BFRate4Sum, aes(x=Days, y=APer.Sat)) + theme_bw() + 
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="red")+
  labs(y="Percent Saturation", x = "# Incubation Weeks") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  +
  scale_x_continuous(breaks= seq(0,91,by=10)) +  scale_y_continuous(limits=c(NA,105), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFM4 


BFC4 <-ggplot(data=BFRate4Sum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(NA,130), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFC4

BFM12 <-ggplot(data=BFRate12Sum, aes(x=Days, y=APer.Sat)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="purple")+
  labs(y="Percent Saturation", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) + scale_y_continuous(seq(0, 100, by=25)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFM12

BFC12 <-ggplot(data=BFRate12Sum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="purple")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits = c(0,125)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFC12

  BFMCon <-ggplot(data=BFRateADSum, aes(x=Days, y=APer.Sat)) +
  geom_line(color="orange")+
  geom_line(data=BFRateFCSum, aes(x=Days, y=APer.Sat), color="green")+
  geom_line(data=BFRateSATSum, aes(x=Days, y=APer.Sat), color="blue")+
  geom_point(data=BFRateSATSum, color="blue", size=1.3) + 
  geom_point(data=BFRateADSum, color="orange", size=1.3) +
  geom_point(data=BFRateFCSum, color="green", size=1.3) +
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="dark gray")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(0,100), expand_limits(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


BFMCon

BFCCon <-ggplot(data=BFRateADSum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="orange")+
  geom_line(data=BFRateFCSum, aes(x=Days, y=mean_CO2), color="green")+
  geom_line(data=BFRateSATSum, aes(x=Days, y=mean_CO2), color="blue")+
  geom_point(data=BFRateSATSum, color="blue", size=1.2) + 
  geom_point(data=BFRateADSum, color="orange", size=1.3) +
  geom_point(data=BFRateFCSum, color="green", size=1.2) +
  geom_errorbar(data=BFRateADSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="orange")+
  geom_errorbar(data=BFRateFCSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="green")+
  geom_errorbar(data=BFRateSATSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(breaks= seq(0,200, by=20)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


BFCCon 


BFN4 <-ggplot(data=BFRate4Sum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFN4

BFCH4 <-ggplot(data=BFRate4Sum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="red")+
  geom_hline(yintercept=0) +
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFCH4

BFN12 <-ggplot(data=BFRate12Sum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="purple")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  + scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

BFN12

BFCH12 <-ggplot(data=BFRate12Sum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="purple")+
  geom_hline(yintercept=0) +
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="purple")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


BFCH12

BFNCon <-ggplot(data=BFRateADSum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="orange")+
  geom_line(data=BFRateFCSum, aes(x=Days, y=mean_N2O), color="green")+
  geom_line(data=BFRateSATSum, aes(x=Days, y=mean_N2O), color="blue")+
  geom_point(data=BFRateSATSum, color="blue", size=1.2) + 
  geom_point(data=BFRateADSum, color="orange", size=1.3) +
  geom_point(data=BFRateFCSum, color="green", size=1.2) +
  geom_errorbar(data=BFRateADSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="orange")+
  geom_errorbar(data=BFRateFCSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="green")+
  geom_errorbar(data=BFRateSATSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


BFNCon 

BFCHCon <-ggplot(data=BFRateADSum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="orange")+
  geom_line(data=BFRateFCSum, aes(x=Days, y=mean_CH4), color="green")+
  geom_line(data=BFRateSATSum, aes(x=Days, y=mean_CH4), color="blue")+
  geom_point(data=BFRateSATSum, color="blue", size=1.3) + 
  geom_point(data=BFRateADSum, color="orange", size=1.3) +
  geom_point(data=BFRateFCSum, color="green", size=1.3) +
  geom_errorbar(data=BFRateADSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="orange")+
  geom_errorbar(data=BFRateFCSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="green")+
  geom_errorbar(data=BFRateSATSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


BFCHCon 


par(mfrow=c(2,1))
ULM
ULC

require(gridExtra)
BFGas4<-grid.arrange(BFC4, BFN4, BFCH4, BFM4, nrow=4)
BFGas12<-grid.arrange(BFC12, BFN12, BFCH12, BFM12, nrow=4)
BFGasCon<-grid.arrange(BFCCon, BFNCon, BFCHCon, BFMCon, nrow=4)

BFGas<-grid.arrange(BFGas4, BFGas12, BFGasCon, ncol=3)

library(ggplot2)

CFM4 <-ggplot(data=CFRate4Sum, aes(x=Days, y=APer.Sat)) + theme_bw() + 
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="red")+
  labs(y="Percent Saturation", x = "# Incubation Weeks") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  +
  scale_x_continuous(breaks= seq(0,91,by=10)) +  scale_y_continuous(limits=c(NA,105), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFM4 


CFC4 <-ggplot(data=CFRate4Sum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(NA,45), expand = c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFC4

CFM12 <-ggplot(data=CFRate12Sum, aes(x=Days, y=APer.Sat)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="purple")+
  labs(y="Percent Saturation", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) + scale_y_continuous(seq(0, 100, by=25)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFM12

CFC12 <-ggplot(data=CFRate12Sum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CO2 - sd_C, ymax = mean_CO2 + sd_C), width=0.4,  color="purple")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFC12

CFMCon <-ggplot(data=CFRateADSum, aes(x=Days, y=APer.Sat)) +
  geom_line(color="orange")+
  geom_line(data=CFRateFCSum, aes(x=Days, y=APer.Sat), color="green")+
  geom_line(data=CFRateSATSum, aes(x=Days, y=APer.Sat), color="blue")+
  geom_point(data=CFRateSATSum, color="blue", size=1.3) + 
  geom_point(data=CFRateADSum, color="orange", size=1.3) +
  geom_point(data=CFRateFCSum, color="green", size=1.3) +
  geom_errorbar(aes(ymin = APer.Sat - sd_M, ymax = APer.Sat + sd_M), width=0.4,  color="dark gray")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(limits=c(0,100), expand_limits(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


CFMCon

CFCCon <-ggplot(data=CFRateADSum, aes(x=Days, y=mean_CO2)) +
  geom_line(color="orange")+
  geom_line(data=CFRateFCSum, aes(x=Days, y=mean_CO2), color="green")+
  geom_line(data=CFRateSATSum, aes(x=Days, y=mean_CO2), color="blue")+
  geom_point(data=CFRateSATSum, color="blue", size=1.2) + 
  geom_point(data=CFRateADSum, color="orange", size=1.3) +
  geom_point(data=CFRateFCSum, color="green", size=1.2) +
  geom_errorbar(data=CFRateADSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="orange")+
  geom_errorbar(data=CFRateFCSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="green")+
  geom_errorbar(data=CFRateSATSum, aes(ymin = mean_CO2 - SE_C, ymax = mean_CO2 + SE_C), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(breaks= seq(0,200, by=20)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


CFCCon 


CFN4 <-ggplot(data=CFRate4Sum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="red")+
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFN4

CFCH4 <-ggplot(data=CFRate4Sum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="red")+
  geom_hline(yintercept=0) +
  geom_point(color="red", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="red")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFCH4

CFN12 <-ggplot(data=CFRate12Sum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="purple")+
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="purple")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  + scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))

CFN12

CFCH12 <-ggplot(data=CFRate12Sum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="purple")+
  geom_hline(yintercept=0) +
  geom_point(color="purple", size=1.3) + 
  geom_errorbar(aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="purple")+
  labs(y="CO2 Flux (milligrams C/kg OD Soil)", x = "# Incubation Days") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10)) +  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


CFCH12

CFNCon <-ggplot(data=CFRateADSum, aes(x=Days, y=mean_N2O)) +
  geom_line(color="orange")+
  geom_line(data=CFRateFCSum, aes(x=Days, y=mean_N2O), color="green")+
  geom_line(data=CFRateSATSum, aes(x=Days, y=mean_N2O), color="blue")+
  geom_point(data=CFRateSATSum, color="blue", size=1.2) + 
  geom_point(data=CFRateADSum, color="orange", size=1.3) +
  geom_point(data=CFRateFCSum, color="green", size=1.2) +
  geom_errorbar(data=CFRateADSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="orange")+
  geom_errorbar(data=CFRateFCSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="green")+
  geom_errorbar(data=CFRateSATSum, aes(ymin = mean_N2O - SE_N, ymax = mean_N2O + SE_N), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


CFNCon 

CFCHCon <-ggplot(data=CFRateADSum, aes(x=Days, y=mean_CH4)) +
  geom_line(color="orange")+
  geom_line(data=CFRateFCSum, aes(x=Days, y=mean_CH4), color="green")+
  geom_line(data=CFRateSATSum, aes(x=Days, y=mean_CH4), color="blue")+
  geom_point(data=CFRateSATSum, color="blue", size=1.3) + 
  geom_point(data=CFRateADSum, color="orange", size=1.3) +
  geom_point(data=CFRateFCSum, color="green", size=1.3) +
  geom_errorbar(data=CFRateADSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="orange")+
  geom_errorbar(data=CFRateFCSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="green")+
  geom_errorbar(data=CFRateSATSum, aes(ymin = mean_CH4 - SE_CH, ymax = mean_CH4 + SE_CH), width=0.4,  color="blue")+
  labs(y="Soil Moisture (% of Saturation)", x = "# Incubation Days") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_x_continuous(breaks= seq(0,90,by=10))  +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"), 
        axis.title.x = element_blank(), axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=1,vjust=0,face="plain"))


CFCHCon 


par(mfrow=c(2,1))
ULM
ULC

require(gridExtra)
CFGas4<-grid.arrange(CFC4, CFN4, CFCH4, CFM4, nrow=4)
CFGas12<-grid.arrange(CFC12, CFN12, CFCH12, CFM12, nrow=4)
CFGasCon<-grid.arrange(CFCCon, CFNCon, CFCHCon, CFMCon, nrow=4)

CFGas<-grid.arrange(CFGas4, CFGas12, CFGasCon, ncol=3)






 
 