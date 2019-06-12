CumGas <- read.csv("~/Desktop/BEMS R Studio/Data/CumGas.csv")

View(CumGas)

# Reading in CSV file

#Subsetting data by cumulative gas amount collected on SD 25 and SD 1

Cum25 <- subset(CumGas, Date == "25")
Cum1<-subset(CumGas, Date=="1")

#Substracting SD 25 from data from SD 1 to get Net Cumulative Gas Production following Preincubation

Cum25$NetCO2<-Cum25$CO2-Cum1$CO2
Cum25$NetN2O<-Cum25$N2O-Cum1$N2O
Cum25$NetCH4<-Cum25$CH4-Cum1$CH4

#Subsetting data by soils

BFCum <- subset(Cum25, Sample == "BF")
CFCum <- subset(Cum25, Sample == "CF")
ULCum <- subset(Cum25, Sample == "UL")
DMCum <- subset(Cum25, Sample == "DM")

#Running 1-WAY ANOVA with Contrasts
install.packages("psych")
# Set up -------------------------------------------------
library(car)
library(lsmeans)
library(psych)

ULCum$Treatment = factor(ULCum$Treatment, 
                         levels=unique(ULCum$Treatment))

levels(ULCum$Treatment)

DMCum$Treatment = factor(DMCum$Treatment, 
                         levels=unique(ULCum$Treatment))

levels(DMCum$Treatment)

BFCum$Treatment = factor(BFCum$Treatment, 
                         levels=unique(BFCum$Treatment))

levels(BFCum$Treatment)

CFCum$Treatment = factor(CFCum$Treatment, 
                         levels=unique(CFCum$Treatment))

levels(CFCum$Treatment)

Contrasts = list(ADvsFC         = c(1,  -1, 0, 0,  0),
                 ADvsSAT        = c(1, 0, -1, 0,  0),
                 FCvsSAT       = c(0, 1, -1,  0,  0),
                 SATvs4         = c(0, 0,  1,  -1,  0),
                 SATvs12        = c(0,  0,  1, 0,  -1),
                 Fourvs12        = c(0,  0,  0,  1, -1))

# Stats for each measurement --------CO2-----------------


describeBy(ULCum$NetCO2, ULCum$Treatment, mat=TRUE)

model <- aov(NetCO2 ~ Treatment, data = ULCum)

shapiro.test(residuals(model)) # normality PASS
  
leveneTest(NetCO2 ~ Treatment, data = ULCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below #P-VALUE=4.26 X 10^-12
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
# Next measurement --------------------------------------
             
             describeBy(DMCum$NetCO2, DMCum$Treatment, mat=TRUE)
             
             model <- aov(NetCO2 ~ Treatment, data = DMCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCO2 ~ Treatment, data = DMCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below 
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")

             # Next measurement --------------------------------------
             
             describeBy(BFCum$NetCO2, BFCum$Treatment, mat=TRUE)
             
             model <- aov(NetCO2 ~ Treatment, data = BFCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCO2 ~ Treatment, data = BFCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below 
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Next measurement --------------------------------------
             
             describeBy(CFCum$NetCO2, CFCum$Treatment, mat=TRUE)
             
             model <- aov(NetCO2 ~ Treatment, data = CFCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCO2 ~ Treatment, data = CFCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below #P-VALUE=4.26 X 10^-12
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Stats for each measurement --------N2O-----------------
             
             
             describeBy(ULCum$NetN2O, ULCum$Treatment, mat=TRUE)
             
             model <- aov(NetN2O ~ Treatment, data = ULCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetN2O ~ Treatment, data = ULCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below #P-VALUE=4.26 X 10^-12
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Next measurement --------------------------------------
             
             describeBy(DMCum$NetN2O, DMCum$Treatment, mat=TRUE)
             
             model <- aov(NetN2O ~ Treatment, data = DMCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetN2O ~ Treatment, data = DMCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below 
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
          # Next measurement --------------------------------------
             
             describeBy(BFCum$NetN2O, BFCum$Treatment, mat=TRUE)
             
             model <- aov(NetN2O ~ Treatment, data = BFCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetN2O ~ Treatment, data = BFCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below 
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Next measurement --------------------------------------
             
             describeBy(CFCum$NetN2O, CFCum$Treatment, mat=TRUE)
             
             model <- aov(NetN2O ~ Treatment, data = CFCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetN2O ~ Treatment, data = CFCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below #P-VALUE=4.26 X 10^-12
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")

             # Stats for each measurement --------CH4-----------------
             
             
             describeBy(ULCum$NetCH4, ULCum$Treatment, mat=TRUE)
             
             model <- aov(NetCH4 ~ Treatment, data = ULCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCH4 ~ Treatment, data = ULCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below #P-VALUE=4.26 X 10^-12
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Next measurement --------------------------------------
             
             describeBy(DMCum$NetCH4, DMCum$Treatment, mat=TRUE)
             
             model <- aov(NetCH4 ~ Treatment, data = DMCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCH4 ~ Treatment, data = DMCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below 
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Next measurement --------------------------------------
             
             describeBy(BFCum$NetCH4, BFCum$Treatment, mat=TRUE)
             
             model <- aov(NetCH4 ~ Treatment, data = BFCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCH4 ~ Treatment, data = BFCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below 
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             # Next measurement --------------------------------------
             
             describeBy(CFCum$NetCH4, CFCum$Treatment, mat=TRUE)
             
             model <- aov(NetCH4 ~ Treatment, data = CFCum)
             
             shapiro.test(residuals(model)) # normality PASS
             
             leveneTest(NetCH4 ~ Treatment, data = CFCum , center = mean) # homoskedasticity
             plot(model) # further diagnostics
             
             summary(model) # If p < 0.05, proceed below #P-VALUE=4.26 X 10^-12
             
             leastsquare = lsmeans(model, "Treatment")  
             contrast(leastsquare, Contrasts, adjust = "sidak")
             
             
