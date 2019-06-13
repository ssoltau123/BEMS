Enzymes <- read.csv("~/Desktop/Enzymes.csv")

#Converting factor to numeric

Enzymes$BG.corr<-as.numeric(Enzymes$BG.corr)
Enzymes$NAG.corr<-as.numeric(Enzymes$NAG.corr)
Enzymes$AP.corr<-as.numeric(Enzymes$AP.corr)

#Subsetting with removed outliers

EnzymesNO<-subset(Enzymes, outlier=="n")

#Subsetting by Pre Wet ups for 2 WAY ANOVA

EPre<-subset(EnzymesNO, Days=="0"|Days=="20"|Days=="83")

#Subsetting by Soil

EPreUL<-subset(EPre, Meadow=="UL")
EPreDM<-subset(EPre, Meadow=="DM")
EPreBF<-subset(EPre, Meadow=="BF")
EPreCF<-subset(EPre, Meadow=="CF")

#Setting Up Model for 2 WAY ANOVA 

model <- aov(as.numeric(BG.corr) ~ Treatment*Days, data = EPreUL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(BG.corr) ~ Treatment*Days, data = EPreDM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(BG.corr) ~ Treatment*Days, data = EPreBF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(BG.corr) ~ Treatment*Days, data = EPreCF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 

#_______________________

model <- aov(as.numeric(NAG.corr) ~ Treatment*Days, data = EPreUL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(NAG.corr) ~ Treatment*Days, data = EPreDM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(NAG.corr) ~ Treatment*Days, data = EPreBF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(NAG.corr) ~ Treatment*Days, data = EPreCF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


#_______________________

model <- aov(as.numeric(AP.corr) ~ Treatment*Days, data = EPreUL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(AP.corr) ~ Treatment*Days, data = EPreDM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 


model <- aov(as.numeric(AP.corr) ~ Treatment*Days, data = EPreBF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 

model <- aov(as.numeric(AP.corr) ~ Treatment*Days, data = EPreCF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below 

#Subsetting by Cycling Treatments

EnzymesNO4<-subset(EnzymesNO, Treatment=="4")
EnzymesNO12<-subset(EnzymesNO, Treatment=="12")

#Subsetting by Time (Pre vs. Post Wet Up)

E41<-subset(EnzymesNO4, Days=="20"|Days=="21")
E42<-subset(EnzymesNO4, Days=="83"|Days=="84")

E121<-subset(EnzymesNO12, Days=="20"|Days=="21")
E122<-subset(EnzymesNO12, Days=="83"|Days=="84")

#Subsetting by Soil

E41UL<-subset(E41, Meadow=="UL")
E41DM<-subset(E41, Meadow=="DM")
E41BF<-subset(E41, Meadow=="BF")
E41CF<-subset(E41, Meadow=="CF")

E42UL<-subset(E42, Meadow=="UL")
E42DM<-subset(E42, Meadow=="DM")
E42BF<-subset(E42, Meadow=="BF")
E42CF<-subset(E42, Meadow=="CF")

E121UL<-subset(E121, Meadow=="UL")
E121DM<-subset(E121, Meadow=="DM")
E121BF<-subset(E121, Meadow=="BF")
E121CF<-subset(E121, Meadow=="CF")

E122UL<-subset(E122, Meadow=="UL")
E122DM<-subset(E122, Meadow=="DM")
E122BF<-subset(E122, Meadow=="BF")
E122CF<-subset(E122, Meadow=="CF")

#Paired T. Test ---> 1 WAY ANOVA spilt between T1 and T2 (unequal group sizes from removed outliers)

#__________________4 CYCLE BG
model <- aov(BG.corr ~ Days, data = E41UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E42UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below  NS


model <- aov(BG.corr ~ Days, data = E41DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E42DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E41BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E42BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.0353


model <- aov(BG.corr ~ Days, data = E41CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E42CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS

#__________________12 CYCLE BG
model <- aov(BG.corr ~ Days, data = E121UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E122UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below  NS


model <- aov(BG.corr ~ Days, data = E121DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E122DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E121BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.00405


model <- aov(BG.corr ~ Days, data = E122BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E121CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(BG.corr ~ Days, data = E122CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS



#__________________4 CYCLE NAG
model <- aov(NAG.corr ~ Days, data = E41UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E42UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below  NS


model <- aov(NAG.corr ~ Days, data = E41DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E42DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E41BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E42BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.0159


model <- aov(NAG.corr ~ Days, data = E41CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.0278


model <- aov(NAG.corr ~ Days, data = E42CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS

#__________________12 CYCLE 
model <- aov(NAG.corr ~ Days, data = E121UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E122UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below  NS


model <- aov(NAG.corr ~ Days, data = E121DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E122DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E121BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E122BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E121CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(NAG.corr ~ Days, data = E122CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS



#__________________4 CYCLE AP
model <- aov(AP.corr ~ Days, data = E41UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E42UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below  NS


model <- aov(AP.corr ~ Days, data = E41DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E42DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.0424


model <- aov(AP.corr ~ Days, data = E41BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E42BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.0106


model <- aov(AP.corr ~ Days, data = E41CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E42CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS

#__________________12 CYCLE 
model <- aov(AP.corr ~ Days, data = E121UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below P-VALUE=0.0455


model <- aov(AP.corr ~ Days, data = E122UL)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below  NS


model <- aov(AP.corr ~ Days, data = E121DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E122DM)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E121BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E122BF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E121CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS


model <- aov(AP.corr ~ Days, data = E122CF)

shapiro.test(residuals(model)) # normality 

summary(model) # If p < 0.05, proceed below NS




EPreWet4UL<-subset(EPreWet4, Meadow=="UL")
EPreWet4DM<-subset(EPreWet4, Meadow=="DM")
EPreWet4BF<-subset(EPreWet4, Meadow=="BF")
EPreWet4CF<-subset(EPreWet4, Meadow=="CF")

EPreWet12UL<-subset(EPreWet12, Meadow=="UL")
EPreWet12DM<-subset(EPreWet12, Meadow=="DM")
EPreWet12BF<-subset(EPreWet12, Meadow=="BF")
EPreWet12CF<-subset(EPreWet12, Meadow=="CF")

EPostWet4UL<-subset(EPostWet4, Meadow=="UL")
EPostWet4DM<-subset(EPostWet4, Meadow=="DM")
EPostWet4BF<-subset(EPostWet4, Meadow=="BF")
EPostWet4CF<-subset(EPostWet4, Meadow=="CF")

EPostWet12UL<-subset(EPostWet12, Meadow=="UL")
EPostWet12DM<-subset(EPostWet12, Meadow=="DM")
EPostWet12BF<-subset(EPostWet12, Meadow=="BF")
EPostWet12CF<-subset(EPostWet12, Meadow=="CF")

#Normality Testing

shapiro.test(EPreWet4UL$BG.corr)
shapiro.test(EPreWet4DM$BG.corr)
shapiro.test(EPreWet4BF$BG.corr)
shapiro.test(EPreWet4CF$BG.corr)

shapiro.test(EPreWet4UL$NAG.corr)
shapiro.test(EPreWet4DM$NAG.corr)
shapiro.test(EPreWet4BF$NAG.corr)
shapiro.test(EPreWet4CF$NAG.corr)

shapiro.test(EPreWet4UL$AP.corr)
shapiro.test(EPreWet4DM$AP.corr)
shapiro.test(EPreWet4BF$AP.corr)
shapiro.test(EPreWet4CF$AP.corr)

shapiro.test(EPreWet12UL$BG.corr)
shapiro.test(EPreWet12DM$BG.corr)
shapiro.test(EPreWet12BF$BG.corr)
shapiro.test(EPreWet12CF$BG.corr)

shapiro.test(EPreWet12UL$NAG.corr)
shapiro.test(EPreWet12DM$NAG.corr)
shapiro.test(EPreWet12BF$NAG.corr)
shapiro.test(EPreWet12CF$NAG.corr)

shapiro.test(EPreWet12UL$AP.corr)
shapiro.test(EPreWet12DM$AP.corr)
shapiro.test(EPreWet12BF$AP.corr)
shapiro.test(EPreWet12CF$AP.corr)

