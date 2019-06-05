options(install.packages.check.source = "no")
install.packages("lsmeans")
install.packages("multcomp")
install.packages("mvtnorm")

library(psych)

describeBy(ULCum$CO2, ULCum$Treatment, mat=TRUE)


ULCum$Treatment = factor(ULCum$Treatment, 
                        levels=unique(ULCum$Treatment))

levels(ULCum$Treatment)

boxplot(CO2 ~ Treatment,
        data = ULCum,
        ylab="CO2",
        xlab="Treatment")


model <- aov(CO2 ~ Treatment, data = ULCum)
summary(model)

library(lsmeans)

leastsquare = lsmeans(model, "Treatment")

Contrasts = list(ADvsFC         = c(1,  1, 0, -0,  0),
                 ADvsSAT        = c(1, 0, -1, 0,  0),
                 FCvsSAT       = c(0, 1, -1,  0,  0),
                 SATvs4         = c(0, 0,  1,  -1,  0),
                 SATvs12        = c(0,  0,  1, 0,  -1),
                 Fourvs12        = c(0,  0,  0,  1, -1))
                 


contrast(leastsquare, Contrasts, adjust="sidak")

plot(model)

