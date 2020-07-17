library(purrr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stargazer)
library("car")

setwd("C:/Users/shrad/Desktop/BAIS/QMB 6304/Project/R")
d <- read.csv("HomicideDataset.csv")

d$GunsPerCap                 <- d$NumGuns / d$Population
d$HomicidesPerCap            <- d$NumHomicides / d$Population
d$DrugUsersPerCap            <- d$NumDrugUsers / d$Population
d$AlcoholAbuserPerCap        <- d$NumAlcoholAbusers / d$Population
d$PolicePerCap               <- d$NumLawEnforcementEmployees /  d$Population

df2017 <- subset(d, d$Year == "2017")
df2012 <- subset(d, d$Year == "2012")
df2010 <- subset(d, d$Year == "2010")

#Model 1
m1_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap , data = df2010)
m1_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap , data = df2012)
m1_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap , data = df2017)

stargazer(m1_2010, m1_2012, m1_2017, type="text")

# Multivariate normality
shapiro.test(m1_2010$res)
shapiro.test(m1_2012$res)
shapiro.test(m1_2017$res)

# Homoskedasticity
bartlett.test(list(m1_2010$res, m1_2010$fit))
bartlett.test(list(m1_2012$res, m1_2012$fit))
bartlett.test(list(m1_2017$res, m1_2017$fit))

# Multicollinearity
vif(m1_2010)
vif(m1_2012)
vif(m1_2017)

plot(m1_2010$res ~ m1_2010$fit)
plot(m1_2012$res ~ m1_2012$fit)
plot(m1_2017$res ~ m1_2017$fit)

#Model 2
m2_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap , data = df2010 )
m2_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap , data = df2012 )
m2_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap , data = df2017 )
stargazer(m2_2010, m2_2012, m2_2017, type="text")

# Multivariate normality
shapiro.test(m2_2010$res)
shapiro.test(m2_2012$res)
shapiro.test(m2_2017$res)

# Homoskedasticity
bartlett.test(list(m2_2010$res, m2_2010$fit))
bartlett.test(list(m2_2012$res, m2_2012$fit))
bartlett.test(list(m2_2017$res, m2_2017$fit))

# Multicollinearity
vif(m2_2010)
vif(m2_2012)
vif(m2_2017)

plot(m2_2010$res ~ m2_2010$fit)
plot(m2_2012$res ~ m2_2012$fit)
plot(m2_2017$res ~ m2_2017$fit)

#Model 3
m3_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap*PolicePerCap , data = df2010 )
m3_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap*PolicePerCap , data = df2012 )
m3_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap*PolicePerCap , data = df2017 )

m3_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita +
                + DrugUsersPerCap*GunsPerCap, data = df2010 )
m3_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita +
                + DrugUsersPerCap*GunsPerCap, data = df2012 )
m3_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita +
                + DrugUsersPerCap*GunsPerCap, data = df2017 )

stargazer(m3_2010, m3_2012, m3_2017, type="text")

m4_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita 
              , data = df2010 )
m4_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita 
              , data = df2012 )
m4_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita 
              , data = df2017 )

# Multicollinearity
vif(m4_2010)
vif(m4_2012)
vif(m4_2017)

# Multivariate normality
shapiro.test(m3_2010$res)
shapiro.test(m3_2012$res)
shapiro.test(m3_2017$res)

# Homoskedasticity
bartlett.test(list(m3_2010$res, m3_2010$fit))
bartlett.test(list(m3_2012$res, m3_2012$fit))
bartlett.test(list(m3_2017$res, m3_2017$fit))

# Multicollinearity
vif(m3_2010)
vif(m3_2012)
vif(m3_2017)

plot(m3_2010$res ~ m3_2010$fit)
plot(m3_2012$res ~ m3_2012$fit)
plot(m3_2017$res ~ m3_2017$fit)

