library(purrr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stargazer)

setwd("C:/Users/shrad/Desktop/BAIS/QMB 6304/Project/R")
d <- read.csv("HomicideDataFinal2.csv")

d$GunsPerCap                 <- d$NumGuns / d$Population
d$HomicidesPerCap            <- d$NumHomicides / d$Population
d$DrugUsersPerCap            <- d$NumDrugUsers / d$Population
d$AlcoholAbuserPerCap        <- d$NumAlcoholAbusers / d$Population
d$PolicePerCap               <- d$NumLawEnforcementEmployees /  d$Population



df2017 <- subset(d, d$Year == "2017")
df2012 <- subset(d, d$Year == "2012")
df2010 <- subset(d, d$Year == "2010")

pairs(df2017[3:8], col=df2017$Year)
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07") 

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = d$Year)
}
pairs(d[,3:8], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

pairs(d[3:8], col= d$Year , lower.panel = NULL)
  (d$NumHomicides ~ d$Year)
#Model 1
m1_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap , data = df2010)
m1_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap , data = df2012)
m1_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap , data = df2017)

stargazer(m1_2010, m1_2012, m1_2017, type="text")

#Model 2
m2_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap , data = df2010 )
m2_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap , data = df2012 )
m2_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap  + RealGDPperCapita +
                AlcoholAbuserPerCap , data = df2017 )
stargazer(m2_2010, m2_2012, m2_2017, type="text")

m3_2010 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita +
                + DrugUsersPerCap*GunsPerCap, data = df2010 )
m3_2012 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita +
                + DrugUsersPerCap*GunsPerCap, data = df2012 )
m3_2017 <- lm(HomicidesPerCap ~ GunsPerCap + PolicePerCap + DrugUsersPerCap + RealGDPperCapita +
                + DrugUsersPerCap*GunsPerCap, data = df2017 )

plot(m3_2010)
plot(m3_2012)
plot(m3_2017)
