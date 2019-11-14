## by.class regressions

##LMIC
#separate the data
lmic.byclass = by.class[by.class$lmic==1 & !is.na(by.class$lmic),]

# standardize data
lmic.byclass$SANITATION = std.fun(lmic.byclass$SANITATION)
lmic.byclass$EDU = std.fun(lmic.byclass$EDU)
lmic.byclass$DEFECATION = std.fun(lmic.byclass$DEFECATION)
lmic.byclass$HOSPBEDS = std.fun(lmic.byclass$HOSPBEDS)
lmic.byclass$HEALTH.EXP = std.fun(lmic.byclass$HEALTH.EXP)
lmic.byclass$phys = std.fun(lmic.byclass$phys)
lmic.byclass$nurse = std.fun(lmic.byclass$nurse)
lmic.byclass$pharma = std.fun(lmic.byclass$pharma)
lmic.byclass$CPI = std.fun(lmic.byclass$CPI)
lmic.byclass$UNDERNOURISH = std.fun(lmic.byclass$UNDERNOURISH)
lmic.byclass$OVER65 = std.fun(lmic.byclass$OVER65)

# regressions 
# switched SANITATION for CPI just curious
l.lm.a2 = lm(a2 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)
l.lm.a4 = lm(a4 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)
l.lm.a6 = lm(a6 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)
l.lm.a10 = lm(a10 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)
l.lm.a12 = lm(a12 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)
l.lm.a17 = lm(a17 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)
l.lm.a18 = lm(a18 ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=lmic.byclass)

# stargazer
library(stargazer)

stargazer(l.lm.a2,l.lm.a4,l.lm.a6,l.lm.a10,l.lm.a12,l.lm.a17,l.lm.a18, type="text", 
          title = "Regression Results",
          flip = TRUE, out="l.reg.class.htm")



##HIC
#separate the data
hic.byclass = by.class[by.class$lmic==0 & !is.na(by.class$lmic),]

# standardize data
hic.byclass$SANITATION = std.fun(hic.byclass$SANITATION)
hic.byclass$EDU = std.fun(hic.byclass$EDU)
hic.byclass$DEFECATION = std.fun(hic.byclass$DEFECATION)
hic.byclass$HOSPBEDS = std.fun(hic.byclass$HOSPBEDS)
hic.byclass$HEALTH.EXP = std.fun(hic.byclass$HEALTH.EXP)
hic.byclass$phys = std.fun(hic.byclass$phys)
hic.byclass$nurse = std.fun(hic.byclass$nurse)
hic.byclass$pharma = std.fun(hic.byclass$pharma)
hic.byclass$CPI = std.fun(hic.byclass$CPI)
hic.byclass$UNDERNOURISH = std.fun(hic.byclass$UNDERNOURISH)
hic.byclass$OVER65 = std.fun(hic.byclass$OVER65)

# regressions 
h.lm.a2 = lm(a2 ~ SANITATION+ HEALTH.EXP + pharma  + EDU, data=hic.byclass)
h.lm.a4 = lm(a4 ~ SANITATION + HEALTH.EXP + pharma + EDU, data=hic.byclass)
h.lm.a6 = lm(a6 ~ SANITATION + HEALTH.EXP + pharma + EDU, data=hic.byclass)
h.lm.a10 = lm(a10 ~ SANITATION + HEALTH.EXP + pharma + EDU, data=hic.byclass)
h.lm.a12 = lm(a12 ~ SANITATION + HEALTH.EXP + pharma + EDU, data=hic.byclass)
h.lm.a17 = lm(a17 ~ SANITATION + HEALTH.EXP + pharma + EDU, data=hic.byclass)
h.lm.a18 = lm(a18 ~ SANITATION + HEALTH.EXP + pharma + EDU, data=hic.byclass)

# stargazer
library(stargazer)

stargazer(h.lm.a2,h.lm.a4,h.lm.a6,h.lm.a10,h.lm.a12,h.lm.a17,h.lm.a18, type="text", 
          title = "HIC Antibiotics Consumption by Class Regression Results",
          flip = TRUE, out="h.reg.class.htm")


# outputting p-values
t=summary(h.lm.a18)
h=t$coefficients
h.one=h[,4]
h.two=h[,4]
h.three=h[,4]
h.four=h[,4]
h.five=h[,4]
h.six=h[,4]
h.seven=h[,4]
h.cl=cbind(h.one,h.two,h.three,h.four,h.five,h.six,h.seven)
stargazer(h.cl, out="h.cl.htm")
