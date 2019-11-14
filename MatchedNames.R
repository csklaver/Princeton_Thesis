getwd()
setwd("/Users/carolinesklaver/Desktop/Thesis/Modified Data CSV")
# read in the global data
UseDataForCarolineSklaver = read.csv("GlobalConsumptionData.csv")
orig = unique(UseDataForCarolineSklaver$country)
sec = unique(Global.Vac$country)


# Health Expenditure per capita
HEALTH.read = read.csv("nHealth.csv")
HEALTH = HEALTH.read[,c(2,3,4)]

Global.Vac = read.csv("Global.Vac.csv")

hmm = merge(Global.Vac, HEALTH, by=c("country","year"), all = TRUE)



# Hospital beds per 1,000
HOSPBEDS.read = read.csv("nHospBeds.csv")
HOSPBEDS = HOSPBEDS.read[,c(2,3,4)]

toot = merge(hmm, HOSPBEDS, by=c("country","year"), all = TRUE)



# Literacy Rate
LITERACY.read = read.csv("nLiteracy.csv")
LITERACY = LITERACY.read[,c(2,3,4)]
head(LITERACY)

qt = merge(toot, LITERACY, by=c("country","year"), all = TRUE)



# % over 65
OVER65.read = read.csv("n65.csv")
OVER65 = OVER65.read[,c(2,3,4)]

vb = merge(qt, OVER65, by=c("country","year"), all = TRUE)



# % defecation
DEFECATION.read = read.csv("nDefecation.csv")
DEFECATION = DEFECATION.read[,c(2,3,4)]

oki = merge(vb, DEFECATION, by=c("country","year"), all = TRUE)



# % Using Basic Sanitation
SANITATION.read = read.csv("nSanitation.csv")
SANITATION = SANITATION.read[,c(2,3,4)]

welp = merge(oki, SANITATION, by=c("country","year"), all = TRUE)


# population 0-14
YOUNG.read = read.csv("nYoung.csv")
YOUNG = YOUNG.read[,c(2,3,4)]

fug = merge(welp, YOUNG, by=c("country","year"), all = TRUE)



# CPI 
CPI.read = read.csv("nCPI.csv")
CPI = CPI.read[,c(2,3,4)]

jig = merge(fug, CPI, by=c("country","year"), all = TRUE)


#EDU
#lowersecondary.read = read.csv("nLowerSecondary.csv")
#lowersecondary = lowersecondary.read[,c(2,3,4)]

oop = merge(jig, education, by=c("country","year"), all = TRUE)


#UNDERNOURISHMENT
undernourishment.read = read.csv("nUndernourishedMod.csv")
undernourishment = undernourishment.read[,c(2,3,4)]
u
try = merge(oop, undernourishment, by=c("country","year"), all = TRUE)


#PHYSICIANS, NURSES, PHARMA PERSONNEL

phys_nurse_pharma.long = read.csv("phys_nurse_pharma.csv")
phys_nurse_pharma = phys_nurse_pharma.long[,c(2,3,4,5,6)]


antibiotic_consumption = merge(try, phys_nurse_pharma, by=c("country","year"), all = TRUE)

# GDP

GDP.read = read.csv("nGDPMod.csv")
GDPPC = GDP.read[,c(2,3,4)]
names(GDPPC) = c("country", "year", "GDP")

antibiotic_consumption2 = merge(antibiotic_consumption, GDPPC, by=c("country","year"), all = TRUE)



# run correlations
cr <- antibiotic_consumption[,c("HEALTH.EXP", "HOSPBEDS", "LITERACY", "OVER65", "DEFECATION", "SANITATION",
                                 "CPI", "EDU", "UNDERNOURISH", "phys", "nurse", "pharma")]
cr$HEALTH.EXP = as.numeric(cr$HEALTH.EXP)
cr$HOSPBEDS = as.numeric(cr$HOSPBEDS)
cr$LITERACY = as.numeric(cr$LITERACY)
cr$DEFECATION= as.numeric(cr$DEFECATION)
cr$CPI = as.numeric(cr$CPI)
cr$SANITATION= as.numeric(cr$SANITATION)
cr$UNDERNOURISH = as.numeric(cr$UNDERNOURISH)
cr$EDU = as.numeric(cr$EDU)
cr$phys = as.numeric(cr$phys)
cr$nurse = as.numeric(cr$nurse)
cr$pharma = as.numeric(cr$pharma)
correlations = cor(cr, use="complete.obs", method="pearson")
cor.test = cor.test(cr$HEALTH.EXP, cr$HOSPBEDS, method="pearson")

correlations
summary(correlations)

# export correlations
library(xlsx)
write.xlsx(correlations, "variable_correlations2.xlsx")




# make all the ddds above zero
antibiotic_consumption$ddd2 = antibiotic_consumption$ddd + 1

# take the log of the ddds
antibiotic_consumption$log = log(antibiotic_consumption$ddd2)

# hist of log
hist(antibiotic_consumption$log, main="Histogram of Logged DDDs", xlab="DDDs", ylab="Frequency", breaks = 40)





# RUN REGRESSIONS

# first running regression
fixed = lm(log ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
             as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
             as.numeric(CPI) + as.numeric(EDU) + lmic + as.factor(year), 
           data=antibiotic_consumption)


fixed_new = lm(log ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
             as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
             as.numeric(CPI) + as.numeric(EDU) +as.numeric(UNDERNOURISH)+ as.numeric(phys)+
               as.numeric(nurse) + as.numeric(pharma)  + lmic + as.factor(year), 
           data=antibiotic_consumption)

summary(fixed_new)


stargazer(fixed_new,type="text", title = "Antibiotic Consumption Variables 2010-2015 (2)", 
          flip = TRUE, out="models.htm")

summary(fixed_new)




# export data set
library(xlsx)
write.xlsx(antibiotic_consumption, "antibiotic_consumption2.xlsx")










# not helpful right now
library(plm)
fixed.lmic <- plm(log ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
                    as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
                    as.numeric(CPI) + as.numeric(EDU), 
                  data=lmic, index=c("country", "year"), model="within")

fixed = lm(log ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
             as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
             as.numeric(CPI) + as.numeric(EDU) + lmic + country + as.factor(year), 
           data=antibiotic_consumption)

class(antibiotic_consumption$HOSPBEDS)
fixed = plm(logged ~ HEALTH.EXP + HOSPBEDS + LITERACY + OVER65 + DEFECATION + SANITATION + 
              UNDER14 + CPI + EDU, data=lmic, index=c("country", "year"), model="within")

fixed = plm(logged ~ HEALTH.EXP + HOSPBEDS + LITERACY + OVER65 + DEFECATION + SANITATION + 
              UNDER14 + CPI + EDU, data=woo, index=c("country", "year"), model="within")

total = lm( log ~ HEALTH.EXP + HOSPBEDS + LITERACY + OVER65 + 
     DEFECATION + SANITATION + CPI + EDU + lmic + as.factor(year), 
   data = antibiotic_consumption)

head(antibiotic_consumption)
head(hic)
head(lmic)


# separate LMICs and HICs
hic = antibiotic_consumption[antibiotic_consumption$lmic == 0,]
lmic = antibiotic_consumption[antibiotic_consumption$lmic == 1,]


# removing NAs from country and year 
lmic <- lmic[!is.na(lmic$country),]
lmic <- lmic[!is.na(lmic$year),]
hic = hic[!is.na(hic$country),]
hic = hic[!is.na(hic$year),]



