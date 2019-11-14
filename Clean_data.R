# making clean data set for Wenying
# 1) logged antibiotic variables: 
# taking out zero's
clean_data = total_data[,c(1,2,3,7,10:20)]
clean_data2 = total_data[,c(1:4,6:17)]

# take the log-transformations of non-zero's
clean_data = clean_data[clean_data$ddd != 0, ]
clean_data$ddd = log(clean_data$ddd)
clean_data2 = clean_data2[clean_data2$ddd != 0, ]
clean_data2$ddd = log(clean_data2$ddd)

# log of HEALTH.EXP, HOSPBEDS, DEFECATION, CPI, UNDERNOURISH, PHYS, nurse, and pharma, GDP
clean_data$HEALTH.EXP = log(clean_data$HEALTH.EXP)
clean_data$CPI = log(clean_data$CPI)
clean_data$UNDERNOURISH = log(clean_data$UNDERNOURISH)

clean_data2$HEALTH.EXP = log(clean_data2$HEALTH.EXP)
clean_data2$CPI = log(clean_data2$CPI)
clean_data2$UNDERNOURISH = log(clean_data2$UNDERNOURISH)
clean_data2$GDP = log(clean_data2$GDP)

# add 1 to make non-negative HOSPBEDS, DEFECATION, phys, nurse, pharma
clean_data$pharma = clean_data$pharma + 1
clean_data$pharma = log(clean_data$pharma)

clean_data$HOSPBEDS = clean_data$HOSPBEDS + 1
clean_data$HOSPBEDS = log(clean_data$HOSPBEDS)

clean_data$DEFECATION = clean_data$DEFECATION + 1
clean_data$DEFECATION = log(clean_data$DEFECATION)

clean_data$phys = clean_data$phys + 1
clean_data$phys = log(clean_data$phys)

clean_data$nurse = clean_data$nurse + 1
clean_data$nurse = log(clean_data$nurse)

clean_data$pharma = clean_data$pharma + 1
clean_data$pharma = log(clean_data$pharma)

#again for clean_data2
clean_data2$HOSPBEDS = clean_data2$HOSPBEDS + 1
clean_data2$HOSPBEDS = log(clean_data2$HOSPBEDS)

clean_data2$DEFECATION = clean_data2$DEFECATION + 1
clean_data2$DEFECATION = log(clean_data2$DEFECATION)

clean_data2$phys = clean_data2$phys + 1
clean_data2$phys = log(clean_data2$phys)

clean_data2$nurse = clean_data2$nurse + 1
clean_data2$nurse = log(clean_data2$nurse)

# exporting data
library(xlsx)
write.xlsx(clean_data, "clean_data.xlsx")

write.csv(clean_data, "clean_data.csv")



# antibiotics by class clean data set
by.class = total_data[,c(1,2,7,10:20)]

# a2, a3, a4, a6, a7, a8, a10, a11, a12, a14, a16, a17
by.class$a2 = log(add_class$tot.a2)
#by.class$a3 = log(add_class$tot.a3)
by.class$a4 = log(add_class$tot.a4)
by.class$a6 = log(add_class$tot.a6)
#by.class$a7 = log(add_class$tot.a7)
#by.class$a8 = log(add_class$tot.a8)
by.class$a10 = log(add_class$tot.a10)
#by.class$a11= log(add_class$tot.a11)
by.class$a12 = log(add_class$tot.a12)
#by.class$a14 = log(add_class$tot.a14)
#by.class$a16 = log(add_class$tot.a16)
by.class$a17 = log(add_class$tot.a17)
by.class$a18 = log(add_class$tot.a18)

by.class$HEALTH.EXP = log(by.class$HEALTH.EXP)
by.class$CPI = log(by.class$CPI)
by.class$UNDERNOURISH = log(by.class$UNDERNOURISH)

# add 1 to make non-negative HOSPBEDS, DEFECATION, phys, nurse, pharma
by.class$pharma = by.class$pharma + 1
by.class$pharma = log(by.class$pharma)

by.class$HOSPBEDS = by.class$HOSPBEDS + 1
by.class$HOSPBEDS = log(by.class$HOSPBEDS)

by.class$DEFECATION = by.class$DEFECATION + 1
by.class$DEFECATION = log(by.class$DEFECATION)

by.class$phys = by.class$phys + 1
by.class$phys = log(by.class$phys)

by.class$nurse = by.class$nurse + 1
by.class$nurse = log(by.class$nurse)

# export the by.class
write.csv(by.class, "by.class.csv")








# plots

# plots with lines by year
lowdat.log.2000 = lowdat.log[lowdat.log$year==2000,]
lowdat.log.2005 = lowdat.log[lowdat.log$year==2005,]
lowdat.log.2010 = lowdat.log[lowdat.log$year==2010,]
lowdat.log.2015 = lowdat.log[lowdat.log$year==2015,]

dev.off()
par(mfrow=c(1,2))
# SANITATION
plot(log~SANITATION,data=lowdat.log[lowdat.log$log>0,], main = "LMIC Sanitation vs. DDD",
     ylab="log(ddd)")
sanitation.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$SANITATION, 10, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=sanitation.mean, col='green', typ='l', lwd=2)
sanitation.mean = meancal.fun(lowdat.log.2000[lowdat.log.2000$log>0,]$SANITATION, 10, 
                              lowdat.log.2000[lowdat.log.2000$log>0,])
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
sanitation.mean = meancal.fun(lowdat.log.2005[lowdat.log.2005$log>0,]$SANITATION, 10, 
                              lowdat.log.2005[lowdat.log.2005$log>0,])
points(meandata ~ var, data=sanitation.mean, col='red', typ='l', lwd=2)
sanitation.mean = meancal.fun(lowdat.log.2010[lowdat.log.2010$log>0,]$SANITATION, 10, 
                              lowdat.log.2010[lowdat.log.2010$log>0,])
points(meandata ~ var, data=sanitation.mean, col='purple', typ='l', lwd=2)
sanitation.mean = meancal.fun(lowdat.log.2015[lowdat.log.2015$log>0,]$SANITATION, 10, 
                              lowdat.log.2015[lowdat.log.2015$log>0,])
points(meandata ~ var, data=sanitation.mean, col='pink', typ='l', lwd=2)
legend("topright",legend=c("2000", "2005", "2010", "2015","total"),
       col=c("blue", "red", "purple", "pink","green"), lty=1, cex=0.8)

# EDU
plot(log~EDU,data=lowdat.log[lowdat.log$log>0,], main = "LMIC Education vs. DDD",
     ylab="log(ddd)")
edu.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$EDU, 10, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=edu.mean, col='green', typ='l', lwd=2)
edu.mean = meancal.fun(lowdat.log.2000[lowdat.log.2000$log>0,]$EDU, 10, lowdat.log.2000[lowdat.log.2000$log>0,])
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
edu.mean = meancal.fun(lowdat.log.2005[lowdat.log.2005$log>0,]$EDU, 10, lowdat.log.2005[lowdat.log.2005$log>0,])
points(meandata ~ var, data=edu.mean, col='red', typ='l', lwd=2)
edu.mean = meancal.fun(lowdat.log.2010[lowdat.log.2010$log>0,]$EDU, 10, lowdat.log.2010[lowdat.log.2010$log>0,])
points(meandata ~ var, data=edu.mean, col='purple', typ='l', lwd=2)
edu.mean = meancal.fun(lowdat.log.2015[lowdat.log.2015$log>0,]$EDU, 10, lowdat.log.2015[lowdat.log.2015$log>0,])
points(meandata ~ var, data=edu.mean, col='pink', typ='l', lwd=2)
legend("topright",legend=c("2000", "2005", "2010", "2015","total"),
       col=c("blue", "red", "purple", "pink","green"), lty=1, cex=0.8)      |
