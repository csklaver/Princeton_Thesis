par(mfrow=c(3,4))
highdat.log = new_totdata[new_totdata$lmic==0,]
plot(log~HEALTH.EXP,data=highdat.log[highdat.log$log>0,])
healthexp.mean = meancal.fun(highdat.log[highdat.log$log>0,]$HEALTH.EXP, 1, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)

plot(log~HOSPBEDS,data=highdat.log[highdat.log$log>0,])
hospbeds.mean = meancal.fun(highdat.log[highdat.log$log>0,]$HOSPBEDS, 0.6, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)

plot(log~OVER65,data=highdat.log[highdat.log$log>0,])
over65.mean = meancal.fun(highdat.log[highdat.log$log>0,]$OVER65, 3, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)


plot(log~SANITATION,data=highdat.log[highdat.log$log>0,])
sanitation.mean = meancal.fun(highdat.log[highdat.log$log>0,]$SANITATION, 5, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)

plot(log~CPI,data=highdat.log[highdat.log$log>0,])
cpi.mean = meancal.fun(highdat.log[highdat.log$log>0,]$CPI, 0.5, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)

plot(log~EDU,data=highdat.log[highdat.log$log>0,])
edu.mean = meancal.fun(highdat.log[highdat.log$log>0,]$EDU, 10, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)

plot(log~UNDERNOURISH,data=highdat.log[highdat.log$log>0,])
und.mean = meancal.fun(highdat.log[highdat.log$log>0,]$UNDERNOURISH, 1, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)

plot(log~phys,data=highdat.log[highdat.log$log>0,])
phys.mean = meancal.fun(highdat.log[highdat.log$log>0,]$phys, 0.3, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)

plot(log~nurse, data=highdat.log[highdat.log$log>0,])
nurse.mean = meancal.fun(highdat.log[highdat.log$log>0,]$nurse, 0.4, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)

plot(log~pharma,data=highdat.log[highdat.log$log>0,])
pharma.mean = meancal.fun(highdat.log[highdat.log$log>0,]$pharma, 0.5, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)


dev.off()


# plots with lines by year
highdat.log.2000 = highdat.log[highdat.log$year==2000,]
highdat.log.2005 = highdat.log[highdat.log$year==2005,]
highdat.log.2010 = highdat.log[highdat.log$year==2010,]
highdat.log.2015 = highdat.log[highdat.log$year==2015,]

# SANITATION
plot(log~SANITATION,data=highdat.log[highdat.log$log>0,], main = "Sanitation vs. DDD")
sanitation.mean = meancal.fun(highdat.log.2000[highdat.log.2000$log>0,]$SANITATION, 10, 
                              highdat.log.2000[highdat.log.2000$log>0,])
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
sanitation.mean = meancal.fun(highdat.log.2005[highdat.log.2005$log>0,]$SANITATION, 10, 
                              highdat.log.2005[highdat.log.2005$log>0,])
points(meandata ~ var, data=sanitation.mean, col='red', typ='l', lwd=2)
sanitation.mean = meancal.fun(highdat.log.2010[highdat.log.2010$log>0,]$SANITATION, 10, 
                              highdat.log.2010[highdat.log.2010$log>0,])
points(meandata ~ var, data=sanitation.mean, col='purple', typ='l', lwd=2)
sanitation.mean = meancal.fun(highdat.log.2015[highdat.log.2015$log>0,]$SANITATION, 10, 
                              highdat.log.2015[highdat.log.2015$log>0,])
points(meandata ~ var, data=sanitation.mean, col='pink', typ='l', lwd=2)
legend("topright",legend=c("2000", "2005", "2010", "2015"),
       col=c("blue", "red", "purple", "pink"), lty=1, cex=0.8)

# EDU
plot(log~EDU,data=highdat.log[highdat.log$log>0,], main = "Education vs. DDD")
edu.mean = meancal.fun(highdat.log[highdat.log$log>0,]$EDU, 10, highdat.log[highdat.log$log>0,])
points(meandata ~ var, data=edu.mean, col='green', typ='l', lwd=2)
edu.mean = meancal.fun(highdat.log.2000[highdat.log.2000$log>0,]$EDU, 10, highdat.log.2000[highdat.log.2000$log>0,])
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
edu.mean = meancal.fun(highdat.log.2005[highdat.log.2005$log>0,]$EDU, 10, highdat.log.2005[highdat.log.2005$log>0,])
points(meandata ~ var, data=edu.mean, col='red', typ='l', lwd=2)
edu.mean = meancal.fun(highdat.log.2010[highdat.log.2010$log>0,]$EDU, 10, highdat.log.2010[highdat.log.2010$log>0,])
points(meandata ~ var, data=edu.mean, col='purple', typ='l', lwd=2)
edu.mean = meancal.fun(highdat.log.2015[highdat.log.2015$log>0,]$EDU, 10, highdat.log.2015[highdat.log.2015$log>0,])
points(meandata ~ var, data=edu.mean, col='pink', typ='l', lwd=2)
legend("topright",legend=c("2000", "2005", "2010", "2015", "total"),
       col=c("blue", "red", "purple", "pink", "green"), lty=1, cex=0.8)
