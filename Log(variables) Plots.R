# log of HEALTH.EXP, HOSPBEDS, DEFECATION, CPI, UNDERNOURISH, PHYS, nurse, and pharma, and GDP
head(total_data)
new_totdata = total_data

hist(total_data$ddd)
hist(log(total_data$GDP))
nrow(total_data[total_data$ddd==0,])/nrow(total_data) # 22.5% of zeros.

new_totdata$HEALTH.EXP = log(new_totdata$HEALTH.EXP)
new_totdata$HOSPBEDS  = log(new_totdata$HOSPBEDS)
new_totdata$DEFECATION  = log(new_totdata$DEFECATION + 0.1)
new_totdata$CPI  = log(new_totdata$CPI)
new_totdata$UNDERNOURISH  = log(new_totdata$UNDERNOURISH)
new_totdata$phys  = log(new_totdata$phys)
new_totdata$nurse  = log(new_totdata$nurse)
new_totdata$pharma  = log(new_totdata$pharma)
new_totdata$GDP = log(new_totdata$GDP)

# plot against log(ddd)

# function to calculate mean trend
meancal.fun = function(var,cls,data){
  data$cls = floor(var/cls)*cls
  meandata = tapply(data$log, data$cls, mean)
  newdata = as.data.frame(meandata)
  newdata$var = as.numeric(rownames(newdata))
  return(newdata)
}



dev.off()
#LMIC with log()
par(mfrow=c(3,4))

new_totdata

lowdat.log = new_totdata[new_totdata$lmic==1,]
plot(log~HEALTH.EXP,data=lowdat.log)
plot(log~HEALTH.EXP,data=lowdat.log[lowdat.log$log>0,])
healthexp.mean = meancal.fun(lowdat.log$HEALTH.EXP, 100, lowdat.log)
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)
plot(log~HOSPBEDS, data=lowdat.log)
hospbeds.mean = meancal.fun(lowdat.log$HOSPBEDS, 3, lowdat.log)
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)
plot(log~OVER65, data=lowdat.log)
over65.mean = meancal.fun(lowdat.log$OVER65, 3, lowdat.log)
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)
plot(log~DEFECATION, data=lowdat.log)
defecation.mean = meancal.fun(lowdat.log$DEFECATION, 5, lowdat.log)
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)
plot(log~SANITATION, data=lowdat.log)
sanitation.mean = meancal.fun(lowdat.log$SANITATION, 10, lowdat.log)
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
plot(log~CPI, data=lowdat.log)
cpi.mean = meancal.fun(lowdat.log$CPI, 10, lowdat.log)
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)
plot(log~EDU, data=lowdat.log)
edu.mean = meancal.fun(lowdat.log$EDU, 10, lowdat.log)
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
plot(log~UNDERNOURISH, data=lowdat.log)
und.mean = meancal.fun(lowdat.log$UNDERNOURISH, 3, lowdat.log)
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)
plot(log~phys, data=lowdat.log)
phys.mean = meancal.fun(lowdat.log$phys, 1, lowdat.log)
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)
plot(log~nurse, data=lowdat.log)
nurse.mean = meancal.fun(lowdat.log$nurse, 1, lowdat.log)
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)
plot(log~pharma, data=lowdat.log)
pharma.mean = meancal.fun(lowdat.log$pharma, 0.2, lowdat.log)
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)

dev.off()
#HIC with log()
par(mfrow=c(3,4))

highdat.log = new_totdata[new_totdata$lmic==0,]
plot(log~HEALTH.EXP,data=highdat.log)
healthexp.mean = meancal.fun(highdat.log$HEALTH.EXP, 100, highdat.log)
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)
plot(log~HOSPBEDS, data=highdat.log)
hospbeds.mean = meancal.fun(highdat.log$HOSPBEDS, 3, highdat.log)
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)
plot(log~OVER65, data=highdat.log)
over65.mean = meancal.fun(highdat.log$OVER65, 3, highdat.log)
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)
plot(log~DEFECATION, data=highdat.log)
defecation.mean = meancal.fun(highdat.log$DEFECATION, 5, highdat.log)
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)
plot(log~SANITATION, data=highdat.log)
sanitation.mean = meancal.fun(highdat.log$SANITATION, 10, highdat.log)
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
plot(log~CPI, data=highdat.log)
cpi.mean = meancal.fun(highdat.log$CPI, 10, highdat.log)
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)
plot(log~EDU, data=highdat.log)
edu.mean = meancal.fun(highdat.log$EDU, 10, highdat.log)
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
plot(log~UNDERNOURISH, data=highdat.log)
und.mean = meancal.fun(highdat.log$UNDERNOURISH, 3, highdat.log)
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)
plot(log~phys, data=highdat.log)
phys.mean = meancal.fun(highdat.log$phys, 1, highdat.log)
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)
plot(log~nurse, data=highdat.log)
nurse.mean = meancal.fun(highdat.log$nurse, 1, highdat.log)
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)
plot(log~pharma, data=highdat.log)
pharma.mean = meancal.fun(highdat.log$pharma, 0.2, highdat.log)
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)

dev.off()
#new_totdata 
par(mfrow=c(3,4))

plot(log~HEALTH.EXP,data=new_totdata)
healthexp.mean = meancal.fun(new_totdata$HEALTH.EXP, 100, new_totdata)
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)
plot(log~HOSPBEDS, data=new_totdata)
hospbeds.mean = meancal.fun(new_totdata$HOSPBEDS, 3, new_totdata)
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)
plot(log~OVER65, data=new_totdata)
over65.mean = meancal.fun(new_totdata$OVER65, 3, new_totdata)
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)
plot(log~DEFECATION, data=new_totdata)
defecation.mean = meancal.fun(new_totdata$DEFECATION, 5, new_totdata)
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)
plot(log~SANITATION, data=new_totdata)
sanitation.mean = meancal.fun(new_totdata$SANITATION, 10, new_totdata)
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
plot(log~CPI, data=new_totdata)
cpi.mean = meancal.fun(new_totdata$CPI, 10, new_totdata)
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)
plot(log~EDU, data=new_totdata)
edu.mean = meancal.fun(new_totdata$EDU, 10, new_totdata)
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
plot(log~UNDERNOURISH, data=new_totdata)
und.mean = meancal.fun(new_totdata$UNDERNOURISH, 3, new_totdata)
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)
plot(log~phys, data=new_totdata)
phys.mean = meancal.fun(new_totdata$phys, 1, new_totdata)
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)
plot(log~nurse, data=new_totdata)
nurse.mean = meancal.fun(new_totdata$nurse, 1, new_totdata)
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)
plot(log~pharma, data=new_totdata)
pharma.mean = meancal.fun(new_totdata$pharma, 0.2, new_totdata)
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)



plot(log~HEALTH.EXP,data=total_data)
healthexp.mean = meancal.fun(total_data$HEALTH.EXP, 100, total_data)
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)
plot(log~HOSPBEDS, data=total_data)
hospbeds.mean = meancal.fun(total_data$HOSPBEDS, 3, total_data)
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)
plot(log~OVER65, data=total_data)
over65.mean = meancal.fun(total_data$OVER65, 3, total_data)
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)
plot(log~DEFECATION, data=total_data)
defecation.mean = meancal.fun(total_data$DEFECATION, 5, total_data)
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)
plot(log~SANITATION, data=total_data)
sanitation.mean = meancal.fun(total_data$SANITATION, 10, total_data)
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
plot(log~CPI, data=total_data)
cpi.mean = meancal.fun(total_data$CPI, 10, total_data)
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)
plot(log~EDU, data=total_data)
edu.mean = meancal.fun(total_data$EDU, 10, total_data)
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
plot(log~UNDERNOURISH, data=total_data)
und.mean = meancal.fun(total_data$UNDERNOURISH, 3, total_data)
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)
plot(log~phys, data=total_data)
phys.mean = meancal.fun(total_data$phys, 1, total_data)
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)
plot(log~nurse, data=total_data)
nurse.mean = meancal.fun(total_data$nurse, 1, total_data)
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)
plot(log~pharma, data=total_data)
pharma.mean = meancal.fun(total_data$pharma, 0.2, total_data)
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)



# PCA with log data

# Principle component analysis.
dev.off()
pca = prcomp(na.omit(new_totdata[,6:17]),center=TRUE,scale=TRUE)
biplot(pca, xlim=c(-0.2, 0.25), ylim=c(-0.2, 0.25), cex=0.5)
summary(pca)
pca
