# Separating high and low income

dat_hc = total_data[total_data$lmic == 0,]
dat_lmic = total_data[total_data$lmic == 1,]



# explore the relationship between ind. and dep. var. to establish your hypotheses (what kind of regression?)
plot(log~HEALTH.EXP,data=dat_lmic)
plot(ddd~HOSPBEDS,data=dat_lmic)
plot(ddd~OVER65,data=dat_lmic)
plot(ddd~DEFECATION,data=dat_lmic)
plot(ddd~SANITATION,data=dat_lmic)
plot(log~GDP, data=dat_hc)



#Wenying's Code
getwd()

dat = read.csv('total_data2.csv',header=TRUE)[,-1]
inddat = total_data[,6:17]

# correlation tests.

sumtab.p = matrix(NA,ncol=ncol(inddat),nrow=ncol(inddat))

for(i in 1:ncol(inddat)){
  x = as.numeric(as.character(inddat[,i]))
  normtest.x = shapiro.test(x)$p
  for(j in 1:ncol(inddat)){
    y = as.numeric(as.character(inddat[,j]))
    normtest.y = shapiro.test(y)$p
    if(normtest.x<0.05|normtest.y<0.05){
      corp = cor.test(x,y,method='kendall')$p.value
    }
    else{
      corp = cor.test(x,y,method='pearson')$p.value
    }
    sumtab.p[i,j] = corp
  }
}

sumtab.p[sumtab.p<0.05] = 'HighCor'








dev.off()

par(mfrow=c(3,4))

hist(total_data$HEALTH.EXP, main = "HEALTH.EXP", breaks = 30)
hist(total_data$HOSPBEDS, main = "HOSPBEDS", breaks = 20)
hist(total_data$OVER65, main = "OVER65", breaks = 20)
hist(total_data$DEFECATION, main = "DEFECATION", breaks = 20)
hist(total_data$SANITATION, main = "SANITATION", breaks = 30)
hist(total_data$CPI, main ="CPI", breaks = 30)
hist(total_data$EDU, main = "EDU", breaks = 30)
hist(total_data$UNDERNOURISH, main = "UNDERNOURISH", breaks = 20)
hist(total_data$phys,main = "phys", breaks = 20)
hist(total_data$nurse, main = "nurse", breaks = 20)
hist(total_data$pharma, main = "pharma", breaks = 30)
hist(as.numeric(total_data$GDP), main = "GDP", breaks = )


par(mfrow=c(3,4))

hist(lowdat$HEALTH.EXP, main = "HEALTH.EXP", breaks = 30)
hist(lowdat$HOSPBEDS, main = "HOSPBEDS", breaks = 20)
hist(lowdat$OVER65, main = "OVER65", breaks = 20)
hist(lowdat$DEFECATION, main = "DEFECATION", breaks = 20)
hist(lowdat$SANITATION, main = "SANITATION", breaks = 30)
hist(lowdat$CPI, main ="CPI", breaks = 30)
hist(lowdat$EDU, main = "EDU", breaks = 30)
hist(lowdat$UNDERNOURISH, main = "UNDERNOURISH", breaks = 20)
hist(lowdat$phys,main = "phys", breaks = 20)
hist(lowdat$nurse, main = "nurse", breaks = 20)
hist(lowdat$pharma, main = "pharma", breaks = 30)

par(mfrow=c(3,4))

hist(highdat$HEALTH.EXP, main = "HEALTH.EXP", breaks = 30)
hist(highdat$HOSPBEDS, main = "HOSPBEDS", breaks = 20)
hist(highdat$OVER65, main = "OVER65", breaks = 20)
hist(highdat$DEFECATION, main = "DEFECATION", breaks = 20)
hist(highdat$SANITATION, main = "SANITATION", breaks = 30)
hist(highdat$CPI, main ="CPI", breaks = 30)
hist(highdat$EDU, main = "EDU", breaks = 30)
hist(highdat$UNDERNOURISH, main = "UNDERNOURISH", breaks = 20)
hist(highdat$phys,main = "phys", breaks = 20)
hist(highdat$nurse, main = "nurse", breaks = 20)
hist(highdat$pharma, main = "pharma", breaks = 30)


#Wen's PCA and MLA code
getwd()
inddat = total_data[,6:17]

# function to calculate mean trend
meancal.fun = function(var,cls,data){
  data$cls = floor(var/cls)*cls
  meandata = tapply(data$log, data$cls, mean)
  newdata = as.data.frame(meandata)
  newdata$var = as.numeric(rownames(newdata))
  return(newdata)
}



dev.off()
#LMIC
par(mfrow=c(3,4))

lowdat = total_data[total_data$lmic==1,]
plot(log~HEALTH.EXP,data=lowdat)
healthexp.mean = meancal.fun(lowdat$HEALTH.EXP, 100, lowdat)
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)
plot(log~HOSPBEDS, data=lowdat)
hospbeds.mean = meancal.fun(lowdat$HOSPBEDS, 3, lowdat)
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)
plot(log~OVER65, data=lowdat)
over65.mean = meancal.fun(lowdat$OVER65, 3, lowdat)
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)
plot(log~DEFECATION, data=lowdat)
defecation.mean = meancal.fun(lowdat$DEFECATION, 5, lowdat)
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)
plot(log~SANITATION, data=lowdat)
sanitation.mean = meancal.fun(lowdat$SANITATION, 10, lowdat)
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
plot(log~CPI, data=lowdat)
cpi.mean = meancal.fun(lowdat$CPI, 10, lowdat)
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)
plot(log~EDU, data=lowdat)
edu.mean = meancal.fun(lowdat$EDU, 10, lowdat)
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
plot(log~UNDERNOURISH, data=lowdat)
und.mean = meancal.fun(lowdat$UNDERNOURISH, 3, lowdat)
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)
plot(log~phys, data=lowdat)
phys.mean = meancal.fun(lowdat$phys, 1, lowdat)
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)
plot(log~nurse, data=lowdat)
nurse.mean = meancal.fun(lowdat$nurse, 1, lowdat)
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)
plot(log~pharma, data=lowdat)
pharma.mean = meancal.fun(lowdat$pharma, 0.2, lowdat)
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)
plot(log~GDP, data=lowdat)
GDP.mean = meancal.fun(as.numeric(lowdat$GDP), 20, lowdat)
points(meandata ~ var, data=GDP.mean, col='blue', typ='l', lwd=2)



# HIC
dev.off()
# HIC
par(mfrow=c(3,4))

highdat = total_data[total_data$lmic==0,]
plot(log~HEALTH.EXP,data=highdat)
healthexp.mean = meancal.fun(highdat$HEALTH.EXP, 100, highdat)
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)
plot(log~HOSPBEDS, data=highdat)
hospbeds.mean = meancal.fun(highdat$HOSPBEDS, 3, highdat)
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)
plot(log~OVER65, data=highdat)
over65.mean = meancal.fun(highdat$OVER65, 3, highdat)
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)
plot(log~DEFECATION, data=highdat)
defecation.mean = meancal.fun(highdat$DEFECATION, 5, highdat)
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)
plot(log~SANITATION, data=highdat)
sanitation.mean = meancal.fun(highdat$SANITATION, 10, highdat)
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)
plot(log~CPI, data=highdat)
cpi.mean = meancal.fun(highdat$CPI, 10, highdat)
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)
plot(log~EDU, data=highdat)
edu.mean = meancal.fun(highdat$EDU, 10, highdat)
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)
plot(log~UNDERNOURISH, data=highdat)
und.mean = meancal.fun(highdat$UNDERNOURISH, 3, highdat)
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)
plot(log~phys, data=highdat)
phys.mean = meancal.fun(highdat$phys, 1, highdat)
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)
plot(log~nurse, data=highdat)
nurse.mean = meancal.fun(highdat$nurse, 1, highdat)
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)
plot(log~pharma, data=highdat)
pharma.mean = meancal.fun(highdat$pharma, 0.2, highdat)
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)

# PCA correlation tests.

sumtab.p = matrix(NA,ncol=ncol(inddat),nrow=ncol(inddat))

for(i in 1:ncol(inddat)){
  x = as.numeric(as.character(inddat[,i]))
  normtest.x = shapiro.test(x)$p
  for(j in 1:ncol(inddat)){
    y = as.numeric(as.character(inddat[,j]))
    normtest.y = shapiro.test(y)$p
    if(normtest.x<0.05|normtest.y<0.05){
      corp = cor.test(x,y,method='kendall')$p.value
    }
    else{
      corp = cor.test(x,y,method='pearson')$p.value
    }
    sumtab.p[i,j] = corp
  }
}

sumtab.p[sumtab.p<0.05] = 'HighCor'

# Principle component analysis.
dev.off()
pca = prcomp(na.omit(inddat[,1:12]),center=TRUE,scale=TRUE)
biplot(pca, xlim=c(-0.2, 0.25), ylim=c(-0.2, 0.25), cex=0.5)
summary(pca)
pca




