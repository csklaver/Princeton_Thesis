par(mfrow=c(3,4))
lowdat.log = new_totdata[new_totdata$lmic==1,]
plot(log~HEALTH.EXP,data=lowdat.log[lowdat.log$log>0,])
healthexp.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$HEALTH.EXP, 1, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=healthexp.mean, col='blue', typ='l', lwd=2)

plot(log~HOSPBEDS,data=lowdat.log[lowdat.log$log>0,])
hospbeds.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$HOSPBEDS, 1, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=hospbeds.mean, col='blue', typ='l', lwd=2)

plot(log~OVER65,data=lowdat.log[lowdat.log$log>0,])
over65.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$OVER65, 3, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=over65.mean, col='blue', typ='l', lwd=2)


plot(log~DEFECATION,data=lowdat.log[lowdat.log$log>0,])
defecation.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$DEFECATION, 1, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=defecation.mean, col='blue', typ='l', lwd=2)

plot(log~SANITATION,data=lowdat.log[lowdat.log$log>0,])
sanitation.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$SANITATION, 10, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=sanitation.mean, col='blue', typ='l', lwd=2)

plot(log~CPI,data=lowdat.log[lowdat.log$log>0,])
cpi.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$CPI, 0.5, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=cpi.mean, col='blue', typ='l', lwd=2)

plot(log~EDU,data=lowdat.log[lowdat.log$log>0,])
edu.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$EDU, 10, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=edu.mean, col='blue', typ='l', lwd=2)

plot(log~UNDERNOURISH,data=lowdat.log[lowdat.log$log>0,])
und.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$UNDERNOURISH, 1, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=und.mean, col='blue', typ='l', lwd=2)

plot(log~phys,data=lowdat.log[lowdat.log$log>0,])
phys.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$phys, 1, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=phys.mean, col='blue', typ='l', lwd=2)

plot(log~nurse, data=lowdat.log[lowdat.log$log>0,])
nurse.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$nurse, 2, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=nurse.mean, col='blue', typ='l', lwd=2)

plot(log~pharma,data=lowdat.log[lowdat.log$log>0,])
pharma.mean = meancal.fun(lowdat.log[lowdat.log$log>0,]$pharma, 2, lowdat.log[lowdat.log$log>0,])
points(meandata ~ var, data=pharma.mean, col='blue', typ='l', lwd=2)






# LMIC run pca
dev.off()
pca.low = prcomp(na.omit(lowdat.log[,6:17]),center=TRUE,scale=TRUE)
biplot(pca, xlim=c(-0.3, 0.3), ylim=c(-0.2, 0.2), 
       cex=0.5)
summary(pca.low)
pca.low

# export the loadings
library(xlsx)
write.xlsx(pca.low$rotation, "pca.low2.xlsx")

# read in PC1 and PC2 loadings
pca.xy.low2 = read.csv("pca.low2.csv")

## making color-coded plot
library(ggplot2)
p =  ggplot(pca.xy.low2, aes(x=PC1, y=PC2)) + geom_point(aes(colour=group), size=1)+
  geom_text(aes(label=variable),hjust=.65, vjust=-1, size = 2)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2, color=group), 
               arrow=arrow(length=unit(0.2,"cm")), alpha=1) + 
                 ggtitle("LMIC PCA Plot") + labs(color = "Variable Group")
p


# HIC run pca XDEFECATION
pca.high = prcomp(na.omit(highdat.log[,c(6:8, 10:17)]), center=T, scale=T)
biplot(pca.high, xlim=c(-0.3, 0.3), ylim=c(-0.2, 0.2), cex=0.5)
pca.high
summary(pca.high)

# export the loadings
library(xlsx)
write.xlsx(pca.high$rotation, "pca.high2.xlsx")

# read in PC1 and PC2 loadings
pca.xy.high2 = read.csv("pca.high2.csv")

## making color-coded plot
library(ggplot2)
ph =  ggplot(pca.xy.high2, aes(x=PC1, y=PC2)) + geom_point(aes(colour=group), size=1, )+
  geom_text(aes(label=variable),hjust=.65, vjust=-1, size = 2)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2, color=group), arrow=arrow(length=unit(0.2,"cm")), 
               alpha=1) + ggtitle("HIC PCA Plot") + labs(color = "Variable Group")
ph
