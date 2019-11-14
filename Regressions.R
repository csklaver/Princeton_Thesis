dat = read.csv('clean_data.csv',header=TRUE)[,-1]
hist(as.numeric(as.character(dat$ddd)))
hist(as.numeric(as.character(dat$HEALTH.EXP)))
hist(as.numeric(as.character(dat$OVER65))) # bimodal
hist(as.numeric(as.character(dat$SANITATION))) # heavily left-skewed
hist(as.numeric(as.character(dat$CPI)))
hist(as.numeric(as.character(dat$EDU)))
hist(as.numeric(as.character(dat$UNDERNOURISH))) # lots of small values.

# log(var+1)
hist(as.numeric(as.character(dat$HOSPBEDS)))
hist(as.numeric(as.character(dat$DEFECATION))) # lots of small values.
hist(as.numeric(as.character(dat$phys)))
hist(as.numeric(as.character(dat$nurse)))
hist(as.numeric(as.character(dat$pharma)))

# get the data into right format.
dat$ddd = as.numeric(as.character(dat$ddd))
dat$HEALTH.EXP = as.numeric(as.character(dat$HEALTH.EXP))
dat$OVER65 = as.numeric(as.character(dat$OVER65)) # bimodal
dat$SANITATION = as.numeric(as.character(dat$SANITATION)) # heavily left-skewed
dat$CPI = as.numeric(as.character(dat$CPI))
dat$EDU = as.numeric(as.character(dat$EDU))
dat$UNDERNOURISH = as.numeric(as.character(dat$UNDERNOURISH)) # lots of small values.
dat$HOSPBEDS = as.numeric(as.character(dat$HOSPBEDS))
dat$DEFECATION = as.numeric(as.character(dat$DEFECATION)) # lots of small values.
dat$phys = as.numeric(as.character(dat$phys))
dat$nurse = as.numeric(as.character(dat$nurse))
dat$pharma = as.numeric(as.character(dat$pharma))
dat$lmic = as.numeric(as.character(dat$lmic))

# function to standardize data.
std.fun = function(x){
  meanval = mean(x, na.rm = TRUE)
  sdval = sd(x, na.rm = TRUE)
  newx = (x-meanval)/sdval
  return(newx)
}

### regressions

# low-income
# pca after log
ldat = dat[dat$lmic==1 & !is.na(dat$lmic),]
l.pca = prcomp(na.omit(ldat[,5:(5+10)]),center=TRUE,scale=TRUE)
biplot(l.pca)
l.pca

clean_ldat = clean_data2[clean_data2$lmic==1 & !is.na(clean_data2$lmic),]
clean.pca.l = prcomp(na.omit(clean_ldat[,5:(5+11)]),center=TRUE,scale=TRUE)
biplot(clean.pca.l)

#export the loadings
library(xlsx)
write.xlsx(clean.pca.l$rotation, "clean.pca.l.xlsx")

#import with group
pca.l = read.csv("pca.low2.csv")
pca.l$group = as.factor(pca.l$group)

pca.l.2 = read.csv("clean.pca.csv")
pca.l.2$group = as.factor(pca.l.2$group)

##ggplot of PCA
library(ggplot2)
p2 = ggplot(pca.l, aes(x=PC1, y=PC2)) + geom_point(size=1)+
  geom_text(aes(label=variable),hjust=0.5, vjust=2.5, size = 1.6)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2, color=group), 
               arrow=arrow(length=unit(0.2,"cm")), alpha=1, size = 0.8) + 
  ggtitle("LMIC PCA Plot") + labs(color = "Group") + theme_bw()+
  theme(legend.position=c(0.1, 0.2), plot.title = element_text(hjust = 0.5))
ggsave("LMIC_pca3.png",width=5.5, height=5, unit="in", dpi=300)
p2
##LMIC PCA WITH GDP
library(ggplot2)
pl2 = ggplot(pca.l.2, aes(x=PC1, y=PC2)) + geom_point(size=1)+
  geom_text(aes(label=variable),hjust=0.5, vjust=2.5, size = 1.6)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2, color=group), 
               arrow=arrow(length=unit(0.2,"cm")), alpha=1, size = 0.8) + 
  ggtitle("LMIC PCA Plot") + labs(color = "Group") + theme_bw()+
  theme(legend.position=c(0.1, 0.2), plot.title = element_text(hjust = 0.5))
ggsave("LMIC_pca3.png",width=5.5, height=5, unit="in", dpi=300)
pl2


# standardize data
ldat$SANITATION = std.fun(ldat$SANITATION)
ldat$EDU = std.fun(ldat$EDU)
ldat$DEFECATION = std.fun(ldat$DEFECATION)
ldat$HOSPBEDS = std.fun(ldat$HOSPBEDS)
ldat$HEALTH.EXP = std.fun(ldat$HEALTH.EXP)
ldat$phys = std.fun(ldat$phys)
ldat$nurse = std.fun(ldat$nurse)
ldat$pharma = std.fun(ldat$pharma)
ldat$CPI = std.fun(ldat$CPI)
ldat$UNDERNOURISH = std.fun(ldat$UNDERNOURISH)
ldat$OVER65 = std.fun(ldat$OVER65)
#clean_ldat standardized
clean_ldat$SANITATION = std.fun(clean_ldat$SANITATION)
clean_ldat$EDU = std.fun(clean_ldat$EDU)
clean_ldat$DEFECATION = std.fun(clean_ldat$DEFECATION)
clean_ldat$HOSPBEDS = std.fun(clean_ldat$HOSPBEDS)
clean_ldat$HEALTH.EXP = std.fun(clean_ldat$HEALTH.EXP)
clean_ldat$phys = std.fun(clean_ldat$phys)
clean_ldat$nurse = std.fun(clean_ldat$nurse)
clean_ldat$pharma = std.fun(clean_ldat$pharma)
clean_ldat$CPI = std.fun(clean_ldat$CPI)
clean_ldat$UNDERNOURISH = std.fun(clean_ldat$UNDERNOURISH)
clean_ldat$OVER65 = std.fun(clean_ldat$OVER65)
clean_ldat$GDP = std.fun(clean_ldat$GDP)



l.lm = lm(ddd ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI, data=ldat)
summary(l.lm)
plot(l.lm)

l.lm.2 = lm(ddd ~ SANITATION + HEALTH.EXP + pharma + EDU + CPI + GDP, data=clean_ldat)
summary(l.lm.2)

# medium high income
hdat = dat[dat$lmic==0 & !is.na(dat$lmic),-8] # got rid of defecation (all zero's)
hdat = hdat[hdat$country != "IRELAND",]
h.pca = prcomp(na.omit(hdat[,5:(5+9)]),center=TRUE,scale=TRUE)
biplot(h.pca)
h.pca

clean_hdat = clean_data2[clean_data2$lmic==0 & !is.na(clean_data2$lmic),-8]
clean.pcah = prcomp(na.omit(clean_hdat[,5:(5+10)]),center=TRUE,scale=TRUE)
biplot(clean.pcah)

write.xlsx(clean.pcah$rotation, "clean_pcah.xlsx")

pca.h = read.csv("pca.high2.csv")
pca.h$group = as.factor(pca.h$group)
library(ggplot2)
ph2 = ggplot(pca.h, aes(x=PC1, y=PC2)) + geom_point(size=1)+
  geom_text(aes(label=variable),hjust=0.62, vjust=2.5, size = 1.6)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2, color=group), 
               arrow=arrow(length=unit(0.2,"cm")), alpha=1, size=.8) + 
  ggtitle("HIC PCA Plot") + labs(color = "Group") + theme_bw()+
  theme(legend.position=c(0.9, 0.2), plot.title = element_text(hjust = 0.5))
ggsave("HIC_pca3.png",width=5.5, height=5, unit="in", dpi=300)
ph2

#PCA WITH GDP
pca.h2 = read.csv("clean_pcah.csv")
pca.h2$group = as.factor(pca.h2$group)
library(ggplot2)
pcah2 = ggplot(pca.h2, aes(x=PC1, y=PC2)) + geom_point(size=1)+
  geom_text(aes(label=variable),hjust=0.62, vjust=2.5, size = 1.6)+
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2, color=group), 
               arrow=arrow(length=unit(0.2,"cm")), alpha=1, size=.8) + 
  ggtitle("HIC PCA Plot") + labs(color = "Group") + theme_bw()+
  theme(legend.position=c(0.9, 0.2), plot.title = element_text(hjust = 0.5))
ggsave("HIC_pca3.png",width=5.5, height=5, unit="in", dpi=300)
pcah2

hdat$SANITATION = std.fun(hdat$SANITATION)
hdat$EDU = std.fun(hdat$EDU)
hdat$CPI = std.fun(hdat$CPI)
hdat$HEALTH.EXP = std.fun(hdat$HEALTH.EXP)
hdat$OVER65 = std.fun(hdat$OVER65)
hdat$HOSPBEDS = std.fun(hdat$HOSPBEDS)
hdat$HEALTH.EXP = std.fun(hdat$HEALTH.EXP)
hdat$phys = std.fun(hdat$phys)
hdat$nurse = std.fun(hdat$nurse)
hdat$pharma = std.fun(hdat$pharma)
hdat$CPI = std.fun(hdat$CPI)
hdat$UNDERNOURISH = std.fun(hdat$UNDERNOURISH)
hdat$OVER65 = std.fun(hdat$OVER65)

clean_hdat$SANITATION = std.fun(clean_hdat$SANITATION)
clean_hdat$EDU = std.fun(clean_hdat$EDU)
clean_hdat$CPI = std.fun(clean_hdat$CPI)
clean_hdat$HEALTH.EXP = std.fun(clean_hdat$HEALTH.EXP)
clean_hdat$OVER65 = std.fun(clean_hdat$OVER65)
clean_hdat$HOSPBEDS = std.fun(clean_hdat$HOSPBEDS)
clean_hdat$HEALTH.EXP = std.fun(clean_hdat$HEALTH.EXP)
clean_hdat$phys = std.fun(clean_hdat$phys)
clean_hdat$nurse = std.fun(clean_hdat$nurse)
clean_hdat$pharma = std.fun(clean_hdat$pharma)
clean_hdat$CPI = std.fun(clean_hdat$CPI)
clean_hdat$UNDERNOURISH = std.fun(clean_hdat$UNDERNOURISH)
clean_hdat$OVER65 = std.fun(clean_hdat$OVER65)
clean_hdat$GDP = std.fun(clean_hdat$GDP)

h.lm = lm(ddd ~ SANITATION + EDU+ HEALTH.EXP + pharma, data=hdat)
h.lm2 = lm(ddd~GDP+ SANITATION + EDU+ HEALTH.EXP + pharma, data = clean_hdat)

# Stepwise Regression
library(MASS)
step <- stepAIC(h.lm, direction="both")
step$anova # display results


summary(h.lm)
plot(h.lm)

summary(h.lm2)


# stargazer
library(stargazer)

stargazer(l.lm, h.lm, type="text", 
          title = "Regression Results", 
          column.labels = c("LMIC", "HIC"), flip = FALSE)
stargazer(summary(l.lm))

# STARGAZER WITH GDP
stargazer(l.lm.2, h.lm2, type="text", 
          title = "Regression Results", 
          column.labels = c("LMIC", "HIC"), flip = FALSE,
          out="newnew.htm")

