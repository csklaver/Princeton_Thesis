getwd()
setwd("/Users/carolinesklaver/Desktop/Thesis/Modified Data CSV")



## HEALTH EXPENDITURE PER CAPITA

# read in health expenditure per capita
health.exp.wide = read.csv("health.expPC.csv")

# reshape Health Expenditure Per Capita data
health.exp.long = reshape(data = health.exp.wide,
              idvar = "country",
              varying = c("HE.2000", "HE.2001",
                          "HE.2002", "HE.2003",
                          "HE.2004", "HE.2005",
                          "HE.2006", "HE.2007",
                          "HE.2008", "HE.2009", 
                          "HE.2010", "HE.2011", 
                          "HE.2012", "HE.2013", 
                          "HE.2014", "HE.2015"),
              timevar = "year",
              times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                        2009,2011,2012,2013,2014,2015),
              new.row.names= 1:10000,
              direction = "long")
health.exp = health.exp.long[,c(2,4,5)]
names(health.exp) = c("country", "year", "HEALTH.EXP")
head(health.exp)

library(xlsx)
write.xlsx(health.exp, "c:HealthExpMod.xlsx")



## HOSPITAL BEDS PER 1,000

# read in hos.beds.csv
hosp.beds.wide = read.csv("hos.beds.csv")

# reshape Hospital Beds (per 1,000)
hosp.beds.long = reshape(data = hos.beds.wide,
                     idvar = "country",
                     varying = c("beds.2000", "beds.2001",
                                 "beds.2002", "beds.2003",
                                 "beds.2004", "beds.2005",
                                 "beds.2006", "beds.2007",
                                 "beds.2008", "beds.2009", 
                                 "beds.2010", "beds.2011", 
                                 "beds.2012", "beds.2013", 
                                 "beds.2014", "beds.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
hosp.beds = hosp.beds.long[,c(2,4,5)]
names(hosp.beds) = c("country", "year", "HOSPBEDS")
head(hosp.beds)



library(xlsx)
write.xlsx(hosp.beds, "HospBedsMod.xlsx")


## ADULT LITERACRY RATE (% of people above 15 years old)

# read in literacy rate, adult total (% of people ages 15 and above)
litrate.wide = read.csv("literacyrate.csv")

# reshape Health Expenditure Per Capita data
litrate.long = reshape(data = litrate.wide,
                     idvar = "country",
                     varying = c("litrate.2000", "litrate.2001",
                                 "litrate.2002", "litrate.2003",
                                 "litrate.2004", "litrate.2005",
                                 "litrate.2006", "litrate.2007",
                                 "litrate.2008", "litrate.2009", 
                                 "litrate.2010", "litrate.2011", 
                                 "litrate.2012", "litrate.2013", 
                                 "litrate.2014", "litrate.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
literacy.rate = litrate.long[,c(2,4,5)]
names(literacy.rate) = c("country", "year", "LITERACY")
head(literacy.rate)



library(xlsx)
write.xlsx(literacy.rate, "LiteracyMod.xlsx")


## % OF TOTAL POPULATION OVER THE AGE OF 65

# read in Population Over the Age of 65 (% of total)
elderly.wide = read.csv("pop65.csv")

# reshape Health Expenditure Per Capita data
elderly.long = reshape(data = elderly.wide,
                     idvar = "country",
                     varying = c("pop65.2000", "pop65.2001",
                                 "pop65.2002", "pop65.2003",
                                 "pop65.2004", "pop65.2005",
                                 "pop65.2006", "pop65.2007",
                                 "pop65.2008", "pop65.2009", 
                                 "pop65.2010", "pop65.2011", 
                                 "pop65.2012", "pop65.2013", 
                                 "pop65.2014", "pop65.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
elderly = elderly.long[,c(2,4,5)]
names(elderly) = c("country", "year", "OVER65")
head(elderly)


library(xlsx)
write.xlsx(elderly, "Over65Mod.xlsx")



## % OF THE POPULATION UNDERNOURISHED

# read in Prevalence of Undernourishment (% of the population)
undernourishment.wide = read.csv("undernourishment.csv")

# reshape Undernourishment data
undernourishment.long = reshape(data = undernourishment.wide,
                idvar = "country",
                varying = c("underN.2000", "underN.2001",
                            "underN.2002", "underN.2003",
                            "underN.2004", "underN.2005",
                            "underN.2006", "underN.2007",
                            "underN.2008", "underN.2009", 
                            "underN.2010", "underN.2011", 
                            "underN.2012", "underN.2013", 
                            "underN.2014", "underN.2015"),
                timevar = "year",
                times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                          2009,2011,2012,2013,2014,2015),
                new.row.names= 1:10000,
                direction = "long")
undernourishment = undernourishment.long[,c(2,4,5)]
names(undernourishment) = c("country", "year", "UNDERNOURISH")
head(undernourishment)


library(xlsx)
write.xlsx(undernourishment, "UndernourishedMod.xlsx")



## % OF THE POPULATION USING BASIC SANITATION FACILITIES

# read in % of population using at least basic sanitation facilities
sanitation.wide = read.csv("sanitation.csv")

# reshape 
sanitation.long = reshape(data = sanitation.wide,
                     idvar = "country",
                     varying = c("san.2000", "san.2001",
                                 "san.2002", "san.2003",
                                 "san.2004", "san.2005",
                                 "san.2006", "san.2007",
                                 "san.2008", "san.2009", 
                                 "san.2010", "san.2011", 
                                 "san.2012", "san.2013", 
                                 "san.2014", "san.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
sanitation = sanitation.long[,c(2,4,5)]
names(sanitation) = c("country", "year", "SANITATION")
head(sanitation)


library(xlsx)
write.xlsx(sanitation, "SanitationMod.xlsx")



## % OF POPOULATION PRACTICING OPEN DEFECATION

# read in % of population practicing open defecation
defecation.wide = read.csv("defecation.csv")

# reshape
defecation.long = reshape(data = defecation.wide,
                     idvar = "country",
                     varying = c("def.2000", "def.2001",
                                 "def.2002", "def.2003",
                                 "def.2004", "def.2005",
                                 "def.2006", "def.2007",
                                 "def.2008", "def.2009", 
                                 "def.2010", "def.2011", 
                                 "def.2012", "def.2013", 
                                 "def.2014", "def.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
defecation = defecation.long[,c(2,4,5)]
names(defecation) = c("country", "year", "DEFECATION")

head(defecation)



library(xlsx)
write.xlsx(defecation, "DefecationMod.xlsx")



## % OF POPULATION AGES 0-14

# read in  % of population ages 0-14
young.wide = read.csv("young.csv")

# reshape 
young.long = reshape(data = young.wide,
                     idvar = "country",
                     varying = c("young.2000", "young.2001",
                                 "young.2002", "young.2003",
                                 "young.2004", "young.2005",
                                 "young.2006", "young.2007",
                                 "young.2008", "young.2009", 
                                 "young.2010", "young.2011", 
                                 "young.2012", "young.2013", 
                                 "young.2014", "young.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
young = young.long[,c(2,4,5)]
names(young) = c("country", "year", "UNDER14")
head(young)


library(xlsx)
write.xlsx(young, "YoungMod.xlsx")


##lower secondary school completion rate
### NOW PRIMARY SCHOOL COMPLETION RATE (% OF RELEVANT AGE GROUP)

# read in Lower secondary completion rate, total (% of relevant age group)
education.wide = read.csv("primaryschool_completion.csv")

# reshape 
education.long = reshape(data = education.wide,
                     idvar = "country",
                     varying = c("pri.2000", "pri.2001",
                                 "pri.2002", "pri.2003",
                                 "pri.2004", "pri.2005",
                                 "pri.2006", "pri.2007",
                                 "pri.2008", "pri.2009", 
                                 "pri.2010", "pri.2011", 
                                 "pri.2012", "pri.2013", 
                                 "pri.2014", "pri.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
education = education.long[,c(2, 3, 4)]
names(education) = c("country", "year", "EDU")



#library(xlsx)
#write.xlsx(education, "LowerSecondaryMod.xlsx")


## CORRUPTION PERCEPTIONS INDEX

CPI.wide = read.csv("CPI.wide.csv")
CPI.wide[,1] = toupper(CPI.wide[,1])


# reshape CPI data
CPI.long = reshape(data = CPI.wide,
                          idvar = 'country',
                          varying = c("CPI2000", "CPI2001",
                                      "CPI2002", "CPI2003",
                                      "CPI2004", "CPI2005",
                                      "CPI2006", "CPI2007",
                                      "CPI2008", "CPI2009", 
                                      "CPI2010", "CPI2011", 
                                      "CPI2012", "CPI2013", 
                                      "CPI2014", "CPI2015"),
                          timevar = "year",
                          times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                                    2009,2011,2012,2013,2014,2015),
                          sep = "",
                          new.row.names= 1:10000,
                          direction = "long")
CPI = CPI.long[,1:3]
names(CPI) = c("country", "year", "CPI")
head(CPI)


library(xlsx)
write.xlsx(CPI, "CPIMod.xlsx")


## # OF PHYSICIANS PER 1000 PEOPLE

# read in Lower secondary completion rate, total (% of relevant age group)
physicians.wide = read.csv("physicians.csv")

# reshape 
physicians.long = reshape(data = physicians.wide,
                         idvar = "country",
                         varying = c("phy.2000", 
                                     "phy.2008", "phy.2009", 
                                     "phy.2010", "phy.2011", 
                                     "phy.2012", "phy.2013", 
                                     "phy.2014", "phy.2015"),
                         timevar = "year",
                         times = c(2000,2008,
                                   2009,2011,2012,2013,2014,2015),
                         new.row.names= 1:10000,
                         direction = "long")
physicians = physicians.long[,c(2,3,4)]
names(physicians) = c("country", "year", "PHY")
tail(physicians)
head(physicians)


library(xlsx)
write.xlsx(physicians, "PHYMod.xlsx")



#GDP per capita
# read in  GDP per capita
GDP.wide = read.csv("GDP.csv")
GDP.wide[,1] = toupper(GDP.wide[,1])

# reshape 
GDP.long = reshape(data = GDP.wide,
                     idvar = "country",
                     varying = c("GDP.2000", "GDP.2001",
                                 "GDP.2002", "GDP.2003",
                                 "GDP.2004", "GDP.2005",
                                 "GDP.2006", "GDP.2007",
                                 "GDP.2008", "GDP.2009", 
                                 "GDP.2010", "GDP.2011", 
                                 "GDP.2012", "GDP.2013", 
                                 "GDP.2014", "GDP.2015"),
                     timevar = "year",
                     times = c(2000,2001,2002,2003,2004,2005,20006,2007,2008,
                               2009,2011,2012,2013,2014,2015),
                     new.row.names= 1:10000,
                     direction = "long")
names(GDP.long) = c("country", "year", "GDPPC")
head(GDP.long)


library(xlsx)
write.xlsx(GDP.long, "GDPMod.xlsx")


