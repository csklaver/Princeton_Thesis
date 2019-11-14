## making all of the variables as.numeric and as.factor
total_data = add_class[,c(1,2,5,9,29,15,16,18,19,20,22:27,30,32,33,43,34:36,
                          44:46,37,47,38,39,48,49,40:42)]
total_data$country = as.factor(total_data$country)
total_data$year = as.factor(total_data$year)
total_data$HEALTH.EXP = as.numeric(as.character(total_data$HEALTH.EXP))
total_data$HOSPBEDS = as.numeric(as.character(total_data$HOSPBEDS))
#total_data$LITERACY = as.numeric(as.character(add_class$LITERACY))
total_data$OVER65 = as.numeric(as.character(total_data$OVER65))
total_data$DEFECATION = as.numeric(as.character(total_data$DEFECATION))
total_data$SANITATION = as.numeric(as.character(total_data$SANITATION))
total_data$CPI = as.numeric(as.character(total_data$CPI))
total_data$EDU = as.numeric(as.character(total_data$EDU))
total_data$UNDERNOURISH = as.numeric(as.character(total_data$UNDERNOURISH))
total_data$phys = as.numeric(as.character(total_data$phys))
total_data$nurse = as.numeric(as.character(total_data$nurse))
total_data$pharma = as.numeric(as.character(total_data$pharma))
total_data$GDP = as.numeric(as.character(total_data$GDP))

elibrary(xlsx)
write.xlsx(total_data, "total_data2.xlsx")


## find summary and standard dev for each variable
library(pastecs)
letssee = stat.desc(total_data) 

library(xlsx)
write.xlsx(letssee, "MeanMedSD2.xlsx")
