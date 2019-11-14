getwd()
setwd("/Users/carolinesklaver/Desktop/Thesis/Modified Data CSV")

# reading in Data by Class
Data_Class = read.csv("DataByClass.csv")
code = read.csv("code.csv")

Data_Class_code = merge(Data_Class, code, by = "class", all = TRUE)

Data_Class_code = Data_Class_code[order(Data_Class_code$country, Data_Class_code$year),]

Data_Class_code$class = NULL
Data_Class_code$order = NULL

Data_Class.wide = reshape(data = Data_Class_code,
               idvar = c("country", "year"),
               v.names = "tot" ,
               timevar = "code",
               direction = "wide")


# code	class
# a1	Aminoglycosides
# a2	Broad spectrum penicillins
# a3	Carbapenems
# a4	Cephalosporins
# a5	Chloramphenicols
# a6	Fluoroquinolones
# a7	Glycopeptides
# a8	Glycylcyclines
# a9	Lipopeptides
# a10	Macrolides
# a11	Monobactams
# a12	Narrow spectrum penicillins
# a13	Others
# a14	Oxazolidinones
# a15	Phosphonics
# a16	Polymyxins
# a17	Tetracyclines
# a18	Trimethoprim

# Make country names capital
Data_Class.wide$country = toupper(Data_Class.wide$country)

#merge data
add_class = merge(antibiotic_consumption2, Data_Class.wide, by= c("country", "year"), all = TRUE)



#OG regression
fixed = lm(log ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
             as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
             as.numeric(CPI) + as.numeric(EDU) + lmic + as.factor(year), 
           data=antibiotic_consumption)

#A1
#need to add new variables: physician, nurse, pharma densities
fixed_a1 = lm(log(tot.a1+1) ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
             as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
             as.numeric(CPI) + as.numeric(EDU) + lmic + as.factor(year), 
           data=add_class)
#A2

fixed_a2 = lm(log(tot.a2+1) ~ as.numeric(HEALTH.EXP) + as.numeric(HOSPBEDS) + as.numeric(LITERACY) + 
             as.numeric(OVER65) + as.numeric(DEFECATION) + as.numeric(SANITATION) + 
             as.numeric(CPI) + as.numeric(EDU) + lmic + as.factor(year), 


# output first 5 important classes
library(stargazer)
stargazer(fixed_a2, fixed_a4, fixed_a6,fixed_a12,fixed_a10,
        type="text", title = "Antibiotic Consumption by Class", flip = TRUE, out="models.htm")


# output important classes from PNAS paper
#glycylicyclines 8, oxazolidinones 14, cabapenems 3, polymyxins 16
library(stargazer)
stargazer(fixed_a8, fixed_a14, fixed_a3,fixed_a16,
          type="text", title = "Antibiotic Consumption by Class", flip = TRUE, out="models.htm")



# output original regression

stargazer(fixed,type="text", title = "Antibiotic Consumption Variables 2010-2015", 
          flip = TRUE, out="models.htm")

summary(fixed)
