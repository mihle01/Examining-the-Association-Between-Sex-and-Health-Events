setwd("/Users/mi/Documents/BU MPH/BS 730 R Assignments/Project 1")
df_nhpi <- read.table("proj1_2022.csv", header=TRUE, sep=",")
attach(df_nhpi)
####   PART 1: Data Cleaning    ###
#recode missing values to NA
df_nhpi$HEIGHT[df_nhpi$HEIGHT %in% c(96,97,98,99)] <- NA
#above code does these 4 lines of code below in one line.
#df_nhpi$HEIGHT[df_nhpi$HEIGHT==96] <- NA
#df_nhpi$HEIGHT[df_nhpi$HEIGHT==97] <- NA
#df_nhpi$HEIGHT[df_nhpi$HEIGHT==98] <- NA
#df_nhpi$HEIGHT[df_nhpi$HEIGHT==99] <- NA
df_nhpi$WEIGHT[df_nhpi$WEIGHT %in% c(996,997,998,999)] <- NA
df_nhpi$HTN[df_nhpi$HTN %in% c(7,8,9)] <- NA
df_nhpi$HYBPLEV[df_nhpi$HYBPLEV %in% c(7,8,9)] <- NA
df_nhpi$HTNMED[df_nhpi$HTNMED %in% c(7,8,9)] <- NA
df_nhpi$MI[df_nhpi$MI %in% c(7,8,9)] <- NA
df_nhpi$COPD[df_nhpi$COPD %in% c(7,8,9)] <- NA
df_nhpi$CANCER[df_nhpi$CANCER %in% c(7,8,9)] <- NA
df_nhpi$DIABETES[df_nhpi$DIABETES %in% c(7,8,9)] <- NA
#create BMI variable using height and weight
weight_kg <- df_nhpi$WEIGHT/2.2
height_m <- df_nhpi$HEIGHT/39.37
df_nhpi$BMI <- weight_kg/(height_m^2)
#using SMKSTAT2 variable to create new categorical SMOKING variable
df_nhpi$SMOKING <- ifelse(df_nhpi$SMKSTAT2==1|df_nhpi$SMKSTAT2==2|df_nhpi$SMKSTAT2==5,"current", 
                          ifelse(df_nhpi$SMKSTAT2==3,"former",
                                 ifelse(df_nhpi$SMKSTAT2==4, "never",
                                        ifelse(df_nhpi$SMKSTAT2==9, "unknown",NA))))
#for the following vars, assign a label where 1=Yes, 2=No, and all others=Unknown
df_nhpi$HTN <- ifelse(df_nhpi$HTN==1, "Yes",
                      ifelse(df_nhpi$HTN==2, "No","Unknown"))
      #address the NAs that we set earlier because the above code does not detect NAs:
      df_nhpi$HTN[is.na(df_nhpi$HTN)] <- "Unknown"

df_nhpi$HTNMED <- ifelse(df_nhpi$HTNMED==1, "Yes",
                      ifelse(df_nhpi$HTNMED==2, "No","Unknown"))
df_nhpi$HTNMED[is.na(df_nhpi$HTNMED)] <- "Unknown"

df_nhpi$MI <- ifelse(df_nhpi$MI==1, "Yes",
                         ifelse(df_nhpi$MI==2, "No","Unknown"))
df_nhpi$MI[is.na(df_nhpi$MI)] <- "Unknown"

df_nhpi$COPD <- ifelse(df_nhpi$COPD==1, "Yes",
                     ifelse(df_nhpi$COPD==2, "No","Unknown"))
df_nhpi$COPD[is.na(df_nhpi$COPD)] <- "Unknown"

df_nhpi$CANCER <- ifelse(df_nhpi$CANCER==1, "Yes",
                     ifelse(df_nhpi$CANCER==2, "No","Unknown"))
df_nhpi$CANCER[is.na(df_nhpi$CANCER)] <- "Unknown"

df_nhpi$DIABETES <- ifelse(df_nhpi$DIABETES==1, "Yes",
                         ifelse(df_nhpi$DIABETES==2, "No",
                                ifelse(df_nhpi$DIABETES==3, "Borderline", "Unknown")))
df_nhpi$DIABETES[is.na(df_nhpi$DIABETES)] <- "Unknown"

df_nhpi$SEX <- ifelse(df_nhpi$SEX==1, "Male","Female")

#recode numerical mar_stat
df_nhpi$MAR_STAT <- ifelse(df_nhpi$MAR_STAT==4,0,
                           ifelse(df_nhpi$MAR_STAT==2|df_nhpi$MAR_STAT==3,1,
                                  ifelse(df_nhpi$MAR_STAT==1|df_nhpi$MAR_STAT==5,2,NA)))
#Descriptive statistics for numerical variables & 2-sample t-test:
table(df_nhpi$SEX)
tapply(df_nhpi$AGE, df_nhpi$SEX, mean, na.rm=TRUE) #redundant code - t-test will give you the means of the 2 groups.
tapply(df_nhpi$AGE, df_nhpi$SEX, sd, na.rm=TRUE)
var.test(df_nhpi$AGE ~ df_nhpi$SEX) #default: Female is num, male is denom. Regardless, p-value will be the same
    #Alternatively:
    var.test(df_nhpi$AGE[df_nhpi$SEX=="Female"], df_nhpi$AGE[df_nhpi$SEX=="Male"])
    var.test(df_nhpi$AGE[df_nhpi$SEX=="Male"], df_nhpi$AGE[df_nhpi$SEX=="Female"])
    #stats are diff bc of which group is the num/denom, but ultimately the p-value is the same.
t.test(df_nhpi$AGE ~ df_nhpi$SEX, var.equal=TRUE)

var.test(df_nhpi$HEIGHT ~ df_nhpi$SEX)
t.test(df_nhpi$HEIGHT ~ df_nhpi$SEX, var.equal=FALSE)
tapply(df_nhpi$HEIGHT, df_nhpi$SEX, sd, na.rm=TRUE)
