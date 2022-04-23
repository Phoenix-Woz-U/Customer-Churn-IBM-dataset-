library(readxl)
library(tidyverse)
library(dplyr)
library(polycor)
Telco<- read_excel("C:/Users/Richmond/Documents/GitHub/Customer-Churn-IBM-dataset-/Data/Telco.xlsx")
View(Telco)

colnames(Telco) #View all column names in order


#Drop first 9 columns and the column named "Churn Label" by subsetting dataset
Telco1<- Telco[ -c(1:9,29,33) ]

#View datatype for each variable
str(Telco2) 

#Replace/remove spaces in column names
names(Telco1) <- gsub(" ","_", names(Telco1))
View(Telco1)

#View unique values in each variable column
unique(Telco2[c("Payment_Method")])

#Convert two-level categorical variables to factor for Regression Analysis

Telco1$Gender = as.factor(Telco1$Gender)
Telco1$Senior_Citizen = as.factor(Telco1$Senior_Citizen)
Telco1$Partner = as.factor(Telco1$Partner)
Telco1$Dependents = as.factor(Telco1$Dependents)
Telco1$Phone_Service = as.factor(Telco1$Phone_Service)
Telco1$Paperless_Billing = as.factor(Telco1$Paperless_Billing)
Telco1$Churn_Value = as.factor(Telco1$Churn_Value)

Telco2<- Telco1

#CATEGORICAL VARIABLES WITH 3 LEVELS OR MORE

#---Multiple Lines
Telco2$Multiple_Lines = as.factor(Telco2$Multiple_Lines)
Telco2$Multiple_LinesR <- NA
Telco2$Multiple_LinesR[Telco2$Multiple_Lines=='No'] <- 0
Telco2$Multiple_LinesR[Telco2$Multiple_Lines=='Yes'] <- 1
Telco2$Multiple_LinesR[Telco2$Multiple_Lines=='No phone service'] <- 2

Telco2$Multiple_LinesR = as.factor(Telco2$Multiple_LinesR)
Telco2$Multiple_LinesRR = relevel(Telco2$Multiple_LinesR, ref ="0")

#---Internet Service
Telco2$Internet_ServiceR <- NA
Telco2$Internet_ServiceR[Telco2$Internet_Service=='No'] <- 0
Telco2$Internet_ServiceR[Telco2$Internet_Service=='DSL'] <- 1
Telco2$Internet_ServiceR[Telco2$Internet_Service=='Fiber optic'] <- 2

Telco2$Internet_ServiceR = as.factor(Telco2$Internet_ServiceR)
Telco2$Internet_ServiceRR = relevel(Telco2$Internet_ServiceR, ref ="0")

#---Online Security
Telco2$Online_SecurityR <- NA
Telco2$Online_SecurityR[Telco2$Online_Security=='No'] <- 0
Telco2$Online_SecurityR[Telco2$Online_Security=='Yes'] <- 1
Telco2$Online_SecurityR[Telco2$Online_Security=='No internet service'] <- 2

Telco2$Online_SecurityR = as.factor(Telco2$Online_SecurityR)
Telco2$Online_SecurityRR = relevel(Telco2$Online_SecurityR, ref ="0")

#---Online Backup
Telco2$Online_BackupR <- NA
Telco2$Online_BackupR[Telco2$Online_Backup=='No'] <- 0
Telco2$Online_BackupR[Telco2$Online_Backup=='Yes'] <- 1
Telco2$Online_BackupR[Telco2$Online_Backup=='No internet service'] <- 2

Telco2$Online_BackupR = as.factor(Telco2$Online_BackupR)
Telco2$Online_BackupRR = relevel(Telco2$Online_BackupR, ref ="0")

#---Device Protection
Telco2$Device_ProtectionR <- NA
Telco2$Device_ProtectionR[Telco2$Device_Protection=='No'] <- 0
Telco2$Device_ProtectionR[Telco2$Device_Protection=='Yes'] <- 1
Telco2$Device_ProtectionR[Telco2$Device_Protection=='No internet service'] <- 2

Telco2$Device_ProtectionR = as.factor(Telco2$Device_ProtectionR)
Telco2$Device_ProtectionRR = relevel(Telco2$Device_ProtectionR, ref ="0")

#---Tech_Support
Telco2$Tech_SupportR <- NA
Telco2$Tech_SupportR[Telco2$Tech_Support=='No'] <- 0
Telco2$Tech_SupportR[Telco2$Tech_Support=='Yes'] <- 1
Telco2$Tech_SupportR[Telco2$Tech_Support=='No internet service'] <- 2

Telco2$Tech_SupportR = as.factor(Telco2$Tech_SupportR)
Telco2$Tech_SupportRR = relevel(Telco2$Tech_SupportR, ref ="0")

#---Streaming TV
Telco2$Streaming_TVR <- NA
Telco2$Streaming_TVR[Telco2$Streaming_TV=='No'] <- 0
Telco2$Streaming_TVR[Telco2$Streaming_TV=='Yes'] <- 1
Telco2$Streaming_TVR[Telco2$Streaming_TV=='No internet service'] <- 2

Telco2$Streaming_TVR = as.factor(Telco2$Streaming_TVR)
Telco2$Streaming_TVRR = relevel(Telco2$Streaming_TVR, ref ="0")

#---Streaming Movies
Telco2$Streaming_MoviesR <- NA
Telco2$Streaming_MoviesR[Telco2$Streaming_Movies=='No'] <- 0
Telco2$Streaming_MoviesR[Telco2$Streaming_Movies=='Yes'] <- 1
Telco2$Streaming_MoviesR[Telco2$Streaming_Movies=='No internet service'] <- 2

Telco2$Streaming_MoviesR = as.factor(Telco2$Streaming_MoviesR)
Telco2$Streaming_MoviesRR = relevel(Telco2$Streaming_MoviesR, ref ="0")

#---Contract
Telco2$ContractR <- NA
Telco2$ContractR[Telco2$Contract=='Month-to-month'] <- 0
Telco2$ContractR[Telco2$Contract=='One year'] <- 1
Telco2$ContractR[Telco2$Contract=='Two year'] <- 2

Telco2$ContractR = as.factor(Telco2$ContractR)
Telco2$ContractRR = relevel(Telco2$ContractR, ref ="0")

#---Payment_Method
Telco2$Payment_MethodR <- NA
Telco2$Payment_MethodR[Telco2$Payment_Method=='Mailed check'] <- 0
Telco2$Payment_MethodR[Telco2$Payment_Method=='Electronic check'] <- 1
Telco2$Payment_MethodR[Telco2$Payment_Method=='Bank transfer (automatic)'] <- 2
Telco2$Payment_MethodR[Telco2$Payment_Method=='Credit card (automatic)'] <- 3

Telco2$Payment_MethodR = as.factor(Telco2$Payment_MethodR)
Telco2$Payment_MethodRR = relevel(Telco2$Payment_MethodR, ref ="0")


#Subset dataframe to be used for Analysis

Telco3 = Telco2[c("Gender","Senior_Citizen","Partner","Dependents","Tenure_Months","Phone_Service","Paperless_Billing","Multiple_LinesRR",
                  "Internet_ServiceRR","Online_SecurityRR","Online_BackupRR","Device_ProtectionRR","Tech_SupportRR","Streaming_TVRR",
                  "Streaming_MoviesRR","ContractRR","Payment_MethodRR","Monthly_Charges","Churn_Value")]


## ANALYSIS

#Baseline
FitAll = lm(as.numeric(Churn_Value) ~ ., data = Telco3)
summary(FitAll)

str(Telco3)

#Resolve error message: Coefficients: (7 not defined because of singularities)
hetcor(Telco3) #still error


#Backward Elimination

step(FitAll, direction = 'backward')



