library(imputeMissings)
library(magrittr)
library(dplyr)
library(broom)
library(tidyverse)
library(ggplot2)
library(InformationValue)
library(ROCR)
library(glmnet)

##Read Training csv
setwd("...")
ins <- read.csv(file = "aggregated_policy_label_2023/aggregated_policy_label_2023B_oversampled.csv", header = TRUE)

##Predicting NOT Fraud
ins$NOTFraud <- ifelse(ins$Fraud == 1, 0, 1)
ins$NOTFraud <- as.factor(ins$NOTFraud)
weight <- ifelse(ins$NOTFraud == 1, 17.95, 1)

# Find number distinct values 
sapply(ins, function(x) n_distinct(x))

##Number of Missing Value per Variable
sapply(ins, function(x) sum(is.na(x)))

##Variables to drop
  #Most descriptive variables were dropped; second line were removed due to quasi-complete separation
ins_t <- subset(ins, select = -c(GivenName,MiddleInitial,Surname,StreetAddress,City,State,Country,Birthday,TelephoneNumber,MothersMaiden,Cov_ID,Cust_ID,FeetInches,ZipCode,Adj_ZIP,Date_Initial,Adj_ID,Tech_ID,
  Med_Leuk_Flag,Fam_Med_Healthy,Med_CL_Healthy,Med_CL_Fam_Healthy,Exclusion_Payout,Reward_Larger_CL,Number_ZIP_Transactions,Inc_CL_Dec_Income,Med_SE_Flag, Med_TB_Flag, Cov_2M))

##Condensing categorical levels
ins_t$Changes_Med_History[which(ins_t$Changes_Med_History > 2)] <- "3+"
ins_t$Pol_Count[which(ins_t$Pol_Count > 1)] <- "2+"
ins_t$Widow_Count[which(ins_t$Widow_Count > 1)] <- "2+"

##Set categorical variables as factors
categorical <- c("BloodType","Type","Tobacco_IN","Caffeine_IN","Alcohol_IN","Med_BP_Flag","Med_Chol_Flag","Med_Arth_Flag","Med_Can_Flag","Med_Diab_Flag","Med_Asth_Flag","Med_Gla_Flag","Med_Kid_Flag",
                 "Med_Ment_Flag","Med_HA_Flag","Med_SCA_Flag","Med_Str_Flag","Med_TD_Flag","Med_Ul_Flag","Med_Healthy","Susp_Adj_Tech","Changes_Med_History","Med_Deletion_Flag",
                 "Exclusion","Pol_Count","Cov_1M","IN_After_CL","Single_Income10K_Increase","Widow_Count","Fraud")

##Set categorical variables as factors
ins_t[categorical] <- lapply(ins_t[categorical], as.factor)

##Check each categorical variable for separation
table(ins_t$Pol_Count, ins_t$Fraud)

##Impute Missing Values
  #Diff_Income_DeathCodeIncome, Time_LastChange_Claim, Distance_Claim_Adj, Pop_CZIP, MedInc_CZIP, Inc_ZIPInc were imputed with median
ins_t <- imputeMissings::impute(ins_t, method = "median/mode", flag = TRUE)

##Training Model
initial.model <- glm(NOTFraud ~ .-WEIGHT-Fraud,
                     data = ins_t, family = binomial(link = "logit"),
                     weights = weight)
summary(initial.model)

final.model <- glm(NOTFraud ~ BloodType+Reward_Amount+Alcohol_IN+Number_Changes+Income_Claim+Initial_to_Claim_Difference+
                     Distance_Claim_Adj+Coverage_Income_Ratio_Claim+Coverage_Income_Ratio_Initial+CH_Count_6Mon+Adj_Tech_Avg_Reward+
                     Time_Between_CL_R+Pol_Count+Age_CL+Reward_Avg_Rew_Ratio+Cov_1M+CH_AvgCH_Ratio+Widow_Count+Time_LastChange_Claim_flag,
                   data = ins_t, family = binomial(link = "logit"),
                   weights = weight)
summary(final.model)

##P-hat Values
ins$p_hat <- predict(final.model, type = "response")
p1 <- ins$p_hat[ins$NOTFraud == 1]
p0 <- ins$p_hat[ins$NOTFraud == 0]

##ROC Curve
pred <- prediction(ins$p_hat, ins$NOTFraud)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, lwd = 3, colorize = TRUE, colorkey = TRUE, colorize.palette = rev(gray.colors(256)))
abline(a = 0, b = 1, lty = 3)
AUROC(ins$NOTFraud, ins$p_hat)

##Top 500 observations with the lowest p_hat value
top_500 <- ins[order(ins$p_hat, decreasing = FALSE),][1:500,]
top_500 <- subset(top_500, select = c(Cov_ID,p_hat))

##Write Top 500 to csv
write.csv(top_500, "LR_NOTFraud_OversampledBlue.csv")
