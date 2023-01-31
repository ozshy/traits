### Paper on unbanked with Claire
# Reference to tables and figures corresponds to those of Cole and Greene BOS Fed 2016 and the 2021 Greene and Shy Policy Hub
#
# The following packages are used:
#library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
#library(xtable)# for LaTeX tables
#library(writexl)# export to Excel 
#library(ggplot2)
#library(spatstat) # for weighted.median
#library(mfx)
#library(texreg)# exports regression result to LaTeX (just like stargazer) or HTML (can be read by Excel or Word)
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
library(performanceEstimation)# for SMOTE (balancing data by generating synthetic data classification tree) => hard to balance. Not used, bad results even when balanced.
#library(ROSE)# balancing data w.r.t. minority class (similar to SMOTE) => generates negative income. Do not use!
#library(caret)
library("randomForest")

setwd("C:/Oz_local_workspace_1")
dir()

### Reading CVS dataset, writing it as RDS
#i1.df = read.csv("traits_2023_1_18.csv")# use full dataset from Ruth
#saveRDS(i1.df, "traits_2023_1_18.rds")
#dir()
### Reading RDS dataset
i2.df = readRDS("traits_2023_1_18.rds")
dim(i2.df)
length(unique(i2.df$uasid))# num resp
names(i2.df)
str(i2.df)

# 
### Start redefining feature variables 
## Construct factor variables
i6.df = i2.df
# verify unbanked consumers
names(select(i6.df, contains("adopt")))
str(i6.df$bnk_acnt_adopt)
table(i6.df$bnk_acnt_adopt)
table(i6.df$unbanked)# use this one
sum(is.na(i6.df$unbanked))

# Age
str(i6.df$age)
sum(is.na(i6.df$age))
i6.df$age = as.numeric(i6.df$age)
str(i6.df$age)# age continuous (use this one)
i6.df$age_factor = NA #construct age factors (may not be needed)
i6.df$age_factor[i6.df$age < 25] = "Age_18_25"
i6.df$age_factor[i6.df$age >= 25 & i6.df$age < 34] = "Age_25_34"
i6.df$age_factor[i6.df$age >= 35 & i6.df$age < 44] = "Age_35_44"
i6.df$age_factor[i6.df$age >= 45 & i6.df$age < 54] = "Age_45_55"
i6.df$age_factor[i6.df$age >= 55 & i6.df$age < 64] = "Age_55_64"
i6.df$age_factor[i6.df$age >= 65] = "Age_65_and_older"
table(i6.df$age_factor)
sum(table(i6.df$age_factor))
nrow(i6.df)
i6.df$age_factor = as.factor(i6.df$age_factor)
str(i6.df$age_factor)
table(i6.df$age_factor)
i6.df$age_factor = relevel(i6.df$age_factor, ref = "Age_35_44") # reference age
levels(i6.df$age_factor)

# Gender
str(i6.df$gender)
table(i6.df$gender)
i6.df$gender_factor = NA #construct age factors
i6.df$gender_factor[i6.df$gender==0] = "Female"
i6.df$gender_factor[i6.df$gender==1] = "Male"
table(i6.df$gender_factor)
sum(table(i6.df$gender_factor))
nrow(i6.df)
str(i6.df$gender_factor)
i6.df$gender_factor = as.factor(i6.df$gender_factor)
#i6.df$gender_factor = relevel(i6.df$gender_factor, ref = "Gender_male") # reference male
levels(i6.df$gender_factor)

# Race
str(i6.df$race)
table(i6.df$race)
i6.df$race_factor = NA #construct age factors
i6.df$race_factor[i6.df$race==1] = "White"
i6.df$race_factor[i6.df$race==2] = "Black"
i6.df$race_factor[i6.df$race==4] = "Asian"
i6.df$race_factor[i6.df$race %in% c(3,5,6)] = "Other"
table(i6.df$race_factor)
sum(table(i6.df$race_factor))
nrow(i6.df)
str(i6.df$race_factor)
i6.df$race_factor = as.factor(i6.df$race_factor)
#i6.df$race_factor = relevel(i6.df$race_factor, ref = "Race_white")# white is reference
levels(i6.df$race_factor)

# Hispanic Latino => define Ethnicity
str(i6.df$hispaniclatino)
table(i6.df$hispaniclatino)
i6.df$ethnicity_factor = NA #construct age factors
i6.df$ethnicity_factor[i6.df$hispaniclatino==1] = "Hispaniclatino"
i6.df$ethnicity_factor[i6.df$hispaniclatino==0] = "Not_hispaniclatino"
table(i6.df$ethnicity_factor)
sum(table(i6.df$ethnicity_factor))
nrow(i6.df)
str(i6.df$ethnicity_factor)
i6.df$ethnicity_factor = as.factor(i6.df$ethnicity_factor)
#i6.df$ethnicity_factor = relevel(i6.df$ethnicity_factor, ref = "ethnicity_not_hispaniclatino")# non-latino is reference
levels(i6.df$ethnicity_factor)

# Education factor
str(i6.df$highest_education)
table(i6.df$highest_education, exclude = T)
i6.df$edu_factor = NA #construct age factors
i6.df$edu_factor[i6.df$highest_education < 9] = "Less_than_high_school"
i6.df$edu_factor[i6.df$highest_education == 9] = "High_school"
i6.df$edu_factor[i6.df$highest_education %in% c(10, 11, 12)] = "Some_college_or_associate"
#i6.df$edu_factor[i6.df$highest_education == 13] = "Education_college"
i6.df$edu_factor[i6.df$highest_education >= 13] = "College_and_higher"
#i6.df$edu_factor[i6.df$highest_education > 13] = "Education_graduate"
table(i6.df$edu_factor)
sum(table(i6.df$edu_factor))
nrow(i6.df)
str(i6.df$edu_factor)
i6.df$edu_factor = as.factor(i6.df$edu_factor)
#i6.df$edu_factor = relevel(i6.df$edu_factor, ref = "Education_college")# college education is reference
#i6.df$edu_factor = relevel(i6.df$edu_factor, ref = "Education_high_school")# HS education is reference
levels(i6.df$edu_factor)
#levels(i6.df$edu_factor) = c("Education_no_college_degree", "Education_college_and_higher")

# Marital status
str(i6.df$marital_status)
table(i6.df$marital_status)
i6.df$marital_status = as.numeric(i6.df$marital_status)
i6.df$marital_factor = NA #construct marital factors
i6.df$marital_factor[i6.df$marital_status < 3] = "Married"
i6.df$marital_factor[i6.df$marital_status > 2] = "Not_married"
table(i6.df$marital_factor)
sum(table(i6.df$marital_factor))
nrow(i6.df)
str(i6.df$marital_factor)
i6.df$marital_factor = as.factor(i6.df$marital_factor)
#i6.df$marital_factor = relevel(i6.df$marital_factor, ref = "Marital_status_other") #not married is reference
levels(i6.df$marital_factor)
table(i6.df$marital_factor)

# Household income
str(i6.df$income_hh)# used in the regression instead of the income_factor constructed below
i6.df$income_hh = as.numeric(i6.df$income_hh)
str(i6.df$income_hh)
summary(i6.df$income_hh)
i6.df$income_factor = NA #construct HH income factors [probably not needed]
i6.df$income_factor[i6.df$income_hh  < 30000] = "HH_income_less_than_30k"
i6.df$income_factor[i6.df$income_hh  >= 30000 & i6.df$income_hh < 60000] = "HH_income_30k_60k"
i6.df$income_factor[i6.df$income_hh  >= 60000 & i6.df$income_hh < 90000] = "HH_income_60k_90k"
i6.df$income_factor[i6.df$income_hh  >= 90000] = "HH_income_90k_and_higher"
table(i6.df$income_factor, exclude = T)
sum(table(i6.df$income_factor))
nrow(i6.df)
str(i6.df$income_factor)
i6.df$income_factor = as.factor(i6.df$income_factor)
i6.df$income_factor = relevel(i6.df$income_factor, ref = "HH_income_60k_90k")
levels(i6.df$income_factor)
levels(i6.df$income_factor) = c("HH_income_60k_90k", "HH_income_less_than_30k", "HH_income_30k_60k", "HH_income_90k_and_higher")

# Employment
str(i6.df$work_employed)
table(i6.df$work_employed)
i6.df$work_factor = NA #construct employment factor
i6.df$work_factor[i6.df$work_employed == 1] = "Employed"
i6.df$work_factor[i6.df$work_employed == 0] = "Not_employed"
table(i6.df$work_factor)
sum(table(i6.df$edu_factor))
nrow(i6.df)
str(i6.df$work_factor)
i6.df$work_factor = as.factor(i6.df$work_factor)
#i6.df$work_factor = relevel(i6.df$work_factor, ref = "Work_not_employed")
levels(i6.df$work_factor)
table(i6.df$work_factor)

# Household size
str(i6.df$hh_size)
table(i6.df$hh_size)

# Home ownership
str(i6.df$homeowner)
table(i6.df$homeowner)
i6.df$homeowner_factor = NA #construct home ownership factors
i6.df$homeowner_factor[i6.df$homeowner == 1] = "Homeowner"
i6.df$homeowner_factor[i6.df$homeowner == 0] = "Not_homeowner"
table(i6.df$homeowner_factor)
sum(table(i6.df$homeowner_factor))
nrow(i6.df)
str(i6.df$homeowner_factor)
i6.df$homeowner_factor = as.factor(i6.df$homeowner_factor)
#i6.df$homeowner_factor = relevel(i6.df$homeowner_factor, ref = "Homeowner_no")
levels(i6.df$homeowner_factor)

# new variable: income_hh/10k. [Not needed for trees (only regressions, which I don't do here)]
i6.df$income_hh_div_10k = i6.df$income_hh/10000
summary(i6.df$income_hh_div_10k)

# Trait: Extroversion (numeric standarized w/ mean=0)
names(select(i6.df, contains("Extro")))
summary(i6.df$extroversion)
summary(i6.df$extroversion_std)
#
names(select(i6.df, contains("agreeable")))
summary(i6.df$agreeableness)
summary(i6.df$agreeableness_std)
#
names(select(i6.df, contains("cons")))
summary(i6.df$conscientiousness)
summary(i6.df$conscientiousness_std)
#
names(select(i6.df, contains("Neuroticism")))
summary(i6.df$Neuroticism)
summary(i6.df$neuroticism_std)
#
names(select(i6.df, contains("open")))
summary(i6.df$openness)
summary(i6.df$openness_std)

# Urbanity
names(select(i6.df, contains("urban")))
str(i6.df$urban_cat)
i6.df$urban_factor = NA
i6.df$urban_factor[i6.df$urban_cat==1]="Rural"
i6.df$urban_factor[i6.df$urban_cat==2]="Mixed_rural_urban"
i6.df$urban_factor[i6.df$urban_cat==3]="Urban"
str(i6.df$urban_factor)
i6.df$urban_factor = factor(i6.df$urban_factor)
table(i6.df$urban_factor, exclude = T)

# revolving: 2 variables for the LHS
names(select(i6.df, contains("rev")))
# revolved during the past 12 months pu009 in codebook
str(i6.df$rev_12mos)
table(i6.df$rev_12mos, exclude = T)
i6.df$revolve_12m_factor = NA# making it a factor
i6.df$revolve_12m_factor[i6.df$rev_12mos==1]="Revolve_12m_yes"
i6.df$revolve_12m_factor[i6.df$rev_12mos==0]="Revolve_12m_no"
str(i6.df$revolve_12m_factor)
i6.df$revolve_12m_factor = as.factor(i6.df$revolve_12m_factor)
table(i6.df$revolve_12m_factor)

#revolved during the last month, constructed from pu010 in codebook by rev_lastmo = 1 if pu010 > 0 & rev_lastmo = 0 if pu010 == 0 & rev_lastmo = 0 if rev_12mos ==0
str(i6.df$rev_lastmo)# revolved during last month
table(i6.df$rev_lastmo, exclude = T)
i6.df$revolve_1m_factor = NA# making it a factor
i6.df$revolve_1m_factor[i6.df$rev_lastmo==1]="Revolve_1m_yes"
i6.df$revolve_1m_factor[i6.df$rev_lastmo==0]="Revolve_1m_no"
str(i6.df$revolve_1m_factor)
i6.df$revolve_1m_factor = as.factor(i6.df$revolve_1m_factor)
table(i6.df$revolve_1m_factor)

## Renaming variables for display in VIP and trees
i7.df = subset(i6.df, select = c(age,  gender_factor, race_factor, ethnicity_factor, edu_factor,  marital_factor, income_hh, work_factor, hh_size,  homeowner_factor, urban_factor, uasid, extroversion_std, agreeableness_std, conscientiousness_std, neuroticism_std, openness_std, revolve_12m_factor,  revolve_1m_factor, cc_adopt,  unbanked))
#
dim(i7.df)
colnames(i7.df)
str(i7.df)
#
colnames(i7.df)[colnames(i7.df)== "age"] = "Age"
colnames(i7.df)[colnames(i7.df)== "gender_factor"] = "Gender"
colnames(i7.df)[colnames(i7.df)== "race_factor"] = "Race"
colnames(i7.df)[colnames(i7.df)== "ethnicity_factor"] = "Ethnicity"
colnames(i7.df)[colnames(i7.df)== "edu_factor"] = "Education"
colnames(i7.df)[colnames(i7.df)== "marital_factor"] = "Marital"
colnames(i7.df)[colnames(i7.df)== "income_hh"] = "HH_income"
colnames(i7.df)[colnames(i7.df)== "work_factor"] = "Work"
colnames(i7.df)[colnames(i7.df)== "hh_size"] = "HH_size"
colnames(i7.df)[colnames(i7.df)== "homeowner_factor"] = "Homeowner"
colnames(i7.df)[colnames(i7.df)== "urban_factor"] = "Urbanicity"
colnames(i7.df)[colnames(i7.df)== "extroversion_std"] = "Extroversion"
colnames(i7.df)[colnames(i7.df)== "agreeableness_std"] = "Agreeableness"
colnames(i7.df)[colnames(i7.df)== "conscientiousness_std"] = "Conscientiousness"
colnames(i7.df)[colnames(i7.df)== "neuroticism_std"] = "Neuroticism"
colnames(i7.df)[colnames(i7.df)== "openness_std"] = "Openness"

### Preparing data for RF and trees (revolvers)
names(select(i7.df, contains("adopt")))
table(i7.df$cc_adopt, exclude = T)
# restricting the revolving data to CC adopters only
rev1.df = subset(i7.df, cc_adopt==1)
dim(rev1.df)

# restricting to actually used variables
rev2.df = rev1.df
table(rev2.df$unbanked)# too few => remove
#
rev3.df = subset(rev2.df, select = c(Age,  Gender, Race, Ethnicity, Education,  Marital, HH_income, Work, HH_size,  Homeowner, Urbanicity, uasid, Extroversion, Agreeableness, Conscientiousness, Neuroticism, Openness, revolve_12m_factor,  revolve_1m_factor))
#
dim(rev3.df)
# last preview of all variables
summary(rev3.df$Age)
table(rev3.df$Gender, exclude = T)
table(rev3.df$Race, exclude = T)
table(rev3.df$Ethnicity, exclude = T)
table(rev3.df$Education, exclude = T)
table(rev3.df$Marital, exclude = T)
summary(rev3.df$HH_income)
table(rev3.df$Work, exclude = T)
table(rev3.df$HH_size, exclude = T)
table(rev3.df$Homeowner, exclude = T)
table(rev3.df$Urbanicity, exclude = T)
summary(rev3.df$Extroversion)
summary(rev3.df$Agreeableness)
summary(rev3.df$Conscientiousness)
summary(rev3.df$Neuroticism)
summary(rev3.df$Openness)
# LHS variables
table(rev3.df$revolve_12m_factor)
table(rev3.df$revolve_1m_factor)
# 
names(rev3.df)

## models revolved 12 months for trees and random forest
# model 1 has all variables. 
rev_12mos_model1 = revolve_12m_factor ~ Age + Gender + Race + Ethnicity + Education + Marital + HH_income + Work + HH_size + Homeowner + Urbanicity + Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness
# Model 2: 5 traits only on the RHS
rev_12mos_model2 = revolve_12m_factor ~ Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness


# Random forest VIP of the revolved 12 months model (all variables)
#
set.seed(1955)
rev_12mos.rf=randomForest(rev_12mos_model1, data=rev3.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(rev_12mos.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(rev_12mos.rf, type = 1, main ='Revolved during the past 12 months: Explanatory variable importance plot', bg = "blue", cex=1)#default type 1&2, main='' will delete title

# Random forest VIP of the revolved 12 months model (5 traits only)
#
set.seed(1955)
rev_12mos_5var.rf=randomForest(rev_12mos_model2, data=rev3.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(rev_12mos_5var.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(rev_12mos_5var.rf, type = 1, main ='Revolved during the past 12 months: Explanatory variable importance plot', bg = "blue", cex=1)#default type 1&2, main='' will delete title


## Classification tree revolved past 12 months full model
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
rev_12mos.tree = rpart(rev_12mos_model1, data = rev3.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
#prp(rev_12mos.tree, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(rev_12mos.tree)# plot cp: Not used for this demo plot. See training data below
names(rev_12mos.tree)
rev_12mos.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = rev_12mos.tree$cptable[5, "CP"]) # for prunning 6 for long and 5 for a short tree
rev_12mos_prune = prune.rpart(rev_12mos.tree, cp=cp.choice)
#
prp(rev_12mos_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main='Revolved during the past 12 months: Classification tree')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.
#
# Classification tree revolved past 12 months: 5 Traits only
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
rev_12mos_5var.tree = rpart(rev_12mos_model2, data = rev3.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
#prp(rev_12mos.tree, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(rev_12mos_5var.tree)# plot cp: Not used for this demo plot. See training data below
#names(rev_12mos.tree)
rev_12mos_5var.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = rev_12mos_5var.tree$cptable[2, "CP"]) # for prunning 6 for long and 5 for a short tree
rev_12mos_5var_prune = prune.rpart(rev_12mos_5var.tree, cp=cp.choice)
#
prp(rev_12mos_5var_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, )#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.
# main='Revolved during the past 12 months: Classification tree' Caption removed from paper.


## models revolved last 1 month for trees and random forest
# model 1 has all variables. 
rev_1m_model1 = revolve_1m_factor ~ Age + Gender + Race + Ethnicity + Education + Marital + HH_income + Work + HH_size + Homeowner + Urbanicity + Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness
#
# model 2 has 5 traits only
rev_1m_model2 = revolve_1m_factor ~ Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness


# Random forest VIP of the revolved 1 month model
#
set.seed(1955)
rev_1m.rf=randomForest(rev_1m_model1, data=rev3.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(rev_1m.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(rev_1m.rf, type = 1, main ='Revolved during last month: Explanatory variable importance plot', bg = "blue", cex=1)#default type 1&2, 

# Random forest VIP of the revolved 1 month model (5 traits only)
#
set.seed(1955)
rev_1m_5var.rf=randomForest(rev_1m_model2, data=rev3.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(rev_1m_5var.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(rev_1m_5var.rf, type = 1, main ='Revolved during the last month: Explanatory variable importance plot', bg = "blue", cex=1)#default type 1&2, main='' will delete title


## Classification tree revolved last 1 month: All variables
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
rev_1m.tree = rpart(rev_1m_model1, data = rev3.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
#prp(rev_12mos.tree, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(rev_1m.tree)# plot cp: Not used for this demo plot. See training data below
names(rev_1m.tree)
rev_1m.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = rev_1m.tree$cptable[6, "CP"]) # for prunning 6 for short (according to good cp and 7 for overfitting long)
rev_1m_prune = prune.rpart(rev_1m.tree, cp=cp.choice)
#
prp(rev_1m_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main='Revolved during last month: Classification tree')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.

## Classification tree revolved last 1 month: 5 traits only
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
rev_1m_5var.tree = rpart(rev_1m_model2, data = rev3.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
#prp(rev_12mos.tree, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(rev_1m_5var.tree)# plot cp: Not used for this demo plot. See training data below
names(rev_1m_5var.tree)
rev_1m_5var.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = rev_1m_5var.tree$cptable[3, "CP"]) # for prunning 6 for short (according to good cp and 7 for overfitting long)
rev_1m_5var_prune = prune.rpart(rev_1m_5var.tree, cp=cp.choice)
#
prp(rev_1m_5var_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main='Revolved during last month: Classification tree')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.

# end Revolver trees

### Start credit card adoption cc_adopt
str(i7.df$cc_adopt)
table(i7.df$cc_adopt, exclude = T)
#

cc1.df = subset(i7.df, select = c(cc_adopt, Age,  Gender, Race, Ethnicity, Education,  Marital, HH_income, Work, HH_size,  Homeowner, Urbanicity, uasid, Extroversion, Agreeableness, Conscientiousness, Neuroticism, Openness, revolve_12m_factor,  revolve_1m_factor))
#
# Construct cc adopt factor
cc2.df = cc1.df
cc2.df$Credit_card = NA
cc2.df$Credit_card = ifelse(cc2.df$cc_adopt==1, "Has credit card", "No credit card")
table(cc2.df$cc_adopt, exclude = T)
table(cc2.df$Credit_card, exclude = T)
prop.table(table(cc2.df$Credit_card, exclude = T))
str(cc2.df$Credit_card)
cc2.df$Credit_card = as.factor(cc2.df$Credit_card)
levels(cc2.df$Credit_card)
#levels(cc2.df$Credit_card) = c("No credit card", "Has credit card")# changing level orders so has CC will be on the right side in classification trees => Generates error (reversed classification)!

# Construct synthetic dataset with equal number of cc adopters and non-adopters
set.seed(1955)
cc2_syn.df = smote(Credit_card ~ ., cc2.df, perc.over = 2, perc.under = 1.5)
table(cc2_syn.df$Credit_card)

## models "has credit card" (cc_adopt) for trees and random forest
# model 1 has all variables. 
cc_adopt_model1 = Credit_card ~ Age + Gender + Race + Ethnicity + Education + Marital + HH_income + Work + HH_size + Homeowner + Urbanicity + Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness
#
# model 2 has 5 traits only
cc_adopt_model2 = Credit_card ~ Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness

# Random forest VIP cc_adopt [original non-syn dataset]
#
set.seed(1955)
cc_adopt.rf=randomForest(cc_adopt_model1, data=cc2.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(cc_adopt.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(cc_adopt.rf, type = 1, main ='Credit card adoption: Explanatory variable importance plot (original dataset)', bg = "blue", cex=1)#default type 1&2, 

# Random forest VIP [Synthetic dataset] (all variables)
#
set.seed(1955)
cc_adopt_syn.rf=randomForest(cc_adopt_model1, data=cc2_syn.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(cc_adopt_syn.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(cc_adopt_syn.rf, type = 1, main ='Credit card adoption: Explanatory variable importance plot (synthetic dataset)', bg = "blue", cex=1)#default type 1&2, main='' will delete title

# Random forest VIP [Synthetic dataset] (5 traits only)
#
set.seed(1955)
cc_adopt_syn_5var.rf=randomForest(cc_adopt_model2, data=cc2_syn.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(cc_adopt_syn_5var.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(cc_adopt_syn_5var.rf, type = 1, main ='Credit card adoption: Explanatory variable importance plot (synthetic dataset)', bg = "blue", cex=1)#default type 1&2, main='' will delete title

## Classification tree: cc_adopt [original non-syn dataset] 
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
cc_adopt.tree = rpart(cc_adopt_model1, data = cc2.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
plotcp(cc_adopt.tree)# plot cp: Not used for this demo plot. See training data below
names(cc_adopt.tree)
cc_adopt.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = cc_adopt.tree$cptable[7, "CP"]) # for prunning 6 for long and 5 for a short tree
cc_adopt_prune = prune.rpart(cc_adopt.tree, cp=cp.choice)
#
prp(cc_adopt_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2,  main ='Credit card adoption: Classification tree (original dataset)')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.

## Classification tree: cc_adopt [synthetic dataset] (all variables)
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
cc_adopt_syn.tree = rpart(cc_adopt_model1, data = cc2_syn.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
plotcp(cc_adopt_syn.tree)# plot cp: Not used for this demo plot. See training data below
names(cc_adopt_syn.tree)
cc_adopt_syn.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = cc_adopt_syn.tree$cptable[8, "CP"]) # for pruning 9 for long and 5 for a short tree
cc_adopt_syn_prune = prune.rpart(cc_adopt_syn.tree, cp=cp.choice)
#
prp(cc_adopt_syn_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main = 'Credit card adoption: Classification tree (synthetic dataset)')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.
#
## Classification tree: cc_adopt [synthetic dataset] (5 traits only)
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
cc_adopt_syn_5var.tree = rpart(cc_adopt_model2, data = cc2_syn.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
plotcp(cc_adopt_syn_5var.tree)# plot cp: Not used for this demo plot. See training data below
#names(cc_adopt_syn.tree)
cc_adopt_syn_5var.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = cc_adopt_syn_5var.tree$cptable[6, "CP"]) # for pruning 9 for long and 5 for a short tree
cc_adopt_syn_5var_prune = prune.rpart(cc_adopt_syn_5var.tree, cp=cp.choice)
#
prp(cc_adopt_syn_5var_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main = 'Credit card adoption: Classification tree (synthetic dataset)')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.

# End card adoption

### Start unbanked RF and classification trees
str(i7.df$unbanked)
table(i7.df$unbanked, exclude = T)
#
unbanked1.df = subset(i7.df, select = c(unbanked, Age,  Gender, Race, Ethnicity, Education,  Marital, HH_income, Work, HH_size,  Homeowner, Urbanicity, uasid, Extroversion, Agreeableness, Conscientiousness, Neuroticism, Openness, revolve_12m_factor,  revolve_1m_factor))
#
unbanked1.df$unbanked_factor = NA
unbanked1.df$unbanked_factor = ifelse(unbanked1.df$unbanked==1, "Unbanked", "Banked") 
table(unbanked1.df$unbanked_factor)
str(unbanked1.df$unbanked_factor)
unbanked1.df$unbanked_factor = as.factor(unbanked1.df$unbanked_factor)

#
# Construct synthetic dataset with equal number of banked and unbanked
set.seed(1955)
unbanked1_syn.df = smote(unbanked_factor ~ ., unbanked1.df, perc.over = 2, perc.under = 1.5)
table(unbanked1_syn.df$unbanked_factor)

## model unbanked_factor for trees and random forest
# model 1 has all variables. 
unbanked_model1 = unbanked_factor ~ Age + Gender + Race + Ethnicity + Education + Marital + HH_income + Work + HH_size + Homeowner + Urbanicity + Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness
#
# model 2 has only 5 traits
unbanked_model2 = unbanked_factor ~ Extroversion + Agreeableness + Conscientiousness + Neuroticism + Openness

# Random forest VIP unbanked [original non-syn dataset]
#
set.seed(1955)
unbanked.rf=randomForest(unbanked_model1, data=unbanked1.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(unbanked.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(unbanked.rf, type = 1, main ='Bank account adoption: Explanatory variable importance plot (original dataset)', bg = "blue", cex=1)#default type 1&2, 

# Random forest VIP [Synthetic dataset] (all variables)
#
set.seed(1955)
unbanked_syn.rf=randomForest(unbanked_model1, data=unbanked1_syn.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(unbanked_syn.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(unbanked_syn.rf, type = 1, main ='Bank account adoption: Explanatory variable importance plot (synthetic dataset)', bg = "blue", cex=1)#default type 1&2, main='' will delete title

# Random forest VIP [Synthetic dataset] (5 traits only)
#
set.seed(1955)
unbanked_syn_5var.rf=randomForest(unbanked_model2, data=unbanked1_syn.df, mtry=3, importance=T, na.action=na.roughfix)
#
importance(unbanked_syn_5var.rf) # Table of variable importance
# Below, Plot of variable importance (displayed in paper)
varImpPlot(unbanked_syn_5var.rf, type = 1, main ='Bank account adoption: Explanatory variable importance plot (synthetic dataset)', bg = "blue", cex=1)#default type 1&2, main='' will delete title

## Classification tree: unbanked [original non-syn dataset] (all variables)
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
unbanked.tree = rpart(unbanked_model1, data = unbanked1.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#
plotcp(unbanked.tree)# plot cp: Not used for this demo plot. See training data below
names(unbanked.tree)
unbanked.tree$cptable # List cp, number of splits and errors => Bank, increasing!
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = cc_adopt.tree$cptable[4, "CP"]) # for prunning unreliable because increasing cp
unbanked_prune = prune.rpart(unbanked.tree, cp=cp.choice)
#
prp(unbanked_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2,  main ='Bank account adoption: Classification tree (original dataset)')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.

## Classification tree: unbanked [synthetic dataset] (all variables)
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
unbanked_syn.tree = rpart(unbanked_model1, data = unbanked1_syn.df, method = "class", control = #
rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#
plotcp(unbanked_syn.tree)# plot cp: Not used for this demo plot. See training data below
names(cc_adopt_syn.tree)
unbanked_syn.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = cc_adopt_syn.tree$cptable[9, "CP"]) # for pruning 
unbanked_syn_prune = prune.rpart(unbanked_syn.tree, cp=cp.choice)
#
prp(unbanked_syn_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main = 'Bank account adoption: Classification tree (synthetic dataset)')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.

# Classification tree: unbanked [synthetic dataset] (5 traits only)
# run rpart
set.seed(1955)# to be able to reproduce the rpart CV below
unbanked_syn_5var.tree = rpart(unbanked_model2, data = unbanked1_syn.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#
plotcp(unbanked_syn_5var.tree)# plot cp: Not used for this demo plot. See training data below
#names(cc_adopt_syn.tree)
unbanked_syn_5var.tree$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning (highest rel error below the dashed line)
(cp.choice = cc_adopt_syn_5var.tree$cptable[5, "CP"]) # for pruning 
unbanked_syn_5var_prune = prune.rpart(unbanked_syn_5var.tree, cp=cp.choice)
#
prp(unbanked_syn_5var_prune, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA,  under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, main = 'Bank account adoption: Classification tree (synthetic dataset)')#faclet=0 avoids abbreviations, tweak for char size. I remove extra = 100 to remove percentage of observations.



# End card adoption


