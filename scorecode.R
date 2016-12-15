# Import and load data files and libraries
setwd("../Users/kphas_000/Desktop/PREDICT 411/Unit02/Insurance")
train <- read.csv("logit_insurance.csv",header=TRUE,na.strings=c("","NA"))
test <- read.csv("logit_insurance_test.csv",header=TRUE,na.strings=c("","NA"))
prob <- read.csv("submission/DTMODEL.csv",header=TRUE)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(useful)
library(party)

# combine train and test datasets for data preparation
combi <- rbind(train,test)

# check data
str(combi)
summary(combi)

# convert dollar columns to numeric
cur2num <- function(x) {as.numeric(sub(',','',(sub('\\$','',as.character(x)))))}
combi$N_INCOME <- cur2num(combi$INCOME)
combi$N_HOME_VAL <- cur2num(combi$HOME_VAL)
combi$N_BLUEBOOK <- cur2num(combi$BLUEBOOK)
combi$N_OLDCLAIM <- cur2num(combi$OLDCLAIM)
head(combi)

# drop the old columns
combi$TARGET_AMT <- NULL
combi$INCOME <- NULL
combi$HOME_VAL <- NULL
combi$BLUEBOOK <- NULL
combi$OLDCLAIM <- NULL

summary(combi)

# there is a negative value which makes no sense so we fix it
combi$CAR_AGE[6941] <- 3  # most likely the -3 was typo and should have been 3
summary(combi)

## impute columns with missing values using decision tree
imp_age <- rpart(AGE ~ . - TARGET_FLAG, data=combi[!is.na(combi$AGE),], method="anova")
combi$AGE[is.na(combi$AGE)] <- predict(imp_age, combi[is.na(combi$AGE),])
fancyRpartPlot(imp_age, main="Decision Tree for AGE",sub="")

imp_yoj <- rpart(YOJ ~ . - TARGET_FLAG, data=combi[!is.na(combi$YOJ),], method="anova")
combi$YOJ[is.na(combi$YOJ)] <- predict(imp_yoj, combi[is.na(combi$YOJ),])
fancyRpartPlot(imp_yoj, main="Decision Tree for YOJ",sub="")

imp_car <- rpart(CAR_AGE ~ . - TARGET_FLAG, data=combi[!is.na(combi$CAR_AGE),], method="anova")
combi$CAR_AGE[is.na(combi$CAR_AGE)] <- predict(imp_car, combi[is.na(combi$CAR_AGE),])
fancyRpartPlot(imp_car, main="Decision Tree for CAR_AGE",sub="")

imp_income <- rpart(N_INCOME ~ . - TARGET_FLAG, data=combi[!is.na(combi$N_INCOME),], method="anova")
fancyRpartPlot(imp_income)  # too many levels to code so let's prune the tree
plotcp(imp_income)
printcp(imp_income)
imp_income2 <- prune(imp_income,cp=0.05)
fancyRpartPlot(imp_income2, main="Decision Tree for INCOME",sub="")
combi$N_INCOME[is.na(combi$N_INCOME)] <- predict(imp_income2, combi[is.na(combi$N_INCOME),])

imp_homeval <- rpart(N_HOME_VAL ~ . - TARGET_FLAG, data=combi[!is.na(combi$N_HOME_VAL),], method="anova")
fancyRpartPlot(imp_homeval)
plotcp(imp_homeval)
printcp(imp_homeval)
imp_homeval2 <- prune(imp_homeval,cp=0.05)
fancyRpartPlot(imp_homeval2, main="Decision Tree for HOMEVAL",sub="")
combi$N_HOME_VAL[is.na(combi$N_HOME_VAL)] <- predict(imp_homeval2, combi[is.na(combi$N_HOME_VAL),])

imp_job <- rpart(JOB ~ . - TARGET_FLAG, data=combi[!is.na(combi$JOB),], method="class")
fancyRpartPlot(imp_job)
plotcp(imp_job)
printcp(imp_job)
imp_job2 <- prune(imp_job,cp=0.05)
fancyRpartPlot(imp_job2, main="Decision Tree for JOB",sub="")
combi$JOB[is.na(combi$JOB)] <- predict(imp_job2, combi[is.na(combi$JOB),],type="class")

# check to make sure NAs are gone
summary(combi)
anyNA(combi[,!names(combi) %in% "TARGET_FLAG"])

# cap extreme values
summary(combi$N_HOME_VAL)
combi$N_HOME_VAL[combi$N_HOME_VAL > 497746] <- 497746
summary(combi$N_HOME_VAL)

summary(combi$N_BLUEBOOK)
combi$N_BLUEBOOK[combi$N_BLUEBOOK > 39090] <- 39090
summary(combi$N_BLUEBOOK)

summary(combi$TRAVTIME)
combi$TRAVTIME[combi$TRAVTIME > 75] <- 75
summary(combi$TRAVTIME)

# split out the imputed datasets back to training and test
train <- combi[1:8161,]
test <- combi[8162:10302,]
anyNA(train)
dim(train)
dim(test)

# fit full model
fit <- rpart(TARGET_FLAG ~ . - INDEX, data=train, method="class") # will get prob two columns for 0 (no crash) and 1 (crash)
summary(fit)
fancyRpartPlot(fit)

# write score file
Prediction <- predict(fit, test)
submit <- data.frame(INDEX = test$INDEX, P_TARGET_FLAG = Prediction)
colnames(submit)[2] <- "P_TARGET_FLAG"
submit$P_TARGET_FLAG.1 <- NULL
head(submit)
write.csv(submit, file = "model.csv", row.names = FALSE)



## Other imputation/modeling methods such as K-Nearest Neighbors and Random Forests
# impute using mice
library(mice)
miceImp <- mice(combi[,!names(combi) %in% "TARGET_FLAG"],method="rf")
miceOut <- complete(miceImp)
anyNA(miceOut)

miceFull <- cbind(miceOut,TARGET_FLAG=combi$TARGET_FLAG) # bring TARGET_FLAG back into data so we can run model

train <- miceFull[1:8161,]
test <- miceFull[8162:10302,]
anyNA(train)
anyNA(test)

set.seed(222)
fit <- cforest(TARGET_FLAG ~ . - INDEX, data=train, controls=cforest_unbiased(ntree=2000))
fit

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(INDEX = test$INDEX, P_TARGET_FLAG = Prediction)
colnames(submit)[2] <- "P_TARGET_FLAG"
write.csv(submit, file = "miceForestCap.csv", row.names = FALSE)

## impute missing values using kNN
library(DMwR)
knnOut <- knnImputation(combi[,!names(combi) %in% "TARGET_FLAG"])
anyNA(knnOut)
summary(knnOut)





library(data.table)
## predict losses extra credit using mean of $1504?
summary(train$TARGET_AMT)
str(prob)
prob <- as.data.table(prob)
prob[,P_TARGET_AMT:=P_TARGET_FLAG*1504]
prob[,P_TARGET_FLAG:=NULL]
prob
write.csv(prob, file="Ins_Losses.csv",row.names=FALSE)
