# Analysis on Dataset provided from:
# Regression Technique is used to do the House Price Predict on the field medv
# Author: Abinash Anand G(DBA17002), Anne Grace(DBA17004) & MadanaGanesh (DBA17009)
# Created Date: 19/02/2018
#*********************************************************************************
## Steps :
## load the data
## EDA
## GENERATE BASE MODEL and other models
## scale numeric data
## Feature engeering  and feature selecion
## observation :
#As per the analysis the RMSE values have significantly increased and the best RMSE value has
#been obtained in Random Forest with the value of 2.78
#*********************************************************************************#
#**************************** Load Data ******************************************#
#*********************************************************************************#

### Set current working dir
setwd('D:/AdvR/Final/')

#Reading the data from csv file and storing in a data frame
#raw_data <- read.csv("gp1.csv",header = TRUE)
raw_data = scan(file.choose());

#Structure of the Date Frame created from csv
str(raw_data) ## 506 observations and 15 Features

#Summary of the data set - Provide the Mean, Median, Mode and Quratile values of all the columns
summary(raw_data)

#Data is in the form of data.frame
class(raw_data)

#Removal of all the invalid data with NA and storing in Terminal_Corrected Data from Terminal_Data
mydata <- raw_data[complete.cases(raw_data), ]

#No NA's or missing values and the total cells are 7590.
table(is.na(mydata))

#*********************************************************************************#
#************************ Exploratory Data Analysis *****************************#
#*********************************************************************************#

summary(mydata) ## This also confirms there is no missing values in the dataset


## Observation : Data is not perfectly normal distribution and has outliers.
hist(mydata$medv)
qqnorm(mydata$medv)
qqline(mydata$medv)

#Density plot which shows that there are certain oulliers.
den= density(mydata$medv)
plot(den ,main=" Density plot of Housing Price based by Median Value ")
polygon(den, col="blue", border="green")


##Box plot
## Observation : Box plot based on median values shows clear outliers
boxplot(mydata$medv)
range(mydata$medv)

## Find correlation using scatterplot ,pairs and  Corrplot
## Observation  :  indus and nox has strong positive correlation , lstat with zone has negative correlation
pairs(~zn+chas+nox+tax+black+lstat,data=mydata,main="Scatterplot",pch=3)

#It is also obviously evident that INDUS and NOX are strongly positively correlated with one another, as nitric oxide levels tend to go up with increase in industries.
plot(mydata[,c(4,6,7,12,14,15)],pch=3)

plot(mydata$indus,mydata$nox) ## positive correlation
plot(mydata$zn,mydata$lstat) ## negative correlation

# RM has the strongest positive correlation
# LSTAT and the pupil-teacher ratio, PTRATIO, have strong negative correlation.
# Least correlation to MEDV is CHAS.
cor(mydata,mydata$medv)

# Calculate near zero variance 
# Output shows that there are no variable with zero or near zero variance.
nzv <- nearZeroVar(mydata, saveMetrics = TRUE)
sum(nzv$nzv)

#Correlation Plot
library(corrplot)
c = mydata[,2:15]
corr=cor(c,method="s")
corrplot(corr,method = 'number')
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients.

corrplot(corr, type="lower")

# Table format to check correction of median value against all the other variable
# observation : Room - strong positive correlation, lstat - strong negative correlation
library(caret)
cor(c,c$medv)


#Data Scaling
#Output prediction variable median value should be excluded from scaling
scale <- cbind(scale(c[1:13]), mydata[15])
head(scale)

#Splitting of data
#Creating Partition of data from the dataframe
train_data <- createDataPartition(scale$medv,p = 0.7,list = FALSE)

#Pushing the 70% of the data of train_data from scale into training
train <- scale[train_data,]
#Pushing remaining 30% of the data of scale into testing
test <- scale[-train_data,]
#Total number of rows in training 
NROW(train)
#Total number of rows in Terminal_Data 
NROW(test)

#*********************************************************************************#
#******************************* Model Creation **********************************#
#*********************************************************************************#

#LINEAR MODEL 1
#Using All the Parameters
set.seed(150)
base_mdl = lm(medv~.,data=train)
summary(base_mdl) ## Rsquare :  0.7208
#Value of the Coeffeciants
base_mdl$coefficients

#prediction
pred_base_mdl = predict(base_mdl,newdata = test)


#Output of both RMSE and R Square
rmse.lm <- sqrt(sum((pred_base_mdl - test$medv)^2)/length(test$medv))
c(RMSE = rmse.lm, R2 = summary(base_mdl)$r.squared)
#RMSE - 6.1044456
#R Square  - 0.7874536


#*********************************************************************************#

#LINEAR MODEL2(TAKING LOGS)


#Since there is more variance in median value lets take log on it
set.seed(150)


mdl2 = lm(log(medv)~.,data=train)
summary(mdl2) ## Rsquare :  0.7307
## coffefients value
mdl2$coefficients


## prediction
pre_mdl2 = predict(mdl2,newdata = test)

##Output of both RMSEa nd R Square
## since we took log on medv ( output prediction variable), need to exponential while calculatin RMSE
rmse.mdl2 <- sqrt(sum((exp(pre_mdl2) - test$medv)^2)/length(test$medv))
c(RMSE = rmse.mdl2, R2 = summary(mdl2)$r.squared)
## RMSE - 5.7299446
## R Square  - 0.8087551


#*********************************************************************************#

#LINEAR MODEL3

set.seed(150)
mdl1 = lm(medv~crim+chas+rm+zn+age+dis,data=train)
summary(mdl1) ## Rsquare :  0.7307
#Value of the Coeffeciants
mdl1$coefficients

#Prediction
pre_mdl1 = predict(mdl1,newdata = test)

#Output of both RMSEa nd R Square
rmse.mdl1 <- sqrt(sum((pre_mdl1 - test$medv)^2)/length(test$medv))
c(RMSE = rmse.mdl1, R2 = summary(mdl1)$r.squared)
#RMSE - 7.3646127
#R Square  - 0.6839942

#*********************************************************************************#

#LINEAR MODEL 4 (LOGS)

## since there is more variance in median value lets take log on it
set.seed(150)
mdl3 = lm(log(medv)~crim+chas+rm+zn+age+dis,data=train)
summary(mdl3) ## Rsquare :  0.7307
## coffefients value
mdl3$coefficients

## prediction
pre_mdl3 = predict(mdl3,newdata = test)

##Output of both RMSEa nd R Square
## since we took log on medv ( output prediction variable), need to exponential while calculatin RMSE
rmse.mdl3 <- sqrt(sum((exp(pre_mdl3) - test$medv)^2)/length(test$medv))
c(RMSE = rmse.mdl3, R2 = summary(mdl3)$r.squared)
## RMSE - 7.1439079
## R Square  - 0.6693411

#*********************************************************************************#

#Subset Regression

#regsubsets only takes data frame as input
library(leaps)
subset_result = regsubsets(medv~.,data=train, nbest=2, nvmax = 14)
summary(subset_result)
plot(subset_result, scale="bic")

#Forward/Backward/Stepwise Regression Using AIC

nullmodel=lm(medv~1, data=train)
fullmodel=lm(medv~., data=train)

#Backward Elimination
model.step = step(fullmodel,direction='backward')

#Forward Selection
model.step = step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')

#Stepwise Selection (Output Omitted)
model.step=step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')

extractAIC(mdl1)  #7.000 1169.379
AIC(mdl1)  #181.663

extractAIC(mdl2)  #14.000 -1220.331
AIC(mdl2)  #208.0471

#*********************************************************************************#
#Regularisation Lasso

##Normal Data
str(train)
x.tr <- model.matrix(medv~., data = train)[,-1]; y.tr <- train$medv
x.val <- model.matrix(medv~., data = test)[,-1]; y.val <- test$medv;
library(glmnet)
set.seed(10)
rr.cv <- cv.glmnet(x.tr, y.tr, alpha = 0); plot(rr.cv)
rr.bestlam <- rr.cv$lambda.min; log(rr.bestlam) # -0.2855522
rr.goodlam <- rr.cv$lambda.1se; log(rr.goodlam) # 1.296021
rr.fit <- glmnet(x.tr, y.tr, alpha = 0)
plot(rr.fit, xvar = "lambda", label = TRUE)

#Accuracy Prediction
rr.pred <- predict(rr.fit, s = rr.bestlam, newx = x.tr)
sqrt(mean((rr.pred - y.tr)^2)) # Train accuracy with glmnet # 4.848816
model<-lm(medv~.,data=train); summary(model)
pred <- predict(model,train)
sqrt(mean((pred - train$medv)^2)) # Train accuracy with normal model # 4.155459
rr.pred <- predict(rr.fit, s = rr.bestlam, newx = x.val)
sqrt(mean((rr.pred - y.val)^2)) # Test Accuracy with glmnet # 6.180217
pred <- predict(model,test)
sqrt(mean((pred - test$medv)^2)) # Test Accuracy with normal model # 6.104446


#*********************************************************************************#
#5-fold Cross Validation

#We need to use glm instead of lm to fit the model (if we want to use cv.glm fucntion in boot package)
#The default measure of performance is the Mean Squared Error (MSE). 
#If we want to use another measure we need to define a cost function.
library(boot)
mdl5 = glm(medv~indus + rm, data = mydata)
cv.glm(data = mydata, glmfit = mdl5, K = 5)$delta[2]
#40.79736 - MSE

#LOOCV (Leave-one-out Cross Validation)
cv.glm(data = mydata, glmfit = mdl5, K = nrow(mydata))$delta[2]
#39.940 - MSE

#*********************************************************************************#

#BAGGING

## Bagging
library(ipred)
set.seed(150)
fit.bag <- bagging(medv~., data = train)
fit.bag <- bagging(medv~crim+chas+rm+zn+age+dis,data=train)
summary(fit.bag)

bag.predict <- predict(fit.bag, test, type = "regression") ## test is part of train dataset
sqrt(mean((test$medv -bag.predict)^2))
#Best RMSE in Bagging:6.952446

#After doing Bagging we see that the RMSE Value has decreased by 1.
#*********************************************************************************#

#Decision Tree

library(rpart)
set.seed(100)

decision_fit <- rpart(medv~.,method="anova", data=train,control = rpart.control(cp = 0.02))
summary(decision_fit)
decision.predict <- predict(decision_fit, test)
decision.predict
actuals = test$medv
decision_rmsepreds <- sqrt(mean((actuals - decision.predict)^2))
decision_rmsepreds # rmse value::::6.317116
#*********************************************************************************#

#Random Forest

require(randomForest)
set.seed(10)
fit.rf <- randomForest(medv~.,data=train) 
rf.predict <- predict(fit.rf, test, type = "response")
random_rmsepreds<-sqrt(mean((test$medv -rf.predict)^2))
random_rmsepreds
#RMSE VALUE: 5.058421

#*********************************************************************************#
#************************** Feature Engineering **********************************#
#*********************************************************************************#

## FEATURE ENGINEERING
#SQUARING AND CUBING NUMERIC DATA AND CREATING NEW COLUMNS
head(mydataf)
mydataf <- mydata
mydataf$attr1 <- mydataf$indus^2
mydataf$attr2 <- mydataf$indus^3
mydataf$attr3 <- mydataf$rm^2
mydataf$attr4<- mydataf$rm^3
mydataf$attr5 <- mydataf$age^2
mydataf$attr6<- mydataf$age^3
mydataf$attr7 <- mydataf$indus^2
mydataf$attr8 <- mydataf$indus*mydataf$rm
mydataf$attr9 <- mydataf$age*mydataf$rm
mydataf$attr10 <- mydataf$age*mydataf$indus
mydataf$attr11 <- mydataf$indus*(mydataf$rm^2) 
mydataf$attr12 <- (mydataf$indus^2)*mydataf$rm
summary(mydataf)


#Data Scaling
#Output prediction variable median value should be excluded from scaling

fmydata <- mydataf[, c(1:14,16:27,15)]
str(fmydata)
dim(fmydata)
fscale <- cbind(scale(fmydata[1:26]), fmydata[27])
head(fscale)

#Splitting of data
#Creating Partition of data from the dataframe
ftrain_data <- createDataPartition(fscale$medv,p = 0.7,list = FALSE)

#Pushing the 70% of the data of train_data from scale into training
ftrain <- fscale[ftrain_data,]
#Pushing remaining 30% of the data of scale into testing
ftest <- fscale[-ftrain_data,]
#Total number of rows in training 
NROW(ftrain)
#Total number of rows in Terminal_Data 
NROW(ftest)
#********************************************************************************#


#LINEAR MODEL 1
#Using All the Parameters
set.seed(150)
base_mdl = lm(medv~.,data=ftrain)
summary(base_mdl) ## Rsquare :  0.7208
#Value of the Coeffeciants
base_mdl$coefficients

#prediction
pred_base_mdl = predict(base_mdl,newdata = ftest)


#Output of both RMSE and R Square
rmse.lm <- sqrt(sum((pred_base_mdl - ftest$medv)^2)/length(ftest$medv))
c(RMSE = rmse.lm, R2 = summary(base_mdl)$r.squared)
#RMSE - 3.8226286
#R Square  - 0.8200235

# we see that after feature Engineering the value of RMSE has improved a lot.


#*********************************************************************************#

#LINEAR MODEL2(TAKING LOGS)


#Since there is more variance in median value lets take log on it
set.seed(150)


mdl2 = lm(log(medv)~.,data=ftrain)
summary(mdl2) ## Rsquare :  0.7307
## coffefients value
mdl2$coefficients


## prediction
pre_mdl2 = predict(mdl2,newdata = ftest)

##Output of both RMSEa nd R Square
## since we took log on medv ( output prediction variable), need to exponential while calculatin RMSE
rmse.mdl2 <- sqrt(sum((exp(pre_mdl2) - ftest$medv)^2)/length(ftest$medv))
c(RMSE = rmse.mdl2, R2 = summary(mdl2)$r.squared)
## RMSE - 3.8248589
## R Square  - 0.8291162


#*********************************************************************************#

#BAGGING

## Bagging
library(ipred)
set.seed(150)
fit.bag <- bagging(medv~., data = ftrain)
fit.bag <- bagging(medv~crim+chas+rm+zn+age+dis,data=ftrain)
summary(fit.bag)

bag.predict <- predict(fit.bag, ftest, type = "regression") ## ftest is part of ftrain dataset
sqrt(mean((ftest$medv -bag.predict)^2))
#Best RMSE in Bagging:3.438

#After doing Bagging we see that the RMSE Value has decreased by 1.
#*********************************************************************************#
#Regularization - Lasso

str(ftrain)
x.tr <- model.matrix(medv~., data = ftrain)[,-1]; y.tr <- ftrain$medv
x.val <- model.matrix(medv~., data = ftest)[,-1]; y.val <- ftest$medv;
library(glmnet)
set.seed(10)
rr.cv <- cv.glmnet(x.tr, y.tr, alpha = 0); plot(rr.cv)
rr.bestlam <- rr.cv$lambda.min; log(rr.bestlam) # -0.3098624
rr.goodlam <- rr.cv$lambda.1se; log(rr.goodlam) # 1.550812
rr.fit <- glmnet(x.tr, y.tr, alpha = 0)
plot(rr.fit, xvar = "lambda", label = TRUE)
rr.pred <- predict(rr.fit, s = rr.bestlam, newx = x.tr)
sqrt(mean((rr.pred - y.tr)^2)) # Train accuracy with glmnet #4.242984

model<-lm(medv~.,data=ftrain); summary(model)
pred <- predict(model,ftrain)
sqrt(mean((pred - ftrain$medv)^2)) # Train accuracy with normal model # 3.84195
rr.pred <- predict(rr.fit, s = rr.bestlam, newx = x.val)
sqrt(mean((rr.pred - y.val)^2)) # Test Accuracy with glmnet # 4.66537
pred <- predict(model,ftest)
sqrt(mean((pred - ftest$medv)^2)) # Test Accuracy with normal mode # 3.986894

#**********************************************************************#

#Forward/Backward/Stepwise Regression Using AIC

nullmodel=lm(medv~1, data=ftrain)
fullmodel=lm(medv~., data=ftrain)

#Backward Elimination
model.step = step(fullmodel,direction='backward')

#Forward Selection
model.step = step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')

#Stepwise Selection (Output Omitted)
model.step=step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')

extractAIC(mdl1) #7.000 1169.379
AIC(mdl1) #2181.663


extractAIC(mdl2) # 26.00 -1213.79
AIC(mdl2) #-201.5058

#*********************************************************************************#

#Decision Tree

library(rpart)
set.seed(100)

decision_fit <- rpart(medv~.,method="anova", data=ftrain,control = rpart.control(cp = 0.02))
summary(decision_fit)
decision.predict <- predict(decision_fit, ftest)
decision.predict
actuals = ftest$medv
decision_rmsepreds <- sqrt(mean((actuals - decision.predict)^2))
decision_rmsepreds # rmse value::::4.
#*********************************************************************************#

#Random Forest

require(randomForest)
set.seed(10)
fit.rf <- randomForest(medv~.,data=ftrain) 
rf.predict <- predict(fit.rf, ftest, type = "response")
random_rmsepreds<-sqrt(mean((ftest$medv -rf.predict)^2))
random_rmsepreds
#RMSE VALUE: 2.78
#*********************************************************************************#


#As per the analysis the RMSE values have significantly increased and the best RMSE value has
#been obtained in Random Forest with the value of 2.78

#*********************************************************************************#
#*********************************************************************************#

)