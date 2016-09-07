
### Machine Learning Challenge ###

#Load the stocks data
stocks <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_returns.csv')

#Divide the data into training set and testing set
train <- stocks[1:50,]
test <- stocks[51:100,]

#============================================================================================================

##Data Exploration of Training dataset
head(train)
str(train)
dim(train)
attributes(train)
nrow(train)
ncol(train)
row.names(train)
colnames(train)
summary(train)
sapply(train[1,], class)

##Data Exploration of Testing dataset
head(test)
str(test)
dim(test)
attributes(test)
nrow(test)
ncol(test)
row.names(test)
colnames(test)
summary(test)
sapply(test[1,], class)

#==============================================================================================================================

#Discriptive Statistics
library(PerformanceAnalytics)
library(zoo)
library(tseries)
##Discriptive statistics of S1 in training dataset
train <- data.frame(train)
hist(train[, 2], main="histogram", xlab=train$S1, 
     probability=TRUE, col=hcl(h=195,l=65,c=100), ylim = c(0.0,0.5), xlim = c(-2, 3))

boxplot(train[,2], outchar=TRUE, main="boxplot", cex=0.7, 
        xlab=train$S1, col=hcl(h=195,l=65,c=100))

plot(density(train[,2]), type="l", 
     main="Smoothed density", lwd=2,
     xlab=train$S1, ylab="density estimate", 
     col=hcl(h=195,l=65,c=100))

qqnorm(train[,2], col=hcl(h=195,l=65,c=100), cex=0.7)
qqline(train[,2])

##convert date from 'factor' to 'date'
train$date <- as.Date(train$date, format = "%m/%d/%Y")
test$date <- as.Date(test$date, format = "%m/%d/%Y")


##Add a variable s1.cumulative which gives cumulative up and down values of S1
train$s1.cumulative <- cumsum(train$S1)

#Plot which shows ups and downs in the cumulative values of S1
plot(train$s1.cumulative, type = "l", col='blue', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time", ylab = "Cumulative Change in S1", xlab = "Number of Days")

#descriptive Statistics of cumulative change in S1
table.Stats(train$s1.cumulative)

#Run the Jarque Bera test to see if the cumulative S1 are normal
jarque.bera.test(train$s1.cumulative)

#Plot autocorrelations over time lags
acf(train$s1.cumulative)

#=======================================================================================================================================

#1. Which variables matter for predicting S1?
#Lets do principal componenet analysis for this purpose

#Principal Component Ananlysis on train dataset
tr.pca <- prcomp(train[,3:11], center = TRUE, scale. = TRUE) 
print(tr.pca)

# plot method
plot(tr.pca, type = "l")
summary(tr.pca) #

##consider threshold of 1% and remove remaining variables
#remove S7 - S10
train.pcr <- train[,-c(8,9,10,11)]

#======================================================================================================================================

###
#Best Subset Selection

library(leaps)

train <- train[,-1]
test <- test[,-1]
regfit.full <- regsubsets(S1 ~ ., train)
summary(regfit.full) #An asterisk indicates that a given variable is included in the corresponding model

regfit.full <- regsubsets(S1 ~ ., data = train, nvmax = 9)
reg.summary =summary (regfit.full)
reg.summary
names(reg.summary)
#we see that the R2 statistic increases from 88 %, when only
#one variable is included in the model, to almost 90 %, when all variables are included.
reg.summary$rsq

#Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will
#help us decide which model to select.
par(mfrow = c(1,2))

##plot RSS
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
#plot Adj-R square
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points (5,reg.summary$adjr2[5], col="red",cex=2,pch =20)

#plot cp
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(reg.summary$cp )
points (4,reg.summary$cp [4], col ="red",cex=2,pch =20)

#plot BIC
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type="l")
which.min(reg.summary$bic)
points (1,reg.summary$cp [1], col ="red",cex=2,pch =20)

plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")

##2. Forward and Backward Stepwise Selection
regfit.fwd=leaps::regsubsets (S1 ~ .,data=train, nvmax=9, method ="forward")
summary (regfit.fwd)
regfit.bwd=leaps::regsubsets (S1 ~ .,data=train, nvmax=9, method ="backward") 
summary (regfit.bwd)

coef(regfit.full ,5)
coef(regfit.fwd ,5)
coef(regfit.bwd ,5)

pred.regfit.full <- coef(regfit.full,5)[1] + coef(regfit.full,5)[2]*test[,2] + 
  coef(regfit.full,5)[3]*test[,3] + coef(regfit.full,5)[4]*test[,6] + coef(regfit.full,5)[5]*test[,7]+coef(regfit.full,5)[6]*test[,8]

train.regfit.full <- coef(regfit.full,5)[1] + coef(regfit.full,5)[2]*train[,3] + 
  coef(regfit.full,5)[3]*train[,4] + coef(regfit.full,5)[4]*train[,7] + coef(regfit.full,5)[5]*train[,8]+coef(regfit.full,5)[6]*train[,9]

## Combining the predicted result
write.csv (pred.regfit.full, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\regfitPred.csv")
stockRegfit <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_regfit.csv')
##Plot cumulative S1
plot(cumsum(stockRegfit$S1), type = "l", col='blue', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Best Selection Method)", ylab = "Cumulative Change in S1", xlab = "Number of Days")

##plotting training data
plot(cumsum(train$S1), type = "l", col='blue', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Best Selection Method)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(train.regfit.full), type = "l", col='red', main = "Cumulative Changes in S1 Over the Period of Time (Best Selection Method)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
test.err <- mean((train$S1 - train.regfit.full)^2)
RMSE <- sqrt(test.err)
print(RMSE)###0.2858645

#==================================================================================================================================================================================================================

# ####################
# # RIDGE REGRESSION # 
# ####################

library(MASS)

trainNew <- train[, -c(1,5,6,10,11)]
test <- test
model.ridge <- lm.ridge(S1 ~ ., data = trainNew, lambda = seq(0,10,0.1))

plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]
lambda.ridge

# We can plot the coefficients and see how they vary as a function of lambda

colors <- rainbow(8)

matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(tr)[c(-1)], pos=4, col=colors)

beta.ridge <- coef(model.ridge)[which.min(model.ridge$GCV),]
resid.ridge <- tr$S1 - beta.ridge[1] - as.matrix(tr[,c(2:6)])%*%beta.ridge[1:5]

# To find df
d <- svd(as.matrix(trainNew[,2:6]))$d
df <- 50 - sum(d^2/(lambda.ridge+d^2))

rss.ridge <- sum(resid.ridge^2)/df

##use optimum value of lamba i.e.lambda.ridge and perform ridge regression
final.ridge <- lm.ridge(S1 ~ ., data = trainNew, lambda = 7.5)

##prediction
pred.ridge <- coef(final.ridge)[1] + coef(final.ridge)[2]*test[,2] + 
  coef(final.ridge)[3]*test[,3] + coef(final.ridge)[4]*test[,4] + coef(final.ridge)[5]*test[,5]+coef(final.ridge)[5]*test[,6]
summary(trainNew$S1)
summary(pred.ridge)
table.Stats(pred.ridge)

# Comparing the ridge-regression fit to the original least-squares fit:

# The X matrix for this problem:
X.matrix <- cbind(rep(1,length=length(trainNew$S1)),trainNew$S2, trainNew$S3, trainNew$S6, trainNew$S7, trainNew$S8)

# Getting the fitted values for the ridge-regression fit:
fitted.vals <- X.matrix %*% c(-0.05216783, -0.20928856,  0.11510360,  0.26852694,  0.11318670, -0.08043500)

# Getting the SSE for the ridge-regression fit:
sse.ridge <- sum((trainNew$S1-fitted.vals)^2); sse.ridge
mse.ridge <- mean((trainNew$S1-fitted.vals)^2); mse.ridge
rmse.ridge <- sqrt(mse.ridge); rmse.ridge ###0.2926605
# The original least-squares fit:
linear.reg <- lm(S1 ~ ., data = trainNew)

# Getting the SSE for the original least-squares fit:
sse.linear.reg <- sum(resid(linear.reg)^2)
sse.linear.reg  
# The SSE for the ridge-regression fit is not much higher, which is good

write.csv (pred.ridge, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\ridgePred.csv")
stockRidge <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_ridge.csv')
##Plot cumulative S1
plot(cumsum(stockRidge$S1), type = "l", col='brown', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Ridge Regression)", ylab = "Cumulative Change in S1", xlab = "Number of Days")

#====================================================================================================================================================================================================================

# ###############################
##Principal Component Regression
# ###############################

library(pls)

pcr.fit=pcr(S1 ~ ., data=trainNew, scale=F, validation ="CV")
summary(pcr.fit)

validationplot(pcr.fit ,val.type="MSEP")

set.seed(1)
pcr.fit1=pcr(S1 ~ ., data=trainNew, scale=TRUE, validation ="CV")
validationplot(pcr.fit ,val.type="MSEP")
pcr.pred.train=predict (pcr.fit ,newdata = trainNew,ncomp =5)
pcr.pred.test=predict (pcr.fit ,newdata = test,ncomp =5)
mse.pcr <- mean((trainNew$S1 - pcr.pred.train)^2)
rmse.pcr <- sqrt(mse.pcr)
print(rmse.pcr) ##0.2858645

write.csv (pcr.pred.test, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_pcr.csv")
stockPCR <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock1_pcr.csv')
##Plot cumulative S1
plot(cumsum(stockPCR$S1), type = "l", col='red', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Principal Component Regression)", ylab = "Cumulative Change in S1", xlab = "Number of Days")

#==============================================================================================================================================================================================================================

# #########################
#Support Vector Regression
# #########################
library(e1071)
#linear model
model.lm <- lm(S1 ~ ., data = trainNew)
pred.lm <- predict(model.lm, data = test[,-1])
error <- model.lm$residuals  # same as data$Y - predictedY

rmse <- function(error)
{
  sqrt(mean(error^2))
}
pred.lmRMSE <- rmse(error)
pred.lmRMSE###0.2808513

#svm model
model.svm <- svm(S1 ~ ., data = trainNew)
pred.svm.train <- predict(model.svm, data = trainNew)
pred.svm.test <- predict(model.svm, data = test)


error <- model.svm$residuals  # same as data$Y - predictedY
pred.svmRMSE <- rmse(error)
pred.svmRMSE

#Tuning your support vector regression model
# perform a grid search
tuneResult <- tune(svm, S1 ~ .,  data = trainNew, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# best performance: MSE = 0.1561428, RMSE = 0.395 epsilon 0.2 cost 4
# Draw the tuning graph
plot(tuneResult)

tuneResult <- tune(svm, S1 ~ .,  data = trainNew, ranges = list(epsilon = seq(0.2,0.5,0.01), cost = 2^(2:9))) 
print(tuneResult)
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelS1.tr <- predict(tunedModel, data = trainNew) 
tunedModelS1.ts <- predict(tunedModel, data = test) 

#error <- rmse(tunedModel$residuals)
error <- trainNew$S1 - tunedModelS1.tr  
error1 <- trainNew$S1 - tunedModelS1.ts

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error) ### Rmse is reduced after tuning 0.2557318
tunedModelRMSE1 <- rmse(error1) 

write.csv (tunedModelS1.ts, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\svm_tuned.csv")
stockSvmTuned <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_svmTuned.csv')
##Plot cumulative S1
plot(cumsum(stockSvmTuned$S1), type = "l", col='red', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Support Vector Regression)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
abline(v=51, lty = 2)
legend("right", legend = "Predicted Stocks")
legend("left", legend = "Actual Stocks")
#========================================================================================================================================================================================================================

# #########################
#Lasso Regression
# #########################
# load the package
library(lars)
# load data
x <- as.matrix(trainNew[,2:6])
y <- as.matrix(trainNew[,1])
# fit model
fit <- lars(x, y, type="lasso")
# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
lasso.pred.tr <- predict(fit, trainNew[-1], s=best_step, type="fit")$fit
lasso.pred.ts <- predict(fit, test[, -c(1, 4,5,9,10)], s=best_step, type="fit")$fit
# summarize accuracy
lasso.rmse.tr <- sqrt(mean((y - lasso.pred.tr)^2)) ###0.2858645
print(lasso.rmse.tr)
lasso.rmse.ts <- sqrt(mean((y - lasso.pred.ts)^2))
print(lasso.rmse.ts)

write.csv(lasso.pred.ts, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\lasso.csv")
stockLasso <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_lasso.csv')
##Plot cumulative S1
plot(cumsum(stockLasso$S1), type = "l", col='violetred', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Lasso)", ylab = "Cumulative Change in S1", xlab = "Number of Days")

#=============================================================================================================================================================================================================

# ############
#Elastic Net
# ############

#Elastic Net creates a regression model that is penalized with both 
#the L1-norm and L2-norm. This has the effect of effectively 
#shrinking coefficients (as in ridge regression) and setting 
#some coefficients to zero (as in LASSO)

# load the package
library(glmnet)
# load data
x <- as.matrix(trainNew[,2:6])
y <- as.matrix(trainNew[,1])
z <- as.matrix(test[, -c(1, 4,5,9,10)])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
ele.pred.tr <- predict(fit, x, type="link")
ele.pred.ts <- predict(fit, z, type="link")
# summarize accuracy
rmse1.ele <- sqrt(mean((y - ele.pred.tr)^2))
print(rmse1.ele) ###0.2858711
rmse.ele <- mean((y - ele.pred.ts)^2)
print(rmse.ele)

write.csv(ele.pred.ts, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\elasticNet.csv")
stockElasticNet <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_elasticNet.csv')
##Plot cumulative S1
plot(cumsum(stockElasticNet$S1), type = "l", col='salmon', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (Elastic Net)", ylab = "Cumulative Change in S1", xlab = "Number of Days")

#====================================================================================================================================================================================================================

# ##################
#K-Nearest Neighbor
# ##################
library(FNN)
test1 <- test[, -c(4,5,9,10)]
test1[is.na(test1)] <- 0
reg.knn <- knn.reg(trainNew, test = test1[, -c(4,5,9,10)], y, k = 3)
summary(reg.knn)
reg.knn$pred
rmse <- function(error)
{
  sqrt(mean(error^2))
}
knnRes <- reg.knn$residuals
pred.KnnRMSE <- rmse(error)
pred.KnnRMSE ###0.2557318
reg.knn$R2Pred
write.csv(reg.knn$pred, file="C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\knn.csv")
stockKNN <- read.csv(file = 'C:\\Users\\Varsha\\OneDrive\\pro\\Correlation\\stock_knn.csv')
##Plot cumulative S1
plot(cumsum(stockElasticNet$S1), type = "l", col='maroon4', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (K-Nearest Neighbor)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
abline(v=51, lty = 2)
legend("right", legend = "Predicted Stocks")
legend("left", legend = "Actual Stocks")
###
### RMSE Value of each Model arranged from lowest to highest
###

#K-Nearest Neighbor = 0.2557318
#Support Vector Regression Tuned = 0.2557318
#Feature Selection Method = 0.2858645
#Principal Component Regression = 0.2858645
#Lasso Regression = 0.2858645
#Elastic Net = 0.2858711
#Ridge Regression = 0.2926605

plot(cumsum(train$S1), type = "l", lwd = 4, main = "Cumulative Changes in S1 Over the Period of Time", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(reg.knn$pred), type = "l", col='red', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(fitted.vals), type = "l", col='green', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(pcr.pred.train), type = "l", col='pink', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(pred.svm.train), type = "l", col='purple', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(lasso.pred.tr), type = "l", col='orange', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(ele.pred.tr), type = "l", col='red4', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")




plot(cumsum(train$S1), type = "l", col= "blue", lwd = 3,  main = "Cumulative Changes in S1 in 50 days (K Nearest Neighbor Regression)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(reg.knn$pred), type = "l", col='red', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
legend("topleft",legend=c("Atual","Predicted (KNN)"), title="Plot Type:", col=c("blue","red"), lty = "solid")

plot(cumsum(train$S1), type = "l", col= "blue", lwd = 3,  main = "Cumulative Changes in S1 in 50 days (Support Vector Regression)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
points(cumsum(tunedModelS1.tr), type = "l", col='orange', lwd = 2, main = "Cumulative Changes in S1 Over the Period of Time (KNN)", ylab = "Cumulative Change in S1", xlab = "Number of Days")
legend("topleft",legend=c("Atual","Predicted (SVR)"), title="Plot Type:", col=c("blue","orange"), lty = "solid")


