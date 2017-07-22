library(readr)
AmesHousing <- read_delim("C:/Users/NavyaSai/Desktop/UTD/Sem_4/R/Assignments/MidTerm_Project/Ames Housing/AmesHousing.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
View(AmesHousing)
colnames(AmesHousing)
summary(AmesHousing)

# converting all character columns to factors
myData=as.data.frame(unclass(AmesHousing))
summary(myData)
# removing order and PID attributes
myData = myData[,-1:-2]
colnames(myData)
summary(myData)

# removing the outliers with more than 4000 sq.ft Gr.liv area
plot(myData$SalePrice, myData$Gr.Liv.Area)
myData = myData[myData$Gr.Liv.Area<=4000,]
# "Neighborhood","House.Style",,"Garage.Qual"
myNewData = myData[,c("Lot.Frontage","Lot.Area","Neighborhood","House.Style","Overall.Qual","Overall.Cond",
                "Year.Built","Year.Remod.Add","Foundation",
                "Bsmt.Qual","Total.Bsmt.SF","X1st.Flr.SF",
                "Gr.Liv.Area","Garage.Finish","Garage.Qual","Full.Bath","TotRms.AbvGrd","SalePrice")]

summary(myNewData)
dim(myNewData)
myNewData <- na.omit(myNewData)

# taking log of sales price and scaling.
myNewData$lSalePrice <- log(myNewData$SalePrice)
myNewData$SalePrice  <- NULL
summary(myNewData)
colnames(myNewData)
myNewData$Lot.Frontage = scale(myNewData$Lot.Frontage)
myNewData$Lot.Area = scale(myNewData$Lot.Area)
myNewData$Total.Bsmt.SF = scale(myNewData$Total.Bsmt.SF)
myNewData$X1st.Flr.SF = scale(myNewData$X1st.Flr.SF)
myNewData$Gr.Liv.Area = scale(myNewData$Gr.Liv.Area)
myNewData$train <- NULL
##########################################################################

# taking dummy values for housedata

dummyHouseData = data.frame(model.matrix(~., data=myNewData))[,-1]
dummyHouseData$trainTRUE <- NULL
summary(dummyHouseData)
dim(dummyHouseData)
colnames(dummyHouseData)
########################################################################

# K-Means on dummyHouseData

library(ggplot2)

# Elbow Method to compute k value
set.seed(123)

# Compute and plot wss for k = 2 to k = 15.
par(mfrow=c(1,1))
k.max <- 15 
data <- dummyHouseData
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
km <- kmeans(dummyHouseData,8)
table(km$cluster, dummyHouseData$lSalePrice)
km
km$cluster <- as.factor(km$cluster)
ggplot(myNewData, aes(lSalePrice, Gr.Liv.Area, color = km$cluster)) + geom_point()

# PCA
pca_house = prcomp(dummyHouseData,scale. = FALSE)
biplot(pca_house,scale=1)

pca_var = pca_house$sdev ^2
pve = pca_var / sum(pca_var)
par(mfrow=c(1,2))
plot(pve, xlab = "Principle Component", ylab = "PVE", ylim = c(0, 1), type = "b",col="blue")
plot(cumsum(pve),xlab = "Principle Component", ylab = "Cumulative PVE", ylim = c(0, 1), type = "b",col="brown")
pca_data = pca_house$x[, 1:3]

# k-means after PCA
# Elbow Method to compute k value
set.seed(123)

# Compute and plot wss for k = 2 to k = 15.

k.max <- 15
data <- pca_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
par(mfrow = c(1,1))
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

km <- kmeans(pca_data,6 )

table(km$cluster, myNewData$lSalePrice)
km
#km$cluster <- as.factor(km$cluster)
#ggplot(pca_data, aes(lSalePrice, Lot.Area, color = km$cluster)) + geom_point()


# Linear regression model - not working properly due to features selection
#lr.model_1 = lm(lSalePrice~poly(Lot.Area,2)+Neighborhood+House.Style+Overall.Qual+Overall.Cond+Year.Built+Year.Remod.Add
 #               +Foundation+Bsmt.Qual+Total.Bsmt.SF+Gr.Liv.Area+Full.Bath+TotRms.AbvGrd+Garage.Qual, data=myNewData)
lr.model_1 = lm(lSalePrice~.,data=dummyHouseData)
summary(lr.model_1)
# Much improved over single variable (R^2)
plot(lr.model_1$fitted.values,lr.model_1$residuals)
plot(dummyHouseData$lSalePrice, lr.model_1$fitted.values)
dim(dummyHouseData)
#cross validation
train = sample(1300,934)
lr.model.1 = lm(lSalePrice~., data=dummyHouseData,subset = train)
# R automatically uses only the rows in the train subset to
# create the model
summary(lr.model.1)
plot(lr.model.1$fitted.values,lr.model.1$residuals)
#plot(dummyHouseData$lSalePrice, lr.model.1$fitted.values)
dim(dummyHouseData)
# Still looks pretty good ...
# Here is how we can calculate the training MSE
mean(lr.model.1$residuals^2)
# Is this good?
summary(dummyHouseData$lSalePrice)
# The SQRT(MSE) is about 0.7% of the average WGHT, so
# probably a good result. But what about the Test MSE?
attach((dummyHouseData))
summary(myNewData)
mean((lSalePrice - predict(lr.model.1,dummyHouseData))[-train]^2)
# Pretty good so far - but this is just one Test set.
# To do LOOCV, we need to attach 'boot'
library("boot", lib.loc="C:/Program Files/R/R-3.3.2/library")
lr.model.2 = glm(lSalePrice, data=dummyHouseData)
summary(lr.model.2)
# Note this gives the same model (coefficients are the same)
# But the summary provides different information
# This does LOOCV ...
cv.error.2 = cv.glm(myData,lr.model.2)
cv.error.2$delta


#####################################################################################

# LASSO Regression

#install.packages("ElemStatLearn")
library(ElemStatLearn)
library(leaps)
## Note: you may have to uncomment and run the following line. 
#install.packages("glmnet")
## After you have run it once, re-comment it! 
library(glmnet)


########################################################################

# lasso as explained in class -- Harika's Code
x=model.matrix(myNewData$lSalePrice~. ,myNewData)[,-1]
y=myNewData$lSalePrice
grid=10^seq(10,-2,length=100)
set.seed(1)
?sample
train=sample(1:nrow(x),0.8*nrow(x))
test=-train
y.test=y[test]
lasso.mod=glmnet(x[train, ],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
cv.out=cv.glmnet(x[train, ],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test, ])
mean((lasso.pred-y.test)^2)
#avg(myNewData$lSalePrice)
summary(myNewData$lSalePrice)



## Three useful, if somewhat inscrutable, functions: 
build.formulas <- function( resp.var, leaps.which.matrix ) {
  lapply( apply( leaps.which.matrix[,-1], 1, which),
          function(xx) { 
            paste( resp.var, '~', paste(names(xx), collapse=" + ")) } )
}
leaps.AIC <- function( resp.var, leaps.obj, dataset ) {
  unlist(lapply(
    build.formulas(resp.var, summary(leaps.obj)$which ), 
    function(xx) {AIC(lm(xx, data=dataset))}))
}
best.lm <- function( citerion.vector, resp.vector, leaps.obj, dataset) {
  lm( build.formulas( resp.vector, summary(leaps.obj)$which )[[ 
    which( citerion.vector == min(citerion.vector)) 
    ]], data=dataset )  
}




## Setup test and training
set.seed(1)
train=sample(1:nrow(x),0.8*nrow(x))
test=-train
myNewData.train <- dummyHouseData[train,]
myNewData.test <- dummyHouseData[test,]
#myNewData$train <- rbinom(nrow(dummyHouseData), size=1, p=.5)
#myNewData.train <- subset(dummyHouseData, train==TRUE)
myNewData.train$trainTRUE <- NULL
#myNewData.test <- subset(dummyHouseData, train==FALSE)
myNewData.test$trainTRUE <- NULL

summary(myNewData.train)
summary(myNewData.test)
nrow(myNewData.train)
nrow(myNewData.test)
dim(dummyHouseData)

small.leaps <- regsubsets( lSalePrice ~ ., 
                           nvmax  = 15,
                           nbest  = 1, 
                           intercept=TRUE,
                           method = c("forward", "backward"),
                           data=myNewData.train)
small.leaps.summary = summary(small.leaps)
names(small.leaps.summary)
small.leaps.summary$rsq
small.leaps.summary$cp
plot(small.leaps.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
plot(small.leaps.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
plot(small.leaps,scale="adjr2")
small.leaps.summary$cp
coef(small.leaps,15)

## What was selected
summary(small.leaps)$which
summary(small.leaps)
summary(myNewData.train)
## Extract RSS, BIC and AIC
small.rss <- summary(small.leaps)$rss
small.bic <- summary(small.leaps)$bic
small.aic <- leaps.AIC( 'lSalePrice', small.leaps, myNewData.train)
small.rss
small.bic
small.aic
## Plot RSS
plot( 1:16, small.rss,  main="RSS by subset size",
      type="b", xlab="subset size", ylab="Residual Sum Square", 
      col="red2" )
## Look at the differences 
small.rss - min(small.rss)

## Plot BIC
plot( 1:16, small.bic,  main="BIC by subset size",
      type="b", xlab="subset size", ylab="BIC", 
      col="red2" )
small.bic - min(small.bic)

## Plot AIC
plot( 1:16, small.aic,  main="AIC by subset size",
      type="b", xlab="subset size", ylab="AIC", 
      col="red2" )
small.aic - min(small.aic)

## Extract the best RSS, BIC and AIC models
small.model.rss <- best.lm( small.rss, 'lSalePrice', small.leaps, myNewData.train)
small.model.bic <- best.lm( small.bic, 'lSalePrice', small.leaps, myNewData.train)
small.model.aic <- best.lm( small.aic, 'lSalePrice', small.leaps, myNewData.train)

## Examine them
summary( small.model.rss ) 
summary( small.model.bic ) 
summary( small.model.aic ) 

## Which performs better on the test data?
small.rss.pred <- predict(small.model.rss, newdata = myNewData.test) 
mean( (small.rss.pred - myNewData.test$lSalePrice)^2 )
plot(myNewData.test$lSalePrice, small.rss.pred, main="RSS Predictions on test")
abline(0,1)

small.bic.pred <- predict(small.model.bic, newdata = myNewData.test) 
mean( (small.bic.pred - myNewData.test$lSalePrice)^2 )
plot(myNewData.test$lSalePrice, small.bic.pred, main="BIC Predictions on test")
abline(0,1)

small.aic.pred <- predict(small.model.aic, newdata = myNewData.test) 
mean( (small.aic.pred - myNewData.test$lSalePrice)^2 )
plot(myNewData.test$lSalePrice, small.aic.pred, main="AIC Predictions on test")
abline(0,1)

#step-wise regression
# forward and backward subset selection

# Fit the dataset 
pstate.null <- lm(lSalePrice ~ 1, data=myNewData.train)
pstate.full <- lm(lSalePrice ~ ., data=myNewData.train)

pstate.model.fwd <- step(pstate.null, 
                         scope = list(lower = pstate.null, upper = pstate.full), 
                         direction="forward", trace = FALSE) 
## Change trace = TRUE to see more detail

pstate.model.bkwd <- step(pstate.full, 
                          scope = list(lower = pstate.null, upper = pstate.full), 
                          direction="backward", trace = FALSE)

pstate.model.both <- step(pstate.full, 
                          scope = list(lower = pstate.null, upper = pstate.full), 
                          direction="both", trace = FALSE)
#forward
pstate.model.fwd.summary = summary(pstate.model.fwd)
names(pstate.model.fwd.summary)
pstate.model.fwd.summary$r.squared
pstate.model.fwd.summary$adj.r.squared
plot(pstate.model.fwd.summary$r.squared,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
plot(pstate.model.fwd.summary$residuals,xlab="Number of Variables",ylab="Cp",type="l")
plot(pstate.model.fwd,scale="adjr2")
pstate.model.fwd.summary$residuals
coef(pstate.model.fwd.summary,15)

#backward
pstate.model.bkwd.summary = summary(pstate.model.bkwd)
names(pstate.model.bkwd.summary)
pstate.model.bkwd.summary$r.squared
pstate.model.bkwd.summary$adj.r.squared
plot(pstate.model.bkwd.summary$adj.r.squared,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
plot(pstate.model.bkwd.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
plot(pstate.model.bkwd,scale="adjr2")
pstate.model.bkwd.summary$cp
coef(pstate.model.bkwd.summary,15)


## What is in each model:
coef(pstate.model.fwd)
coef(pstate.model.bkwd)
coef(pstate.model.both)

## And compare it to the winning model from last activity 
coef(small.model.aic)

## Does the AIC model have all of the fwd coef? 
(name.sel <- names(coef(small.model.aic)) %in% names(coef(pstate.model.fwd)) )
## Nope. 
## 
## Which ones are missing from the prostate model? (The ! means "not".) 
names(coef(small.model.aic))[ ! name.sel ]

## Does the AIC model have all of the bkwd coef? 
names(coef(small.model.aic)) %in% names(coef(pstate.model.bkwd))
## It does! 

## What are the AIC values?
AIC(pstate.model.fwd)
AIC(pstate.model.bkwd)
AIC(pstate.model.both)

## Which predicts the best?
pstate.fwd.pred <- predict(pstate.model.fwd, newdata = myNewData.test) 
mean( (pstate.fwd.pred - myNewData.test$lSalePrice)^2 )
plot( myNewData.test$lSalePrice, pstate.fwd.pred, main="Fwd predictions on test" )
abline(0,1)

pstate.bkwd.pred <- predict(pstate.model.bkwd, newdata = myNewData.test) 
mean( (pstate.bkwd.pred - myNewData.test$lSalePrice)^2 )
plot( myNewData.test$lSalePrice, pstate.bkwd.pred, main="Bkwd predictions on test" )
abline(0,1)

pstate.both.pred <- predict(pstate.model.both, newdata = myNewData.test) 
mean( (pstate.both.pred - myNewData.test$lSalePrice)^2 )
plot( myNewData.test$lSalePrice, pstate.both.pred, main="Both predictions on test" )
abline(0,1)


#ridge
colnames(myNewData.train)
## We must specify our data as matrices for glmnet
x.pstate      <- as.matrix(myNewData.train[,1:60])
x.pstate.test <- as.matrix(myNewData.test[,1:60])
y.pstate      <- as.matrix(myNewData.train[,61])

## Note that alpha=0 is ridge regression 
ridge.fit <- glmnet(x.pstate, y.pstate, family = "gaussian", alpha=0)
plot(ridge.fit, label=TRUE, xvar="norm")

## Look at all of the coefficients...
## 
coef(ridge.fit)
## Note that all of them a shrunkken at first 

## Try cross validation to pick a model 
## Note that alpha must be zero!
cvfit = cv.glmnet(x.pstate, y.pstate, family = "gaussian", alpha=0)

plot(cvfit)

## Try the minimum
cvfit$lambda.min
## 
coef(cvfit, s = "lambda.min")

## Try the simplest model that is not statistically significantly different 
## from the minimum 
cvfit$lambda.1se
## coef(cvfit, s = "lambda.1se")

# Check MSE on the predicted models
pstate.ridge.min.pred <- predict(cvfit, newx = x.pstate.test, s = "lambda.min")

## For comparison: AIC
mean( (small.aic.pred - myNewData.test$lSalePrice)^2 )
mean( (pstate.ridge.min.pred - myNewData.test$lSalePrice)^2 )

## A little better! 
## Could make a plot here....

pstate.ridge.1se.pred <- predict(cvfit, newx = x.pstate.test, s = "lambda.1se")
mean( (pstate.ridge.1se.pred - myNewData.test$lSalePrice)^2 )


#####   LASSO


## Note that alpha=1 is lasso regression 
lasso.fit <- glmnet(x.pstate, y.pstate, family = "gaussian", alpha=1)
plot(lasso.fit, label=TRUE, xvar="norm")

## Look at all of the coefficients...
## 
coef(lasso.fit)
## Note that all of them a shrunkken at first 

## Try cross validation to pick a model 
## Note that alpha must be zero!
cvfit = cv.glmnet(x.pstate, y.pstate, family = "gaussian", alpha=1)

plot(cvfit)

## Try the minimum
cvfit$lambda.min
## coef(cvfit, s = "lambda.min")

## Try the simplest model that is not statistically significantly different 
## from the minimum 
cvfit$lambda.1se
## coef(cvfit, s = "lambda.1se")

# Check MSE on the predicted models
pstate.lasso.min.pred <- predict(cvfit, newx = x.pstate.test, s = "lambda.min")

## For comparison: ridge
mean( (pstate.lasso.min.pred - myNewData.test$lSalePrice)^2 )
mean( (pstate.ridge.min.pred - myNewData.test$lSalePrice)^2 )
## A lasso does a little better! 
## Could make a plot here....

pstate.lasso.1se.pred <- predict(cvfit, newx = x.pstate.test, s = "lambda.1se")
mean( (pstate.lasso.1se.pred - myNewData.test$lSalePrice)^2 )



#######################################################################################

# KNN
train = sample(1300,934)
summary(dummyHouseData)
dim(dummyHouseData)
lr.model = lm(lSalePrice~., data=dummyHouseData, subset=train)
lr.predict = predict(lr.model, dummyHouseData[-train,-61])
MSE.lr = mean((lr.predict-dummyHouseData[-train, 61])^2)
MSE.lr
# Let's see how PCR does ...
library("pls")
pcr.fit = pcr(lSalePrice~., data=dummyHouseData, subset=train, scale=F, validation="CV")
# The option 'validation' allows us to view CV results on the
# different numbers of components
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
# Incredibly, one component seems to be enough. Also notice that R handled
# the factor predictors!
# Let's compare the MSEs ...
pcr.pred = predict(pcr.fit, dummyHouseData[-train,-61], ncomp=1)
MSE.pcr = mean((pcr.pred - dummyHouseData[-train,61])^2)
MSE.lr
MSE.pcr
# We actually have a lower MSE using just a one dimensional model!
# Now let's try KNN ...
library("FNN")
?knn.reg
# This function works for KNN regression  (there is a separate one for
# classification).
# We'll form test and training sets and scale them for KNN
#Hitters.MM = model.matrix(Salary~. -1, myData)
# Note the -1 removes the intercept terms - will be important later
#head(Hitters.MM)
#train.x = scale(Hitters.MM[train,])
#test.x = scale(Hitters.MM[-train,])
#train.y = myData[train,19]
#test.y = myData[-train,19]
knn.fit = knn.reg(myNewData.train, myNewData.test, myNewData.train[,61], k=5)
mean((myNewData.test[,61] - knn.fit$pred)^2)
MSE.lr
MSE.pcr
# KNN with K=5 is on par with PCR -  can we do better? Choose K
# with CV ...
errs = rep(0,30)
for(i in 1:30){
  knn.fit = knn.reg(myNewData.train, myNewData.test, myNewData.train[,61], k=i)
  errs[i] = mean((myNewData.test[,61] - knn.fit$pred)^2)
}
errs
