
#install.packages('ISLR')
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("mxnet")

library(ISLR)
summary(Caravan)
dim(Caravan)

library(readr)

#training data
insurance_data <- read_delim("C:/Users/NavyaSai/Desktop/UTD/Sem_4/R/Assignments/Final_Project/ticdata2000.txt", 
                               +     "\t", escape_double = FALSE, col_names = FALSE, 
                               +     trim_ws = TRUE)
View(insurance_data)

#test data values
insurance.test.data <- read_delim("C:/Users/NavyaSai/Desktop/UTD/Sem_4/R/Assignments/Final_Project/ticeval2000.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
View(insurance.test.data)

#test data target
insurance.test.target <-  read_delim("C:/Users/NavyaSai/Desktop/UTD/Sem_4/R/Assignments/Final_Project/tictgts2000.txt", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
View(insurance.test.target)


summary(insurance_data)
dim(insurance_data)
dim(insurance.test.data)
dim(insurance.test.target)
colnames(insurance_data)

colnames(insurance_data) <- c(colnames(Caravan[,-86]),"CARAVAN")
colnames(insurance.test.data) <- colnames(Caravan[,-86])
colnames(insurance.test.target) <- c("CARAVAN")

# analyzing data variables - customer data 1-43 cols
a <- table(insurance_data$CARAVAN)
colors=c("red","green")
col=colors
pie(a,main = "CUSTOMERS OF INSURANCE POLICY",col=colors)
box()

b<-table(insurance_data$MOSTYPE[insurance_data$CARAVAN== 1])
barplot(b,border="dark blue",main = "INSURANCE POLICY vs CUSTOMER SUBTYPE",xlab="Customer subtype",ylab="Number of customers")

c<-table(insurance_data$MGEMLEEF[insurance_data$CARAVAN== 1])
names(c)=c("20 to 30","30 to 40","40 to 50","50 to 60","60 to 70","70 to 80")
barplot(c,col=rainbow(6),main = "INSURANCE POLICY vs AVE AGE",xlab="Avg age or Age group",ylab="Number of customers")


# product ownership data analysis 44-85 cols
d<-table(insurance_data$APLEZIER[insurance_data$CARAVAN== 1])
barplot(d,border="dark blue",main = "INSURANCE POLICY vs NUMBER OF BOAT POLICIES",xlab = "Number of boat policies",ylab = "Number of customers")


# new Data - both training and test data

myData <- insurance_data[c("MOSTYPE","MGEMLEEF","MKOOPKLA","MINKGEM","MOSHOOFD","APLEZIER","ABYSTAND","PPERSAUT","ABRAND","CARAVAN")]
myData <-na.omit(myData)
#myData <- as.data.frame(myData)
summary(myData)
nrow(myData)


testData <- insurance.test.data[c("MOSTYPE","MGEMLEEF","MKOOPKLA","MINKGEM","MOSHOOFD","APLEZIER","ABYSTAND","PPERSAUT","ABRAND")]
#testData <- as.data.frame(testData)
summary(myOtherData)
ncol(myOtherData)

target <- insurance.test.target

# set.seed(11)
# 
# train <- myData[sample(row.names(myData), size = round(nrow(myData)*0.7)),]
# test <- myData[!(row.names(myData) %in% row.names(train)), ]
# 
# nrow(train)
# nrow(test)

train = sample(4075,1747)

#classification modelling

library(caret)

# LDA model
library("MASS")
caravan.lda = lda(CARAVAN~., data=myData, subset=train)
caravan.pred = predict(caravan.lda,myData[-train,])
names(caravan.pred)
caravan.class = caravan.pred$class
tab <-table(caravan.class,myData[-train,]$CARAVAN)
mean(caravan.class == myData[-train,]$CARAVAN)
confusionMatrix(tab)

### test data
caravan.pred.1 = predict(caravan.lda,testData)
names(caravan.pred.1)
caravan.class.1 = caravan.pred.1$class
tab.1 <- table(caravan.class.1,target$CARAVAN)
mean(caravan.class.1 == target$CARAVAN)
confusionMatrix(tab.1)

# QDA Model
qda.model = qda(CARAVAN~., data=myData, subset = train)
qda.model
qda.class = predict(qda.model,myData[-train,])
qda.tab <- table(qda.class$class,myData[-train,]$CARAVAN)
mean(qda.class$class==myData[-train,]$CARAVAN)
confusionMatrix(qda.tab)

## test data
qda.class.1 = predict(qda.model,testData)
table(qda.class.1$class,target$CARAVAN)
mean(qda.class.1$class==target$CARAVAN)


#naive bayes
library("e1071")
nb.model = naiveBayes(as.factor(CARAVAN)~., data=myData, subset = train)
#rf.model
nb.pred = predict(nb.model,myData[-train,])
summary(nb.pred)
nb.tab <- table(nb.pred,myData[-train,]$CARAVAN)
#mean(nb.pred==myData[-train,]$CARAVAN)
confusionMatrix(nb.tab)

##test data
nb.pred = predict(nb.model,testData)
summary(nb.pred)
nb.tab.1 <- table(nb.pred,target$CARAVAN)
# mean(nb.pred==target$CARAVAN)
confusionMatrix(nb.tab.1)


# Random Forest
library(randomForest)
rf.model = randomForest(as.factor(CARAVAN)~., data=myData,subset=train,mtry=8,importance=TRUE)
rf.pred = predict(rf.model,myData[-train,])
summary(rf.pred)
rf.tab <- table(rf.pred,myData[-train,]$CARAVAN)
confusionMatrix(rf.tab)

##test data
rf.pred = predict(rf.model,testData)
summary(rf.pred)
rf.tab.1 <- table(rf.pred,target$CARAVAN)
confusionMatrix(rf.tab.1)


# Linear Kernel - SVM
svm.model = svm(as.factor(CARAVAN)~., data=myData[train,],kernel = "linear", cost=1, scale=FALSE)
svm.pred = predict(svm.model,myData[-train,])
summary(svm.pred)
svm.tab <- table(svm.pred,myData[-train,]$CARAVAN)
confusionMatrix(svm.tab)

##test data
svm.pred = predict(svm.model,testData)
summary(svm.pred)
svm.tab.1 <- table(svm.pred,target$CARAVAN)
confusionMatrix(svm.tab.1)


# Non - Linear Kernel - SVM
svm.model = svm(as.factor(CARAVAN)~., data=myData[train,],kernel="polynomial", gamma=1, cost=1,scale = FALSE)
svm.pred = predict(svm.model,myData[-train,])
summary(svm.pred)
svm.tab <- table(svm.pred,myData[-train,]$CARAVAN)
confusionMatrix(svm.tab)

##test data
svm.pred = predict(svm.model,testData)
summary(svm.pred)
svm.tab.1 <- table(svm.pred,target$CARAVAN)
confusionMatrix(svm.tab.1)

# Radial Kernel - SVM
svm.model = svm(as.factor(CARAVAN)~., data=myData[train,],kernel="radial", gamma=1, cost=1,scale = FALSE)
svm.pred = predict(svm.model,myData[-train,])
summary(svm.pred)
svm.tab <- table(svm.pred,myData[-train,]$CARAVAN)
confusionMatrix(svm.tab)

##test data
svm.pred = predict(svm.model,testData)
summary(svm.pred)
svm.tab.1 <- table(svm.pred,target$CARAVAN)
confusionMatrix(svm.tab.1)


# neural net
library(neuralnet)
library(nnet)
n = names(myData)
f = as.formula(paste("CARAVAN ~", paste(n[!n %in% "CARAVAN"], collapse = " + ")))
nn.fit = neuralnet(f,data=myData[train,],hidden=c(3),linear.output=TRUE)
plot(nn.fit)
# Cool - how did it do?
nn.pred = compute(nn.fit,myData[-train, ])
names(nn.pred)
nn.MSE = sum((myData[-train,]$CARAVAN - nn.pred$net.result)^2)/nrow(myData[-train,])
nn.MSE
nn.RMSE = sqrt(nn.MSE)

#test data
nn.pred = compute(nn.fit,testData)
names(nn.pred)
nn.MSE = sum((target$CARAVAN - nn.pred$net.result)^2)/nrow(testData)
nn.MSE
nn.RMSE = sqrt(nn.MSE)
nn.RMSE
#yhat <- predict(nnclas_model, x, type = 'class')
#confusionMatrix(as.factor(yhat), y)





























