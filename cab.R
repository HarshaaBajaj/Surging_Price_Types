#read
library(readr)
train <- read_csv("C:/Users/mom1/Desktop/cab/train.csv")
View(train)
#structure
str(train)
sapply(train, class)
#gross predictor rate
table(train$Surge_Pricing_Type)
hist(train$Surge_Pricing_Type,col="grey")
#to factor 
train$Surge_Pricing_Type<-as.factor(train$Surge_Pricing_Type)
str(train$Surge_Pricing_Type)
#check how many are of what type
numVars <- names(which(sapply(train, is.numeric)))
catVars <- names(which(sapply(train, is.factor)))
logicalVars <- names(which(sapply(train, is.logical)))
charVars<-names(which(sapply(train,is.character)))
#Make sure you didn't miss any variables
stopifnot(length(numVars) + length(catVars) + length(logicalVars)+length(charVars)== ncol(train))
#Calculate percent NA
Null_Counter <- apply(train, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | 
                                                          x == "999" | x == "0"))/length(x))
sort(Null_Counter,decreasing = TRUE)

#var1 is 50% missing that is a lot
#rows that have incomplete data
nrow(train[!complete.cases(train),])
#check distributions of discrete variables and check with summary if Ms align 
summary(train)
#double checking for NA's
sum(is.na(train$Trip_Distance))
boxplot(train$Trip_Distance)
hist(train$Trip_Distance,col = "grey")

boxplot(train$Customer_Since_Months)
hist(train$Customer_Since_Months,col = "grey")
#it looks wierd
unique(train$Customer_Since_Months)
#check how many NA's
sum(is.na(train$Customer_Since_Months))
#replace with 0
train$Customer_Since_Months=  replace(train$Customer_Since_Months,which(is.na(train$Customer_Since_Months)),0)
unique(train$Customer_Since_Months)
boxplot(train$Customer_Since_Months)
hist(train$Customer_Since_Months,col = "grey")

summary(train)
#check how many NA's
sum(is.na(train$Life_Style_Index))
boxplot(train$Life_Style_Index)
hist(train$Life_Style_Index,col = "grey")
#has normal distribution but with lot of outliers and 15% NA's let's come back to it later

#check how many NA's
sum(is.na(train$Customer_Rating))
boxplot(train$Customer_Rating)
hist(train$Customer_Rating,col = "grey")
#-vely skewed which is a good sign

#check how many NA's
sum(is.na(train$Cancellation_Last_1Month))
boxplot(train$Cancellation_Last_1Month)
hist(train$Cancellation_Last_1Month,col = "grey")

summary(train)
#check how many NA's
sum(is.na(train$Var1))
table(train$Var1, useNA = 'always')
boxplot(train$Var1)
hist(train$Var1,col = "grey") #lots of NAs

#check how many NA's
sum(is.na(train$Var2))
boxplot(train$Var2)
hist(train$Var2,col = "grey")

#check how many NA's
sum(is.na(train$Var3))
boxplot(train$Var3)
hist(train$Var3,col = "grey")

#char vars
charVars
unique(train$Type_of_Cab)
#check how many NA's
sum(is.na(train$Type_of_Cab))
#replace with "unknown"
train$Type_of_Cab=  replace(train$Type_of_Cab,which(is.na(train$Type_of_Cab)),'unknown')
unique(train$Type_of_Cab)
#convert to factor
train$Type_of_Cab<- as.factor(train$Type_of_Cab)
str(train$Type_of_Cab)
plot(train$Type_of_Cab)

charVars
unique(train$Confidence_Life_Style_Index)
#check how many NA's
sum(is.na(train$Confidence_Life_Style_Index))
#replace with "unknown"
train$Confidence_Life_Style_Index=  replace(train$Confidence_Life_Style_Index,
                                           which(is.na(train$Confidence_Life_Style_Index)),'unknown')
unique(train$Confidence_Life_Style_Index)
#convert to factor
train$Confidence_Life_Style_Index<- as.factor(train$Confidence_Life_Style_Index)
str(train$Confidence_Life_Style_Index)
plot(train$Confidence_Life_Style_Index)

unique(train$Destination_Type)
sum(is.na(train$Destination_Type))
train$Destination_Type<- as.factor(train$Destination_Type)
str(train$Destination_Type)
plot(train$Destination_Type)

unique(train$Gender)
sum(is.na(train$Gender))
train$Gender<- as.factor(train$Gender)
str(train$Gender)
plot(train$Gender)
#structure check
str(train)
summary(train)

#relations
library('corrplot')
cor_mat <- cor(train[, numVars], use = "pairwise.complete.obs")
corrplot(cor_mat,type="lower", method = "number", diag = TRUE)
cor_mat[upper.tri(cor_mat,diag=TRUE)]=NA
cor_mat=as.data.frame(as.table(cor_mat))
cor_mat=na.omit(cor_mat)
cor_mat=cor_mat[order(-abs(cor_mat$Freq)),]
strong_cor<-cor_mat[1:5,]
strong_cor
#verify with plot
par(mfrow=c(2,3))
plot(train$Var2,train$Var3)
plot(train$Var2,train$Customer_Rating)
plot(train$Var2,train$Life_Style_Index)
plot(train$Life_Style_Index,train$Trip_Distance)
plot(train$Life_Style_Index,train$Var3)
#we know Life_Style_Index had 15% NA's so let's remove it
train1<-train[,-which(names(train) == "Life_Style_Index")]
numVars1<-numVars[which(numVars!="Life_Style_Index")]

par(mfrow=c(1,1))
cor_mat1 <- cor(train1[, numVars1], use = "pairwise.complete.obs")
corrplot(cor_mat1,type="lower", method = "number", diag = TRUE)
cor_mat1[upper.tri(cor_mat1,diag=TRUE)]=NA
cor_mat1=as.data.frame(as.table(cor_mat1))
cor_mat1=na.omit(cor_mat1)
cor_mat1=cor_mat1[order(-abs(cor_mat1$Freq)),]
strong_cor1<-cor_mat1[1:5,]
strong_cor1
#verify with plot
par(mfrow=c(2,3))
plot(train1$Var2,train1$Var3)
plot(train1$Var2,train1$Customer_Rating)
plot(train1$Var2,train1$Trip_Distance)
plot(train1$Var3,train1$Trip_Distance)
plot(train1$Var3,train1$Customer_Rating)
#var2 and 3 are highly correlated
summary(train$Var2)
summary(train$Var3)
#removing var2
train1<-train1[,-which(names(train1) == "Var2")]
numVars1<-numVars1[which(numVars1!="Var2")]
par(mfrow=c(1,1))
cor_mat1 <- cor(train1[, numVars1], use = "pairwise.complete.obs")
corrplot(cor_mat1,type="lower", method = "number", diag = TRUE)
cor_mat1[upper.tri(cor_mat1,diag=TRUE)]=NA
cor_mat1=as.data.frame(as.table(cor_mat1))
cor_mat1=na.omit(cor_mat1)
cor_mat1=cor_mat1[order(-abs(cor_mat1$Freq)),]
strong_cor1<-cor_mat1[1:5,]
strong_cor1  #looks better

summary(train1)

# var1 is 54% missing let's fix by imputation

#using mice
library(VIM)
library(mice)
md.pattern(train1)
mice_plot <- aggr(train1, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train1), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
imp<-mice(train1)
summary(imp)
imp$imp$Var1
#check which result you wanna use 
summary(train1$Var1)
summary(imp$imp$Var1[1])
summary(imp$imp$Var1[2])
summary(imp$imp$Var1[3])
summary(imp$imp$Var1[4])
summary(imp$imp$Var1[5])
#make a complete new set with new imputed values
train2<-complete(imp,2)
summary(train2)
#missforest
install.packages("missForest")
library(missForest)
imp.rf <- missForest(train1)
imp.rf$ximp         #check imputed values
imp.rf$OOBerror   #check imputation error #NRMSE:normalized mean squared error for continous variables
                  #PFC:proportion of falsely classified for categorical
#comparing actual data accuracy
imp.rf.err <- mixError(imp.rf$ximp, train1, train)           #(computed,missing,actual)
imp.rf.err

#MI uses predictive mean matching, For each missing value,find available value with 
#closest predictive mean to that variable.
install.packages("mi")
library(mi)
imp.mi <- mi(train1, seed = 335)
summary(imp.mi)

#amelia assumes normal distribution
library(Amelia)
AmeliaView()

#creating training and test datasets
trainset<-train2[1:91700,]
testset<-train2[91701:131662,]
training_target<-train2[1:91700,'Surge_Pricing_Type']
testing_target<-train2[91701:131662,'Surge_Pricing_Type']

K = sqrt(nrow(train1))
require(class)
m1_knn<-knn(train=trainset,test = testset,cl= training_target,k)
predictions<-NULL
predictions <- knn_predict(test, train,training$X3,K) #calling knn_predict()

library(randomForest)
#train a random forest with pclass and title
set.seed(1234)
rf.1=randomForest(x=trainset,y=training_target,importance = TRUE,ntree = 1000)
rf.1
varImpPlot(rf.1)

set.seed(24)
library(arm)
library(caret)
knn<-train(trainset,
      training_target,
      method = "rf",
      na.action = na.pass)
