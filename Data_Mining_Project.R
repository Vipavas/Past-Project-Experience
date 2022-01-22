#kNN 
#Cross validation (train and validation sets) 
library(DMwR)
#Read the data
OJ_del <- read.csv(file.choose(),header = T)
#standardized column 1 to column 11
OJ[,1:11]=scale(OJ[,1:11])
#Split in train (80% of original data) + validation sets
idxs = sample(1:nrow(OJ_del), as.integer(0.8*nrow(OJ_del))) 
trainOJ_del = OJ_del[idxs,]
testOJ_del = OJ_del[-idxs,]
#kNN with k = 1 without normalizing the data because it is done earlier
nn1 = kNN(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff 
          + PctDiscMM + PctDiscCH + ListPriceDiff,trainOJ_del,testOJ_del,norm = FALSE, k = 9)
table(testOJ_del[, "Purchase"], nn1)

library(ipred)
library(mlbench)
library(klaR)
predict <- read.csv(file.choose(),header = T)
predict[,1:11]=scale(predict[,1:11])
new <- predict[1071,]
train <- predict[1:1070,]
KNN = ipredknn(Purchase~PriceCH + PriceMM + DiscCH + DiscMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff 
               + PctDiscMM + PctDiscCH + ListPriceDiff, data = train, k = 5)
result = predict(KNN,new,"class")
result


#kNN with deleted related variables
#Cross validation (train and validation sets) 
library(DMwR)
#Read the data
OJ_del <- read.csv(file.choose(),header = T)
#standardized column 1 to column 6
OJ[,2:11]=scale(OJ[,2:11])
#Split in train (80% of original data) + validation sets
idxs = sample(1:nrow(OJ_del), as.integer(0.8*nrow(OJ_del))) 
trainOJ_del = OJ_del[idxs,]
testOJ_del = OJ_del[-idxs,]
#kNN with k = 1 without normalizing the data because it is done earlier
nn1 = kNN(Purchase~  DiscCH + DiscMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff 
          ,trainOJ_del,testOJ_del,norm = FALSE, k = 1)
table(testOJ_del[, "Purchase"], nn1)

library(ipred)
library(mlbench)
library(klaR)
predict <- read.csv(file.choose(),header = T) #reading data
predict[,1:11]=scale(predict[,1:11]) #standardized column 1 to column 11
new <- predict[1071,] #define new data
train <- predict[1:1070,]#define train set
KNN = ipredknn(Purchase~PriceCH + PriceMM + DiscCH + DiscMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff 
               + PctDiscMM + PctDiscCH + ListPriceDiff, data = train, k = 5)
result = predict(KNN,new,"class")
result

#second attempt
nn1 = kNN(Purchase~ DiscCH + DiscMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff 
          ,trainOJ_del,testOJ_del,norm = FALSE, k = 1 )
table(testOJ_del[, "Purchase"], nn1)

#third attempt
nn1 = kNN(Purchase~PriceCH + PriceMM + DiscCH + DiscMM + LoyalCH 
          + ListPriceDiff,trainOJ_del,testOJ_del,norm = FALSE, k = 1)
table(testOJ_del[, "Purchase"], nn1)

#fourth attempt
nn1 = kNN(Purchase~PriceCH + PriceMM + DiscCH + DiscMM + LoyalCH + PriceDiff 
          + ListPriceDiff,trainOJ_del,testOJ_del,norm = FALSE, k = 1)
table(testOJ_del[, "Purchase"], nn1)

summary(OJ)

#Naive Bayes
library("e1071")
library("klaR")
library("caret")
data1 <- read.csv(file.choose(),header = T)
data1$Purchase.f <- factor(data1$Purchase)
data1$HorLPriceCH.f <- factor(data1$HorLPriceCH)
data1$HorLPriceMM.f <- factor(data1$HorLPriceMM)
data1$SalePriceLess.f <- factor(data1$SalePriceLess)
data1$HorLSalePriceMM.f <- factor(data1$HorLSalePriceMM)
data1$HorLSalePriceCH.f <- factor(data1$HorLSalePriceCH)
data1$Loyal.f <- factor(data1$Loyal)

model <- naiveBayes(Purchase ~ HorLPriceCH.f + HorLPriceMM.f + SalePriceLess.f + HorLSalePriceMM.f + HorLSalePriceCH.f + Loyal.f, data = data1)
model


#Create predictor matrix x
x <- data.frame(data1$HorLSalePriceCH.f, data1$HorLSalePriceMM.f, data1$SalePriceLess.f, data1$Loyal.f)
#Define y
y = data1$Purchase.f
#10-fold CV with Naive Bayes
model2 <- train(x,y,"nb",trControl=trainControl(method='cv', number=10))
model2
# Show results
predict(model2$finalModel,x)$class
table(predict(model2$finalModel,x)$class,y)

#classification tree
library("rpart")
OJ <- read.csv(file.choose(),header = T)
fit1 <- rpart(Purchase ~ PriceCH + PriceMM + DiscCH + DiscMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff 
              + PctDiscMM + PctDiscCH + ListPriceDiff,method='class',data=OJ,control=rpart.control(minsplit=1,xval=10))
printcp(fit1)
plot(fit1,uniform = TRUE,
     main='Classification Tree for OJ')
text(fit1,use.n=TRUE,all=TRUE,cex=.5)
post(fit1, file = "~/Downloads/Classification Tree for OJ.ps",
     title = "Classification Tree for OJ")


#Prune Tree
pfit1 <- prune(fit1,cp = fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"])
#Show result
printcp(pfit1)
#Plot pruned tree
plot(pfit1, uniform=TRUE)
text(pfit1, use.n=TRUE, all=TRUE, cex=.7)
# create a postscript plot of tree
post(pfit1, file = "~/Downloads/PrunedClassification Tree for OJ.ps",
     title = "Pruned Classification Tree for OJ")

