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


