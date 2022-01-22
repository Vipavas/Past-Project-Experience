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
