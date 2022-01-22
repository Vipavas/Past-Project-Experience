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


