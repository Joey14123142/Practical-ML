
# 1 -----------------------------------------------------------------------

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# 2 -----------------------------------------------------------------------

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)
# ?cut2
library(dplyr)
training <- mutate(training, index=1:nrow(training))

cutIndex <- cut2(training$index, g=10)
breaks <- 10
par(mfrow=c(8,1))
for(i in 1:ncol(training)){
  x <- training[,i]
  qplot(index, CompressiveStrength, data = training, color = cut2(x, g=breaks))
}


# 3 -----------------------------------------------------------------------

hist(training$Superplasticizer, breaks = 20)
hist(log(training$Superplasticizer+1), breaks = 20)
sum(training$Superplasticizer==0)
sum(training$Superplasticizer<0)


# 4, 5 -----------------------------------------------------------------------

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.9)
preObj

subtrain <- training[, IL_col_idx]
subtrain$diagnosis <- training$diagnosis
test <- testing[ ,IL_col_idx]
test$diagnosis <- testing$diagnosis
nonPCA <- train(diagnosis~., data = subtrain, method="glm")
nonPCA.pred <- confusionMatrix(test[,13],
                               predict(nonPCA, test[,-13]))
pca.pre <- preProcess(subtrain[, -13], method=c('center', 'scale', 'pca'), thresh=0.8)
pca.train <- predict(pca.pre, subtrain[, -13])
pca.test <- predict(pca.pre, test[, -13])

pca <- train(subtrain$diagnosis~., data=pca.train, method="glm")
pca.pred <- confusionMatrix(test[, 13], predict(pca, pca.test))

nonPCA.pred

