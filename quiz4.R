
# 1 Agreed Accuracy-----------------------------------------------------------------------

library(ElemStatLearn)
data("vowel.train")
data("vowel.test")

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)
rf <- train(y~., data = vowel.train, method ='rf')
gbm <- train(y~., data = vowel.train, method = 'gbm')
rf.pred <- predict(rf, newdata = vowel.test)
gbm.pred <- predict(gbm, newdata = vowel.test)

confusionMatrix(vowel.test$y, rf.pred)$overall['Accuracy'] # 0.5996
confusionMatrix(vowel.test$y, gbm.pred)$overall['Accuracy'] # 0.5238

agree <- (rf.pred == gbm.pred)
confusionMatrix(vowel.test$y[agree], rf.pred[agree])$overall['Accuracy'] # 0.6366


# 2 Stacked Accuracy-----------------------------------------------------------------------

library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)
rf2 <- train(diagnosis~., training, method = 'rf')
gbm2 <- train(diagnosis~., training, method = 'gbm')
lda <- train(diagnosis~., training, method = 'lda')

rf.pred2 <- predict(rf2, testing)
gbm.pred2 <- predict(gbm2, testing)
lda.pred2 <- predict(lda, testing)

confusionMatrix(testing$diagnosis, rf.pred2)$overall['Accuracy'] # 0.7683
confusionMatrix(testing$diagnosis, gbm.pred2)$overall['Accuracy'] #0.7927
confusionMatrix(testing$diagnosis, lda.pred2)$overall['Accuracy'] #0.7683

combi <- data.frame(rf.pred2, gbm.pred2, lda.pred2, diagnosis = testing$diagnosis)
combi.model <- train(diagnosis~., combi, method = 'rf')
combi.pred <- predict(combi.model, testing)
confusionMatrix(testing$diagnosis, combi.pred)$overall['Accuracy'] # 0.8049


# 3 -----------------------------------------------------------------------

set.seed(3523)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]
set.seed(233)
lasso <- train(CompressiveStrength~., training, method = "lasso")
?plot.enet
plot.enet(lasso$finalModel, xvar = 'penalty', use.color = TRUE)


# 4 -----------------------------------------------------------------------

library(lubridate) # For year() function below

dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)
?bats
model <- bats(y = tstrain)
plot(forecast(model))
cast <- forecast(object = model, level = 95, h = nrow(testing))
cast

sum(cast$lower < testing$visitsTumblr & testing$visitsTumblr < cast$upper)/nrow(testing) # 0.9617


# 5 -----------------------------------------------------------------------

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
model <- svm(CompressiveStrength~., training)
pred <- predict(model, testing)
accuracy(f = pred, x = testing$CompressiveStrength)
