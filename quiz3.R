
# 1 -----------------------------------------------------------------------

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
library(ggplot2)
library(rattle)

dt <- segmentationOriginal
trainIndex = createDataPartition(dt$Case, p = 0.50,list=FALSE)
training = dt[trainIndex,]
testing = dt[-trainIndex,]

set.seed(125)
model <- train(Class~., data = training, method = "rpart")
fancyRpartPlot(model$finalModel)



# 3 -----------------------------------------------------------------------

library(pgmm)
data(olive)
dim(olive)
names(olive)
olive = olive[,-1]

tree <- train(Area~., data = olive, method = "rpart")
newdata = as.data.frame(t(colMeans(olive)))
predict(tree, newdata)

# 4 -----------------------------------------------------------------------

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
# current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol
model <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, 
               data = trainSA,
               method = "glm",
               family = "binomial")
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd, predict(model, testSA)) # 0.33117
missClass(trainSA$chd, predict(model, trainSA)) # 0.2727



# 5 -----------------------------------------------------------------------

data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
rf <- train(y~., data = vowel.train, method = "rf")
varImp(rf)
