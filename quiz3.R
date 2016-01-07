
# 1 -----------------------------------------------------------------------

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

dt <- segmentationOriginal
trainIndex = createDataPartition(dt$Case, p = 0.50,list=FALSE)
training = dt[trainIndex,]
testing = dt[-trainIndex,]

set.seed(125)
