library(e1071)
#Dataset to be used in  : train_data1
library(caret)

forest_data = train_data1

logistic1 = glm(opened1 ~ ., data = log_data1, family = binomial)
model_svm = svm(opened1 ~ ., data = log_data1, kernel='linear')

#We apply randomforest with default arguments. Tweaking the paarmeters using cross-validation will be taken up in the future work
randForModel = randomForest(opened1~., data = forest_data)
varImp(randForModel)

