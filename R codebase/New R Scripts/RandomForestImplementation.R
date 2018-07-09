library(randomForest)
#Dataset to be used in  : train_data1

forest_data = train_data1
userNum = table(train_data1$user_cluster_no)

#Since the randomForest function cannot handle categorical predictors with more than 53 categories
#We remove 42 and 23 from training as well as for test dataset
forest_data = subset(forest_data, user_cluster_no != 23)
forest_data = subset(forest_data, user_cluster_no != 42)

str(forest_data)

#We apply randomforest with default arguments. Tweaking the paarmeters using cross-validation will be taken up in the future work
randForModel = randomForest(opened1~., data = forest_data)

