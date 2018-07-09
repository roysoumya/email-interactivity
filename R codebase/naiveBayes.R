library(e1071)

forest_data = train_data1
randForModel = naiveBayes(opened1~., data = forest_data)
naive_pred = predict(randForModel, newdata = test_data3, type = "class")

check_pred1 = test_data3$opened1
misClError_naive_bayes <- mean(naive_pred != check_pred1 )
accuracy1 = 1-misClassificError1
