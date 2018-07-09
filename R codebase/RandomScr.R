library(dplyr)
t_data = read.csv(file = '~/R/HackerRank Email/TrainDecisionTree21may_test.csv')

tree_data1 = t_data %>% select(opened1, hacker_timezone_ns,hacker_confirm, sent_time_gap_ns,user_cluster_no,age_week_ns,open_percent_ns)

#Converting the categorical variables
tree_data1$opened1 = as.factor(tree_data1$opened1)
tree_data1$hacker_timezone_ns = as.factor(tree_data1$hacker_timezone_ns)
tree_data1$hacker_confirm = as.factor(tree_data1$hacker_confirm)
tree_data1$user_cluster_no = as.factor(tree_data1$user_cluster_no)
tree_data1$hacker_timezone_ns = t_data$hacker_timezone

library(rpart)
tree_data1_model = rpart(opened1 ~ ., data = tree_data1)
dec_tree1_pred_rough = predict(tree_data1_model, newdata = test_data3_copy, type = 'class')
check_pred1_rough = test_data3_copy$opened1
misClassificError1_rough <- mean(dec_tree1_pred != check_pred1_rough )
accuracy1_rough = 1-misClassificError1_rough

test_data3_copy = test_data3
colnames(test_data3_copy) = c('opened1','sent_time_gap_ns','hacker_timezone_ns','open_percent_ns','age_week_ns','hacker_confirm','user_cluster_no')
