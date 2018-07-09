test_input_data = read.csv(file = "~/R/HackerRank Email/test dataset final.csv", header = TRUE, row.names = 1)

preprocessed = '
#Creating timeline for each record, by substracting the hacker_created_at
#Filling the missing values of last_online
for(i in 1:nrow(test_input_data)){
  if(is.na(test_input_data[i,6])==TRUE){
    test_input_data[i,6] = test_input_data[i,7]
  }
}



test_input_data = test_input_data %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60)

summary(test_input_data)
min_hacker_created_at = min(test_input_data$hacker_created_at)
test_input_data = test_input_data %>% mutate( hacker_creation_temporal = (hacker_created_at - min_hacker_created_at)/86400 ) 

#Removing the redundant features
test_input_data = test_input_data %>% select(-sent_time,-last_online, -hacker_created_at,-mail_id,-user_id)

#Dealing with categorical variables
library(dummies)'
#test_new_data = dummy.data.frame(test_input_data, names = c('mail_category','mail_type','hacker_timezone'))


#Removing highly correlated features
test_input_data = test_input_data %>% select(-contest_login_count, -contest_participation_count, -forum_count, -ipn_count, -ipn_read, -sent_time_t, -submissions_count, -submissions_count_365_days, -submissions_count_1_days)
test_input_data = test_input_data %>% select(-contest_login_count_365_days, -submissions_count_30_days, -submissions_count_master, -submissions_count_7_days)
test_input_data = test_input_data %>% select(-mail_category_1, -mail_category_6, -mail_category_8, -mail_category_10)

#Removing the target variable before applying PCA
test_input_data = test_input_data %>% select(-opened_n)


#transform test into PCA
test.data <- predict(prin_comp, newdata = test_input_data)
test.data <- as.data.frame(test.data)

#select the first 50 components
test_data <- test.data[,1:50]

#run a decision tree

#Converting "opened" to a factor variable
train_data$opened = as.factor(train_data$opened)




library(rpart)
rpart.model <- rpart(opened ~ .,data = train_data, method = "class")
rpart.model

library(e1071)
svm.model = svm(opened ~ ., data = train_data, cost = 100, gamma=1)


summary(rpart.model)
#Predict Output 
predicted= predict(fit,x_test)

#make prediction on test data
rpart.prediction <- predict(rpart.model, test_data)
