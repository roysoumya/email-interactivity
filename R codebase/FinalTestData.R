#Attributes required for user clustering
# clus.fit2 - User category

library(dplyr)
total_data = read.csv(file = '~/R/HackerRank Email/New datatsets/email_datset_23rdApril.csv', row.names = 1)
#total_data = total_data %>% mutate(user_id_n = as.numeric(use))
entire_data = entire_data %>% mutate(sent_time_gap = as.numeric(difftime(sent_date,last_online_date,units="days")), open_sent_time_gap = as.numeric(difftime(open_date,sent_date,units="days")), open_time_gap = as.numeric(difftime(open_date,last_online_date,units="days")), clicked_sent_time_gap = as.numeric(difftime(click_date,sent_date,units="days")), unsubscribe_sent_time_gap = as.numeric(difftime(unsubscribed_date,sent_date,units="days")))
test_dataset = entire_data[seq(400001,nrow(entire_data)),]

#Creating the test dataset
test_data1 = test_dataset %>% select(user_id_n, opened1,sent_time_gap,hacker_timezone, )

#Filling the NA values of hacker_timezone with the median values
test_data1$hacker_timezone[is.na(test_data1$hacker_timezone) == TRUE] = median(test_data1$hacker_timezone, na.rm = TRUE)

test_data2 = merge(test_data1, sep_user, by='user_id_n', all.x = TRUE)
colnames(test_data2)[8] = 'user_cluster_no'

test_data3 = test_data2[complete.cases(test_data2),]
write.csv(test_data3, file = 'Test_Dataset18thMay.csv')

#All info test dataset
all_test_data1 = test_dataset
all_test_data1$hacker_timezone[is.na(all_test_data1$hacker_timezone) == TRUE] = median(all_test_data1$hacker_timezone, na.rm = TRUE)

all_test_data2 = merge(all_test_data1, sep_user, by='user_id_n', all.x = TRUE)
colnames(all_test_data2)[87] = 'user_cluster_no'

all_test_data3 = all_test_data2[complete.cases(all_test_data2[,87]),]
write.csv(all_test_data3, file = 'TestDatasetComplete19thMay.csv')

#Considering the categorical variables
test_data3$user_cluster_no = as.factor(test_data3$user_cluster_no)
test_data3$hacker_timezone = as.factor(test_data3$hacker_timezone)
test_data3$hacker_confirm = as.factor(test_data3$hacker_confirm)

#Test data ready for applying the Decision Tree Method
test_data3 = test_data3 %>% select(-user_id_n)
dec_tree1_pred = predict(dec_tree1, newdata = test_data3, type = 'class')
check_pred1 = test_data3$opened1
misClassificError1 <- mean(dec_tree1_pred != check_pred1 )
accuracy1 = 1-misClassificError1

write.csv(,file = 'TestDatasetDecisionTree.csv')

# Making the test data ready for applying Logistic Regression model
#We deal with the categorical variables using one-hot encoding
library(dummies)
log_data1_test = dummy.data.frame(test_data3, names = c('hacker_timezone','user_cluster_no','hacker_confirm'))

log_data1_test = log_data1_test %>% select(-hacker_confirm0)
log_data1_test$opened1 = as.factor(log_data1_test$opened1)

predict_log_data = predict(logistic1, newdata = log_data1_test,type = 'response')

check_pred = log_data1_test$opened1
start = 0.40
for (i in 1:25) {
  if(start > 1.0)
    break
  
  fitted.results <- ifelse(predict_log_data > start,1,0)
  #check_pred = ifelse(data_read_valid$opened == 'true',1,0)
  misClasificError <- mean(fitted.results != check_pred )
  print(start)
  print(paste('Accuracy',1-misClasificError))
  start = start + 0.025
}


fitted.results <- ifelse(predict_log_data > 0.775,1,0)

table(check_pred,fitted.results)
fitted.results
check_pred
    0     1
0 50466  5034
1 10949 11183

table(check_pred, dec_tree1_pred)
dec_tree1_pred
check_pred     
     0     1
0 43110 12390
1  6332 15800

table(check_pred1, dec_tree1_pred)
dec_tree1_pred
check_pred1     
     0     1
0 43110 12390
1  6332 15800
log_table = table(check_pred, fitted.results)
log_table = table(check_pred1, dec_tree1_pred)

true_pos = log_table[2,2]
false_pos = log_table[1,2] 
true_neg = log_table[2,1]
false_neg = log_table[1,1]

precision = true_pos/(true_pos + false_pos)
recall = true_pos/(true_pos+true_neg)
f1_score = (2*(precision*recall))/(precision+recall)

accuracy1 = mean(dec_tree1_pred != check_pred1 )
accuracy2 = 1 -accuracy1
print(paste('Accuracy',1-misClasificError))

library(caret)
logistic_var_imp = varImp(logistic1)
logistic_var_imp$var_name = rownames(logistic_var_imp)
logistic_var_imp = logistic_var_imp %>% arrange(desc(Overall))

#With open_percent
varImpDecTree = varImp(dec_tree1)
varImpDecTree$var_name = rownames(varImpDecTree)
varImpDecTree = varImpDecTree %>% arrange(desc(Overall))

#Without open_percent
varImpDecTree1 = varImp(dec_tree2)
varImpDecTree1$var_name = rownames(varImpDecTree1)
varImpDecTree1 = varImpDecTree1 %>% arrange(desc(Overall))
