#Separating the attributes  for the final merge 
#We choose the attributes : user_id_n , opened_percent, clus.fit2.cluster, hacker_confirm

library(dplyr)
day_prof2_mod = read.csv("~/R/HackerRank Email/Sent_Day_Categories.csv", header = TRUE, row.names = 1)

#Subsetting the attributes to be added to final training set
sep_user = user_prof2 %>% select(user_id_n, open_percent, age_week, hacker_confirm, clus.fit2.cluster)

#Subsetting the attributes of the day_prof2_mod for the final merge
#We choose the attributes to be clus.fit1.cluster, sent_days, total_sent
sep_day = day_prof2_mod %>% select(sent_days, total_sent, clus.fit1.cluster)

#Preparing the final email dataset
#We choose the following attributes
sep_email = train_dataset1 %>% select(user_id_n,sent_days,hacker_timezone, opened1, clicked1, unsubscribed1, sent_time_gap, open_sent_time_gap, open_time_gap,clicked_sent_time_gap,unsubscribe_sent_time_gap)

#Starting the merge operation with the email_dataset
#With the user profile
final_data = merge(sep_email,sep_day,by= 'sent_days', all.x = TRUE)

#With the day profile
final_data = merge(final_data, sep_user, by='user_id_n', all.x = TRUE)

#Renaming the clus.fits 
colnames(final_data)[13] = 'sent_day_cluster_no'
colnames(final_data)[17] = 'user_cluster_no'

#Final training dataset for analysis
write.csv(final_data, file = 'FinalTrainingDataset.csv')

#Categorical variables : hacker_timezone, sent_day_cluster_no, hacker_confirm, user_cluster_no
#Continuous variables : sent_days,sent_time_gap, open_sent_time_gap, open_time_gap, clicked_sent_time_gap, unsubscribe_sent_time_gap, total_sent, open_percent, age_week

#We convert the categorical variables to factor variables
final_data_cat  =  final_data

final_data$hacker_timezone = as.factor(final_data$hacker_timezone)
final_data$sent_day_cluster_no = as.factor(final_data$sent_day_cluster_no)
final_data$hacker_confirm = as.factor(final_data$hacker_confirm)
final_data$user_cluster_no = as.factor(final_data$user_cluster_no)

#Removing the attributes : clicked1, unsubscribed1
final_data = final_data %>% select(-clicked1, -unsubscribed1)

#Converting the open variable to categorical variable
final_data$opened1 = as.factor(final_data$opened1)

#Ready for applying the Decision Tree and then random forests 
#For logistic regression we remove the hacker_timezone attribute 
#Selecting the variables that will be used to make the model
train_data1 = final_data %>% select(opened1, hacker_timezone, hacker_confirm, sent_time_gap, user_cluster_no, age_week,open_percent)

#Filling the missing values of hacker_timezone with the median values
train_data1$hacker_timezone = as.numeric(train_data1$hacker_timezone)
train_data1$hacker_timezone[is.na(train_data1$hacker_timezone) == TRUE] = median(train_data1$hacker_timezone, na.rm=TRUE)
train_data1$hacker_timezone = as.factor(train_data1$hacker_timezone)
levels(train_data1$hacker_timezone) = levels(test_data3$hacker_timezone)

library(rpart)
dec_tree1 = rpart(opened1 ~ ., data = train_data1)

#Seeing the variable importance using the caret package
library(caret)
varImp(dec_tree1)

#The results obtained are : open_percent, user_cluster_no, hacker_timezone, sent_time_gap, hacker_confirm show strong contribution while the remaining show zero importance

#Removing the open_percent attribute 
final_data1 = final_data %>% select(-open_percent)
dec_tree2 = rpart(opened1 ~ ., data = final_data1)
plot(dec_tree2)

#Based on the variable importance obtained from the decision tree method, deault configuration retained
#For more simplicity, we remove the variables which have not shown significance
final_data_cat1 = final_data_cat %>% select(user_id_n,opened1,open_percent, user_cluster_no, sent_time_gap,hacker_timezone,hacker_confirm,age_week)
final_data_cat1$hacker_timezone = as.factor(final_data_cat1$hacker_timezone)
final_data_cat1$user_cluster_no = as.factor(final_data_cat1$user_cluster_no)

#We deal with the categorical variables using one-hot encoding
library(dummies)
log_data1 = dummy.data.frame(train_data1, names = c('hacker_timezone','user_cluster_no','hacker_confirm'))

log_data1 = log_data1 %>% select(-hacker_confirm0)

#Data ready for Logistic regression
#Creating corresponding model
#Developing a logistic regression model 
logistic1 = glm(opened1 ~ ., data = log_data1, family = binomial)

library(caret)
varImp(logistic1)

#Initial 2 models : Decision Tree and Logistic Regression is ready
#Clustering models are also ready : user_model and sent_day_model

#Jobs left to be done
# Build test dataset, analysis of clustering as well as training models, identify user patterns, try to validate them and use reallife case-study to validate them
#Prediction data still not available
preTrials_val = predict(Logistic1,newdata = cat_valid_data,type = "response")

