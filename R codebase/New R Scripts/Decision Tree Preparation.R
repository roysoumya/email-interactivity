library(dplyr)

edata = read.csv('~/R/HackerRank Email/Email/training_dataset.csv')

#Filling the missing values of the last_online attribute
for(i in 1:nrow(edata)){
  if(is.na(edata[i,7] == TRUE)){
    edata[i,7] = edata[i,8]
  }
}

#Removing data points which is NOT intuitively matching :
# 1. opened = false , clicked = true
# 2. opened = false , unsubscribed = true
# 3. hacker_created_at > last_online

edata_mod = edata %>% filter(hacker_created_at <= last_online)
edata_mod = subset(edata_mod, !(opened == 'false' & clicked == 'true'))
edata_mod = subset(edata_mod, !(opened == 'false' & unsubscribed == 'true'))

#Creating timeline for each record, by substracting the hacker_created_at
edata_mod = edata_mod %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60, open_time_t = (open_time - hacker_created_at)/60, click_time_t = (click_time - hacker_created_at)/60, unsubscribe_time_t = (unsubscribe_time - hacker_created_at)/60)

min_hacker_created_at = min(edata_mod$hacker_created_at)
edata_mod = edata_mod %>% mutate( hacker_creation_temporal = (hacker_created_at - min_hacker_created_at)/86400 ) 

edata_mod = edata_mod %>% select(-user_id, -mail_id)

#Converting hacker_timezone into a categorical variable
edata_mod$hacker_timezone = factor(edata_mod$hacker_timezone)
write.csv(edata_mod, file = "Decision Tree Preparation Tree Total.csv")

data_to_consider = edata_mod %>% select(-clicked,-click_time,-click_time_t, -open_time,-open_time_t,-unsubscribed,-unsubscribe_time,-unsubscribe_time_t,-sent_time)
data_to_consider = data_to_consider %>% select(-last_online,-hacker_created_at)

library(caTools)
set.seed(42)
sampl = sample.split(data_to_consider$opened, SplitRatio = 0.7)
data_train = subset(data_to_consider, sampl == TRUE)
data_test = subset(data_to_consider, sampl == FALSE)

#Find correlation between the variables of data_train
#For simplicity

#Removing the categorical variables 
data_train_1 = data_train %>% select(-opened, -hacker_confirmation,-hacker_timezone,-mail_type,-mail_category)

cor_data_train = cor(data_train_1)
cor_data_train[is.na(cor_data_train) == TRUE] = 0.0

for(i in 1:nrow(cor_data_train)){
  cor_data_train[i,i] = 0.0
}

write.csv(cor_data_train, file = "Decision Tree Correlation Before.csv")

#Removing the correlatex features
data_train = data_train %>% select(-ipn_count,-ipn_read,-sent_time_t,-submissions_count,-submissions_count_1_days,-submissions_count_365_days,-submissions_count_30_days,-submissions_count_7_days,-submissions_count_master,-contest_login_count,-contest_participation_count,-contest_login_count_365_days,-forum_count)

library(rpart)
rpart.model <- rpart(opened ~ .,data = data_train_t, method = "class")

set.seed(97)
sampl1 = sample.split(data_train$opened, SplitRatio = 0.5)
data_train_t = subset(data_train, sampl1 == TRUE)
data_train_cv = subset(data_train, sampl1 == FALSE)


data_test1 = data_test %>% select(-ipn_count,-ipn_read,-sent_time_t,-submissions_count,-submissions_count_1_days,-submissions_count_365_days,-submissions_count_30_days,-submissions_count_7_days,-submissions_count_master,-contest_login_count,-contest_participation_count,-contest_login_count_365_days,-forum_count)
rpart.prediction = predict(rpart.model, newdata = data_test1)




