#Clearing the workspace
rm(list = ls())

library(dplyr)

entire_data = read.csv(file = '~/R/HackerRank Email/Email/training_dataset.csv', header = TRUE)
#test_input_form =  read.csv(file = '~/R/HackerRank Email/Email/test_dataset.csv', header =TRUE)

#Splitting the dataset into the training and test dataset

library(caTools)
set.seed(881)
spliT = sample.split(entire_data$opened, SplitRatio = 0.70)
train_dataset = subset(entire_data, spliT == TRUE)
test_dataset = subset(entire_data, spliT == FALSE)

#Removing the entire dataset after splitting it into training and testing
#Preparing the test dataset
test_dataset = test_dataset %>% select(-open_time, -clicked, -click_time,-unsubscribed, -unsubscribe_time)
rm(entire_data)


#Splitting the training dataset into training and crossvalidation parts
set.seed(123)
split1 = sample.split(train_dataset$opened, SplitRatio = 0.70)
train_data = subset(train_dataset, split1 == TRUE)
validation_data = subset(train_dataset, split1 == FALSE)

#Removing the redundant variables
rm(train_dataset)

#Generating summary of the training data
summary(train_data)

train_data$hacker_timezone = as.factor(train_data$hacker_timezone)

#Filling the missing values of the last_online attribute
for(i in 1:nrow(train_data)){
  if(is.na(train_data[i,9] == TRUE)){
    train_data[i,9] = train_data[i,10]
  }
}

#Removing inconsistent points 
train_data_mod = train_data %>% filter(hacker_created_at <= last_online)
train_data_mod = subset(train_data_mod, !(opened == 'false' & clicked == 'true'))
train_data_mod = subset(train_data_mod, !(opened == 'false' & unsubscribed == 'true'))


#Removing the user_id and mail_id
train_data_mod = train_data_mod %>% select(-user_id, -mail_id)
rm(train_data)

#Adding new features
#Creating timeline for each record, by substracting the hacker_created_at
train_data_mod = train_data_mod %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60, open_time_t = (open_time - hacker_created_at)/60, click_time_t = (click_time - hacker_created_at)/60, unsubscribe_time_t = (unsubscribe_time - hacker_created_at)/60)

#Removing the redundant features
train_data_mod = train_data_mod %>% select(-sent_time,-open_time,-click_time,-unsubscribe_time,-last_online, -hacker_created_at)

#Aggregating the ipn_count and ipn_read details
train_data_mod = train_data_mod %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
train_data_mod = train_data_mod %>% mutate(contest_not_login_cnt = contest_participation_count - contest_login_count, contest_not_login_count_1_days=contest_participation_count_1_days - contest_login_count_1_days, contest_not_login_count_7_days=contest_participation_count_7_days - contest_login_count_7_days, contest_not_login_count_30_days=contest_participation_count_30_days - contest_login_count_30_days,contest_not_login_count_365_days=contest_participation_count_365_days - contest_login_count_365_days)

edata = train_data_mod
#Generating the correlation matrix for the present variables
#Removing the categorical variables, except opened
edata = edata %>% select(-unsubscribed, -hacker_confirmation, -clicked,-hacker_timezone,-mail_category,-mail_type)

edata$opened = as.numeric(edata$opened)
cor_edata = cor(edata)

#Writing it to a file 
write.csv(cor_edata, file = 'Correlation matrix feature 6th April.csv')



train_data_mod = train_data_mod %>% mutate(ipn_read_aggr = (cor_edata[25,16]*ipn_read_1_days + cor_edata[25,19]*ipn_read_7_days+cor_edata[25,17]*ipn_read_30_days+cor_edata[25,18]*ipn_read_365_days+cor_edata[25,15]*ipn_read)/(cor_edata[25,15]+cor_edata[25,16]+cor_edata[25,17]+cor_edata[25,18]+cor_edata[25,19]))
train_data_mod = train_data_mod %>% mutate(ipn_not_read_aggr = (cor_edata[25,48]*ipn_not_read_1_days + cor_edata[25,49]*ipn_not_read_7_days +cor_edata[25,50]*ipn_not_read_30_days +cor_edata[25,51]*ipn_not_read_365_days + cor_edata[25,47]*ipn_not_read)/(cor_edata[25,47]+cor_edata[25,48]+cor_edata[25,49]+cor_edata[25,50]+cor_edata[25,51]))
train_data_mod = train_data_mod %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
train_data_mod = train_data_mod %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
train_data_mod = train_data_mod %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)

#Aggregating the contest participation and login details
train_data_mod = train_data_mod %>% mutate(contest_login_aggr = (cor_edata[25,2]*contest_login_count_1_days + cor_edata[25,5]*contest_login_count_7_days + cor_edata[25,3] * contest_login_count_30_days + cor_edata[25,4]*contest_login_count_365_days +cor_edata[25,1]*contest_login_count)/(cor_edata[25,1]+cor_edata[25,2]+cor_edata[25,3]+cor_edata[25,4]+cor_edata[25,5]))
train_data_mod = train_data_mod %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)
train_data_mod = train_data_mod %>% mutate(contest_not_login_aggr = (cor_edata[25,48]*contest_not_login_count_1_days + cor_edata[25,49]*contest_not_login_count_7_days + cor_edata[25,50]*contest_not_login_count_30_days + cor_edata[25,51]*contest_not_login_count_365_days + cor_edata[25,47]*contest_not_login_cnt)/(cor_edata[25,47]+cor_edata[25,48]+cor_edata[25,49]+cor_edata[25,50]+cor_edata[25,51]))
train_data_mod = train_data_mod %>% select(-contest_not_login_cnt,-contest_not_login_count_1_days,-contest_not_login_count_7_days,-contest_not_login_count_30_days,-contest_not_login_count_365_days)
train_data_mod = train_data_mod %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)

train_data_mod = train_data_mod %>% mutate(sub_contest_aggr = (cor_edata[25,27]*submissions_count_contest_1_days + cor_edata[25,30]*submissions_count_contest_7_days + cor_edata[25,28]*submissions_count_contest_30_days + cor_edata[25,29]*submissions_count_contest_365_days +cor_edata[25,26]*submissions_count_contest)/(cor_edata[25,26]+cor_edata[25,27]+cor_edata[25,28]+cor_edata[25,29]+cor_edata[25,30]))
train_data_mod = train_data_mod %>% mutate(sub_master_aggr = (cor_edata[25,32]*submissions_count_master_1_days + cor_edata[25,35]*submissions_count_master_7_days + cor_edata[25,33]*submissions_count_master_30_days + cor_edata[25,34]*submissions_count_master_365_days + cor_edata[25,31]*submissions_count_master)/(cor_edata[25,31]+cor_edata[25,32]+cor_edata[25,33]+cor_edata[25,34]+cor_edata[25,35]))

train_data_mod = train_data_mod %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
train_data_mod = train_data_mod %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)
#Removing redundant data
train_data_mod = train_data_mod %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)


#Now the training dataset has 23 variabkes. Now we will deal with the categorical variables using one-hot encoding
library(dummies)
#cat_rem_data= dummy.data.frame(train_data_mod, names = c('hacker_timezone','mail_type', 'mail_category'))

#Since results are having a very high variance due to large number of attributes due to various levels
#We remove the attribute : mail_category and hacker_timezone
train_data_mod = train_data_mod %>% select(-mail_category, -hacker_timezone)

cat_rem_data= dummy.data.frame(train_data_mod, names = c('mail_type'))

#Converting the binary boolean values to : true = 1 and false = 0
#hacker_confirmation
cat_rem_data$hacker_confirmation = as.numeric(cat_rem_data$hacker_confirmation)


#clicked
cat_rem_data$clicked = as.numeric(cat_rem_data$clicked)

#unsubscribed
cat_rem_data$unsubscribed = as.numeric(cat_rem_data$unsubscribed)

#Removing the user activitiies that is the click time, unsubscribed tme and opened time
cat_rem_data = cat_rem_data %>% select(-open_time_t, -click_time_t, -unsubscribed)

#Storing the processed training dataset for Logistic regression
write.csv(cat_rem_data, file = '~/R/HackerRank Email/Logistic Regression train data 6th April.csv')


#We try with a smaller sample to train the model
#cat_1lakh = sample(cat_rem_data, size = 100000, replace = FALSE)






#We decide the threshold using the cross-validation data









#--------------Preparing the crossvalidated data-----------------------------------------




valid_data = validation_data
valid_data = valid_data %>% select(-open_time, -clicked, -click_time,-unsubscribed, -unsubscribe_time)

#Preparing the test data in order to run the above model
valid_data$hacker_timezone = as.factor(valid_data$hacker_timezone)

#Filling the missing values of the last_online attribute, with th hacker_created_at value
for(i in 1:nrow(valid_data)){
  if(is.na(valid_data[i,6] == TRUE)){
    valid_data[i,6] = valid_data[i,7]
  }
}

#Removing the user_id and mail_id
valid_data_mod = valid_data %>% select(-user_id, -mail_id)
rm(valid_data)

#Adding new features
#Creating timeline for each record, by substracting the hacker_created_at
valid_data_mod = valid_data_mod %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60)

#Removing the redundant features
valid_data_mod = valid_data_mod %>% select(-sent_time,-last_online, -hacker_created_at)

#Aggregating the ipn_count and ipn_read details
valid_data_mod = valid_data_mod %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
valid_data_mod = valid_data_mod %>% mutate(contest_not_login_cnt = contest_participation_count - contest_login_count, contest_not_login_count_1_days=contest_participation_count_1_days - contest_login_count_1_days, contest_not_login_count_7_days=contest_participation_count_7_days - contest_login_count_7_days, contest_not_login_count_30_days=contest_participation_count_30_days - contest_login_count_30_days,contest_not_login_count_365_days=contest_participation_count_365_days - contest_login_count_365_days)



valid_data_mod = valid_data_mod %>% mutate(ipn_read_aggr = (cor_edata[25,16]*ipn_read_1_days + cor_edata[25,19]*ipn_read_7_days+cor_edata[25,17]*ipn_read_30_days+cor_edata[25,18]*ipn_read_365_days+cor_edata[25,15]*ipn_read)/(cor_edata[25,15]+cor_edata[25,16]+cor_edata[25,17]+cor_edata[25,18]+cor_edata[25,19]))
valid_data_mod = valid_data_mod %>% mutate(ipn_not_read_aggr = (cor_edata[25,48]*ipn_not_read_1_days + cor_edata[25,49]*ipn_not_read_7_days +cor_edata[25,50]*ipn_not_read_30_days +cor_edata[25,51]*ipn_not_read_365_days + cor_edata[25,47]*ipn_not_read)/(cor_edata[25,47]+cor_edata[25,48]+cor_edata[25,49]+cor_edata[25,50]+cor_edata[25,51]))
valid_data_mod = valid_data_mod %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
valid_data_mod = valid_data_mod %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
valid_data_mod = valid_data_mod %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)

#Aggregating the contest participation and login details
valid_data_mod = valid_data_mod %>% mutate(contest_login_aggr = (cor_edata[25,2]*contest_login_count_1_days + cor_edata[25,5]*contest_login_count_7_days + cor_edata[25,3] * contest_login_count_30_days + cor_edata[25,4]*contest_login_count_365_days +cor_edata[25,1]*contest_login_count)/(cor_edata[25,1]+cor_edata[25,2]+cor_edata[25,3]+cor_edata[25,4]+cor_edata[25,5]))
valid_data_mod = valid_data_mod %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)
valid_data_mod = valid_data_mod %>% mutate(contest_not_login_aggr = (cor_edata[25,48]*contest_not_login_count_1_days + cor_edata[25,49]*contest_not_login_count_7_days + cor_edata[25,50]*contest_not_login_count_30_days + cor_edata[25,51]*contest_not_login_count_365_days + cor_edata[25,47]*contest_not_login_cnt)/(cor_edata[25,47]+cor_edata[25,48]+cor_edata[25,49]+cor_edata[25,50]+cor_edata[25,51]))
valid_data_mod = valid_data_mod %>% select(-contest_not_login_cnt,-contest_not_login_count_1_days,-contest_not_login_count_7_days,-contest_not_login_count_30_days,-contest_not_login_count_365_days)
valid_data_mod = valid_data_mod %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)

valid_data_mod = valid_data_mod %>% mutate(sub_contest_aggr = (cor_edata[25,27]*submissions_count_contest_1_days + cor_edata[25,30]*submissions_count_contest_7_days + cor_edata[25,28]*submissions_count_contest_30_days + cor_edata[25,29]*submissions_count_contest_365_days +cor_edata[25,26]*submissions_count_contest)/(cor_edata[25,26]+cor_edata[25,27]+cor_edata[25,28]+cor_edata[25,29]+cor_edata[25,30]))
valid_data_mod = valid_data_mod %>% mutate(sub_master_aggr = (cor_edata[25,32]*submissions_count_master_1_days + cor_edata[25,35]*submissions_count_master_7_days + cor_edata[25,33]*submissions_count_master_30_days + cor_edata[25,34]*submissions_count_master_365_days + cor_edata[25,31]*submissions_count_master)/(cor_edata[25,31]+cor_edata[25,32]+cor_edata[25,33]+cor_edata[25,34]+cor_edata[25,35]))

valid_data_mod = valid_data_mod %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
valid_data_mod = valid_data_mod %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)
#Removing redundant data
valid_data_mod = valid_data_mod %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)


#Now the training dataset has 23 variabkes. Now we will deal with the categorical variables using one-hot encoding
library(dummies)
valid_data_mod = valid_data_mod %>% select(-mail_category, -hacker_timezone)
cat_valid_data= dummy.data.frame(valid_data_mod, names = c('mail_type'))

#Converting the binary boolean values to : true = 1 and false = 0
#hacker_confirmation
cat_valid_data$hacker_confirmation = as.numeric(cat_valid_data$hacker_confirmation)

cat_valid_data = cat_valid_data %>% select(-unsubscribe_time_t)


#Developing a logistic regression model 
Logistic1 = glm(opened ~ ., data = cat_rem_data, family = binomial)
preTrials_val = predict(Logistic1,newdata = cat_valid_data,type = "response")


library(ROCR)
ROCRpred = prediction(preTrials_val, cat_valid_data$opened)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

#Using the cross-validation dataset to choose the best threshold.
preTrials1 = ifelse(preTrials >= 0.54,'true','false')

#Now we will crossvalidate with the ROCR package and optimise it
misClasificError = mean(preTrials1 != cat_valid_data$opened)
Accuracy = 1 - misClasificError






#------------------------------- PREPARING THE TEST DATASET ----------------------------------





#Creating a copy of the test dataset, before performing any preprocessing
test_data = test_dataset

#Preparing the test data in order to run the above model
test_data$hacker_timezone = as.factor(test_data$hacker_timezone)

#Filling the missing values of the last_online attribute, with th hacker_created_at value
for(i in 1:nrow(test_data)){
  if(is.na(test_data[i,6] == TRUE)){
    test_data[i,6] = test_data[i,7]
  }
}

#Removing the user_id and mail_id
test_data_mod = test_data %>% select(-user_id, -mail_id)
rm(test_data)

#Adding new features
#Creating timeline for each record, by substracting the hacker_created_at
test_data_mod = test_data_mod %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60)

#Removing the redundant features
test_data_mod = test_data_mod %>% select(-sent_time,-last_online, -hacker_created_at)

#Aggregating the ipn_count and ipn_read details
test_data_mod = test_data_mod %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
test_data_mod = test_data_mod %>% mutate(contest_not_login_cnt = contest_participation_count - contest_login_count, contest_not_login_count_1_days=contest_participation_count_1_days - contest_login_count_1_days, contest_not_login_count_7_days=contest_participation_count_7_days - contest_login_count_7_days, contest_not_login_count_30_days=contest_participation_count_30_days - contest_login_count_30_days,contest_not_login_count_365_days=contest_participation_count_365_days - contest_login_count_365_days)



test_data_mod = test_data_mod %>% mutate(ipn_read_aggr = (cor_edata[25,16]*ipn_read_1_days + cor_edata[25,19]*ipn_read_7_days+cor_edata[25,17]*ipn_read_30_days+cor_edata[25,18]*ipn_read_365_days+cor_edata[25,15]*ipn_read)/(cor_edata[25,15]+cor_edata[25,16]+cor_edata[25,17]+cor_edata[25,18]+cor_edata[25,19]))
test_data_mod = test_data_mod %>% mutate(ipn_not_read_aggr = (cor_edata[25,48]*ipn_not_read_1_days + cor_edata[25,49]*ipn_not_read_7_days +cor_edata[25,50]*ipn_not_read_30_days +cor_edata[25,51]*ipn_not_read_365_days + cor_edata[25,47]*ipn_not_read)/(cor_edata[25,47]+cor_edata[25,48]+cor_edata[25,49]+cor_edata[25,50]+cor_edata[25,51]))
test_data_mod = test_data_mod %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
test_data_mod = test_data_mod %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
test_data_mod = test_data_mod %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)

#Aggregating the contest participation and login details
test_data_mod = test_data_mod %>% mutate(contest_login_aggr = (cor_edata[25,2]*contest_login_count_1_days + cor_edata[25,5]*contest_login_count_7_days + cor_edata[25,3] * contest_login_count_30_days + cor_edata[25,4]*contest_login_count_365_days +cor_edata[25,1]*contest_login_count)/(cor_edata[25,1]+cor_edata[25,2]+cor_edata[25,3]+cor_edata[25,4]+cor_edata[25,5]))
test_data_mod = test_data_mod %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)
test_data_mod = test_data_mod %>% mutate(contest_not_login_aggr = (cor_edata[25,48]*contest_not_login_count_1_days + cor_edata[25,49]*contest_not_login_count_7_days + cor_edata[25,50]*contest_not_login_count_30_days + cor_edata[25,51]*contest_not_login_count_365_days + cor_edata[25,47]*contest_not_login_cnt)/(cor_edata[25,47]+cor_edata[25,48]+cor_edata[25,49]+cor_edata[25,50]+cor_edata[25,51]))
test_data_mod = test_data_mod %>% select(-contest_not_login_cnt,-contest_not_login_count_1_days,-contest_not_login_count_7_days,-contest_not_login_count_30_days,-contest_not_login_count_365_days)
test_data_mod = test_data_mod %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)

test_data_mod = test_data_mod %>% mutate(sub_contest_aggr = (cor_edata[25,27]*submissions_count_contest_1_days + cor_edata[25,30]*submissions_count_contest_7_days + cor_edata[25,28]*submissions_count_contest_30_days + cor_edata[25,29]*submissions_count_contest_365_days +cor_edata[25,26]*submissions_count_contest)/(cor_edata[25,26]+cor_edata[25,27]+cor_edata[25,28]+cor_edata[25,29]+cor_edata[25,30]))
test_data_mod = test_data_mod %>% mutate(sub_master_aggr = (cor_edata[25,32]*submissions_count_master_1_days + cor_edata[25,35]*submissions_count_master_7_days + cor_edata[25,33]*submissions_count_master_30_days + cor_edata[25,34]*submissions_count_master_365_days + cor_edata[25,31]*submissions_count_master)/(cor_edata[25,31]+cor_edata[25,32]+cor_edata[25,33]+cor_edata[25,34]+cor_edata[25,35]))

test_data_mod = test_data_mod %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
test_data_mod = test_data_mod %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)
#Removing redundant data
test_data_mod = test_data_mod %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)


#Now the training dataset has 23 variabkes. Now we will deal with the categorical variables using one-hot encoding
library(dummies)
test_data_mod = test_data_mod %>% select(-mail_category, -hacker_timezone)
cat_test_data= dummy.data.frame(test_data_mod, names = c('mail_type'))

#Converting the binary boolean values to : true = 1 and false = 0
#hacker_confirmation
cat_test_data$hacker_confirmation = as.numeric(cat_test_data$hacker_confirmation)

#Use sampling to select any 50000 as the trainng point
#Left to be implemented                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               

#----------------Building the Logistic regression model---------------
#Removing the clicked and the unsubscribed attribute
cat_rem_data = cat_rem_data %>% select(-clicked, -unsubscribe_time_t)


Logistic1 = glm(opened ~ ., data = cat_rem_data, family = binomial)
preTrials = predict(Logistic1,newdata = cat_test_data,type = "response")


#----------- Evaluating the Logistic regression model-----------------------------


#Using the cross-validation dataset to choose the best threshold.
library(ROCR)
ROCRpred = prediction(preTrials, cat_test)


preTrials1 = ifelse(preTrials >= 0.30,'true','false')
conf_mat = table(preTrials1, cat_test_data$opened)


precis = conf_mat[2,2]/(conf_mat[2,2]+conf_mat[2,1])
recal = conf_mat[2,2]/(conf_mat[2,2] + conf_mat[1,2])
f1_score = (2*precis*recal)/(precis+recal)



Sscore = conf_mat[2,2]/(conf_mat[1,2] + conf_mat[2,2])

#Now we will crossvalidate with the ROCR package and optimise it
misClasificError = mean(preTrials1 != cat_test_data$opened)
Accuracy = 1 - misClasificError

#Accuracy is the desired evalation metric here 
#We focus on what percentage of the mails that were opened was correctly identified (S)
conf_mat = table(preTrials1, cat_test_data$opened)

S = conf_mat[2,2]/(conf_mat[1,2] + conf_mat[2,2])




#By comparing the S-score and the Accuracy, we get to understand  that a linear decision boundary will not solve the problem
#0.3 to 0.34 (S-score from 86.4 - 80 - 73 - 64)
# We intuitively think that svm would best solve our problem

#But we will repet and compare with the Decision tree model



#-------------------------- Starting the decision tree model -------------------------------------------

library(rpart)
rpart.model <- rpart(opened ~ .,data = data_train_t, method = "class")


