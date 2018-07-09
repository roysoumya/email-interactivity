library('dplyr')
library("e1071")

edata = read.csv('~/R/HackerRank Email/Email/training_dataset.csv')
etest = read.csv('~/R/HackerRank Email/Email/test_dataset.csv')

#Remove uncommon columns between training dataset and testing dataset
edata = edata %>% select(-click_time,-clicked,-open_time,-unsubscribed,-unsubscribe_time)

var_list = edata$last_online
str_list =  edata$hacker_created_at
for (i in 1:length(var_list)) {
  if(is.na(var_list[i]))
    var_list[i]= str_list[i]
}
edata$last_online = var_list

edata = edata %>% select(-hacker_timezone)
edata = edata %>% mutate(sent_last = sent_time - last_online)


var_list = etest$last_online
str_list =  etest$hacker_created_at
for (i in 1:length(var_list)) {
  if(is.na(var_list[i]))
    var_list[i]= str_list[i]
}
etest$last_online = var_list

etest = etest %>% select(-hacker_timezone)
etest = etest %>% mutate(sent_last = sent_time - last_online)

#Removing user_id and mail_id from both
edata = edata %>% select(-user_id, -mail_id)
etest = etest %>% select(-user_id, -mail_id)

edata = edata %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)
etest = etest %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)

edata = edata %>% mutate(sub_contest_aggr = 0.50*submissions_count_contest_1_days + 0.30*submissions_count_contest_7_days + 0.10*submissions_count_contest_30_days + 0.05*submissions_count_contest_365_days +0.05*submissions_count_contest)
edata = edata %>% mutate(sub_master_aggr = 0.50*submissions_count_master_1_days + 0.30*submissions_count_master_7_days + 0.10*submissions_count_master_30_days + 0.05*submissions_count_master_365_days + 0.05*submissions_count_master)
etest = etest %>% mutate(sub_contest_aggr = 0.50*submissions_count_contest_1_days + 0.30*submissions_count_contest_7_days + 0.10*submissions_count_contest_30_days + 0.05*submissions_count_contest_365_days +0.05*submissions_count_contest)
etest = etest %>% mutate(sub_master_aggr = 0.50*submissions_count_master_1_days + 0.30*submissions_count_master_7_days + 0.10*submissions_count_master_30_days + 0.05*submissions_count_master_365_days + 0.05*submissions_count_master)
edata = edata %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
edata = edata %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)
etest = etest %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
etest = etest %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)

#Aggregating the contest participation and login details
edata = edata %>% mutate(contest_not_login_cnt = contest_participation_count - contest_login_count, contest_not_login_count_1_days=contest_participation_count_1_days - contest_login_count_1_days, contest_not_login_count_7_days=contest_participation_count_7_days - contest_login_count_7_days, contest_not_login_count_30_days=contest_participation_count_30_days - contest_login_count_30_days,contest_not_login_count_365_days=contest_participation_count_365_days - contest_login_count_365_days)
edata = edata %>% mutate(contest_login_aggr = 0.50*contest_login_count_1_days + 0.30*contest_login_count_7_days + 0.10 * contest_login_count_30_days + 0.05*contest_login_count_365_days +0.05*contest_login_count)
edata = edata %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)
edata = edata %>% mutate(contest_not_login_aggr = 0.50*contest_not_login_count_1_days + 0.30*contest_not_login_count_7_days + 0.10*contest_not_login_count_30_days + 0.05*contest_not_login_count_365_days + 0.05*contest_not_login_cnt)
edata = edata %>% select(-contest_not_login_cnt,-contest_not_login_count_1_days,-contest_not_login_count_7_days,-contest_not_login_count_30_days,-contest_not_login_count_365_days)
edata = edata %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)
etest = etest %>% mutate(contest_not_login_cnt = contest_participation_count - contest_login_count, contest_not_login_count_1_days=contest_participation_count_1_days - contest_login_count_1_days, contest_not_login_count_7_days=contest_participation_count_7_days - contest_login_count_7_days, contest_not_login_count_30_days=contest_participation_count_30_days - contest_login_count_30_days,contest_not_login_count_365_days=contest_participation_count_365_days - contest_login_count_365_days)
etest = etest %>% mutate(contest_login_aggr = 0.50*contest_login_count_1_days + 0.30*contest_login_count_7_days + 0.10 * contest_login_count_30_days + 0.05*contest_login_count_365_days +0.05*contest_login_count)
etest = etest %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)
etest = etest %>% mutate(contest_not_login_aggr = 0.50*contest_not_login_count_1_days + 0.30*contest_not_login_count_7_days + 0.10*contest_not_login_count_30_days + 0.05*contest_not_login_count_365_days + 0.05*contest_not_login_cnt)
etest = etest %>% select(-contest_not_login_cnt,-contest_not_login_count_1_days,-contest_not_login_count_7_days,-contest_not_login_count_30_days,-contest_not_login_count_365_days)
etest = etest %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)


#Aggregating the ipn_count and ipn_read details
edata = edata %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
edata = edata %>% mutate(ipn_read_aggr = 0.50*ipn_read_1_days + 0.30*ipn_read_7_days+0.10*ipn_read_30_days+0.05*ipn_read_365_days+0.05*ipn_read)
edata = edata %>% mutate(ipn_not_read_aggr = 0.50*ipn_not_read_1_days + 0.30*ipn_not_read_7_days + 0.10*ipn_not_read_30_days + 0.05*ipn_not_read_365_days + 0.05*ipn_not_read)
edata = edata %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
edata = edata %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
edata = edata %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)
etest = etest %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
etest = etest %>% mutate(ipn_read_aggr = 0.50*ipn_read_1_days + 0.30*ipn_read_7_days+0.10*ipn_read_30_days+0.05*ipn_read_365_days+0.05*ipn_read)
etest = etest %>% mutate(ipn_not_read_aggr = 0.50*ipn_not_read_1_days + 0.30*ipn_not_read_7_days + 0.10*ipn_not_read_30_days + 0.05*ipn_not_read_365_days + 0.05*ipn_not_read)
etest = etest %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
etest = etest %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
etest = etest %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)

edata = edata %>% mutate(mail_sent_age = sent_time - hacker_created_at, user_age = last_online - hacker_created_at)
etest = etest %>% mutate(mail_sent_age = sent_time - hacker_created_at, user_age = last_online - hacker_created_at)
edata = edata %>% select(-sent_time,-hacker_created_at,-last_online)
etest = etest %>% select(-sent_time,-hacker_created_at,-last_online)


#Except opened, we convert the categorical variable into numeric
edata$mail_type = as.numeric(edata$mail_type)
edata$hacker_confirmation = as.numeric(edata$hacker_confirmation)
edata$mail_category = as.numeric(edata$mail_category)
etest$mail_type = as.numeric(etest$mail_type)
etest$hacker_confirmation = as.numeric(etest$hacker_confirmation)
etest$mail_category = as.numeric(etest$mail_category)

#Split into 70% training and 30% for cross-validating
library(caTools)
set.seed(881)
spliT = sample.split(edata$opened, SplitRatio = 0.05)
data_read_train = subset(edata, spliT == TRUE)
data_read_valid = subset(edata, spliT == FALSE)
spliT1 = sample.split(data_read_valid$opened, SplitRatio = 0.02)
data_read_valid1 = subset(data_read_valid, spliT1 == TRUE)


y = data_read_valid1$opened
x = data_read_valid1 %>% select(-opened) 
summary(data_read_train)

data_train_small = data_read_train[1:25000,]

svm_model1 <- svm(opened ~ ., data=data_read_train, kernel="radial", cost=1, gamma=0.5)
svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

pred_class <- predict(svm_model1,newdata = etest)
fitted.results = fitted.results <- ifelse(pred_class == 'true',1,0)
prediction_fr = data.frame(fitted.results)
write.csv(prediction_fr, file='prediction.csv')
