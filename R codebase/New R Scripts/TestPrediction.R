library('dplyr')

etest = read.csv('~/Documents/HackerRank/HackerRank Email/Email/test_dataset.csv')

var_list = etest$last_online
str_list =  etest$hacker_created_at
for (i in 1:length(var_list)) {
  if(is.na(var_list[i]))
    var_list[i]= str_list[i]
}
etest$last_online = var_list

#Eliminating hacker_timezone from the present calculation
etest = etest %>% select(-hacker_timezone)
etest = etest %>% mutate(sent_last = sent_time - last_online)
write.csv(edata, file = 'training_dataset_mod.csv')

#Feature engineering
etest = etest %>% mutate(sub_count_others = submissions_count - submissions_count_contest,sub_count_others_1_days = submissions_count_1_days - submissions_count_contest_1_days, sub_count_others_7_days = submissions_count_7_days - submissions_count_contest_7_days,sub_count_others_30_days = submissions_count_30_days - submissions_count_contest_30_days, sub_count_others_365_days = submissions_count_365_days - submissions_count_contest_365_days)

#Generating new sub_other and sub_contest parameter by assigning weights to the submissions
#1_day = 0.50, 7_days = 0.30, 30_days = 0.10, 365_days = 0.05, total = 0.05
etest = etest %>% mutate(sub_others_aggr = 0.50*sub_count_others_1_days + 0.30*sub_count_others_7_days + 0.10*sub_count_others_30_days + 0.05*sub_count_others_365_days+0.05*sub_count_others)
etest = etest %>% mutate(sub_contest_aggr = 0.50*submissions_count_contest_1_days + 0.30*submissions_count_contest_7_days + 0.10*submissions_count_contest_30_days + 0.05*submissions_count_contest_365_days +0.05*submissions_count_contest)
etest = etest %>% mutate(sub_master_aggr = 0.50*submissions_count_master_1_days + 0.30*submissions_count_master_7_days + 0.10*submissions_count_master_30_days + 0.05*submissions_count_master_365_days + 0.05*submissions_count_master)

#Removing the columns that were aggregated
etest = etest %>% select(-sub_count_others,-sub_count_others_1_days,-sub_count_others_7_days,-sub_count_others_30_days,-sub_count_others_365_days)
etest = etest %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)
etest = etest %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
etest = etest %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)

#Removing sub_master_aggr as it is the same as sub_others_aggr
etest = etest %>% select(-sub_master_aggr)

#Aggregating the contest participation and login details
etest = etest %>% mutate(contest_login_aggr = 0.50*contest_login_count_1_days + 0.30*contest_login_count_7_days + 0.10 * contest_login_count_30_days + 0.05*contest_login_count_365_days +0.05*contest_login_count)
etest = etest %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)

#Aggregating the ipn_count and ipn_read details
etest = etest %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
etest = etest %>% mutate(ipn_read_aggr = 0.50*ipn_read_1_days + 0.30*ipn_read_7_days+0.10*ipn_read_30_days+0.05*ipn_read_365_days+0.05*ipn_read)
etest = etest %>% mutate(ipn_not_read_aggr = 0.50*ipn_not_read_1_days + 0.30*ipn_not_read_7_days + 0.10*ipn_not_read_30_days + 0.05*ipn_not_read_365_days + 0.05*ipn_not_read)
etest = etest %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
etest = etest %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
etest = etest %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)
corr_feature_no_fact = cor(edata_no_fact)
write.csv(corr_feature_no_fact, file = 'Corr_Featured_No_Factor.csv')

etest = etest %>% mutate(mail_sent_age = sent_time - hacker_created_at, user_age = last_online - hacker_created_at)
etest = etest %>% select(-sent_time,-last_online,-hacker_created_at)
etest = etest %>% select(-mail_sent_age)

etest_final = data.frame(etest$hacker_confirmation, etest$user_id, etest$mail_id, etest$mail_category, etest$mail_type,etest)
write.csv(edata_final, file = 'test_dataset_mod_final.csv')

#Manually changed their column names from edata.mail_id to mail_id








#Changing epoch to date to get better understanding
edata = edata %>% mutate(send_time_info = as.Date(as.POSIXct(sent_time,origin="1970-01-01")))
edata = edata %>% mutate(open_time_info = as.Date(as.POSIXct(open_time,origin="1970-01-01")))

edata = edata %>% mutate(click_time_info = as.Date(as.POSIXct(click_time,origin="1970-01-01")))
edata = edata %>% mutate(last_time_info = as.Date(as.POSIXct(last_online,origin="1970-01-01")))
edata = edata %>% mutate(create_time_info = as.Date(as.POSIXct(hacker_created_at,origin="1970-01-01")))

#Changing epoch to date to get better understanding
etest = etest %>% mutate(send_time_info = as.Date(as.POSIXct(sent_time,origin="1970-01-01")))
etest = edata %>% mutate(open_time_info = as.Date(as.POSIXct(open_time,origin="1970-01-01")))

etest = etest %>% mutate(click_time_info = as.Date(as.POSIXct(click_time,origin="1970-01-01")))
etest = etest %>% mutate(last_time_info = as.Date(as.POSIXct(last_online,origin="1970-01-01")))
etest= etest %>% mutate(create_time_info = as.Date(as.POSIXct(hacker_created_at,origin="1970-01-01")))



#------------------------------------------------------------------------------------------------
#Baseline model where we randomly predict the values
result = sample(c(0,1),size = nrow(etest),replace = TRUE)
res_fr = as.data.frame(result)
write.csv(res_fr,'~/Documents/HackerRank/HackerRank Email/Email/prediction.csv')

#Random stuff
opened = subset(edata,opened=='true')
closed = subset(edata,opened=='false')

cedata = edata %>% arrange(user_id)

user_sent = edata %>% group_by(user_id,mail_id) %>% summarise(comm_no = n()) %>% arrange(desc(comm_no))
sample(c(0,1),size = 5,replace = TRUE)

head(sort(summary(edata$mail_id),decreasing = TRUE))

