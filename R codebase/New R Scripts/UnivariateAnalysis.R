#Clearing the workspace
#Creating the email profile perfectly

rm(list = ls())
library(dplyr)

entire_data = read.csv('~/R/HackerRank Email/Email/training_dataset.csv', header = TRUE, stringsAsFactors = TRUE)

#Converting all the blank spaces to NA so that they can be treated at one go
entire_data[entire_data == ''] = NA
entire_data = subset(entire_data,(hacker_created_at <= last_online))
entire_data = subset(entire_data,!(opened == 'false' & clicked == 'true'))
entire_data = subset(entire_data,!(opened == 'false' & unsubscribed == 'true'))
entire_data = subset(entire_data,is.na(mail_category)==FALSE)

#Arranging the entire dataset based on non-decreasing sent_time
entire_data = entire_data %>% arrange(sent_time) %>% mutate(last_online_date = as.Date(as.POSIXct(last_online, origin="1970-01-01", tz="IST")),hacker_created_date = as.Date(as.POSIXct(hacker_created_at, origin="1970-01-01", tz="IST")),user_id_n = as.numeric(user_id),sent_date = as.Date(as.POSIXct(sent_time, origin="1970-01-01", tz="IST")),open_date = as.Date(as.POSIXct(open_time, origin="1970-01-01", tz="IST")))
entire_data = entire_data %>% mutate(age_weeks = as.numeric(difftime(last_online_date,hacker_created_date, units="weeks")))

#Removing mail_id, converting user_id to numeric
entire_data = entire_data %>% select(-mail_id,-user_id)
entire_data = entire_data %>% mutate(click_date = as.Date(as.POSIXct(click_time, origin="1970-01-01", tz="IST")), unsubscribed_date = as.Date(as.POSIXct(unsubscribe_time, origin="1970-01-01", tz="IST")))
entire_data = entire_data %>% select(-open_time, -click_time,-unsubscribe_time,-last_online,-sent_time,-hacker_created_at)

#Keeping the time granularity to be in days
#Transforming all the time variables relative to 11th Feb,2017 in terms of days difference
#Testing with dates in R
dt1 <- as.Date("2012-07-22")
dt2 = as.Date("2016-02-11")  #11th Feb, 2016
entire_data = entire_data %>% mutate(hacker_created_date = as.Date(as.POSIXct(hacker_created_at, origin="1970-01-01", tz="IST")),user_id_n = as.numeric(user_id),sent_date = as.Date(as.POSIXct(sent_time, origin="1970-01-01", tz="IST")),open_date = as.Date(as.POSIXct(open_time, origin="1970-01-01", tz="IST")))
entire_data = entire_data %>% mutate(open_days = as.numeric(difftime(open_date,dt2,units="days")), sent_days = as.numeric(difftime(sent_date,dt2,units="days")), click_days = as.numeric(difftime(click_date,dt2,units="days")), unsubscribed_days = as.numeric(difftime(unsubscribed_date,dt2,units="days")))
levels(entire_data$opened) = c(0,1)
levels(entire_data$clicked) = c(0,1)
levels(entire_data$unsubscribed) = c(0,1)
levels(entire_data$hacker_confirmation) = c(0,1)

library(dummies)
entire_data = dummy.data.frame(entire_data, names = c('mail_category', 'mail_type'))
entire_data = dummy.data.frame(entire_data, names = c('opened','clicked','unsubscribed'))
entire_data = entire_data %>% select(-unsubscribed0,-opened0, -clicked0)

#Storing this dataset, we will use this in the future 
write.csv(entire_data, file = 'email_datset_23rdApril.csv')
entire_data = read.csv(file = 'Profile datasets/email_datset_24thApril.csv', header = TRUE)

#Email profile done for descriptive purposes
#Separating the training and test set, as the above profiles will be created only on the training dataset
test_dataset = entire_data[seq(400001,nrow(entire_data)),]
train_dataset = entire_data[seq(1,400000),]
rm(entire_data)

#Creating user profile, date profile
# Number of rows = 29519
# Number of unique user_ids = 28509
user_prof1 = train_dataset %>% group_by(user_id_n) %>% summarise(hacker_confirm = max(hacker_confirmation),total_sent = n(), total_opened = sum(opened1), total_clicked = sum(clicked1), total_unsubscribed = sum(unsubscribed1),total_cat1 = sum(mail_categorymail_category_1), total_cat2 = sum(mail_categorymail_category_2),total_cat3 = sum(mail_categorymail_category_3),total_cat4 = sum(mail_categorymail_category_4),total_cat5 = sum(mail_categorymail_category_5),total_cat6 = sum(mail_categorymail_category_6),total_cat7 = sum(mail_categorymail_category_7),total_cat8 = sum(mail_categorymail_category_8),total_cat9 = sum(mail_categorymail_category_9), total_cat10 = sum(mail_categorymail_category_10), total_cat11 = sum(mail_categorymail_category_11), total_cat12 = sum(mail_categorymail_category_12),total_cat13 = sum(mail_categorymail_category_13),total_cat14 = sum(mail_categorymail_category_14),total_cat15 = sum(mail_categorymail_category_15),total_cat16 = sum(mail_categorymail_category_16),total_cat17 = sum(mail_categorymail_category_17),total_cat18 = sum(mail_categorymail_category_18),total_type1 = sum(mail_typemail_type_1), total_type2 = sum(mail_typemail_type_2),total_type3 = sum(mail_typemail_type_3),total_type4 = sum(mail_typemail_type_4),age_week = max(age_weeks))
user_prof1 = user_prof1 %>% mutate(open_percent = total_opened/total_sent)

write.csv(user_prof1, file = 'UserProfile23rdApril.csv')
write.csv(user_prof1, file = 'USerProfile24thApril.csv')

#Adding the features of the user with respect to the most recent entry to the user
#Creating day-wise profile
#Requires previous added features in the email profile
day_prof1 = train_dataset %>% group_by(sent_days) %>% summarise(total_sent = n(), total_opened = sum(opened1), total_clicked = sum(clicked1), total_unsubscribed = sum(unsubscribed1)) 

#Adding percentage to the features
day_prof1 = day_prof1 %>% mutate(opened_percent = total_opened/total_sent, clicked_percent = total_clicked/total_sent, unsubscribed_percent = total_unsubscribed/total_sent)
write.csv(day_prof1, file = 'DayProfile23rdApril.csv')
write.csv(day_prof2, file = 'DayProfile24thApril.csv')

extract_join = train_dataset1 %>% select(X,user_id_n) %>% arrange(user_id_n)
extract_last_update = extract_join %>% group_by(user_id_n) %>% summarise(last_index = max(X))

#Using the Decision Tree method to determine feature importance (Approach 1)
train_dataset = train_dataset %>% mutate(sent_time_gap = as.numeric(difftime(sent_date,last_online_date,units="days")), open_sent_time_gap = as.numeric(difftime(open_date,sent_date,units="days")), open_time_gap = as.numeric(difftime(open_date,last_online_date,units="days")), clicked_sent_time_gap = as.numeric(difftime(click_date,sent_date,units="days")), unsubscribe_sent_time_gap = as.numeric(difftime(unsubscribed_date,sent_date,units="days")))
train_open = subset(train_dataset, opened1 == 1)

plot(train_open$open_days, train_open$open_time_gap)

#For open_time_gap greater than the 3rd quantile, i.e, equal to : 15.0, removing the remaining points
train_open_third = subset(train_open, open_time_gap > 15.0)
plot(train_open_third$open_days, train_open_third$open_time_gap)
lines(train_open_third$open_days, rep(100,nrow(train_open_third)), col= 'red')

#Creating certain features that will help in the building of the day profile
train_dataset1 = train_dataset %>% mutate(open_time_mid = (open_time_gap > 15 & open_time_gap <=100), open_time_high = (open_time_gap > 100))
train_dataset1$open_time_mid = as.factor(train_dataset1$open_time_mid)
train_dataset1$open_time_high = as.factor(train_dataset1$open_time_high)

library(dummies)
train_dataset1 = dummy.data.frame(train_dataset1, names = c('open_time_mid','open_time_high'))
train_dataset1 = train_dataset1 %>% select(-open_time_highFALSE,-open_time_highNA,-open_time_midFALSE, -open_time_midNA)

#Storing this dataset, we will use this in the future 
write.csv(train_dataset1, file = 'email_datset_24thApril.csv')

train_open = subset(train_dataset1, opened1 == 1)
open_day_prof1 = train_open %>% group_by(open_days) %>% summarise(total_opened = n(), total_clicked = sum(clicked1), total_unsubscribed = sum(unsubscribed1), total_mid_open = sum(open_time_midTRUE), total_high_open = sum(open_time_highTRUE), max_open_time_gap = max(open_time_gap)) 
write.csv(open_day_prof1, file = 'open_day_profile dataset 24th April.csv')


plot(open_day_prof1$open_days, open_day_prof1$total_high_open, type = 'l')
plot(open_day_prof1$open_days, open_day_prof1$max_open_time_gap, type = 'l')


train_dataset1$open_time_mid = as.factor(train_dataset1$open_time_mid)


# We create box plots of important variables to show that their range such that it is not possible to use decision tree 
#differentiatiate between the action email opened and not opened

plot(subset(user_prof1,hacker_confirm == 1)$age_week, col='green')
points(subset(user_prof1, hacker_confirm == 0)$age_week, col='red')
user_prof1$hacker_confirm = as.factor(user_prof1$hacker_confirm)

#Using boxplot to understand the distribution of age in weeks with the fact whether hacker is confirmed or not
boxplot(age_week ~ hacker_confirm, data = user_prof1)
title('Understanding dist. of age_in_week with hacker_confirmation')


open_day_prof2 = subset(open_day_prof1, open_days<=87)

day_prof2 = day_prof1 %>% select(-total_clicked, -total_opened, -total_unsubscribed)
day_prof2 = cbind(day_prof2, open_day_prof2)

plot(day_prof2$sent_days,day_prof2$total_sent, type = 'l', col='red')
lines(day_prof2$sent_days, day_prof2$total_opened, type = 'l', col = 'green')
legend(x="topleft",legend= c('total sent', 'total opened'),lty=c(1,1), lwd=c(2.5,2.5),col=c('red','green'),cex = 0.6) 


#Adding features w.r.t last update to user_profile
user_prof1 = cbind(extract_last_update$last_index,user_prof1)
#Merging the train_dataset with the user_profile dataset
train_data_merge = train_dataset %>% select(X,contest_login_count,contest_login_count_1_days,contest_login_count_30_days,contest_login_count_365_days,contest_login_count_7_days,contest_participation_count,contest_participation_count_1_days,contest_participation_count_30_days,contest_participation_count_365_days,contest_participation_count_7_days,ipn_count,ipn_count_1_days,ipn_count_30_days,ipn_count_365_days,ipn_count_7_days,ipn_read,ipn_read_1_days,ipn_read_30_days,ipn_read_365_days,ipn_read_7_days,forum_comments_count,forum_count,forum_expert_count,forum_questions_count,submissions_count,submissions_count_1_days,submissions_count_30_days,submissions_count_365_days,submissions_count_7_days,submissions_count_contest,submissions_count_contest_1_days,submissions_count_contest_30_days,submissions_count_contest_365_days,submissions_count_contest_7_days,submissions_count_master,submissions_count_master_1_days,submissions_count_master_30_days,submissions_count_master_365_days,submissions_count_master_7_days)

#Renaming the last_update column as X to allow merge
colnames(user_prof1)[1] = 'X'
user_prof2 = merge(user_prof1, train_data_merge, by='X')

write.csv(user_prof2, "New datatsets/USerProfile24thApril.csv")

write.csv(open_day_prof1, "Opened mails daily 149days.csv")

#----------------------------------------OLD ANALYSIS---------------------------------------------------------






entire_data[,sent_time_gap := (sent_time - last_online)/60][,last_online_t := (last_online - hacker_created_at)/60] [,sent_time_t := (sent_time - hacker_created_at)/60]
entire_data[,open_time_h := open_time - hacker_created_at][,open_time_t := open_time-sent_time] [,click_time_t := click_time - sent_time] [,unsubscribe_time_t := unsubscribe_time - sent_time]

min_hacker_created_at = min(entire_data$hacker_created_at)
min_sent_time = min(entire_data$sent_time, na.rm=TRUE)
entire_data[,hacker_creation_temporal := (hacker_created_at - min_hacker_created_at)/86400 ][, sent_time_temporal := (sent_time - min_sent_time)/86400]
entire_data[,c('sent_time','hacker_created_at','last_online','mail_id','user_id'):= NULL]

#Starting the univariate analysis
train_open = train_dataset1 %>% filter(opened == 'true')
train_closed = train_dataset1 %>% filter(opened == 'false')

train_open_t = as.data.table(train_open)
train_open_t[,prop.table(table(forum_count))]
train_closed_t = as.data.table(train_closed)



train_closed_t[,prop.table(table(forum_count))]

train_open_sent_time_gap = train_open$sent_time_gap/1440
train_closed_sent_time_gap = train_closed$sent_time_gap/1440
plot(train_open_sent_time_gap,train_open$sent_time_temporal)
points(train_closed_sent_time_gap, train_closed$sent_time_temporal, col='red')



#Some inferences from the plot is that : the mails are opened in bulk by the partcipants before an event of their choice
#Planning to group by ont the sent_time_temporal

#Checking the number of unique values of sent_time_temporal
length(unique(train_open$sent_time_temporal))
entire_sent_data = train_open %>% group_by(sent_time_temporal) %>% summarise(total_mail = n()) %>% arrange(desc(total_mail))