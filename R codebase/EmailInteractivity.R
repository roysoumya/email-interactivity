library(dplyr)
library(lubridate)
#email_data = read.csv('~/R/HackerRank Email/Email/training_dataset.csv', header = TRUE, stringsAsFactors = TRUE)
email_data = read.csv('~/Documents/Personal/Email project/HackerRank Email/Email/training_dataset.csv', header = TRUE, stringsAsFactors = TRUE)

#Converting all the blank spaces to NA so that they can be treated at one go
email_data[email_data == ''] = NA
email_data = subset(email_data,(hacker_created_at <= last_online))
email_data = subset(email_data,!(opened == 'false' & clicked == 'true'))
email_data = subset(email_data,!(opened == 'false' & unsubscribed == 'true'))
email_data = subset(email_data,is.na(mail_category)==FALSE)

#Selecting the attributes that will be required for finding email interactivity
email_inter = email_data %>% select(opened,open_time, clicked,click_time, unsubscribed, unsubscribe_time,sent_time, hacker_timezone)
rm(email_data)

email_inter = email_inter %>% mutate(sent_open_minute = ceiling((open_time - sent_time)/3600), open_click_minute = ceiling((click_time - open_time)/3600), open_unsub_minute = ceiling((unsubscribe_time - open_time)/3600))
email_opened_w = subset(email_inter, opened=='true')

email_opened_w = email_opened_w %>% mutate(log_sent_open_minute = log2(sent_open_minute))
summary(email_opened_w$log_sent_open_minute)


#Generating cumulative frequency distribution
duration = email_opened_w$log_sent_open_minute 
breaks = seq(0.0, 12.0, by=1.0) 
duration.cut = cut(duration, breaks, right=FALSE) 
duration.freq = table(duration.cut)
duration.freq1 = duration.freq

for( i in 1:length(duration.freq1)){
  duration.freq1[i] = log2(duration.freq1[i])
}

#plot(duration.freq, type="o", xlab ='log2(sent_time - open_time in hours)', ylab = 'log2(count of mails opened)')
plot(duration.freq1, type="o", xlab ='log2(sent_time - open_time in hours)', ylab = 'log2(count of mails opened)')
abline(lm(duration.freq1 ~ seq(0, length(duration.freq1)-1)), col='blue')
#Drawing the best fit line






email_20 = subset(email_opened_w, sent_open_days < 20)

day_cat=NULL
for( i in 1:nrow(email_opened_w)){
  if(email_opened_w[i,9] <= 10 ){
    day_cat = c(day_cat, email_opened_w[i,9])
  }
  else{
    day_cat = c(day_cat, 11)
  }
}
email_opened_w$day_category = day_cat
email_opened_w$day_category = as.factor(round(email_opened_w$day_category))

barplot(table(email_opened_w$day_category),xlab = 'Time gap between sent_time and open_time in days', ylab = 'Number of mails opened', main = 'Time gap distribution in days')

#Marking sent_open_days > 10, as 11 which is a separate level altogether


#In order to determine the email_interactivity, we do not deal with the missing values of hacker_timezone as we had done previously
email_inter = subset(email_inter, is.na(hacker_timezone)==FALSE)
email_inter$hacker_timezone = as.factor(email_inter$hacker_timezone)
barplot(table(email_inter$hacker_timezone), xlab = 'Hacker timezones', ylab = 'Freq. of records in dataset', main = 'Freq. dist. of data among timezones')

#From the plot we see timezone 18000 has 212892 records, so we use it for determining the time of day mails are opened, clicked or unsubscribed
#otherwise this time would have varied for the different timezones
email18000 = subset(email_inter, hacker_timezone == 18000)

#Converting the sent_time and open_time to POSIXct variable
email18000$sent_time = as.POSIXct(email18000$sent_time, origin="1970-01-01", tz="IST")
email18000$open_time = as.POSIXct(email18000$open_time, origin="1970-01-01", tz="IST")
email18000 = email18000 %>% mutate(opened_hour = hour(open_time), sent_hour = hour(sent_time))

email18000$sent_hour = as.factor(email18000$sent_hour)
plot(table(email18000$sent_hour), type = 'l', xlab = 'Hours of the day', ylab = 'Frequency of mails sent', main = 'Freq. dist. of mails sent in 24 hours')

email_opened = subset(email18000, is.na(opened_hour)==FALSE)
#Converting hour to a categorical variable
email_opened$opened_hour = as.factor(email_opened$opened_hour)
barplot(table(email_opened$opened_hour), type = 'l', xlab = 'Hours of the day', ylab = 'Frequency of mails opened', main = 'Freq. dist. of mails opened in 24 hours')

rm(email18000, email_opened)

#Extracting weekday
#Converting the sent_time and open_time to POSIXct variable
email_inter$sent_time = as.POSIXct(email_inter$sent_time, origin="1970-01-01", tz="IST")
email_inter$open_time = as.POSIXct(email_inter$open_time, origin="1970-01-01", tz="IST")
email_inter$click_time = as.POSIXct(email_inter$click_time, origin="1970-01-01", tz="IST")
email_inter$unsubscribe_time = as.POSIXct(email_inter$unsubscribe_time, origin="1970-01-01", tz="IST")
email_inter = email_inter %>% mutate(opened_wday = wday(open_time, label=TRUE,abbr = TRUE), sent_wday = wday(sent_time, label=TRUE, abbr=TRUE))
barplot(table(email_inter$opened_wday),xlab = 'Days of the week', ylab = 'Freq. of mails opened', main='Freq. dist. of mails opened in 1 week')
barplot(table(email_inter$sent_wday),xlab = 'Days of the week', ylab = 'Freq. of mails sent', main='Freq. dist. of mails sent in 1 week')



