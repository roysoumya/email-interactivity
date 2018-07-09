library(dplyr)

edata = read.csv('~/R/HackerRank Email/Email/training_dataset.csv')
#etest = read.csv('~/R/HackerRank Email/Email/test_dataset.csv')

table(edata$opened)
table(edata$clicked)
table(edata$unsubscribed)

length(unique(edata$mail_id))
length(unique(edata$user_id))

train_unique_mail_id = unique(edata$mail_id)
train_unique_user_id = unique(edata$user_id)
test_unique_mail_id = unique(etest$mail_id)
test_unique_user_id = unique(etest$user_id)

#Creating histogram to understand what proportion of users received how many number of mails
hist(table(edata$user_id))
hist(table(edata$mail_id))


#opened = True (161347 records); opened = False (324701)
train_opened = filter(edata, opened == "true")
train_closed = edata %>% filter(opened == "false")

#edata = edata %>% mutate(mail_id_n = as.numeric(mail_id))
#edata = edata %>% mutate(user_id_n = as.numeric(user_id))

not_online = filter(train_closed, is.na(last_online)== TRUE)

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

#Removing the attributes - hacker_timezone and mail_id
edata_mod = edata_mod %>% select(-mail_id, -hacker_timezone)

#Creating timeline for each record, by substracting the hacker_created_at
edata_mod = edata_mod %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60, open_time_t = (open_time - hacker_created_at)/60, click_time_t = (click_time - hacker_created_at)/60, unsubscribe_time_t = (unsubscribe_time - hacker_created_at)/60)

summary(edata_mod)
min_hacker_created_at = min(edata_mod$hacker_created_at)
edata_mod = edata_mod %>% mutate( hacker_creation_temporal = (hacker_created_at - min_hacker_created_at)/86400 ) 

#Removing the redundant features
edata_ready = edata_mod %>% select(-sent_time,-open_time,-click_time,-unsubscribe_time,-last_online, -hacker_created_at,-user_id)

#Dealing with the categorical variables
library(dummies)
edata_new = dummy.data.frame(edata, names = c('mail_type','mail_category'))

edata_ready = edata_ready %>% mutate(opened_n = as.numeric(opened))
edata_ready = edata_ready %>% mutate(clicked_n = as.numeric(clicked))
edata_ready = edata_ready %>% mutate(unsubscribed_n = as.numeric(unsubscribed))
edata_ready = edata_ready %>% mutate(hacker_confirmation_n = as.numeric(hacker_confirmation))
edata_ready = edata_ready %>% select(-opened, -clicked, -unsubscribed, -hacker_confirmation)


#edata_ready$mail_category = as.numeric(edata_ready$mail_category)
#edata_ready$mail_type = as.numeric(edata_ready$mail_type)

#Dealing with the categorical variables: (mail_category, mail_type) by assigning each level to be a separate category and giving them values 0 or 1
cat_zero ='
all_zero = rep(0,nrow(edata_ready))
mail_type_1 = mail_type_2 = mail_type_3 = mail_type_4 = mail_type_5 = all_zero
mail_category_1 = mail_category_2 = mail_category_3 = mail_category_4 = mail_category_5 = mail_category_6 = mail_category_7 = mail_category_8 = mail_category_9 = mail_category_10 = mail_category_11 = mail_category_12 = mail_category_13 = mail_category_14 = mail_category_15 = mail_category_16 = mail_category_17 = mail_category_18 = mail_category_19 = all_zero 

for (i in 1:nrow(edata_ready)) {
  
  #mail_type
  if(edata_ready[i,2] == 1){
    mail_type_1[i] = 1 
  }
  else if(edata_ready[i,2] == 2){
    mail_type_2[i] = 1 
  }
  else if(edata_ready[i,2] == 3){
    mail_type_3[i] = 1 
  }
  else if(edata_ready[i,2] == 4){
    mail_type_4[i] = 1 
  }
  else{
    mail_type_5[i]=1
  }
  
  #mail_category
  if(edata_ready[i,1] == 1){
    mail_category_1[i] = 1 
  }
  else if(edata_ready[i,1] == 2){
    mail_category_2[i] = 1 
  }
  else if(edata_ready[i,1] == 3){
    mail_category_3[i] = 1 
  }
  else if(edata_ready[i,1] == 4){
    mail_category_4[i] = 1 
  }
  else if(edata_ready[i,1] == 5){
    mail_category_5[i] = 1 
  }
  else if(edata_ready[i,1] == 6){
    mail_category_6[i] = 1 
  }
  else if(edata_ready[i,1] == 7){
    mail_category_7[i] = 1 
  }
  else if(edata_ready[i,1] == 8){
    mail_category_8[i] = 1 
  }
  else if(edata_ready[i,1] == 9){
    mail_category_9[i] = 1 
  }
  else if(edata_ready[i,1] == 10){
    mail_category_10[i] = 1 
  }
  else if(edata_ready[i,1] == 11){
    mail_category_11[i] = 1 
  }
  else if(edata_ready[i,1] == 12){
    mail_category_12[i] = 1 
  }
  else if(edata_ready[i,1] == 13){
    mail_category_13[i] = 1 
  }
  else if(edata_ready[i,1] == 14){
    mail_category_14[i] = 1 
  }
  else if(edata_ready[i,1] == 15){
    mail_category_15[i] = 1 
  }
  else if(edata_ready[i,1] == 16){
    mail_category_16[i] = 1 
  }
  else if(edata_ready[i,1] == 17){
    mail_category_17[i] = 1 
  }
  else if(edata_ready[i,1] == 18){
    mail_category_18[i] = 1 
  }
  else{
    mail_category_19[i] = 1 
  }
}


edata_ready = cbind(mail_type_1,mail_type_2,mail_type_3,mail_type_4,mail_type_5,mail_category_1,mail_category_2,mail_category_3,mail_category_4,mail_category_5,mail_category_6,mail_category_7,mail_category_8,mail_category_9,mail_category_10,mail_category_11,mail_category_12,mail_category_13,mail_category_14,mail_category_15,mail_category_16,mail_category_17,mail_category_18,mail_category_19,edata_ready)
edata_ready = edata_ready %>% select(-mail_type,-mail_category)'

#Hackertimezone is also a categorical variable
cat_one = '
hacker_timezone_1 = hacker_timezone_2 = hacker_timezone_3 = hacker_timezone_4 = hacker_timezone_5 = hacker_timezone_6 = hacker_timezone_7 = hacker_timezone_8 = hacker_timezone_9 = hacker_timezone_10 = hacker_timezone_11 = hacker_timezone_12 = hacker_timezone_13 = hacker_timezone_14 = hacker_timezone_15 = hacker_timezone_16 = hacker_timezone_17 = hacker_timezone_18 = hacker_timezone_19 = hacker_timezone_20 = hacker_timezone_21 = hacker_timezone_22 = hacker_timezone_23 = hacker_timezone_24 = all_zero
for (i in 1:nrow(edata_ready)) {
  
  #mail_category
  if(is.na(edata_ready[i,25]) == TRUE){
    hacker_timezone_24[i] = 1
  }
  else if(edata_ready[i,25] == -43200){
    hacker_timezone_1[i] = 1 
  }
  else if(edata_ready[i,25] == -36000){
    hacker_timezone_2[i] = 1 
  }
  else if(edata_ready[i,25] == -28800){
    hacker_timezone_3[i] = 1 
  }
  else if(edata_ready[i,25] == -25200){
    hacker_timezone_4[i] = 1 
  }
  else if(edata_ready[i,25] == -21600){
    hacker_timezone_5[i] = 1 
  }
  else if(edata_ready[i,25] == -18000){
    hacker_timezone_6[i] = 1 
  }
  else if(edata_ready[i,25] == -14400){
    hacker_timezone_7[i] = 1 
  }
  else if(edata_ready[i,25] == -10800){
    hacker_timezone_8[i] = 1 
  }
  else if(edata_ready[i,25] == 0){
    hacker_timezone_9[i] = 1 
  }
  else if(edata_ready[i,25] == 3600){
    hacker_timezone_10[i] = 1 
  }
  else if(edata_ready[i,25] == 7200){
    hacker_timezone_11[i] = 1 
  }
  else if(edata_ready[i,25] == 10800){
    hacker_timezone_12[i] = 1 
  }
  else if(edata_ready[i,25] == 14400){
    hacker_timezone_13[i] = 1 
  }
  else if(edata_ready[i,25] == 18000){
    hacker_timezone_14[i] = 1 
  }
  else if(edata_ready[i,25] == 21600){
    hacker_timezone_15[i] = 1 
  }
  else if(edata_ready[i,25] == 25200){
    hacker_timezone_16[i] = 1 
  }
  else if(edata_ready[i,25] == 28800){
    hacker_timezone_17[i] = 1 
  }
  else if(edata_ready[i,25] == 32400){
    hacker_timezone_18[i] = 1 
  }
  else if(edata_ready[i,25] == 36000){
    hacker_timezone_19[i] = 1 
  }
  else if(edata_ready[i,25] == 39600){
    hacker_timezone_20[i] = 1 
  }
  else if(edata_ready[i,25] == 43200){
    hacker_timezone_21[i] = 1 
  }
  else if(edata_ready[i,25] == 46800){
    hacker_timezone_22[i] = 1 
  }
  else if(edata_ready[i,25] == 50400){
    hacker_timezone_23[i] = 1 
  }
}

edata_ready = cbind(edata_ready, hacker_timezone_1,hacker_timezone_2,hacker_timezone_3,hacker_timezone_4,hacker_timezone_5,hacker_timezone_6,hacker_timezone_7,hacker_timezone_8,hacker_timezone_9,hacker_timezone_10,hacker_timezone_11,hacker_timezone_12,hacker_timezone_13,hacker_timezone_14,hacker_timezone_15,hacker_timezone_16,hacker_timezone_17,hacker_timezone_18,hacker_timezone_19,hacker_timezone_20,hacker_timezone_21,hacker_timezone_22,hacker_timezone_23,hacker_timezone_24)
edata_ready = edata_ready %>% select(-hacker_timezone)'

rm(edata, edata_mod, edata_ready)
rm(train_closed, train_opened)

write.csv(edata_new, file = "Email Preprocessed Data 11th April.csv")


library(caTools)
set.seed(881)
spliT = sample.split(entire_data$opened, SplitRatio = 0.70)
train_dataset = subset(entire_data, spliT == TRUE)
test_dataset = subset(entire_data, spliT == FALSE)

#Splitting the training dataset into training and crossvalidation parts
set.seed(123)
split1 = sample.split(train_dataset$opened, SplitRatio = 0.70)
train_data = subset(train_dataset, split1 == TRUE)
validation_data = subset(train_dataset, split1 == FALSE)

rm(train_dataset)



