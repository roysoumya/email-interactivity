library(dplyr)

edata = read.csv('~/R/HackerRank Email/Email/training_dataset.csv')
etest = read.csv('~/R/HackerRank Email/Email/test_dataset.csv')

table(edata$opened)
table(edata$clicked)
table(edata$unsubscribed)

length(unique(edata$mail_id))
length(unique(edata$user_id))

edata = edata %>% mutate(mail_id_n = as.numeric(mail_id))
edata = edata %>% mutate(user_id_n = as.numeric(user_id))

#Ignoring user_id and mail_id
edata = edata %>% select(-user_id,-mail_id)
write.csv(edata, file="Minified training set.csv")


save_data = edata
save_cor = cor_cedata
#Substract sent_time by lowest value
min_sent_time = min(edata$sent_time)
min_hacker_created = min(edata$hacker_created_at)
min_last_online  = min(edata$last_online,na.rm = TRUE)
edata = edata %>% mutate(new_sent_time = sent_time - min_sent_time, new_hacker_created_at = hacker_created_at - min_hacker_created)
edata = edata %>% mutate(new_last_online = last_online - min_last_online)
edata = edata %>% mutate(new_hacker_timezone = as.numeric(as.factor(hacker_timezone)))
edata = edata %>% select(-sent_time,-hacker_created_at,-last_online,-hacker_timezone)

#Aggregating the ipn_count and ipn_read details
edata = edata %>% mutate(ipn_not_read = ipn_count - ipn_read, ipn_not_read_1_days = ipn_count_1_days - ipn_read_1_days, ipn_not_read_7_days = ipn_count_7_days - ipn_read_7_days, ipn_not_read_30_days = ipn_count_30_days - ipn_read_30_days, ipn_not_read_365_days = ipn_count_365_days - ipn_read_365_days)
edata = edata %>% mutate(ipn_read_aggr = (abs_cor_cedata[61,24]*ipn_read_1_days + abs_cor_cedata[61,27]*ipn_read_7_days+abs_cor_cedata[61,25]*ipn_read_30_days+abs_cor_cedata[61,26]*ipn_read_365_days+abs_cor_cedata[61,23]*ipn_read)/(abs_cor_cedata[61,24]+abs_cor_cedata[61,27]+abs_cor_cedata[61,25]+abs_cor_cedata[61,26]+abs_cor_cedata[61,23]))
edata = edata %>% mutate(ipn_not_read_aggr = (abs_cor_cedata[61,48]*ipn_not_read_1_days + abs_cor_cedata[61,49]*ipn_not_read_7_days +abs_cor_cedata[61,50]*ipn_not_read_30_days +abs_cor_cedata[61,51]*ipn_not_read_365_days + abs_cor_cedata[61,47]*ipn_not_read)/(abs_cor_cedata[61,48]+abs_cor_cedata[61,49]+abs_cor_cedata[61,50]+abs_cor_cedata[61,51]+abs_cor_cedata[61,47]))
edata = edata %>% select(-ipn_count, -ipn_count_1_days,-ipn_count_30_days,-ipn_count_365_days,-ipn_count_7_days)
edata = edata %>% select(-ipn_read,-ipn_read_1_days,-ipn_read_7_days,-ipn_read_30_days,-ipn_read_365_days)
edata = edata %>% select(-ipn_not_read,-ipn_not_read_1_days,-ipn_not_read_7_days,-ipn_not_read_30_days,-ipn_not_read_365_days)

#Aggregating the contest participation and login details
edata = edata %>% mutate(contest_not_login_cnt = contest_participation_count - contest_login_count, contest_not_login_count_1_days=contest_participation_count_1_days - contest_login_count_1_days, contest_not_login_count_7_days=contest_participation_count_7_days - contest_login_count_7_days, contest_not_login_count_30_days=contest_participation_count_30_days - contest_login_count_30_days,contest_not_login_count_365_days=contest_participation_count_365_days - contest_login_count_365_days)
edata = edata %>% mutate(contest_login_aggr = (abs_cor_cedata[61,5]*contest_login_count_1_days + abs_cor_cedata[61,8]*contest_login_count_7_days + abs_cor_cedata[61,6] * contest_login_count_30_days + abs_cor_cedata[61,7]*contest_login_count_365_days +abs_cor_cedata[61,4]*contest_login_count)/(abs_cor_cedata[61,8]+abs_cor_cedata[61,5]+abs_cor_cedata[61,6]+abs_cor_cedata[61,7]+abs_cor_cedata[61,4]))
edata = edata %>% select(-contest_login_count, -contest_login_count_1_days,-contest_login_count_7_days,-contest_login_count_30_days,-contest_login_count_365_days)
edata = edata %>% mutate(contest_not_login_aggr = (abs_cor_cedata[61,54]*contest_not_login_count_1_days + abs_cor_cedata[61,55]*contest_not_login_count_7_days + abs_cor_cedata[61,56]*contest_not_login_count_30_days + abs_cor_cedata[61,57]*contest_not_login_count_365_days + abs_cor_cedata[61,53]*contest_not_login_cnt)/(abs_cor_cedata[61,54]+abs_cor_cedata[61,55]+abs_cor_cedata[61,56]+abs_cor_cedata[61,57]+abs_cor_cedata[61,53]))
edata = edata %>% select(-contest_not_login_cnt,-contest_not_login_count_1_days,-contest_not_login_count_7_days,-contest_not_login_count_30_days,-contest_not_login_count_365_days)
edata = edata %>% select(-contest_participation_count,-contest_participation_count_1_days,-contest_participation_count_7_days,-contest_participation_count_30_days,-contest_participation_count_365_days)

edata = edata %>% mutate(sub_contest_aggr = (abs_cor_cedata[61,34]*submissions_count_contest_1_days + abs_cor_cedata[61,37]*submissions_count_contest_7_days + abs_cor_cedata[61,35]*submissions_count_contest_30_days + abs_cor_cedata[61,36]*submissions_count_contest_365_days +abs_cor_cedata[61,33]*submissions_count_contest)/(abs_cor_cedata[61,34]+abs_cor_cedata[61,37]+abs_cor_cedata[61,35]+abs_cor_cedata[61,36]+abs_cor_cedata[61,33]))
edata = edata %>% mutate(sub_master_aggr = (abs_cor_cedata[61,39]*submissions_count_master_1_days + abs_cor_cedata[61,42]*submissions_count_master_7_days + abs_cor_cedata[61,40]*submissions_count_master_30_days + abs_cor_cedata[61,41]*submissions_count_master_365_days + abs_cor_cedata[61,38]*submissions_count_master)/(abs_cor_cedata[61,38]+abs_cor_cedata[61,41]+abs_cor_cedata[61,40]+abs_cor_cedata[61,39]+abs_cor_cedata[61,42]))

edata = edata %>% select(-submissions_count_contest,-submissions_count_contest_1_days,-submissions_count_contest_7_days,-submissions_count_contest_30_days,-submissions_count_contest_365_days)
edata = edata %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)
#Removing redundant data
edata = edata %>% select(-submissions_count,-submissions_count_1_days,-submissions_count_7_days,-submissions_count_30_days,-submissions_count_365_days)

#For generating correlation matrix, we convert the categorical variables to numeric
cedata = edata
cedata = cedata %>% mutate(new_mail_category = as.numeric(mail_category),new_mail_type = as.numeric(mail_type),new_unsubscribed = as.numeric(unsubscribed),new_opened = as.numeric(opened),new_clicked = as.numeric(clicked),new_hacker_confirmation = as.numeric(hacker_confirmation))
cedata = cedata %>% select(-mail_category,-mail_type,-opened,-clicked,-unsubscribed,-hacker_confirmation)

#Not considering correlation between the same features since they will always be 1.0
cor_cedata_mod = cor(cedata)
cor_cedata_mod[cor_cedata_mod == 1.0] = 0.0
write.csv(cor_cedata_mod,file = 'New_Corr_No_FactorMod.csv')
write.csv(cedata,file = 'FeatureEngTrain1.csv')
abs_cor_cedata = abs(cor_cedata)

#Removing the column having the mising values
cedata_nov9 = cedata %>% select(-open_time,-click_time,-unsubscribe_time,-new_hacker_timezone,-new_last_online)

#Choosing the first 50000 observations
caseno = seq(1,nrow(cedata_nov9),1)
cedata_nov9 = cbind(caseno,cedata_nov9)
cedata_nov9_50000 = cedata_nov9 %>% filter(caseno <= 50000)

#Dealing with the categorical variables by assigning each level to be a separate category and giving them values 0 or 1
all_zero = rep(0,nrow(cedata_nov9_50000))
hacker_confirmation_true = hacker_confirmation_false = all_zero
clicked_true = clicked_false = all_zero
unsubscribed_true = unsubscribed_false = all_zero
mail_type_1 = mail_type_2 = mail_type_3 = mail_type_4 = mail_type_5 = all_zero
mail_category_1 = mail_category_2 = mail_category_3 = mail_category_4 = mail_category_5 = mail_category_6 = mail_category_7 = mail_category_8 = mail_category_9 = mail_category_10 = mail_category_11 = mail_category_12 = mail_category_13 = mail_category_14 = mail_category_15 = mail_category_16 = mail_category_17 = mail_category_18 = mail_category_19 = all_zero 

for (i in 1:nrow(cedata_nov9_50000)) {
  
  #opened
  if(cedata_nov9_50000[i,19] == 2){
    hacker_confirmation_true[i] = 1 
  }
  else{
    hacker_confirmation_false[i] = 1 
  }
  
  #hacker confirmation
  if(cedata_nov9_50000[i,19] == 2){
    hacker_confirmation_true[i] = 1 
  }
  else{
    hacker_confirmation_false[i] = 1 
  }
  
  #clicked
  if(cedata_nov9_50000[i,18] == 2){
    clicked_true[i] = 1 
  }
  else{
    clicked_false[i] = 1 
  }
  
  #unsubscribed
  if(cedata_nov9_50000[i,16] == 2){
    unsubscribed_true[i] = 1 
  }
  else{
    unsubscribed_false[i] = 1 
  }
  
  #mail_type
  if(cedata_nov9_50000[i,15] == 1){
    mail_type_1[i] = 1 
  }
  else if(cedata_nov9_50000[i,15] == 2){
    mail_type_2[i] = 1 
  }
  else if(cedata_nov9_50000[i,15] == 3){
    mail_type_3[i] = 1 
  }
  else if(cedata_nov9_50000[i,15] == 4){
    mail_type_4[i] = 1 
  }
  else{
    mail_type_5[i]=1
  }
  
  #mail_category
  if(cedata_nov9_50000[i,14] == 1){
    mail_category_1[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 2){
    mail_category_2[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 3){
    mail_category_3[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 4){
    mail_category_4[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 5){
    mail_category_5[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 6){
    mail_category_6[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 7){
    mail_category_7[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 8){
    mail_category_8[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 9){
    mail_category_9[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 10){
    mail_category_10[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 11){
    mail_category_11[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 12){
    mail_category_12[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 13){
    mail_category_13[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 14){
    mail_category_14[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 15){
    mail_category_15[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 16){
    mail_category_16[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 17){
    mail_category_17[i] = 1 
  }
  else if(cedata_nov9_50000[i,14] == 18){
    mail_category_18[i] = 1 
  }
  else{
    mail_category_19[i] = 1 
  }
}

cedata_nov9_50000 = cbind(cedata_nov9_50000,hacker_confirmation_true,hacker_confirmation_false,clicked_true,clicked_false,unsubscribed_true,unsubscribed_false,mail_type_1,mail_type_2,mail_type_3,mail_type_4,mail_type_5,mail_category_1,mail_category_2,mail_category_3,mail_category_4,mail_category_5,mail_category_6,mail_category_7,mail_category_8,mail_category_9,mail_category_10,mail_category_11,mail_category_12,mail_category_13,mail_category_14,mail_category_15,mail_category_16,mail_category_17,mail_category_18,mail_category_19)

cedata_nov9_cor_mod = cedata_nov9_50000 %>% select(-new_hacker_confirmation,-new_clicked,-new_unsubscribed,-new_mail_type,-new_mail_category)
cor_cedata_mod_nov9 = cor(cedata_nov9_cor_mod)
cor_cedata_mod_nov9[cor_cedata_mod_nov9 == 1.0] = 0.0
write.csv(cor_cedata_mod_nov9,file = 'Cor_cedata_nov9.csv')

opened=NULL
for (i in 1:nrow(cedata_nov9_cor_mod)) {
  #opened
  if(cedata_nov9_cor_mod[i,14] == 2){
    opened = c(opened,1) 
  }
  else{
    opened = c(opened,0)
  }
}

cedata_nov9_cor_mod = cbind(opened,cedata_nov9_cor_mod)
cedata_nov9_cor_mod = cedata_nov9_cor_mod %>% select(-new_opened,-caseno)

#Split into 70% training and 30% for cross-validating
library(caTools)
set.seed(881)
spliT = sample.split(cedata_nov9_cor_mod$new_opened, SplitRatio = 0.70)
cedata_train = subset(cedata_nov9_cor_mod, spliT == TRUE)
cedata_test = subset(cedata_nov9_cor_mod, spliT == FALSE)
ctest = cedata_test %>% select(-opened)

Logistic1 = glm(opened ~ ., data = cedata_train,family = binomial)
preTrials = predict(Logistic1,newdata = ctest,type = "response")
preTrials = ifelse(preTrials > 0.5,1,0)

misClasificError <- mean(preTrials != cedata_test$opened)
Accuracy = 1 - misClasificError
