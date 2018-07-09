#Clearing the workspace
rm(list = ls())
library(data.table)
library(dplyr)

entire_data = fread('~/R/HackerRank Email/Email/training_dataset.csv', header = TRUE, stringsAsFactors = TRUE)


#Summarising the data 
str(entire_data)
summary(entire_data)

#Converting all the blank spaces to NA so that they can be treated at one go
entire_data[entire_data == ''] = NA

#manipulation to get correct categorical variables
entire_data$mail_category = as.factor(as.numeric(entire_data$mail_category))
entire_data$mail_type = as.factor(as.numeric(entire_data$mail_type))

#Removing the user_id and mail_id
entire_data[,c('user_id','mail_id'):= NULL] 
entire_data[,c('click_time', 'unsubscribe_time', 'open_time'):=NULL]

#Transforming hacker_timezone into a factor variable
entire_data$hacker_timezone = as.factor(entire_data$hacker_timezone)

#Removing data points which is NOT intuitively matching :
# 1. opened = false , clicked = true
# 2. opened = false , unsubscribed = true
# 3. hacker_created_at > last_online

entire_data = entire_data[hacker_created_at <= last_online]
entire_data = entire_data[!(opened == 'false' & clicked == 'true')]
entire_data = entire_data[!(opened == 'false' & unsubscribed == 'true')]
entire_data = entire_data[is.na(mail_category)==FALSE]


#Removing records with missing values
entire_data = entire_data[complete.cases(entire_data),]

#Using the Decision Tree method to determine feature importance (Approach 1)
entire_data = entire_data %>% mutate( sent_time_gap = (sent_time - last_online)/60, last_online_t = (last_online - hacker_created_at)/60, sent_time_t = (sent_time - hacker_created_at)/60)
entrie_data = entire_data %>% mutate(open_time_h = open_time - hacker_created_at, open_time_t = open_time-sent_time, click_time_t = click_time - sent_time,unsubscribe_time_t = unsubscribe_time - sent_time)

min_hacker_created_at = min(entire_data$hacker_created_at)
entire_data = entire_data %>% mutate( hacker_creation_temporal = (hacker_created_at - min_hacker_created_at)/86400 ) 
entire_data = entire_data %>% select(-clicked, -unsubscribed,-sent_time, -hacker_created_at, -last_online)


library(caTools)
set.seed(881)
spliT = sample.split(entire_data$opened, SplitRatio = 0.70)
entire_data = cbind(entire_data,spliT)
train_dataset1 = entire_data %>% filter(spliT == TRUE)
test_dataset = entire_data %>% filter(spliT == FALSE)



aniva_var = anova(opened ~ ., data=num_data, method = 'class')

cor_data = as.numeric(train_dataset1)

cat_data = entire_data %>% select(opened, mail_category,mail_type,hacker_timezone,hacker_confirmation)
num_data = entire_data %>% select(-mail_category,-spliT, -mail_type,-hacker_timezone,-hacker_confirmation)

#Continuous vs. Nominal: calculate the intraclass correlation. In R, you can use ?ICC in the psych package; there is also an ICC package.
#Nominal vs. Nominal: calculate Cramer's V. In R, you can use ?assocstats in the vcd package


library(psych)
cat_cor = ICC(cat_data)

set.seed(1234)
spliT1 = sample.split(train_dataset1$opened, SplitRatio = 0.20)
train_dataset1 = cbind(train_dataset1, spliT1)
train_dataset = train_dataset1 %>% filter(spliT1 == TRUE)


#Removing sent_time, last_online, hacker_created_at
#train_dataset = train_dataset %>% select(-sent_time,-last_online,-hacker_created_at)
#test_dataset = test_dataset %>% select(-sent_time,-last_online,-hacker_created_at)

library(e1071)
svm.model = svm(opened ~ .,data = train_dataset,kernel = 'linear')

library(rpart)
rpart.model <- rpart(opened ~ .,data = train_dataset, method = "class")
rpart.prediction = predict(rpart.model, newdata = test_dataset)

library(caret)
listImp = varImp(rpart.model)

#Using random forests 


#Using decision trees to understand the feature importance
library(caret)
featureImp = varImp(fit1)


#Using RFE to determine feature importance





#We will directly go into inferential statistics(13th April, 2017)



#We will go into descriptive statistics(14th April, 2017)


