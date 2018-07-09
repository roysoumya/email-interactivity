library(dplyr)

# To classify users into categories
#We are performing with respect to open_percent

user_prof2 = read.csv(file = '~/R/HackerRank Email/New datatsets/UserProfileMay.csv',row.names = 1)

duration = user_prof2$open_percent
breaks = seq(0.0, 1.0, by=0.025) 
duration.cut = cut(duration, breaks, right=FALSE) 
duration.freq = table(duration.cut)
duration.cumfreq = cumsum(duration.freq)
plot(duration.cumfreq)

#BAsed on the ratio amongs the total number of emails opened and closed and choosing 
#the baseline model, the threshold chosen is 0.45

#Creating a categorical variable called opened or not
user_prof2 = user_prof2 %>% mutate(opened = open_percent>0.45)
user_prof2$opened = as.factor(user_prof2$opened)

#Finding the important variables using the random forests method
#Removing X,user_id_n,open_percent
varimp_data = user_prof2 %>% select(-X,-user_id_n,-open_percent)

#Converting the categorical variables to factors
varimp_data$hacker_confirm = as.factor(varimp_data$hacker_confirm)

library(randomForest)
fit.variable = randomForest(opened ~ ., data = varimp_data)

library(caret)
varImp(fit.variable)
varImpPlot(fit.variable, type = 2)

#Taking the top 22 features
varimp_data = user_prof2 %>% select(user_id_n,opened,total_opened,total_sent,total_clicked,total_type1,total_type3,total_cat15,age_week,total_unsubscribed,ipn_count_365_days,total_cat4,total_cat3,submissions_count_master_365_days,ipn_count_30_days,total_cat7,submissions_count_30_days,submissions_count_master_30_days,contest_participation_count_365_days,ipn_count_7_days,contest_login_count_365_days,total_type2,total_cat13,total_cat10,ipn_read)
clus_data3 = varimp_data %>% select(-user_id_n,-opened)

set.seed(1024)
clus.fit2 = kmeans(clus_data3, 20, nstart = 20)

#Adding the cluster no. to the table
user_prof2 = data.frame(user_prof2, clus.fit2$cluster)
write.csv(user_prof2, file= 'UserProfile14thMay.csv')
