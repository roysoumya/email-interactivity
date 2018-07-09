library(dplyr)

training = read.csv('~/R/HackerRank Email/Old Analysis/training_dataset_mod_final.csv',row.names = 1)

#Generating boxplots 
boxplot(forum_comments_count ~ opened, data = training, xlab = 'opened', main = 'Forum comments count')
boxplot(forum_expert_count ~ opened, data = training, xlab = 'opened', main = 'Forum expert count')
boxplot(sent_last ~ opened, data = training, xlab = 'opened', main = 'sent time and last online diff.')
boxplot(ipn_read_aggr ~ opened, data = training, xlab = 'opened', main = 'ipn read aggregate')
boxplot(user_age_weeks ~ opened, data = training, xlab = 'opened', main = 'user profile age in weeks')
boxplot(contest_not_login_aggr ~ opened, data = training, xlab = 'opened', main = 'Contest not login aggregate')

training = training %>% mutate(user_age_weeks = user_lifespan/604800)
