library(dplyr)
cedata = read.csv(file = "~/R/HackerRank Email/Email Preprocessed Data 24th March.csv", header = TRUE, row.names = 1)

#Splitting the data into training and test
library(caTools)
set.seed(42)
sampl = sample.split(cedata$opened_n, SplitRatio = 0.7)
data_train = subset(cedata, sampl == TRUE)
data_test = subset(cedata, sampl == FALSE)

set.seed(97)
sampl1 = sample.split(data_train$opened_n, SplitRatio = 0.9)
data_train_t = subset(data_train, sampl1 == TRUE)
data_train_cv = subset(data_train, sampl1 == FALSE)

rm(data_train)
rm(cedata)

set.seed(119)
#Reducing the number of training set to improve predicting time
sampl2 = sample.split(data_train_t$opened_n, SplitRatio = 0.33)
data_consider = subset(data_train_t, sampl2 == TRUE)


#For simplicity
cedata = data_consider


cor_cedata = cor(cedata)
cor_cedata[is.na(cor_cedata) == TRUE] = 0.0

for(i in 1:nrow(cor_cedata)){
  cor_cedata[i,i] = 0.0
}

write.csv(cor_cedata, file = "Correlation among features 25th March.csv")


#Removing highly correlated features
cedata_mod = cedata %>% select(-mail_category_1, -mail_category_6, -mail_category_8, -mail_category_10, -contest_login_count, -contest_participation_count, -forum_count, -ipn_count, -ipn_read, -sent_time_t, -submissions_count, -submissions_count_365_days, -submissions_count_1_days)
cedata_mod = cedata_mod %>% select(-contest_login_count_365_days, -submissions_count_30_days, -submissions_count_master, -submissions_count_7_days)

cedata_mod$open_time_t[is.na(cedata_mod$open_time_t) == TRUE] = median(cedata_mod$open_time_t, na.rm = TRUE)
cedata_mod$click_time_t[is.na(cedata_mod$click_time_t) == TRUE] = median(cedata_mod$click_time_t, na.rm = TRUE)
cedata_mod$unsubscribe_time_t[is.na(cedata_mod$unsubscribe_time_t) == TRUE] = median(cedata_mod$unsubscribe_time_t, na.rm = TRUE)

#Ignoring the pca. Going with Decision tree to get an understanding about the data, rather than worrying about accuracy









#Again generating the correlation matrix after removing the highly correlated features
cor_cedata_n = cor(cedata_mod)
cor_cedata_n[is.na(cor_cedata_n) == TRUE] = 0.0

for(i in 1:nrow(cor_cedata_n)){
  cor_cedata_n[i,i] = 0.0
}

write.csv(cor_cedata_n, file = "Correlation among features 25th March After.csv")






#Preparing data for Principal Component Analysis
#Removing the dependent and identifier attributes
pca_label_data = cedata_mod %>% select(opened_n)
pca_data = cedata_mod %>% select(-clicked_n,-click_time_t,-opened_n,-open_time_t, -unsubscribed_n,-unsubscribe_time_t)

prin_comp = prcomp(pca_data, scale. = TRUE)
std_dev = prin_comp$sdev
pr_var = std_dev^2

prop_varex = pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of variance explained", type = 'b')
plot(cumsum(prop_varex),xlab = "Principal Component", ylab = "Cumulation sum of variance", type = 'b')

#After pca
train_data = data.frame(opened = pca_label_data$opened_n, prin_comp$x)
#we are interested in first 50 PCAs
train_data = train_data[,1:51]
write.csv(train_data, file = "After PCA top 50.csv")

