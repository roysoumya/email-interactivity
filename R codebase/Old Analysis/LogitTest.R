library(dplyr)

data_read = read.csv('test_dataset_mod_final.csv', row.names = 1)
#Removing the mail_id and user_id fields
data_read = data_read %>% select(-user_id, -mail_id)
data_read = data_read %>% select(-submissions_count_master,-submissions_count_master_1_days,-submissions_count_master_7_days,-submissions_count_master_30_days,-submissions_count_master_365_days)


#Data need to be normalised before applying logistic regression
#Min-max normalisation to be used




#Applying Logistic regression
data_log = glm(opened ~ . , data=data_read_train, family = binomial)
summary(data_log)

predict_data = predict(data_log, newdata = data_read_valid,type = 'response')

start = 0.40
for (i in 1:25) {
  if(start > 1.0)
    break
  
  fitted.results <- ifelse(predict_data > start,1,0)
  check_pred = ifelse(data_read_valid$opened == 'true',1,0)
  misClasificError <- mean(fitted.results != check_pred )
  print(start)
  print(paste('Accuracy',1-misClasificError))
  start = start + 0.025
}




library(ROCR)
ROCRpred = prediction(data_log, data_read_valid$opened)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))