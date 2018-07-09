library(dplyr)

data_read1 = read.csv('training_dataset_mod_final.csv', row.names = 1)
#Removing the mail_id and user_id fields
data_read1 = data_read1 %>% select(-user_id, -mail_id)

#For prediction of values , we need to remove certain columns
data_read1 = data_read1 %>% select(-user_lifespan,-click_sent,-open_sent,-clicked,-unsubscribed,-contest_not_login_aggr)
data_read1_store = data_read1

#Data need to be normalised
# Using Min-max normalisation

for (i in 5:15) {
  min_val = min(data_read1[[i]])
  max_val = max(data_read1[[i]])
  
  for (j in 1:nrow(data_read1)) {
    data_read1[j,i] = (data_read1[j,i] - min_val)/(max_val - min_val)
  }
}

#Split into 70% training and 30% for cross-validating
library(caTools)
set.seed(881)
spliT = sample.split(data_read1$opened, SplitRatio = 0.80)
data_read_train = subset(data_read1, spliT == TRUE)
data_read_valid = subset(data_read1, spliT == FALSE)

#Data need to be normalised before applying logistic regression
#Min-max normalisation to be used

#Applying Logistic regression
data_log = glm(opened ~ . , data=data_read_train, family = binomial)
summary(data_log)

predict_data = predict(data_log, newdata = data_read,type = 'response')
fitted.results <- ifelse(predict_data > 0.5,1,0)
result = sample(c(0,1),size = 61610,replace = TRUE)
res = c(as.list(fitted.results),result)
res_fr = as.data.frame(res)
write.csv(as.data.frame(res),file='Prediction_Logit.csv')

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