library(dplyr)

#Normalising the attributes : total_opened, total_closed vs max_open_time_gap
open_min = min(day_prof2$total_opened)
open_max = max(day_prof2$total_opened)
click_min = min(day_prof2$total_clicked)
click_max = max(day_prof2$total_clicked)
gap_min = min(day_prof2$max_open_time_gap)
gap_max = max(day_prof2$max_open_time_gap)
sent_min = min(day_prof2$total_sent)
sent_max = max(day_prof2$total_sent)
open_high_max = max(day_prof2$total_high_open)
open_mid_min = min(day_prof2$total_mid_open)
open_mid_max = max(day_prof2$total_mid_open)

day_prof2 = day_prof2 %>% mutate(total_opened_norm = (total_opened - open_min)/(open_max - open_min), total_clicked_norm = (total_clicked - click_min)/(click_max - click_min),gap_norm = (max_open_time_gap - gap_min)/(gap_max - gap_min))
day_prof2 = day_prof2 %>% mutate(total_sent_norm = (total_sent - sent_min)/(sent_max - sent_min))
day_prof2  = day_prof2 %>% mutate(high_open_norm  = (total_high_open/open_high_max))
day_prof2 = day_prof2 %>% mutate(mid_open_norm = (total_mid_open - open_mid_min)/(open_mid_max - open_mid_min))

plot(day_prof2$high_open_norm, day_prof2$total_opened_norm)
plot(day_prof2$high_open_norm, day_prof2$total_clicked_norm)
plot(day_prof2$high_open_norm, day_prof2$total_sent_norm)
plot(day_prof2$mid_open_norm, day_prof2$total_opened_norm)
plot(day_prof2$high_open_norm, day_prof2$total_opened_norm)

day_prof2 =  day_prof2 %>% select(-clus.fit1.cluster)
day_prof2_mod = subset(day_prof2, mid_open_norm < 1.0)

clus_data1 = day_prof2_mod %>% select(mid_open_norm,high_open_norm, gap_norm)
clus_data2 = day_prof2 %>% select(mid_open_norm, high_open_norm, gap_norm)

#Applying kmeans clustering with the number of centres to be 5
#As the initial clusters selected are random, we fix a seed
set.seed(1024)
clus.fit1 = kmeans(clus_data1, 5, nstart = 20)

#Adding the cluster no. to the table
day_prof2_mod = data.frame(day_prof2_mod, clus.fit1$cluster)
write.csv(day_prof2_mod, file= 'Sent_Day_Categories.csv')

library(plot3D)
scatter2D(day_prof2_mod$mid_open_norm, day_prof2_mod$gap_norm, colvar = day_prof2_mod$clus.fit1.cluster)



