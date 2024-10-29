library(lubridate)
library("randomForest")
library(tidyverse)


train_5k <- results2024[results2024$Split=="5K" | results2024$Split=="Finish Net", ]
View(train_5k)


test <- train_5k %>% 
  filter(`Race State` == "Finished") %>% 
  select(Name, Split, Time) %>% 
  pivot_wider(names_from = Split, values_from = Time)

colnames(test)[2] <- "fiveK"
colnames(test)[3] <- "finish"

test <- test %>% 
  mutate(fiveK = seconds(hms(fiveK)),
         finish = seconds(hms(finish)))

test_complete <- test[complete.cases(test),]
rf <- randomForest(finish~ fiveK, data = test_complete)
plot(test_complete$finish, predict(rf))

filter(test, is.na(`Finish Net`))

predictions <- tibble()
i <- 860
for(i in seq(from = 860, to = 3385, by = 5)){
  temp <- tibble(fiveK=i, finish = predict(rf, data.frame(fiveK=i)))
  predictions <- predictions %>% 
    rbind(temp)
}
plot(predictions, type = 'l')
  


plot(t, type = 'l')
