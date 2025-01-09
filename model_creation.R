library(lubridate)
library(randomForest)
library(tidyverse)
library(ranger)



# 5k Finish Time Predictor ------------------------------------------------------------

rf5ktime <- ranger(finish ~ age + sex + fiveK, data = runnerresults %>% filter(isTRUE(finisher)))























test <- runnerresults %>% 
  # filter(finish == 1) %>% 
  tail(10000) %>% 
  mutate(finish_time = max(time)) %>% 
  select(-time, -distance) %>% 
  pivot_wider(names_from = point, values_from = split) %>% 
  filter(!is.na(fiveK) & !is.na(FINISH)) %>% 
  ungroup() %>% 
  mutate(finish = as.logical(finish))

rf <- ranger(finish ~ age + sex + fiveK, data = test)


runnerresults$finish %>% mean()
ranger::predictions(rf, data = data)
predict(rf, data = data, type = 'terminalNodes') %>% str() %>% 
a$pred

ranger::predictions(rf) %>% plot()
10784/60

importance(rf)
varImpPlot(rf)

data <- tibble(age = 44,
               sex= "M",
               fiveK = seconds(600))


