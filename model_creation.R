library(lubridate)
library(randomForest)
library(tidyverse)
library(ranger)
library("tidymodels")


# 5k Finish Time Predictor ------------------------------------------------------------

rf5ktime <- ranger(FINISH ~ age + sex + class + fiveK, 
                   data = runnerresults %>% 
                     select(FINISH, age, sex, class, fiveK) %>% 
                     remove_missing(),
                   importance = 'permutation',
                   scale.permutation.importance = TRUE)


rf5ktime <- randomForest(FINISH ~ age + sex + class + fiveK,
             data = runnerresults %>% 
               select(FINISH, age, sex, class, fiveK) %>% 
               remove_missing())


# 10k Finish Time Predictor ------------------------------------------------------------

rf10ktime <- randomForest(FINISH ~ age + sex + class + fiveK + tenK,
                         data = runnerresults %>% 
                           select(FINISH, age, sex, class, fiveK, tenK) %>% 
                           remove_missing())



# 15k Finish Time Predictor ------------------------------------------------------------

rf15ktime <- ranger(FINISH ~ age + sex + class + fiveK + tenK + fifteenK,
                          data = runnerresults %>% 
                            select(FINISH, age, sex, class, fiveK, tenK, fifteenK) %>% 
                            remove_missing())



example <- runnerresults %>% 
  filter(pid == "RBDGHF3W")

predict(rf15ktime, 
        data = example %>% 
          select(FINISH, age, sex, class, fiveK, tenK, fifteenK)) %>% str()

example$FINISH


varImpPlot(rf15ktime)
