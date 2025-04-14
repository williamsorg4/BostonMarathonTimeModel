library(plumber)
library(ranger)
library(tidyverse)
load("BostonMarathonPredictions/goal_time_models.RData")

#* @apiTitle Boston Marathon Predicted Splits API
#* @apiDescription API for predicting Boston Marathon splits given a finish time


#* Return Predicted Split Times Given Finish
#* @param split Predicted Split location
#* @param age Runner's age
#* @param sex Runner's sex
#* @param class Runner's class
#* @param FINISH Finish Time
#* @get /splitTimes
function(split, age, sex, class, FINISH) {
  predict(get(paste0("rfFINISHto", split)), 
          data = tibble(age = age,
                        sex = sex,
                        class = class,
                        FINISH = FINISH))$prediction
}

