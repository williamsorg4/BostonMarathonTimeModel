library(lubridate)
library(tidyverse)
library(ranger)

# Data Restriction --------------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing() %>% 
  filter(pid != "REAJ6ZY2")


# Hyperparameter Tuning -----------------------------------------------------
# Base Model
rf5ktime <- ranger(FINISH ~ age + sex + class + fiveK, 
                   data = timeModelData,
                   importance = 'permutation',
                   scale.permutation.importance = TRUE,
                   quantreg = TRUE,
                   keep.inbag = TRUE,
                   mtry = 4)
rf5ktime$mtry
rf5ktime$prediction.error %>% sqrt() / 60
summary(rf5ktime$predictions) / 60

m1 <- lm(FINISH ~ age + sex + class + fiveK, 
   data = timeModelData %>% 
     mutate(FINISH = as.double(FINISH),
            fiveK = as.double(fiveK)))



summary(m1)
hist(m1$residuals[-35475], breaks = 30)
qqnorm(m1$residuals)
qqline(m1$residuals)
timeModelData$pid[35475]

numtrees <- tibble()
for (n in floor(seq(from = 100, to = 1000, length.out = 20))) {
  rf5ktime <- ranger(FINISH ~ age + sex + class + fiveK, 
                     data = runnerresults %>% 
                       select(FINISH, age, sex, class, fiveK) %>% 
                       remove_missing(),
                     importance = 'permutation',
                     scale.permutation.importance = TRUE,
                     quantreg = TRUE,
                     keep.inbag = TRUE,
                     num.trees = n)
  numtrees <- numtrees %>% rbind(tibble(trees = n,
                                        oob = rf5ktime$prediction.error))
}


numtrees %>% 
  ggplot(aes(x = trees, y = sqrt(oob))) +
  geom_line()


# 5k Forests ------------------------------------------------------------
# Time Predictor




min(rf5ktime$predictions - (runnerresults %>% 
  select(FINISH, age, sex, class, fiveK) %>% 
  remove_missing() %>% 
  pull(FINISH)))

-13383.95 / 60

rf10ktime$prediction.error %>% 
  sqrt() / 60

timeModelData %>% 
  mutate(extra = abs(as.numeric(tenKSplit * 8.439 - FINISH))) %>% 
  pull(extra) %>% 
  mean() / 60

# Finish or not
rf5kfinisher <- ranger(finisher ~ age + sex + class + fiveK,
                             data = runnerresults %>% 
                               select(finisher, age, sex, class, fiveK) %>% 
                               remove_missing(),
                       probability = TRUE)




# 10k Finish Time Predictor ------------------------------------------------------------

rf10ktime <- ranger(FINISH ~ age + sex + class + fiveK + tenKSplit,
                    data = timeModelData,
                    importance = 'permutation',
                    scale.permutation.importance = TRUE,
                    quantreg = TRUE,
                    keep.inbag = TRUE,
                    na.action = 'na.omit')


# 15k Finish Time Predictor ------------------------------------------------------------

rf15ktime <- ranger(FINISH ~ age + sex + class + fiveK + tenK + fifteenK,
                     data = runnerresults %>% 
                       select(FINISH, age, sex, class, fiveK, tenK, fifteenK) %>% 
                       remove_missing(),
                     importance = 'permutation',
                     scale.permutation.importance = TRUE,
                     quantreg = TRUE,
                     keep.inbag = TRUE)



# 20k Finish Time Predictor -------------------------------------------------

rf20ktime <- ranger(FINISH ~ age + sex + class + fiveK + tenK + fifteenK + twentyK,
                    data = runnerresults %>% 
                      select(FINISH, age, sex, class, fiveK, tenK, fifteenK, twentyK) %>% 
                      remove_missing(),
                    importance = 'permutation',
                    scale.permutation.importance = TRUE,
                    quantreg = TRUE,
                    keep.inbag = TRUE)


rf25mtime <- ranger(FINISH ~ age + sex + class + fiveK + tenK + fifteenK + twentyK + 
                      HALF + twentyfiveK + thirtyK + twentyM + twentyoneM + thirtyfiveK +
                      fortyK + twentyfivetwoM,
                    data = timeModelData,
                    importance = 'permutation',
                    scale.permutation.importance = TRUE,
                    quantreg = TRUE,
                    keep.inbag = TRUE)
rf25mtime2 <- ranger(FINISH ~ age + sex + class + fiveK + tenKSplit + fifteenKSplit + twentyKSplit + 
                      HALFSplit + twentyfiveKSplit + thirtyKSplit + twentyMSplit + twentyoneMSplit + thirtyfiveKSplit +
                      fortyKSplit + twentyfivetwoMSplit + tenK + fifteenK + twentyK + 
                       HALF + twentyfiveK + thirtyK + twentyM + twentyoneM + thirtyfiveK +
                       fortyK + twentyfivetwoM,
                    data = timeModelData,
                    importance = 'permutation',
                    scale.permutation.importance = TRUE,
                    quantreg = TRUE,
                    keep.inbag = TRUE)

rf25mtime$prediction.error %>% sqrt()




importance(rf25mtime)

example <- runnerresults %>% 
  filter(pid == "R234TNAW")

prediction <- predict(m1, 
        newdata = example %>%
          select(age, sex, class, fiveK))

prediction <- predict(rf15ktime,
                      data = example %>% 
                        select(age, sex, class, fiveK, tenK, fifteenK),
                      type = 'quantiles')



prediction$predictions


rf15ktime$variable.importance
example$FINISH


# Finish or Not --------------------------------------------------------------------

