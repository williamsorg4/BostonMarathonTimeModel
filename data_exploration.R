library(tidyverse)
library(lubridate)
library(ggthemes)

timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing() %>% 
  filter(sex != "X")

split_distance <- tibble(split = c("fiveK", "tenK", "fifteenK", "twentyK", "HALF", "twentyfiveK", 
                                   "thirtyK", "twentyM", "twentyoneM", "thirtyfiveK", "fortyK", "twentyfivetwoM", "FINISH"),
                         distance = c(5000, 10000, 15000, 20000, 21082.41, 25000, 30000, 32186.9, 33796.2, 
                                      35000, 40000, 40555.47, 42164.81))

# Time Model Dependent Variables --------------------------------------------------
# Age
timeModelData %>% 
  ggplot(aes(x = age, y= FINISH)) +
  geom_jitter(alpha = 0.025) +
  geom_smooth(se = FALSE, col = "#A51C30") +
  scale_y_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .)) +
  xlab("Age") +
  ylab("Finish Time (hrs)") +
  theme_minimal()

# Sex
timeModelData %>% 
  ggplot(aes(x = sex, y= FINISH)) +
  geom_tufteboxplot() +
  scale_y_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .)) +
  xlab("Sex") +
  ylab("Finish Time (hrs)") +
  theme_minimal()

# Class
timeModelData %>% 
  ggplot(aes(x = class, y= FINISH)) +
  geom_tufteboxplot(size = 1) +
  scale_y_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .)) +
  scale_x_discrete(labels = c("elite" = "Elite", "open" = "Open")) +
  xlab("Class") +
  ylab("Finish Time (hrs)") +
  theme_minimal()

# Split Times Facet Plot
splits <- c("5k","10k", "15k", "20k", "Half", "25k", "30k", "20mi", "21mi", "35k", "40k", "25.2mi")
names(splits) <- split_distance$split[-13]
timeModelData %>% 
  head(1000) %>% 
  pivot_longer(cols = 10:21, values_to = "time", names_to = "Split") %>% 
  mutate(Split = factor(Split, levels = split_distance$split[-13])) %>% 
  ggplot(aes(x = time, y = FINISH)) +
  geom_point(alpha = 0.05) +
  scale_x_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .),
               breaks = function(x) seq(quantile(x, probs = 0.2), quantile(x, probs = 0.8), length=3)) +
  scale_y_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .),
               breaks = c(seconds(10800), seconds(18000), seconds(25200))) +
  facet_wrap(vars(Split), scales = "free_x", labeller = labeller(Split = splits)) +
  ylab("Finish Time (hrs)") +
  xlab("Time at Checkpoint (hrs)") +
  theme_minimal()

# Average 5k Splits
timeModelData %>% 
  summarise(across(c(10:13, 15:16, 19:20), mean)) %>% 
  pivot_longer(cols = 1:8, names_to = "split", values_to = "avg_time") %>% 
  left_join(split_distance) %>% 
  mutate(avg_time = avg_time - lag(avg_time, default = 0)) %>% 
  ggplot(aes(x = distance, y = avg_time)) +
  geom_point(col = "#A51C30") +
  geom_line(col = "#A51C30") +
  xlab("Distance") +
  ylab("Average 5k Split Time (min)") +
  scale_y_time(labels = function(x) strftime(x, "%M:%S")) +
  scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
  theme_minimal()


