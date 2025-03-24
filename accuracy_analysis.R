library(tidyverse)
library(ggthemes)

# Get data -----------------------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing()

selected <- sample(1:nrow(timeModelData), replace = FALSE, floor(nrow(timeModelData) * .7))
traindata <- #timeModelData[selected, ]
  timeModelData %>% filter(year != 2024)
testdata <- #timeModelData[-selected,  ] 
  timeModelData %>% filter(year == 2024)

# Create Information Tibbles ------------------------------------------------------

split_distance <- tibble(split = c("fiveK", "tenK", "fifteenK", "twentyK", "HALF", "twentyfiveK", 
                                   "thirtyK", "twentyM", "twentyoneM", "thirtyfiveK", "fortyK", "twentyfivetwoM", "FINISH"),
                         distance = c(5000, 10000, 15000, 20000, 21082.41, 25000, 30000, 32186.9, 33796.2, 
                                      35000, 40000, 40555.47, 42164.81))

method_rmse <- tibble(split = c("start", "fiveK", "tenK", "fifteenK", "twentyK", "HALF", "twentyfiveK", 
                                "thirtyK", "twentyM", "twentyoneM", "thirtyfiveK", "fortyK", "twentyfivetwoM"),
                      distance = c(0, 5000, 10000, 15000, 20000, 21082.41, 25000, 30000, 32186.9, 33796.2, 
                                   35000, 40000, 40555.47),
                      randomforest2024 = rep(NA, 13),
                      randomforestOOB = rep(NA, 13),
                      overallpace = rep(NA, 13),
                      lastsplitpace = rep(NA, 13))

elite_rmse <- tibble(split = c("start", "fiveK", "tenK", "fifteenK", "twentyK", "HALF", "twentyfiveK", 
                                "thirtyK", "twentyM", "twentyoneM", "thirtyfiveK", "fortyK", "twentyfivetwoM"),
                      distance = c(0, 5000, 10000, 15000, 20000, 21082.41, 25000, 30000, 32186.9, 33796.2, 
                                   35000, 40000, 40555.47),
                      randomforest2024 = rep(NA, 13),
                      overallpace = rep(NA, 13),
                      lastsplitpace = rep(NA, 13))



# Calculate RMSE of different methods ----------------------------------------------
for (split in method_rmse$split[-1]) {
  testpredict <- predict(get(paste0("rf", split, "toFINISHtime")), testdata, type = 'quantiles', quantiles = c(0.05, 0.125, 0.5, 0.875, 0.95))
  modelrmse <- rmse(testpredict$predictions[,3] + testdata[[split]], testdata$FINISH) / 60
  
  overallpacermse <- rmse(testdata[[split]] * split_distance$distance[split_distance$split == "FINISH"] / split_distance$distance[split_distance$split == split], testdata$FINISH) / 60
  
  lastsplitpacermse <- rmse(testdata[[split]] + testdata[[paste0(split, "Split")]] * (split_distance$distance[split_distance$split == "FINISH"] - split_distance$distance[split_distance$split == split]) / 
                                                                                          (split_distance$distance[split_distance$split == split] - method_rmse$distance[which(method_rmse$split == split) - 1]), testdata$FINISH) / 60

  method_rmse$randomforest2024[method_rmse$split == split] <- modelrmse
  method_rmse$overallpace[method_rmse$split == split] <- overallpacermse
  method_rmse$lastsplitpace[method_rmse$split == split] <- lastsplitpacermse
}

# Elite RMSE -----------------------------------------------------------------------
testdata <- timeModelData %>% filter(year == 2024 & class == "elite")
for (split in elite_rmse$split[-1]) {
  testpredict <- predict(get(paste0("rf", split, "toFINISHtime")), testdata, type = 'quantiles', quantiles = c(0.05, 0.125, 0.5, 0.875, 0.95))
  modelrmse <- rmse(testpredict$predictions[,3] + testdata[[split]], testdata$FINISH) / 60
  
  overallpacermse <- rmse(testdata[[split]] * split_distance$distance[split_distance$split == "FINISH"] / split_distance$distance[split_distance$split == split], testdata$FINISH) / 60
  
  lastsplitpacermse <- rmse(testdata[[split]] + testdata[[paste0(split, "Split")]] * (split_distance$distance[split_distance$split == "FINISH"] - split_distance$distance[split_distance$split == split]) / 
                              (split_distance$distance[split_distance$split == split] - method_rmse$distance[which(method_rmse$split == split) - 1]), testdata$FINISH) / 60
  
  elite_rmse$randomforest2024[elite_rmse$split == split] <- modelrmse
  elite_rmse$overallpace[elite_rmse$split == split] <- overallpacermse
  elite_rmse$lastsplitpace[elite_rmse$split == split] <- lastsplitpacermse
}

saveRDS(elite_rmse, "elite_rmse.rds")

# Add OOB Error -------------------------------------------------------------
for (split in method_rmse$split[-1]) {
  method_rmse$randomforestOOB[method_rmse$split == split] <- get(paste0("rf", split, "toFINISHtime"))$prediction.error %>% sqrt() / 60
}



saveRDS(method_rmse, "method_rmse.rds")


method_rmse %>% 
  filter(distance != 0) %>% 
  pivot_longer(cols = c(3:6),
               names_to = 'method',
               values_to = 'rmse') %>% 
  ggplot(aes(x = distance, y = rmse, color = method)) + 
  geom_line() +
  xlab("Distance") +
  ylab("RMSE (min)") +
  scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
  scale_color_discrete(labels = c("Last Split Pace", "Overall Pace", "RF 2024", "RF OOB")) +
  #scale_color_manual(values = c("#A51C30", "#2c6528", "#355b86", "#75530a")) +
  labs(color = "Method") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.8),
        legend.background = element_rect(fill = "white", color = "grey"),
        legend.key.size = unit(1, "in"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        panel.background = element_rect(fill = "white", color = 'white'),
        plot.background = element_rect(fill = "white", color = 'white'))

ggsave("methodAccuracy.png", height = 8, width = 17, unit = 'in')


elite_rmse %>% 
  filter(distance != 0) %>% 
  pivot_longer(cols = c(3:5),
               names_to = 'method',
               values_to = 'rmse') %>% 
  ggplot(aes(x = distance, y = rmse, color = method)) + 
  geom_line() +
  theme_minimal()
