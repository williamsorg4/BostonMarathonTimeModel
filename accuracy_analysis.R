library(tidyverse)

# Get data -----------------------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing()

selected <- sample(1:nrow(timeModelData), replace = FALSE, floor(nrow(timeModelData) * .7))
traindata <- #timeModelData[selected, ]
  timeModelData %>% filter(year != 2024)
testdata <- #timeModelData[-selected,  ] # 
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



# Calculate RMSE of different methods ----------------------------------------------
for (split in method_rmse$split[-1]) {
  testpredict <- predict(get(paste0("rf", split, "toFINISHtime")), testdata, type = 'quantiles', quantiles = c(0.05, 0.125, 0.5, 0.875, 0.95))
  modelrmse <- mean(abs(testpredict$predictions[,3] + testdata[[split]] - testdata$FINISH)) / 60
  
  overallpacermse <- mean(abs(testdata[[split]] * split_distance$distance[split_distance$split == "FINISH"] / split_distance$distance[split_distance$split == split] - testdata$FINISH)) / 60
  
  lastsplitpacermse <- mean(abs(testdata[[split]] + testdata[[paste0(split, "Split")]] * (split_distance$distance[split_distance$split == "FINISH"] - split_distance$distance[split_distance$split == split]) / 
                                                                                          (split_distance$distance[split_distance$split == split] - method_rmse$distance[which(method_rmse$split == split) - 1]) - testdata$FINISH)) / 60

  method_rmse$randomforest2024[method_rmse$split == split] <- modelrmse
  method_rmse$overallpace[method_rmse$split == split] <- overallpacermse
  method_rmse$lastsplitpace[method_rmse$split == split] <- lastsplitpacermse
}



saveRDS(method_rmse, "method_rmse.rds")


method_rmse %>% 
  pivot_longer(cols = c(3:6),
               names_to = 'method',
               values_to = 'rmse') %>% 
  ggplot(aes(x = distance, y = rmse, color = method)) + 
  geom_line() +
  
