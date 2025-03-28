library(tidyverse)
library(ggthemes)
library(ranger)
library(Metrics)
library("PrettyCols")

# Get data -----------------------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing() %>% 
  filter(sex != "X")

selected <- sample(1:nrow(timeModelData), replace = FALSE, floor(nrow(timeModelData) * .7))
traindata <- #timeModelData[selected, ]
  timeModelData %>% filter(year != 2024)
testdata <- #timeModelData[-selected,  ] 
  timeModelData %>% filter(year == 2024)

colorscheme <- as.vector(prettycols("Bold"))
colorscheme[3] <- "#f38f1d"

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
  testpredict <- predict(get(paste0("rf", split, "toFINISHtime")), testdata, type = 'response')
  modelrmse <- rmse(testpredict$predictions + testdata[[split]], testdata$FINISH) / 60
  
  overallpacermse <- rmse(testdata[[split]] * split_distance$distance[split_distance$split == "FINISH"] / split_distance$distance[split_distance$split == split], testdata$FINISH) / 60
  
  lastsplitpacermse <- rmse(testdata[[split]] + testdata[[paste0(split, "Split")]] * (split_distance$distance[split_distance$split == "FINISH"] - split_distance$distance[split_distance$split == split]) / 
                                                                                          (split_distance$distance[split_distance$split == split] - method_rmse$distance[which(method_rmse$split == split) - 1]), testdata$FINISH) / 60

  method_rmse$randomforest2024[method_rmse$split == split] <- modelrmse
  method_rmse$overallpace[method_rmse$split == split] <- overallpacermse
  method_rmse$lastsplitpace[method_rmse$split == split] <- lastsplitpacermse
}

saveRDS(method_rmse, "method_rmse.rds")

# Elite RMSE 
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




# Overall Method RMSE Plot ----------------------------------------
method_rmse %>% 
  filter(distance != 0) %>% 
  pivot_longer(cols = c(3,5:6),
               names_to = 'method',
               values_to = 'rmse') %>% 
  ggplot(aes(x = distance, y = rmse, color = method, linetype = method)) + 
  geom_line(linewidth = 2) +
  xlab("Distance") +
  ylab("RMSE (min)") +
  scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
  labs(color = "Prediction Method") +
  theme_minimal() +
  scale_color_manual(values = colorscheme[-3],
                       labels = c("Last Split Pace", "Overall Pace", "Random Forest 2024")) +
  scale_linetype_manual(values = c(4, 1, 2)) +
  theme(legend.position = c(0.85, 0.8),
        legend.background = element_rect(fill = "white", color = "grey"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.spacing = unit(1, "in"),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = 'white'),
        plot.background = element_rect(fill = "white", color = 'white'),
        panel.grid.minor = element_blank()) +
  guides(linetype = 'none')

ggsave("methodAccuracy.png", width = 17, height = 8, unit = 'in')


# Elite RMSE Error Plot -------------------------------------------
elite_rmse %>% 
  filter(distance != 0) %>% 
  pivot_longer(cols = c(3:5),
               names_to = 'method',
               values_to = 'rmse') %>% 
  ggplot(aes(x = distance, y = rmse, color = method)) + 
  geom_line() +
  theme_minimal()



# Error Histograms --------------------------------------------
rf5kerror <- (predict(rffiveKtoFINISHtime, data = testdata, type = 'response')$predictions + testdata$fiveK - testdata$FINISH) / 60
extraperror <- (testdata$fiveK*42164.81/5000 - testdata$FINISH) / 60
errorTable <- tibble("Random Forest" = rf5kerror,
                     "Overall Pace" = extraperror)


errorTable %>% 
  pivot_longer(cols = 1:2, names_to = 'method', values_to = 'error') %>% 
  ggplot(aes(x = error, fill = method)) +
  geom_histogram(bins = 50) +
  ylab("Count") +
  xlab("Error (min)") +
  scale_fill_manual(values = colorscheme[1:2]) +
  scale_x_continuous(limits = c(-100, 50)) +
  scale_y_continuous(breaks = c(0, 1000, 2000)) +
  facet_wrap(~method, ncol = 1) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = 'white'),
        plot.background = element_rect(fill = "white", color = 'white'),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 18))

ggsave("errorHistogram.png", width = 9, height = 4, unit = 'in')



testperson <- testdata[sample(1:25290, 2), ]

prediction_range_example <- tibble()
for (split in split_distance$split[-13]) {
  predict <- predict(get(paste0("rf", split, "toFINISHtime")), testperson, type = 'quantiles', quantiles = c(0.05, 0.5, 0.95))
  predict <- predict$predictions + testperson[[split]]
  temp <- tibble("split" = split,
                 "distance" = split_distance$distance[split_distance == split],
                 "lower" = predict[1],
                 "pred" = predict[2],
                 "upper" = predict[3])
  prediction_range_example <- prediction_range_example %>% 
   rbind(temp)
}

prediction_range_example %>% 
  pivot_longer(cols = 3:5, names_to = "type", values_to = "time") %>% 
  ggplot(aes(x = distance, y = time, col = type)) +
  geom_segment(data = prediction_range_example,
               aes(x = distance, xend = distance, y = lower, yend = upper),
               color = "darkgray", linetype = 2, linewidth = 0.75) +
  geom_point(shape = "-", aes(size = type)) +
  geom_hline(yintercept = testperson$FINISH, linetype = 4, linewidth = 1) +
  scale_y_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .)) +
  scale_size_manual(values = c(10, 14, 10)) +
  scale_color_manual(values = c(colorscheme[1], colorscheme[2], colorscheme[1])) +
  scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
  xlab("Distance") +
  ylab("Time (hrs)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = 'white'),
        plot.background = element_rect(fill = "white", color = 'white'),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave("predictionrangeExample.png", width = 10, height = 5, unit = 'in')


# All Models OOB Error -----------------------------------------------------
split_cols <- names(runnerresults)[16:28]
pairs <- combn(split_cols, 2, simplify = FALSE)
oobError <- tibble("checkpoint" = lapply(pairs, function(x) x[[1]]) %>% unlist(),
                   "projection" = lapply(pairs, function(x) x[[2]]) %>% unlist(),
                   "error" = rep(NA, 78))

oobError <- oobError %>% 
  mutate(checkpoint = factor(checkpoint, split_distance$split),
         projection = factor(projection, split_distance$split))

for(pair in pairs[lapply(pairs, function(x) x[[2]]) %>% unlist() == "FINISH"]) {
  oobError$error[oobError$checkpoint == pair[1] & oobError$projection == pair[2]] = get(paste0("rf", pair[1], "to", pair[2], "time"))$prediction.error %>% sqrt() / 60
}

saveRDS(oobError, "oobError.rds")

splits <- c("5k","10k", "15k", "20k", "Half", "25k", "30k", "20mi", "21mi", "35k", "40k", "25.2mi", "Finish")

oobError %>% 
  mutate(distance.x = as.character(distance.x)) %>% 
  ggplot(aes(x = checkpoint, y = projection)) +
  geom_point(aes(size = error, color = error)) +
  geom_text(aes(label = round(error, 0)), size = 3) +
  scale_x_discrete(labels = splits[-13]) +
  scale_y_discrete(labels = splits[-1]) +
  theme_minimal() 



# Quantile Success -----------------------------------------------------
avg_predictions <- tibble()
for (split in split_distance$split[-13]) {
  predictq <- predict(get(paste0("rf", split, "toFINISHtime")), testdata, type = 'quantiles', quantiles = c(0.05, 0.5, 0.95))
  predict <- predict(get(paste0("rf", split, "toFINISHtime")), testdata, type = 'response')
  predictq <- predictq$predictions + testdata[[split]]
  predict <- predict$predictions + testdata[[split]]
  temp <- tibble("split" = split,
                 "distance" = split_distance$distance[split_distance == split],
                 "lower" = predictq[, 1],
                 "pred" = predict,
                 "upper" = predictq[, 3],
                 "included" = testdata$FINISH > predictq[,1] & testdata$FINISH < predictq[, 3])
  avg_predictions <- avg_predictions %>% 
    rbind(temp)
}

avg_predictions <- avg_predictions %>% 
  summarise(lower = mean(lower), pred = mean(pred), upper = mean(upper), accuracy = mean(included), .by = split)

avg_predictions <- avg_predictions %>% 
  left_join(split_distance)

avg_predictions %>% 
  pivot_longer(cols = 2:4, names_to = "type", values_to = "time") %>% 
  ggplot(aes(x = distance, y = time, col = type)) +
  geom_segment(data = avg_predictions,
               aes(x = distance, xend = distance, y = lower, yend = upper),
               color = "darkgray", linetype = 2, linewidth = 1.25) +
  geom_point(shape = "-", aes(size = type)) +
  geom_hline(yintercept = mean(testdata$FINISH), linetype = 4, linewidth = 2) +
  scale_y_time(labels = function(x) strftime(x, "%H:%M") %>% sub("0", "", .)) +
  scale_size_manual(values = c(22, 26, 22)) +
  scale_color_manual(values = c(colorscheme[1], colorscheme[2], colorscheme[1])) +
  scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
  xlab("Distance") +
  ylab("Time (hrs)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = 'white'),
        plot.background = element_rect(fill = "white", color = 'white'),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave("avgPrediction.png", width = 14, height = 8, unit = 'in')



avg_predictions %>% 
  ggplot(aes(x = distance, y = upper - lower)) +
  geom_(color = colorscheme[2], linewidth = 2) +
  geom_point(color = colorscheme[2], size = 3) +
  xlab("Distance") +
  ylab("90% Quantile Range (min)") +
  scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
  scale_y_time(labels = function(x) x / 60) +
  theme_minimal() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18),
        panel.background = element_rect(fill = "white", color = 'white'),
        plot.background = element_rect(fill = "white", color = 'white'),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        plot.margin = margin(20, 5, 5, 5))

ggsave("quantilerange.png", width = 9, height = 4, unit = 'in')
