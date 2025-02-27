plot(timeModelData$FINISH, rf5ktime$predictions, pch = 20)





predDiff <- tibble(pid = timeModelData %>% 
                     pull(pid),
                   fiveK = timeModelData %>% 
                     pull(fiveK),
                   pred = rf5ktime$predictions,
                   pace = fiveK * 8.439,
                   linear = m1$fitted.values,
                   actual = (timeModelData %>% 
                               pull(FINISH)))
predDiff <- predDiff %>% 
  mutate(predDiff = pred - actual,
         paceDiff = pace - actual,
         linearDiff = linear - actual)

predDiff %>% 
  summarise(predDiff = sqrt(mean((as.double(predDiff))^2)),
            paceDiff = sqrt(mean((as.double(paceDiff))^2)),
            linearDiff = sqrt(mean((as.double(linearDiff))^2)))

predDiff %>% 
  mutate(improve = abs(diff) - abs(pace)) %>% 
  pull(improve) %>% 
  plot()


predDiff %>% 
  ggplot(aes(x = pace / 60, y = diff / 60)) +
  geom_point(alpha = 0.1)

predDiff %>% 
  ggplot(aes(y = (pred - actual) / 60, x = actual / 60)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 0, intercept = 0, color = 'red', linetype = 4) +
  geom_abline(slope = 0, intercept = 5, color = 'red', linetype = 3) +
  geom_abline(slope = 0, intercept = -5, color = 'red', linetype = 3)


temp <- runnerresults %>% 
  filter(pid == "REAJ6ZY2")


min(rf25mtime$predictions - as.double(timeModelData$twentyfivetwoMSplit))
7502/60
