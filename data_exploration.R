library(tidyverse)

# Do they finish? ----------------------------------------------------------

m3 <- ranger(finisher ~ age,
       data = runnerresults,
       probability = TRUE)



runnerresults %>% 
  ggplot(aes(x = age, y = as.numeric(finisher))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


runnerresults %>% 
  ggplot(aes(x = sex, fill = finisher)) +
  geom_bar(position = "fill")

runnerresults %>% 
  ggplot(aes(x = class, fill = finisher)) +
  geom_bar(position = "fill")

runnerresults %>% 
  filter(fiveK < 4500) %>% 
  ggplot(aes(x = fiveK, y = as.numeric(finisher))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))
