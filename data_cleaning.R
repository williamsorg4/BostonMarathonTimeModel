library(tidyverse)
library(lubridate)

# Add Years to data before combining
results2021 <- results2021 %>% 
  mutate(year = 2021)

results2022 <- results2022 %>% 
  mutate(year = 2022)

results2023 <- results2023 %>% 
  mutate(year = 2023)

results2024 <- results2024 %>% 
  mutate(year = 2024)

# Combine Results
allresults <- results2021 %>% 
  rbind(results2022) %>% 
  rbind(results2023) %>% 
  rbind(results2024)

# Add Distances for each point
allresults <- allresults %>% 
  mutate(time = seconds(hms(time)),
         distance = case_when(point == "5K" ~ 5000,
                              point == "10K" ~ 10000,
                              point == "15K" ~ 15000,
                              point == "20K" ~ 20000,
                              point == "25K" ~ 25000,
                              point == "30K" ~ 30000,
                              point == "35K" ~ 35000,
                              point == "40K" ~ 40000,
                              point == "20M" ~ 32186.9,
                              point == "20MILE" ~ 32186.9,
                              point == "21M" ~ 33796.2,
                              point == "21MILE" ~ 33796.2,
                              point == "23MILE" ~ 37014.9,
                              point == "24MILE" ~ 38624.3,
                              point == "25.2MILE" ~ 40555.47,
                              point == "252M" ~ 40555.47,
                              point == "HALF" ~ 21082.41,
                              point == "FINISH" ~ 42164.81)) %>% 
  filter(!(point %in% c("ANNOUNCER", "CHARLESGATE-CHEER", "NATICK-CHEER",
                        "POSTSTART", "START")))

# Change point names so they work in random forest function
allresults <- allresults %>% 
  mutate(point = case_when(point == "5K" ~ "fiveK",
                           point == "10K" ~ "tenK",
                           point == "15K" ~ "fifteenK",
                           point == "20K" ~ "twentyK",
                           point == "25K" ~ "twentyfiveK",
                           point == "30K" ~ "thirtyK",
                           point == "35K" ~ "thirtyfiveK",
                           point == "40K" ~ "fortyK",
                           point == "20M" ~ "twentyM",
                           point == "20MILE" ~ "twentyM",
                           point == "21M" ~ "twentyoneM",
                           point == "21MILE" ~ "twentyoneM",
                           point == "23MILE" ~ "twentythreeM",
                           point == "24MILE" ~ "twentyfourM",
                           point == "25.2MILE" ~ "twentyfivetwoM",
                           point == "252M" ~ "twentyfivetwoM",
                           point == "HALF" ~ "HALF",
                           point == "FINISH" ~ "FINISH"))

# Filter out 23 and 24 mile splits (Only done in 2023 and 2024)
allresults <- allresults %>% 
  filter(point != "twentythreeM" & point != "twentyfourM")

# Add column signifying if they finished
allresults <- allresults %>% 
  group_by(pid, year) %>% 
  arrange(pid, distance) %>% 
  mutate(finish = (point == "FINISH")) %>% 
  mutate(finish = as.logical(sum(finish))) %>% 
  rename("finisher" = "finish")


saveRDS(allresults, "allresults.rds")


# Select only runners
runnerresults <- allresults %>% 
  filter(division == "runner" & class != "para")


test <- runnerresults %>% 
  select(-distance, -bib_display) %>% 
  pivot_wider(names_from = point, values_from = time)

test %>% 
  filter(!FINISH)

saveRDS(runnerresults, "runnerresults.rds")
