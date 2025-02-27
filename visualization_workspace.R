library(tidyverse)
library(httr2)
library(jsonlite)
library(lubridate)

# Prep work ----------------------------------------------------------------
appid <- "6099b508b4198c74236b1536"
race_name <- "BOSTON-2024"

# Get API Token
register <- request(paste0("https://api.rtrt.me/register?appid=", appid))
resp <- req_perform(register)
token <- resp_body_string(resp) %>% 
  str_split('"') %>% 
  unlist() %>% 
  .[[4]]


# Top Women -----------------------------------------------------------
request <- request(paste0("https://api.rtrt.me/events/", race_name, "/categories/top-women", "?appid=", appid, "&max=2000", "&amp;token=", token))
resp <- req_perform(request)
json <- resp_body_string(resp)
temporary <- fromJSON(json)$list



request <- request(paste0("https://api.rtrt.me/events/", race_name, "/categories/top-women/splits?appid=", appid, "&max=2000&amp;token=", token))
resp <- req_perform(request)
json <- resp_body_string(resp)
top_women_splits <- fromJSON(json)$list %>% as_tibble()

top_women_splits %>% 
  filter(pid == women_pids[1]) %>% 
  mutate(age = str_split(bib_display, "-") %>% unlist() %>% .[2] %>% substr(1,2) %>% as.integer(), .before = division) %>% 
  select(name, age, sex, class)

top_women_splits %>% 
  group_by(pid)




distances <- tibble(point = c("fiveK", "tenK", "fifteenK", "twentyK", 
                              "twentyfiveK", "thirtyK", "thirtyfiveK", 
                              "fortyK", "twentyM", "twentyoneM","twentythreeM", 
                              "twentyfourM", "twentyfivetwoM", "HALF", "FINISH"),
                    distance = c(5000, 1000, 15000, 20000, 25000, 30000, 35000,
                                 40000, 32186.9, 33796.2, 37014.9, 38624.3, 40555.47,
                                 21082.41, 42164.81))

  onerunner <- runnerresults %>% 
  filter(pid == "R24NMU6A")

onerunner %>% 
  pivot_longer(cols = 16:28,
               names_to = "point",
               values_to = "time") %>% 
  mutate(distance = distances)