library(tidyverse)
library(rvest)
library(RSelenium)
library(netstat)
library(wdman)


result_links <- c("https://results.baa.org/2024/?pid=leaderboard&pidp=leaderboard",
                  "https://results.baa.org/2023/?pid=leaderboard&pidp=leaderboard",
                  "http://registration.baa.org/2022/cf/Public/iframe_ResultsSearch.cfm?mode=entry",
                  "https://boston.r.mikatiming.com/2021/?pid=leaderboard&pidp=leaderboard",
                  "http://registration.baa.org/2019/cf/public/iframe_resultssearch.cfm",
                  "http://registration.baa.org/2018/cf/public/iframe_ResultsSearch.cfm",
                  "http://registration.baa.org/2017/cf/public/iframe_ResultsSearch.cfm")

# 2024 & 2023
# 2022 & 2019 & 2018 ... 2011
# 2021
# 
# 2018 had bad weather


rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "128.0.6613.119",
                             verbose = FALSE, 
                             port = free_port())

remDr <- rs_driver_object$client


remDr$open()

# 2024 --------------------------------------------------------------

remDr$navigate("https://results.baa.org/2024/?pid=start&pidp=start")



# Select 1000 results per page

men2024links <- c()
women2024links <- c()
allrunnerslinks <- c()


getlinksall <- function(){
  athletes <- remDr$findElements(using = 'css selector', 
                                 '.type-fullname a')
  allrunnerslinks <<- append(allrunnerslinks, lapply(athletes, function(x) x$getElementAttribute("href") %>% unlist()))
  nextpagebutton <- remDr$findElement(using = 'css selector',
                                      '.hidden-sm+ .pages-nav-button a')
  nextpagebutton$clickElement()
  Sys.sleep(3)
  print("check")
}

replicate(30, getlinksall())

results2024 <- tibble()


i <- 0
allrunnerslinks2 <- allrunnerslinks[23942:29330]
for(link in allrunnerslinks2){
  page <- read_html(link)
  participant_tibble <- page %>% 
    html_nodes('table') %>% 
    html_table() %>% 
    .[[1]] %>% 
    pivot_wider(names_from = X1, values_from = X2)
  place_table <- page %>% 
    html_nodes('table') %>% 
    html_table() %>% 
    .[[2]] %>% 
    pivot_wider(names_from = X1, values_from = X2)
  race_status <- page %>% 
    html_nodes('table') %>% 
    html_table() %>% 
    .[[3]] %>% 
    pivot_wider(names_from = X1, values_from = X2)
  if (length(race_status) == 2){
    race_status <- race_status %>% 
      add_column(Status = NA)
  }
  identity_tibble <- participant_tibble %>% 
    append(place_table) %>% 
    append(race_status) %>% 
    as_tibble()
  splits_tibble <- page %>% 
    html_nodes('table') %>% 
    html_table() %>% 
    .[[4]]
  if (length(splits_tibble) == 5){
    splits_tibble <- splits_tibble %>% 
      add_column(`Time Of Day` = NA)
  }
  if (place_table$`Place (M/W)` == "DSQ"){
    splits_tibble <- tibble(Split = NA,
                            `Time Of Day` = NA,
                            Time = NA,
                            Diff = NA,
                            `Min/mile` = NA,
                            `Miles/h` = NA
                            )
  }
  athlete_tibble <- identity_tibble %>% 
    append(splits_tibble) %>% 
    as_tibble()
  results2024 <- results2024 %>% 
    rbind(athlete_tibble)
  i <- i + 1
  print(i)
}

test <- results2024 %>% 
  mutate(test = as.integer(`Bib Number`)) %>% 
  filter(is.na(test))


saveRDS(results2024, "results2024.rds")



# 2023 --------------------------------------------------------------------


remDr$navigate("https://results.baa.org/2023/?pid=start&pidp=start")


# Select 1000 results per page

men2024links <- c()
women2024links <- c()
allrunnerslinks <- c()