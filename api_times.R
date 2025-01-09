library(httr2)
library(tidyverse)
library(jsonlite)

appid <- "6099b508b4198c74236b1536"

# Get API Token
register <- request(paste0("https://api.rtrt.me/register?appid=", appid))
resp <- req_perform(register)
token <- resp_body_string(resp) %>% 
  str_split('"') %>% 
  unlist() %>% 
  .[[4]]

# Get Available Races
events <- request(paste0("https://api.rtrt.me/events?appid=", appid, "&amp;token=", token))
resp <- req_perform(events)
json <- resp_body_string(resp)
events_db <- fromJSON(json)$list


# 2022 Results ----------------------------------------------------------
# Race Selection
race_name <- "BOSTON-2022"

# Finishers
finishers <- events_db %>% 
  filter(name == race_name) %>% 
  pull(finishers)


# Get All Participants
desired_info <- c("bib", "city", "class", "country", "country_iso", "course", 
                  "fname", "lname", "name", "pid", "race", "sex", "tag", "bib_display")

participants_db <- tibble()
for (i in seq(from = 1, to = 30000, by = 2000)){
  participants <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles", "?appid=", appid, "&start=", i, "&max=2000", "&amp;token=", token))
  resp <- req_perform(participants)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list
  temporary <- temporary %>% 
    as_tibble() %>% 
    select(all_of(desired_info))
  print(nrow(temporary))
  participants_db <- participants_db %>% 
    rbind(temporary)
}

participants_db <- participants_db %>% 
  filter(course == "marathon")


# Get Race Splits
splits <- tibble()
for(i in seq(from = 1, to = nrow(participants_db)- 130, by = 130)){
  if((i + 129) > nrow(participants_db)){
    end <- nrow(participants_db)
  } else {
    end <- i + 129
  }
  pid <- participants_db$pid[i:end] %>% 
    paste0(collapse = ",")
  athlete <- request(paste0("https://api.rtrt.me/events/BOSTON-2022/profiles/", pid, "/splits?appid=", appid, "&max=2000&amp;token=", token))
  resp <- req_perform(athlete)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list %>% as_tibble()
  splits <- splits %>% 
    bind_rows(temporary)
}

results2022 <- splits %>% 
  left_join(participants_db)

results2022 <- results2022 %>% 
  select(name, fname, lname, sex, division, class, city, country, country_iso,
         time, point, bib, bib_display, pid, tag) %>% 
  group_by(pid) %>% 
  mutate(age = str_split(bib_display, "-") %>% unlist() %>% .[2] %>% substr(1,2) %>% as.integer(), .before = division) %>% 
  ungroup()


saveRDS(results2022, "results2022.rds")

# 2024 Results ------------------------------------------------------------

# Race Selection
race_name <- "BOSTON-2024"

# Finishers
finishers <- events_db %>% 
  filter(name == race_name) %>% 
  pull(finishers)


# Get All Participants
desired_info <- c("bib", "city", "class", "country", "country_iso", "course", 
                  "fname", "lname", "name", "pid", "race", "sex", "tag", "bib_display")

participants_db <- tibble()
for (i in seq(from = 1, to = 32000, by = 2000)){
  participants <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles", "?appid=", appid, "&start=", i, "&max=2000", "&amp;token=", token))
  resp <- req_perform(participants)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list
  temporary <- temporary %>% 
    as_tibble() %>% 
    select(all_of(desired_info))
  print(nrow(temporary))
  participants_db <- participants_db %>% 
    rbind(temporary)
}


# Get Race Splits
splits <- tibble()
for(i in seq(from = 1, to = nrow(participants_db)- 130, by = 130)){
  if((i + 129) > nrow(participants_db)){
    end <- nrow(participants_db)
  } else {
    end <- i + 129
  }
  pid <- participants_db$pid[i:end] %>% 
    paste0(collapse = ",")
  athlete <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles/", pid, "/splits?appid=", appid, "&max=2000&amp;token=", token))
  resp <- req_perform(athlete)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list %>% as_tibble()
  splits <- splits %>% 
    bind_rows(temporary)
}

results2024 <- splits %>% 
  select(-c(name, sex, city, country, country_iso, bib_display, course, tag, bib)) %>% 
  inner_join(participants_db)

results2024 <- results2024 %>% 
  select(name, fname, lname, sex, division, class, city, country, country_iso,
         time, point, bib, bib_display, pid, tag) %>% 
  group_by(pid) %>% 
  mutate(age = str_split(bib_display, "-") %>% unlist() %>% .[2] %>% substr(1,2) %>% as.integer(), .before = division) %>% 
  ungroup()

saveRDS(results2024, "results2024.rds")





# 2023 Results ----------------------------------------------------------

# Race Selection
race_name <- "BOSTON-2023"

# Finishers
finishers <- events_db %>% 
  filter(name == race_name) %>% 
  pull(finishers)


# Get All Participants
desired_info <- c("bib", "city", "class", "country", "country_iso", "course", 
                  "fname", "lname", "name", "pid", "race", "sex", "tag", "bib_display")

participants_db <- tibble()
for (i in seq(from = 1, to = 34000, by = 2000)){
  participants <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles", "?appid=", appid, "&start=", i, "&max=2000", "&amp;token=", token))
  resp <- req_perform(participants)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list
  temporary <- temporary %>% 
    as_tibble() %>% 
    select(all_of(desired_info))
  print(nrow(temporary))
  participants_db <- participants_db %>% 
    rbind(temporary)
}

participants_db <- participants_db %>% 
  filter(course == "marathon") 


# Get Race Splits
splits <- tibble()
for(i in seq(from = 1, to = nrow(participants_db)- 130, by = 130)){
  if((i + 129) > nrow(participants_db)){
    end <- nrow(participants_db)
  } else {
    end <- i + 129
  }
  pid <- participants_db$pid[i:end] %>% 
    paste0(collapse = ",")
  athlete <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles/", pid, "/splits?appid=", appid, "&max=2000&amp;token=", token))
  resp <- req_perform(athlete)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list %>% as_tibble()
  splits <- splits %>% 
    bind_rows(temporary)
}

results2023 <- splits %>% 
  select(-c(name, sex, city, country, country_iso, bib_display, course, tag, bib)) %>% 
  inner_join(participants_db)

results2023 <- results2023 %>% 
  select(name, fname, lname, sex, division, class, city, country, country_iso,
         time, point, bib, bib_display, pid, tag) %>% 
  group_by(pid) %>% 
  mutate(age = str_split(bib_display, "-") %>% unlist() %>% .[2] %>% substr(1,2) %>% as.integer(), .before = division) %>% 
  ungroup()

saveRDS(results2023, "results2023.rds")




# 2021 Results -----------------------------------------------------------


# Race Selection
race_name <- "BOSTON-2021"

# Finishers
finishers <- events_db %>% 
  filter(name == race_name) %>% 
  pull(finishers)


# Get All Participants
desired_info <- c("bib", "city", "class", "country", "country_iso", "course", 
                  "fname", "lname", "name", "pid", "race", "sex", "tag", "bib_display")

participants_db <- tibble()
for (i in seq(from = 1, to = 22000, by = 2000)){
  participants <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles", "?appid=", appid, "&start=", i, "&max=2000", "&amp;token=", token))
  resp <- req_perform(participants)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list
  temporary <- temporary %>% 
    as_tibble() %>% 
    select(all_of(desired_info))
  print(nrow(temporary))
  participants_db <- participants_db %>% 
    rbind(temporary)
}


# Get Race Splits
splits <- tibble()
for(i in seq(from = 1, to = nrow(participants_db)- 130, by = 130)){
  if((i + 129) > nrow(participants_db)){
    end <- nrow(participants_db)
  } else {
    end <- i + 129
  }
  pid <- participants_db$pid[i:end] %>% 
    paste0(collapse = ",")
  athlete <- request(paste0("https://api.rtrt.me/events/", race_name, "/profiles/", pid, "/splits?appid=", appid, "&max=2000&amp;token=", token))
  resp <- req_perform(athlete)
  json <- resp_body_string(resp)
  temporary <- fromJSON(json)$list %>% as_tibble()
  splits <- splits %>% 
    bind_rows(temporary)
}

results2021 <- splits %>% 
  select(-c(name, sex, city, country, country_iso, bib_display, course, tag, bib)) %>% 
  inner_join(participants_db)

results2021 <- results2021 %>% 
  select(name, fname, lname, sex, division, class, city, country, country_iso,
         time, point, bib, bib_display, pid, tag) %>% 
  group_by(pid) %>% 
  mutate(age = str_split(bib_display, "-") %>% unlist() %>% .[2] %>% substr(1,2) %>% as.integer(), .before = division) %>% 
  ungroup()

saveRDS(results2021, "results2021.rds")

