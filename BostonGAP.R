library(tidyverse)
library(XML)
library(lubridate)

# Replicate Strava Model ------------------------------------------------
gradient <- c(-32, -28, -24, -20.5, -17, -15, -8, 0, 10.5, 12, 15, 17, 21, 28, 32)
pace_adjustment <- c(1.6, 1.4, 1.2, 1.1, 1, .9, .9, 1, 1.45, 1.7, 1.85, 2.05, 2.35, 2.95, 3.35)

points <- tibble(gradient, pace_adjustment)

points %>% 
  ggplot(aes(x = gradient, y=pace_adjustment)) +
  geom_smooth(se=F)

gapmodel <- loess(y ~ x, points)



# Load Course GPX ------------------------------------------------------
course_dataframe <- tibble()

gpx_course_function <- function(x, y){
  gpx_parsed <- htmlTreeParse(file = x, useInternalNodes = TRUE)
  coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
  course_dataframe <<- tibble(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elevation)) %>% 
    mutate(race = y) %>% 
    rbind(course_dataframe)
}

gpx_course_function("Data/Boston Marathon 2023.gpx", "Boston")


course_dataframe <- course_dataframe %>% 
  reframe(lat, lon, elevation, race, count = n() - 1) %>% 
  mutate(row = row_number() - 1,
         distance = 42.374028 / count * row) %>% 
  select(-row, -count)

course_dataframe <- course_dataframe %>% 
  mutate(elevation_change = elevation - lag(elevation), 
         grade = 0.1 * (elevation_change / course_dataframe$distance[2]))

course_dataframe$pace_adjustment <- predict(gapmodel, course_dataframe$grade)

# ------
mean_adjustment <- course_dataframe$pace_adjustment %>% mean(na.rm = TRUE)

goal_time <- hms("2:40:00")
grade_adjusted_time <- as.numeric(seconds(goal_time)) / mean_adjustment

grade_adjusted_time_per_segment <- grade_adjusted_time / (nrow(course_dataframe) - 1)


(grade_adjusted_time_per_segment * (course_dataframe$pace_adjustment [-1]))[81:160] %>% sum()
1138.447 / 60
