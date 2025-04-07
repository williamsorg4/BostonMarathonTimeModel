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

saveRDS(course_dataframe, "course_dataframe.rds")


# Calculate Predictions ------------------------------------
mean_adjustment <- course_dataframe$pace_adjustment %>% mean(na.rm = TRUE)

goal_time <- lubridate::hms("3:00:00")


grade_adjusted_time <- as.numeric(seconds(goal_time)) / mean_adjustment

grade_adjusted_time_per_segment <- grade_adjusted_time / (nrow(course_dataframe) - 1)

ideal_pace_df <- course_dataframe %>% 
  mutate(segment_time = pace_adjustment * grade_adjusted_time_per_segment) %>% 
  .[-1, ]

ideal_pace_df <- ideal_pace_df %>% 
  mutate(fiveKgroup = rep(1:(nrow(ideal_pace_df) %/% 80 + 1), each = 80, length.out = nrow(ideal_pace_df)),
         twoMilegroup = rep(1:(nrow(ideal_pace_df) %/% 51 + 1), each = 51, length.out = nrow(ideal_pace_df)))


ideal_pace_df <- ideal_pace_df %>% 
  summarise(split = sum(segment_time), .by = fiveKgroup) %>% 
  .[-9, ]

ideal_pace_df <- ideal_pace_df %>% 
  mutate(pacekm = split / 5,
         pacemi = split / 3.10686)


slowest <- min(ideal_pace_df$split)
fastest <- max(ideal_pace_df$split)


# Fivek Splits 
ideal_pace_df %>% 
  mutate(time_rev = slowest * 1.25 - split,
         text = paste(hms(round(split)) %>% 
                        gsub("^00:", "", .) %>% 
                        gsub("^0", "", .))) %>% 
  ggplot(aes(x = fiveKgroup, y = time_rev, fill = split)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 1.5 * (slowest * 1.25 - fastest)),
                     labels = function(x) {
                       unround <- (-x + slowest * 1.25)
                       paste(hms(round(unround))) %>% 
                         gsub("^00:", "", .) %>% 
                         gsub("^0", "", .)
                     }
  ) +
  scale_x_continuous(labels = function(x) paste0(substr(x * 5, 1, 2), "k")) +
  scale_fill_gradient(low = "#90caf9", high = "#0d47a1") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank())


ideal_pace_df %>% 
  mutate(Distance = paste0(fiveKgroup * 5, "k"),
         `Split Time` = paste(hms(round(split)) %>% 
                                gsub("^00:", "", .) %>% 
                                gsub("^0", "", .)),
         `Pace (km)` = sprintf("%d:%02d", (pacekm %/% 60), as.integer(pacekm %% 60)),
         `Pace (mi)` = sprintf("%d:%02d", (pacemi %/% 60), as.integer(pacemi %% 60)),
         `Total Time` = paste(hms(round(cumsum(split)))) %>% 
           gsub("^00:", "", .) %>% 
           gsub("^0", "", .)) %>% 
  select(Distance, `Split Time`, `Pace (km)`, `Pace (mi)`, `Total Time`)
