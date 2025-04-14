library(shiny)
library(bslib)
library(shinyTime)
library(hms)
library(bsicons)
library(plotly)
library(tidyverse)
library(ranger)
library(readr)



# Load Data --------------------------------------------------------
runnerresults <- readRDS(url("https://github.com/williamsorg4/BostonMarathonTimeModel/raw/main/Data/runnerresults.rds"))
load("goal_time_models.RData")
course_dataframe <- readRDS(url("https://github.com/williamsorg4/BostonMarathonTimeModel/raw/main/Data/course_dataframe.rds"))

# Create Data --------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing() %>% 
  filter(sex != "X")

# Create Data Frame for predicting splits from goal time
goal_time <- tibble(split = c("fiveK", "tenK", "fifteenK", "twentyK", "HALF", "twentyfiveK", 
                              "thirtyK", "thirtyfiveK", "fortyK"),
                    distance = c(5000, 10000, 15000, 20000, 21082.41, 25000, 30000, 
                                 35000, 40000))



function(input, output) {
  # Ideal Splits --------------------------------------------------
  observeEvent(input$idealTimeButton, {
    goal_time <- as.numeric(seconds(as_hms(input$goalTimeIdeal)))
    
    mean_adjustment <- course_dataframe$pace_adjustment %>% mean(na.rm = TRUE)
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
    
    
    
    output$IPEntered <- renderText({
      paste0(hms::hms(goal_time)) %>% 
        gsub("^00:", "", .) %>% 
        gsub("^0", "", .)
    })
    
    output$IPAdjusted <- renderText({
      paste0(hms::hms(round(grade_adjusted_time))) %>% 
        gsub("^00:", "", .) %>% 
        gsub("^0", "", .)
    })
    
    
    output$IPPlot <- renderPlotly({
      plot <- ideal_pace_df %>% 
        mutate(time_rev = slowest * 1.25 - split,
               text = paste(hms::hms(round(split)) %>% 
                              gsub("^00:", "", .) %>% 
                              gsub("^0", "", .))) %>% 
        ggplot(aes(x = fiveKgroup, y = time_rev, fill = split, text = text)) +
        geom_col() +
        scale_y_continuous(limits = c(0, 1.5 * (slowest * 1.25 - fastest)),
                           labels = function(x) {
                             unround <- (-x + slowest * 1.25)
                             unround
                             paste(hms::hms(round(unround))) %>% 
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
      
      ggplotly(plot, tooltip = "text") %>% 
        config(displayModeBar = FALSE)
    })
    
    
    output$IPData <- renderTable(
      ideal_pace_df %>% 
        mutate(Distance = paste0(fiveKgroup * 5, "k"),
               `Split Time` = paste(hms::hms(round(split)) %>% 
                                      gsub("^00:", "", .) %>% 
                                      gsub("^0", "", .)),
               `Pace (km)` = sprintf("%d:%02d", (pacekm %/% 60), as.integer(pacekm %% 60)),
               `Pace (mi)` = sprintf("%d:%02d", (pacemi %/% 60), as.integer(pacemi %% 60)),
               `Total Time` = paste(hms::hms(round(cumsum(split)))) %>% 
                 gsub("^00:", "", .) %>% 
                 gsub("^0", "", .)) %>% 
        select(Distance, `Split Time`, `Pace (km)`, `Pace (mi)`, `Total Time`)
    )
    
    
  })
  
  
  
  # Goal Predictor -----------------------------------------------------
  observeEvent(input$goalTimeButton, {
    # Add raw split predictions
    goal_time <- goal_time %>% 
      mutate(prediction = predict(get(paste0("rfFINISHto", split)), 
                                  data = tibble(age = input$age,
                                                sex = input$sex,
                                                class = input$class,
                                                FINISH = as.numeric(seconds(as_hms(input$goalTime)))))$prediction,
             .by = split)
    
    # Calculate error in total prediction time vs input time
    final_split_pred <- predict(rfGoalTimeFortyKtoFINISH, data = goal_time %>% 
                                  select(-distance) %>% 
                                  pivot_wider(names_from = split, values_from = prediction))$prediction
    
    pred_error <- (goal_time$prediction[goal_time$split == "fortyK"] + final_split_pred) / as.numeric(seconds(as_hms(input$goalTime)))
    
    # Correct predictions and add pace
    goal_time <- goal_time %>% 
      mutate(prediction = prediction / pred_error,
             split_time = case_when(split == "HALF" ~ NA,
                                    split == "fiveK" ~ prediction,
                                    split == "twentyfiveK" ~ prediction - lag(prediction, 2),
                                    .default = prediction - lag(prediction)),
             pace = split_time / 300,
             paceMI = pace / 0.621371)
    
    slowest <- max(goal_time$pace, na.rm = TRUE)
    fastest <- min(goal_time$pace, na.rm = TRUE)
      
    output$GPHalfTime <- renderText({
      paste0(hms::hms(round(seconds(goal_time$prediction[goal_time$split == "HALF"])))) %>% 
        gsub("^00:", "", .) %>% 
        gsub("^0", "", .)
    })
    
    output$GPFinish <- renderText({
      paste0(hms::hms(round(seconds(as_hms(input$goalTime))))) %>% 
        gsub("^00:", "", .) %>% 
        gsub("^0", "", .)
    })
    
    output$goalTimePlot <- renderPlotly(
      {
        plot <- goal_time %>% 
          filter(split != "HALF") %>% 
          mutate(lap = row_number(),
                 pace_rev = slowest * 1.25 - pace,
                 text = paste("<b>Lap ", lap, "<b><br>",
                              gsub("^00:", "", paste0(round_hms(as_hms(split_time), digits = 0))) )) %>% 
          ggplot(aes(x = distance, y = pace_rev, fill = pace, text = text)) +
          geom_col() +
          scale_y_continuous(limits = c(0, 1.5 * (slowest * 1.25 - fastest)),
                             labels = function(x) {
                               unround <- (-x + slowest * 1.25)
                               paste0(unround %/% 1, ":", round((unround %% 1) * 60), "/km")
                               }
          ) +
          scale_x_continuous(labels = function(x) paste0(substr(x, 1, 2), "k")) +
          scale_fill_gradient(low = "#90caf9", high = "#0d47a1") +
          xlab("") +
          ylab("") +
          theme_minimal() +
          theme(legend.position = 'none',
                panel.grid.minor = element_blank())
        
        ggplotly(plot, tooltip = "text") %>% 
          config(displayModeBar = FALSE)
      }
    )
    output$goaltimeData <- renderTable(
      goal_time %>% 
        filter(split != "HALF") %>% 
        mutate(Distance = paste0(distance / 1000, "k"),
               "Split Time" = gsub("^00:", "", paste0(round_hms(as_hms(split_time), digits = 0))),
               "Pace (km)" = sprintf("%d:%02d", (pace %/% 1), as.integer(pace %% 1 * 60)),
               "Pace (mi)" = sprintf("%d:%02d", (paceMI %/% 1), as.integer(paceMI %% 1 * 60)),
               "Total Time" = gsub("^0", "", gsub("^00:", "", paste0(round_hms(as_hms(prediction), digits = 0))))) %>% 
        select(Distance, `Split Time`, `Pace (km)`, `Pace (mi)`, `Total Time`),
      striped = TRUE
      )
  })
  
  

}