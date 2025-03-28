library(shiny)
library(plotly)
library(tidyverse)





# Load Data --------------------------------------------------------
runnerresults <- readRDS("~/R/Boston Marathon Time Model/runnerresults.rds")
load("~/R/Boston Marathon Time Model/Models/goal_time_models.RData")


# Create Data --------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing() %>% 
  filter(sex != "X")

goal_time <- tibble(split = c("fiveK", "tenK", "fifteenK", "twentyK", "HALF", "twentyfiveK", 
                              "thirtyK", "thirtyfiveK", "fortyK"),
                    distance = c(5000, 10000, 15000, 20000, 21082.41, 25000, 30000, 
                                 35000, 40000))



function(input, output) {
  # bs_themer(),
  
  
  
  
  
  # Goal Predictor -----------------------------------------------------
  observeEvent(input$goalTimeButton, {
      goal_time <- goal_time %>% 
        mutate(prediction = predict(get(paste0("rfFINISHto", split)), 
                                    data = tibble(age = input$age,
                                                  sex = input$sex,
                                                  class = input$class,
                                                  FINISH = as.numeric(seconds(input$goalTime))))$prediction,
               .by = split) %>% 
        rbind(tibble(split = "FINISH",
                     distance = 42164.81,
                     prediction = as.numeric(seconds(input$goalTime))))
      
      
      goal_time <- goal_time %>% 
        filter(distance %% 5000 == 0) %>% 
        mutate(pace = case_when(split != "fiveK" ~ ((prediction - lag(prediction)) / 300),
                                split == "fiveK" ~ prediction / 300))
      
    
    output$goalTimePlot <- renderPlotly(
      {
        plot <- goal_time %>%
          ggplot() +
          geom_col(data = goal_time %>%
                     filter(distance %% 5000 == 0),
                   aes(x = distance, y = pace, fill = pace)) +
          scale_y_reverse(labels = function(x) paste0(x, ":00/km"),
                          breaks = seq(from = ceiling(sort(goal_time$pace)[3]),
                                       by = 1,
                                       length.out = 2)) +
          xlab("Distance") +
          ylab("Pace") +
          theme_minimal()
        
        ggplotly(plot)
      }
    )
  })
}