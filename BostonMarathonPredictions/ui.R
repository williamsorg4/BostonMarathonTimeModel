library(shiny)


navbarPage(
  "2025 Boston Marathon",   
  tabPanel("Leaders", "one"),
  tabPanel("Search", "two"),
  tabPanel("Goal Predictor", "three"),
  navbarMenu("More", 
             tabPanel("Course", "four-a"),
             tabPanel("Methodology", "four-b")
  ),
  
  
  fluid = TRUE
)
