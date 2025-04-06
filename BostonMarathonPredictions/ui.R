library(shiny)
library(bslib)
library(shinyTime)
library(hms)
library(bsicons)
library(lubridate)
library(plotly)
library(tidyverse)
library(ranger)


leaders_card <- function(group) {
  card(
    card_title(group)
  )
}


navbarPage(
  theme = bs_theme(bootswatch = 'minty'),
  "2025 Boston Marathon",   
  tabPanel(
    "Leaders", 
    icon = bs_icon("list-ol"),
    leaders_card("Top Men"),
    leaders_card("Top Women"),
    leaders_card("Top Women"),
    leaders_card("Top Women"),
    ),
  
  tabPanel(
    "Search",
    icon = bs_icon("search"),
    layout_columns(
      card(
        card_title("Search")
        ),
      card(
        card_title("Results")
        ),
      
      col_widths = c(4, 8)
      )
    ),
  
  tabPanel(
    "Goal Predictor",
    icon = bs_icon("graph-up"),
    layout_columns(
      card(
        card_title("Input"),
        numericInput("age", "Age: ", value = 40, min = 18, max = 85),
        selectInput("sex", "Sex: ", list("Male" = "M", "Female" = "F", "Other" = "X")),
        selectInput("class", "Class: ", list("Open" = "open", "Elite" = "elite")),
        timeInput("goalTime", "Goal Time: ", value = as_hms("3:00:00")),
        actionButton("goalTimeButton", "Generate")
      ),
      card(
        card_title("Predictor"),
        layout_columns(
          value_box(
            title = "Half Marathon Split", 
            value = textOutput("GPHalfTime"),
            showcase = 
          ), 
          value_box(
            title = "Finish Time",
            value = textOutput("GPFinish")
          )
          ),
        plotlyOutput("goalTimePlot"),
        tableOutput("goaltimeData")
        
      ),
      col_widths = c(4, 8)
      )
    ),
  navbarMenu("More",
             tabPanel("Course"),
             tabPanel("Methodology"),
             tabPanel("Citations")
  ),
  
  
  fluid = TRUE
)
