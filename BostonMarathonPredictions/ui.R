library(shiny)
library(bslib)
library(shinyTime)
library(hms)
library(bsicons)
library(plotly)
library(tidyverse)
library(ranger)
library(readr)



navbarPage(
  theme = bs_theme(bootswatch = 'minty',
                   secondary = "#0d47a1"),
  "2025 Boston Marathon", 
  
  # Ideal Splits --------------------------------------------------------
  tabPanel(
    "Ideal Splits Generator",
    icon = bs_icon("stopwatch"),
    card(
      card_title("Input"),
      timeInput("goalTimeIdeal", "Goal Time: ", value = as_hms("3:00:00")),
      actionButton("idealTimeButton", "Generate", width = "25%")
    ),
    layout_columns(
      value_box(
        title = "Entered Time", 
        value = textOutput("IPEntered"),
        ),
      value_box(
        title = "GAP Time",
        value = textOutput("IPAdjusted")
        )
      ),
    card(
      card_title("Grade Adjusted Even Splits"),
      plotlyOutput("IPPlot"),
      tableOutput("IPData")
    )
    ),
  
  
  
  # Goal Predictor -------------------------------------------------------
  tabPanel(
    "Goal Predictor",
    icon = bs_icon("graph-up"),
    layout_columns(
      card(
        card_title("Input"),
        numericInput("age", "Age: ", value = 30, min = 18, max = 85),
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
  
  
  
  # Methodology -----------------------------------------------------
  tabPanel(
    "Methodology",
    icon = bs_icon("journal-text"),
    mainPanel(
      strong("Ideal Splits Generator"),
      p("The ideal splits were calculated by adjusting for grade changes 
        throughout the race. The grade adjusted pace (GAP) model is similar
        to the one used by Strava, which is described ", 
        a("here", href = "https://medium.com/strava-engineering/an-improved-gap-model-8b07ae8886c3"),
        ". The course GPX route was downloaded from CJ Albertson's 2024 Boston Marathon post on ",
        a("Strava", href = "https://www.strava.com/activities/11188406998"), ", tracked on a Garmin Forerunner 255"),
      br(),
      strong("Goal Predictor"),
      p("The general idea of the Goal Predictor is to predict the splits of a 
        Boston Marathon runner given their goal time and some more information. 
        Essentially, calculating the most likely path to meeting their goal.
        The Goal Predictor uses random forest models to predict race split times. These 
        models use data from the 2021-24 Boston Marathons, accessed via the RTRT.me API. 
        Predictions take into account finish time, age, sex, and elite status. The 
        raw predictions are then adjusted to account for extrapolation
        limitations that come with random forests.")
    )
  ),
  
  
  fluid = TRUE
)
