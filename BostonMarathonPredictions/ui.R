library(shiny)
library(bslib)
library(shinyTime)
library(hms)

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
    leaders_card("Top Men"),
    leaders_card("Top Women"),
    leaders_card("Top Women"),
    leaders_card("Top Women"),
    ),
  
  tabPanel(
    "Search",
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
        plotlyOutput("goalTimePlot")
        
      ),
      col_widths = c(4, 8)
      )
    ),
  navbarMenu("More", 
             tabPanel("Course", "four-a"),
             tabPanel("Methodology", "four-b"),
             tabPanel("Citations")
  ),
  
  
  fluid = TRUE
)
