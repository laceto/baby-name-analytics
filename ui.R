library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(stringr)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(formattable)
library(shinyBS)
library(foreach)
library(doParallel)
library(doSNOW)
library(dplyr)
library(plyr)
library(knitr)
library(openxlsx)
library(rlist)
library(pROC)
library(caret)
library(car)
library(purrr)
library(htmltools)
library(lubridate)
library(shinyalert)
library(rintrojs)
library(hrbrthemes)
library(viridis)
library(latticeExtra) 
library(ggridges)
library(rgl)
library(plotly)
library(broom)
library(shinycssloaders)
library(kableExtra)
library(knitr)
library(shinyscreenshot)


header <- dashboardHeader(

  title = "Baby names",
  titleWidth = 350
)

# sidebar function ----

sidebar <- dashboardSidebar(disable = T,
  width = 350,
  sidebarMenu(
    menuItem("Random names", tabName = "random_name", icon = icon("running"))
  )
)

tab_all_input <- fluidPage(

  fluidRow(
    br()
  ),
  fluidRow(
    box(width = 12,
      column(12,
             # box(
             selectInput(inputId = "num_names", label = "how many names?",  choices = c(1:5), multiple = FALSE),
             selectInput("gender",  "gender", c("M", "F", "ALL"), multiple = F),
             selectInput("lastletter",  "last_letter", c("ALL", "NOT A"), multiple = F)
             # )
             )
      )
    ),
  fluidRow(
    column(3,
           ),
    column(3,
           ),
    column(3,
           ),
    column(3,
           # box(
             actionBttn(
               inputId = "button_names",
               label = "Names",
               color = "primary",
               style = "gradient",
               size = "lg"
               )
             # )
           )
  ),
  fluidRow(
    box(width = 12,
      htmlOutput("names_table")
      )
    ),
  fluidRow(
    column(12,
           box(width = 12,
             selectInput(inputId = "start_letter", label = "choice start letter name", choices = LETTERS, multiple = F),
             selectInput(inputId = "gender_start_letter", label = "gender", c("M", "F"), multiple = F)
             )
           )
  ),
  fluidRow(
    dataTableOutput("all_names_table", width = "100%"),
    verbatimTextOutput('row_selected'),
    br(),
    hr(),
    actionButton("go", "Take a screenshot"),
    dataTableOutput("names_selected_table", width = "100%")
    )
  )

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "random_name",
            tab_all_input
    )
  )
)

ui <- dashboardPage(header, sidebar, body)
