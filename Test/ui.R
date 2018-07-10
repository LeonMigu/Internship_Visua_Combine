library(shiny)
library(DT)
library(plotly)
library(wordcloud2)
library(wordcloud)
library(tm)
library(crosstalk)
library(rmarkdown)
library(knitr)
library(webshot)
library(shinydashboard)
#library(tinytext)
library(knitrProgressBar)
library(rlist)


#Shiny App
#UI and layout. It implements the front-end
#Creating a navbar page with different tabs, which are created by tabPanel
header <- dashboardHeader(title="NLP App",dropdownMenuOutput("warningMenu"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Pre-processing",
             menuSubItem("Overview", tabName = "overview_pre"), icon = icon("fas fa-plane")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "data",
      fileInput("inputdata", "Choose PDF File", multiple = FALSE)
    ),
    tabItem(
      tabName = "overview_ana",
      h2("Plot overview"),
      fluidRow(
        column(width = 4,
               box(
                 selectInput(inputId = 'choice', label = 'Choose a metric', 
                             choice = c('Frequency', 'Random'))               )
        ),
        column(width = 4,
               box(
                 plotlyOutput("plot_overview")
               )
        )
      ),
      fluidRow(
        column(width = 8, offset =4,
               box(
                 DT::dataTableOutput("table_overview")
               ))
      )
    ),
    tabItem(
      tabName = "filter_ana",
      h2("Plot filtering"),
      fluidRow(
        column(width = 8,
               box(
                 wordcloud2Output("wordcloud"),
                 #This HTML script implements the click option for the wordcloud
                 tags$script(HTML(
                   "$(document).on('click', '#canvas', function() {",
                   'word = document.getElementById("wcSpan").innerHTML;',
                   "Shiny.onInputChange('selected_word', word);",
                   "});"
                 )),
                 verbatimTextOutput("test")
               )),
        column(width = 4,
               box(
                 #Slider button to filter the data
                 sliderInput(inputId = "slide_value_freq", label = "Filter the frequency", min = 1, max = m, value = c(1,m), step = 1, dragRange = TRUE),
                 sliderInput(inputId = "slide_value_word", label = "Choose the maximum number of words", min = 1, max = n, value = n, step = 1)
                 
               ))
      )
    )
  ),
  tabItem(
    tabName = "details_pre"
  )
)

ui <- dashboardPage(header, sidebar, body)

return(ui)