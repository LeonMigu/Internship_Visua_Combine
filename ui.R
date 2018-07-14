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
    menuItem("Data", tabName = "data", icon = shiny::icon("th")),
    menuItem("Pre-processing",
             menuSubItem("Overview", tabName = "overview_pre"), icon = icon("fas fa-wrench"),
             menuSubItem("Filter", tabName = "filter_pre", icon = icon("fas fa-binoculars")),
             menuSubItem("Details on demand", tabName = "details_pre", icon = icon("fas fa-sitemap"))
             ),
    menuItem("Analysis", tabName = "ana", 
             menuSubItem("Overview", tabName = "overview_ana"), icon = icon("fab fa-leanpub"),
             menuSubItem("Filter", tabName = "filter_ana", icon = icon("fas fa-binoculars")),
             menuSubItem("Word in context", tabName = "wcontext_ana", icon = icon("fas fa-sitemap"))),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    fluidRow(offset = 10,
    downloadButton("report", "Generate report", class = "butt1"),
    #Putting the color of the button in black and the writing in white so it is easier to see and to interact with
    tags$head(tags$style(".butt1{background-color:black;} .butt1{color: white;}"))
  )
)
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "overview_pre",
      fluidRow(
        box(
          plotlyOutput("plot_data")
        ),
        box(
          checkboxInput("all", label = "Select all the data", value = TRUE),
          checkboxInput("num_check", label = "Choose the data with the numeric input, else you can select it on the graph", value = FALSE),
          numericInput(inputId = "num_offset_data", label = "Choose the number of the first word", min = 1, max = n, value = 1),
          numericInput(inputId = "num_word_data", label = "Choose the number of words follwing the offset", min = 1, max = n, value = n),
          uiOutput("num_data")
        ),
        box(
          checkboxGroupInput("book", "Choose one or more book(s)",
                               check_choices, inline = TRUE)
          )
      )
    ),
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
        column(width = 8,
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