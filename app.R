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
#library(tinytext)


#Shiny App
#UI and layout. It implements the front-end
#Creating a navbar page with different tabs, which are created by tabPanel
ui <- navbarPage("NLP App",
      tabPanel("Plot overview",
                sidebarLayout(
                  #This implements the sidebar that offers the choice of the metric
                  sidebarPanel(
                    selectInput(inputId = 'choice', label = 'Choose a metric', 
                                     choice = c('Frequency', 'Random')),
                    fileInput("file1", "Choose PDF File",
                              multiple = FALSE)
                  ),
                  #The main panel is composed of a plotly graph and a data table
                  mainPanel(
                    plotlyOutput("plot_overview"),
                    DT::dataTableOutput("table_overview"))
                ),
               fluidRow(
                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                              inline = TRUE),
                 downloadButton("report", "Generate report")
               )
      ),
    tabPanel("Plot filtering", 
             #Slider button to filter the data
             sliderInput(inputId = "slide_value_freq", label = "Filter the frequency", min = 1, max = m, value = c(1,m), step = 1, dragRange = TRUE),
             sliderInput(inputId = "slide_value_word", label = "Choose the maximum number of words", min = 1, max = n, value = n, step = 1),
             wordcloud2Output("wordcloud"),
             #This HTML script implements the click option for the wordcloud
             tags$script(HTML(
               "$(document).on('click', '#canvas', function() {",
               'word = document.getElementById("wcSpan").innerHTML;',
               "Shiny.onInputChange('selected_word', word);",
               "});"
             )),
             verbatimTextOutput("test")
      
    ),
    tabPanel("Word in context"
    )
)      

#Server function that implements the back-end 

server <- function(input, output, session){
  #Shared data between the plot and the datatable of the overview and the wordcloud
  d_shared <- SharedData$new(d, ~key)
  
  #Plotting the scatterplot with plotly
  output$plot_overview <- renderPlotly({
    #s matches the row selected by the user
    s <- input$plot_rows_selected
    #The if for the length doesn't seem very useful, because it works without it, however, I found it on the internet and there is maybe a reason I haven't found yet, so I prefer to let for now.
    #if there are no row selected yet, you can highlight the plot by selecting some points
    if(!length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d_shared, x = ~rowname, y = ~freq, key = ~key, type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))
      }
      else if(input$choice=='Random'){
        plot_ly(d_shared, x = ~rowname, y = ~sort(random, decreasing = TRUE), key = ~key, type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect', defaultValues = s, color = I('green'))
        
      }
    }
    #If there are row selected, you can't higlight the plot because it is already highlighted 
    else if(length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d, x = ~rowname, y = ~freq, key = ~key, type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)
      }
      else if(input$choice=='Random'){
        plot_ly(d, x = ~rowname, y = ~sort(random, decreasing = TRUE), key = ~key, type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)      
      }
    }
  })
  
  #Plotting the Data Table
  output$table_overview <- DT::renderDataTable({
    #Choosing the data selected in the plot. It is done by crosstalk, see CRAN R Crosstalk SharedData for more details
    dsel <- d[d_shared$selection(),]
    #Creating the data table with the initial data
    dt <-DT::datatable(d,options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),pageLength = 5, lengthMenu = c(5, 10, 15, 20)),class = 'display')
    #This condition is whether a data is selected on the plot
    if (NROW(dsel) == 0) {
      dt
    } else {
      #If a data is selected, then we change the style of the table in order to highlight the selected rows, which are dsel$rowname
      DT::formatStyle(dt, "rowname", target = "row",
                      color = DT::styleEqual(dsel$rowname, rep("white", length(dsel$rowname))), backgroundColor = DT::styleEqual(dsel$rowname, rep("black", length(dsel$rowname))))
    }
  })

  #Choosing the data which is shared with the plot
  d_real_shared <- reactive({d[d_shared$selection(),]})
  #Choosing the data to give to the wordcloud, depending on which data is taken from plotly
  filter_d <- reactive({
    #Changing the data in order to match what the wordcloud takes as an input
    d_prime_reac <- reactive({data.frame(d_real_shared()$word, d_real_shared()$freq)})
    head(subset(d_prime_reac(), d_real_shared...freq <= input$slide_value_freq[2] & d_real_shared...freq >= 
                                          input$slide_value_freq[1]), 
                                 input$slide_value_word)
  })
 
  #Creating the wordcloud and making it reactive to change in the input values
  output$wordcloud  <- renderWordcloud2(wordcloud2(data = filter_d(),
                                                   shape = 'star', size = 0.8, shuffle =FALSE))
  output$test <- renderPrint({
    input$selected_word
  })
  output$report <- downloadHandler(
    filename = function() {
      paste('my_report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data_complete = d, data_selected_plot = d[d_shared$selection(),], 
                     min_freq_wordcloud = input$slide_value_freq[1], max_freq_wordcloud = input$slide_value_freq[2],
                     max_word_wordcloud = input$slide_value_word)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,switch(
        input$format,
        PDF = pdf_document(toc=TRUE), HTML = html_document(toc=TRUE), Word = word_document(toc=TRUE)
      ), output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)
