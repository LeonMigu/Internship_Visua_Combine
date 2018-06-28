library(shiny)
library(DT)
library(plotly)
library(wordcloud2)
library(tm)
library(crosstalk)
library(rmarkdown)

#Data (it will be the preprocessing of Colette)

# Read the text file from internet
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#Creating the data frame that will be used by the code

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
r <- abs(rnorm(length(v)))
w <- names(v)
#Changing the data structure in order that the id is a number and not a word, if not it doesn't work
names(v) <- seq(1, length(v))
#Implementing rownames in order to access them later. It will be useful to make the key
wr <- seq(1, length(v))
#Creating the dataframe 
d <- data.frame(rowname = wr, word = w, freq=v, random = r)
head(d, 10)

#Find the maximum frequency and the number of words to implement the sliderinputs
m <- max(d$freq)
n <- NROW(d)

# use the key aesthetic/argument to help uniquely identify selected observations
key <- row.names(d)

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
    if(!length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d_shared, x = ~rowname, y = ~freq, key = ~key, type = 'scatter', mode='lines',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))
      }
      else if(input$choice=='Random'){
        plot_ly(d_shared, x = ~rowname, y = ~random, key = ~key, type = 'scatter', mode='markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect', defaultValues = s, color = I('green'))
        
      }
    }
    else if(length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d, x = ~rowname, y = ~freq, key = ~key, type = 'scatter', mode='lines',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)
      }
      else if(input$choice=='Random'){
        plot_ly(d, x = ~rowname, y = ~random, key = ~key, type = 'scatter', mode='markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)      
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
    head(subset(d_prime_reac(), d_real_shared...freq <= (input$slide_value_freq[2]) & d_real_shared...freq >= 
                                          (input$slide_value_freq[1])), 
                                 input$slide_value_word)
  })
 
  #Creating the wordcloud and making it reactive to change in the input values
  output$wordcloud  <- renderWordcloud2(wordcloud2(data = filter_d(),
                                                   shape = 'star', size = 0.8, shuffle =FALSE))
  output$test <- renderPrint({
    input$selected_word
  })
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my_report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}

shinyApp(ui, server)
