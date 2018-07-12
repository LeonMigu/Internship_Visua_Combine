#Server function that implements the back-end 

server <- function(input, output, session){
  
  
  d_num <- reactive({
    head(subset(d, rowname >= input$num_offset_data), input$num_word_data)
  })
  
  # use the key aesthetic/argument to help uniquely identify selected observations
  key_first <- row.names(d)
  
  
  
  #Data that will be given to the analysis part as main data. It depends on the way to choose it (select, numeric input, checkbox) 
  d_selected <- reactive({
    if(input$all == TRUE){
      d
    }
    else if(input$all == FALSE){
      if(input$num_check== TRUE){
        d_num()
      }
      else if(input$num_check == FALSE){
        SharedData$new(d, ~key_first)
      }
    }
  })
  d_sel_use <- reactive({
    if(input$all == TRUE){
      d
    }
    else if(input$all == FALSE){
      if(input$num_check==TRUE){
        d_num()
      }
      else if(input$num_check ==FALSE){
        d[d_selected()$selection(),]
      }
    }
    })
  
  #Select one checkbox when the other is selected
  observeEvent(input$all, {
    if(input$all == TRUE){
      updateCheckboxInput(session, "num_check", value = FALSE)
    }
  })
  
  observeEvent(input$num_check, {
    if(input$num_check == TRUE){
      updateCheckboxInput(session, "all", value = FALSE)
    }
  })

  # use the key aesthetic/argument to help uniquely identify selected observations
  key <- reactive({row.names(d_sel_use())})
  
  #length of the selected data with the numeric conditions
  l <- reactive({NROW(d_sel_use())})
  
  #Plotting the data in the overview of the preprocessing
    output$plot_data <- renderPlotly({
      s <- input$plot_rows_selected
      if(input$num_check==FALSE){
        if(!length(s)){
          plot_ly(d_selected(), x = ~rowname, y = rep(1, n), key = ~key_first, type = 'scatter',source = "select1", mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Data plot', xaxis = list(title ='Word'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))      
        }
        else if(length(s)){
          plot_ly(d, x = ~rowname, y = rep(1, n), key = ~key_first, type = 'scatter',source = "select2", mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Data plot', xaxis = list(title ='Word'), titlefont = 'arial', showlegend = FALSE)
        }
      }
      else if(input$num_check==TRUE){
        plot_ly(d_num(), x = ~rowname, y = rep(1, NROW(d_num())), key = ~row.names(d_num()), type = 'scatter',source = "select3", mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Data plot', xaxis = list(title ='Word'), titlefont = 'arial', showlegend = FALSE)
      }
      # d <- event_data("plotly_selected", source = "select1")
      # if(is.null(d)== FALSE){
      #   updateCheckboxInput(session, "all", value = FALSE)
      # }
      #switch(toString(is.null(d)), "FALSE" = {updateCheckboxInput(session, "all", value = FALSE)})
      # observeEvent(event_data("plotly_selected", source = "select2"), {
      #   updateCheckboxInput(session, "all", value = FALSE)
      # })
      # observeEvent(event_data("plotly_selected", source = "select3"), {
      #   updateCheckboxInput(session, "all", value = FALSE)
      # })
    })
    
  #Printing the number of words and the maximum number of words 
  output$num_data <- renderUI({
    tagList(renderText({"The number of words of the input file is"}),
      renderPrint({n}),
      renderText({"and the maximum number of words you can currently choose is"}),
      renderPrint({n-input$num_offset_data+1}),
      if(input$num_word_data > n-input$num_offset_data+1){
        renderText("You have chosen a number of words that is too high, it will just pick every word after the chosen offset")
      },
      renderPrint({event.data()})
    )
    })
  
  #Shared data between the plot and the datatable of the overview and the wordcloud for the analysis
  d_shared <- reactive({SharedData$new(d_sel_use(), ~key())})
  
  #Plotting the scatterplot with plotly
  output$plot_overview <- renderPlotly({
    #s matches the row selected by the user
    s <- input$plot_rows_selected
    #The if for the length doesn't seem very useful, because it works without it, however, I found it on the internet and there is maybe a reason I haven't found yet, so I prefer to let for now.
    #if there are no row selected yet, you can highlight the plot by selecting some points
    if(!length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d_shared(), x = ~rowname, y = ~freq, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))
      }
      else if(input$choice=='Random'){
        plot_ly(d_shared(), x = ~rowname, y = ~random, key = ~key(), type = 'scatter', mode='markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect', defaultValues = s, color = I('green'))
        
      }
    }
    #If there are row selected, you can't higlight the plot because it is already highlighted 
    else if(length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d_sel_use(), x = ~rowname, y = ~freq, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)
      }
      else if(input$choice=='Random'){
        plot_ly(d_sel_use(), x = ~rowname, y = ~random, key = ~key(), type = 'scatter', mode='markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)      
      }
    }
  })
  
  #Plotting the Data Table
  output$table_overview <- DT::renderDataTable({
    #Choosing the data selected in the plot. It is done by crosstalk, see CRAN R Crosstalk SharedData for more details
    dsel <- d_sel_use()[d_shared()$selection(),]
    #Creating the data table with the initial data
    dt <-DT::datatable(d_sel_use(),options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),pageLength = 5, lengthMenu = c(5, 10, 15, 20)),class = 'display')
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
  d_real_shared <- reactive({d_sel_use()[d_shared()$selection(),]})
  #Choosing the data to give to the wordcloud, depending on which data is taken from plotly
  filter_d <- reactive({
    #Changing the data in order to match what the wordcloud takes as an input
    d_prime_reac <- reactive({data.frame(d_real_shared()$word, d_real_shared()$freq)})
    head(subset(d_prime_reac(), d_real_shared...freq <= input$slide_value_freq[2] & d_real_shared...freq >= 
                  input$slide_value_freq[1]), 
         input$slide_value_word)
  })
  
  #Updating the value of the maximum of the slider input for the number of words and for the frequency
  m_act <- reactive({max(d_sel_use()$freq)})
  n_act <- reactive({NROW(d_sel_use())})
  observeEvent(d_sel_use(),{updateSliderInput(session,inputId = "slide_value_freq", label = "Filter the frequency", min = 1, max = m_act(), value = c(1,m_act()), step = 1)})
  observeEvent(d_sel_use(),{updateSliderInput(session,inputId = "slide_value_word", label = "Choose the maximum number of words", min = 1, max = n_act(), value = n_act(), step = 1)})
  
  
  #Creating the wordcloud and making it reactive to change in the input values
  output$wordcloud  <- renderWordcloud2(wordcloud2(data = filter_d(),
                                                   shape = 'star', size = 0.8, shuffle =FALSE))
  output$test <- renderPrint({
    filter_d()
    m_act()
  })
  
  progress <- reactive({
    capture.output(report, file=NULL)
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
      
      withProgress(message ="Generating report", detail =  "it might takes a little while", expr = {tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data_complete = d, data_selected_plot = d[d_shared()$selection(),], 
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
      },
      min = 1,
      value = 1
      )
    }
  )
  
  output$warningMenu <- renderMenu({
    # Code to generate each of the messageItems
    war <- list(notificationItem(text = "Everything seems to work", icon("users")))
    l_wc <- reactive({length(filter_d()$d_real_shared...word)})
    if(l_wc()==1){
      list.append(war, notificationItem(
        text = "Only one word is selected on the wordcloud and none appears",
        icon = icon("exclamation-triangle"),
        status = "warning"
      )
      )
    }
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = war)
  })
  
}
return(server)