messageData <- data.frame("from" = c("Alexis", "Bob"), "message" = c("Bonjour", "Au revoir"))

 ui <- dashboardPage(dashboardHeader(title = "Header",dropdownMenuOutput("test") ),

 dashboardSidebar(
   sidebarMenu(
     menuItem("Data", tabName = "data", icon = shiny::icon("th"))
 )),
                     dashboardBody(
                       tabItems(tabItem(
                         tabName = "data",
                         h2("Plot overview"),
                         fluidRow(
                           column(width = 4,
                                  box(
                                    selectInput(inputId = 'choice', label = 'Choose a metric',
                                                choice = c('Frequency', 'Random'))),
                                  
                                  box(
                                    uiOutput("test"))
                           )
                     ) ))))

# ui <- dashboardPage(dashboardHeader(title = "Header"),
#
#                     dashboardSidebar(
#                       sidebarMenu(
#                         menuItem("Data", tabName = "data", icon = shiny::icon("th"))
#                       )),
#                     dashboardBody(
#
#                       tabItems(tabItem(
#                         tabName = "data",
#                         h2("Plot overview"),
#                         fluidRow(
#                           column(width = 4,
#                                  box(
#                                    selectInput(inputId = 'choice', label = 'Choose a metric',
#                                                choice = c('Frequency', 'Random'))),
#                                  box(
#                                    conditionalPanel("input.choice == 'Random'",
#                                                     plotlyOutput("plot"))
#                                  )
#                           )
#                         ) ))))

server <- function(input, output){
   # output$messageMenu <- renderMenu({
   #   # Code to generate each of the messageItems here, in a list. This assumes
   #   # that messageData is a data frame with two columns, 'from' and 'message'.
   #   msgs <- apply(messageData, 1, function(row) {
   #     messageItem(from = row[["from"]], message = row[["message"]])
   #   })
   #   if(input$choice == "Random"){
   #     list.append(msgs, data.frame("from" = c("Alice"), "message" = c("You've chosen Random!!")))
   #   }
   #   # This is equivalent to calling:
   #   #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
   #   dropdownMenu(type = "messages", .list = msgs)
   # })
#    output$test <- renderUI(
#      renderPrint({ })
#    )

  output$test <- observeEvent(input$choice, {renderMenu({
    if(input$choice == 'Random'){
      dropdownMenu(data.frame("from" = c("Alice"), "message" = c("You've chosen Random!!")), type="messages")
    }
  })})
#   output$plot <- renderPlotly({plot_ly(iris, x = ~Petal.Length, y = ~Sepal.Length)} )
 }

shinyApp(ui, server)