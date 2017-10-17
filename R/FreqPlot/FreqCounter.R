library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Frequency Counter"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 tags$br(),
                 checkboxInput('header', 'Header', F),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ',')
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Select Field",
             pageWithSidebar(
               headerPanel('Select Column'),
               sidebarPanel(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('col', 'Column Selection', "")
               ),
               mainPanel(
                 plotOutput('BarPlot')
               )
             )
    )
    
    
  )
)
)

#updateSelectInput requires session
server <- shinyServer(function(input, output, session) {
  
  data <- reactive({ 
    req(input$file1) #require that the input is available
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    #Get the count of the columns in the csv
    colCount <- ncol(df)
    
    # Update inputs and choices in total available columns
    updateSelectInput(session, inputId = 'col', label = 'Column Selection',
                      choices = as.list(1:colCount), selected = "")
      
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$BarPlot <- renderPlot({
    
    x <- data()
    see <- as.numeric(input$col)
    
    #To select the column given by the user
    occurences<-table(unlist(x[,as.numeric(input$col)]))
    occurences <- sort(occurences, decreasing = T)
    barplot(head(occurences),col = "light blue")
    
  })
  
})

shinyApp(ui, server)
