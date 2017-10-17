library(shiny)
library(datasets)
library(ggplot2)
library(reshape2)

ui <- shinyUI(fluidPage(
  titlePanel("IT Job Skills"),
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
                 checkboxInput('header', 'Header', TRUE),
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
    tabPanel("Compare Skills",
             pageWithSidebar(
               headerPanel('Comparision Plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "Year")
                 
               ),
               mainPanel(
                 plotOutput('FirstPlot')
               )
             )
    ),
    tabPanel("Visualization Years/Skills",
             pageWithSidebar(
               headerPanel('Row based Visualization'),
               sidebarPanel(
                 
                 selectInput('val', 'variable', "")

               ),
               mainPanel(
                 plotOutput('SecondPlot')
               )
             )
    )
    
  )
)
)

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  data <- reactive({ 
    req(input$file1)#  require that the input is available
    
    inFile <- input$file1 
                                 write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    
    # Update inputs 
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'val', label = 'variable', choices = as.list(df[,1, drop =FALSE]), selected = df[,1][1])
    
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$FirstPlot <- renderPlot({
    
    x1 <- data()
    
    ggplot(x1, aes(x=x1[,c(input$xcol)], y=x1[,c(input$ycol)]), shape=1)+geom_line(col = "red", group = 1) +geom_smooth(method='lm',formula=y~x)
    #plot(x, type = "o")
    #abline(lm(x1[,c(input$ycol)] ~ x1[,c(input$xcol)]), col="blue")
    #lines(predict(x))
    
  })

  output$SecondPlot <- renderPlot({
    x3 <- data() #bad variable naming

    pos <- which(x3 == input$val)
    x2 <- melt(x3[pos,2:ncol(x3)])
    
    ggplot(x2, aes(variable, value)) + geom_point()
    #barplot(x2, col = "blue")
    
    #some code for ref
    #ggplot(x2, aes(x=x2[,c(x2$variable)], y=x2[,c(x2$value)]))+geom_point()
    #abline(lm(x2(variable) ~ x2(value)), col="blue")
    #plot(x2[,c(variable, value)])
    
  })
})

shinyApp(ui, server)
