library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(formattable)
library(shinyWidgets)

df <- read_xlsx('TestCase.xlsx')

# Define UI for application
ui <- navbarPage(
    "Application",
    tabPanel("General",
             setBackgroundColor(
               color = "efedf5",
               gradient = c('linear', 'radial'),
               direction = c('bottom', 'top', 'right', 'left'),
               shinydashboard = False
             ),
             sidebarLayout( #All outputs are reactive.
                 sidebarPanel(uiOutput("rowsSelected"), 
                              uiOutput("sum"), 
                              uiOutput("percentage"),
                              uiOutput("workinghour"),
                              tableOutput("result")
                              ),
                 mainPanel(
                   div(DT::dataTableOutput("datatable"), style = 'font-size: 90%; width: 90%)',
                       splitLayout(style = 'border: 1px solid silver:', cellWidths = c(400, 700),
                                   plotOutput("timePlot"),
                                   plotOutput('monthPlot')
                                   )
                   ) 
                   )
             )
    )
)
    
# Define server logic required
server <- function(input, output) {
    dataFile <- sheet1
    
    filtered_table <- reactive({
        req(input$datatable_rows_selected)
        dataFile[input$datatable_rows_selected, ]  
    })
    

    output$datatable <- DT::renderDataTable({
        datatable(dataFile, 
                  filter = 'top', 
                   options = list(pageLength = 10,
                                  autoWidth = T)
                  )
        })
    

    output$timePlot <- renderPlot({
        ggplot(filtered_table(), aes(hour)) +
            geom_bar() 
      
    })
    
    output$monthPlot <- renderPlot({
      ggplot(filtered_table(), aes(x=factor(Month), levels=c('Nov-21', 'Dec-21', 'Jan-22', 'Feb-22'))) +
        geom_bar(fill = 'ffedao0') 
      
    })
   
    output$rowsSelected <- renderUI({
        numericInput("rowsSelected", "Rows selected",
                     value = length(input$datatable_rows_selected),
                     min = 0)
    })
    
    output$percentage <- renderUI({
        numericInput("percentage", "Percentage(%) of TP vs FP",
                     value = (length(input$datatable_rows_selected) / length(input$datatable_rows_all))*100,
                     min = 0)
    })
    
    
    
    output$sum <- renderUI({
        numericInput("sum", "Total Cost",
                     value =sum(filtered_table()$cost),
                     min = 0)
    })
    
    
    output$result <- renderTable({
        #Uses the reative table so that the 
        x <- filtered_table() %>% count(month_yr, sort = TRUE)
        
        as.data.frame(x)
    })
    
    
    output$workinghour <- renderTable({
      x <- filtered_table() %>% count(Alert_Generated, sort = TRUE)
      
      as.data.frame(x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
