library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)

df <- read_xlsx('TestCase.xlsx',sheet = 'Sheet1')

df$Month_Yr <- format(as.Date(sheet1$Date), "%Y-%m")

df$Date <- format(sheet1$Date, "%Y-%m-%d") #Created this to format the dates

df$Time <- format(sheet1$Time, "%H:%M") #Created this to format the time.

df <- df %>% separate(Time2, c("Hour", "Minutes"), sep = ":")


# Define UI for application
ui <- navbarPage(
    "Application",
    tabPanel("General",
             sidebarLayout( #All outputs are reactive.
                 sidebarPanel(uiOutput("rowsSelected"), 
                              uiOutput("sum"), 
                              uiOutput("percentage"), 
                              tableOutput("result")
                              ),
                 mainPanel(DT::dataTableOutput("datatable"), 
                           
                           plotOutput("timePlot")) 
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
}

# Run the application 
shinyApp(ui = ui, server = server)
