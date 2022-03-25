library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)

sheet1 <- read_xlsx('TestCase.xlsx',sheet = 'Sheet1')

sheet1$Month_Yr <- format(as.Date(sheet1$Date), "%Y-%m")

sheet1$Date <- format(sheet1$Date, "%Y-%m-%d") #Created this to format the dates

sheet1$Time <- format(sheet1$Time, "%H:%M") #Created this to format the time.

sheet1$Time2 <- sheet1$Time

sheet1 <- sheet1 %>% separate(Time2, c("Hour", "Minutes"), sep = ":")

sheet1 <- sheet1 %>% 
    clean_names()

#sheet1$hour <- format(sheet1$time, "%H:00")

# Define UI for application
ui <- navbarPage(
    "Application",
    tabPanel("General",
             sidebarLayout( #All outputs are reactive.
                 sidebarPanel(uiOutput("rowsSelected"), #Display for the number of rows selected
                              uiOutput("sum"), #Display for the sum of cases (cost) for rows selected
                              uiOutput("percentage"), #Display for the % of TTP vs FP (number of rows selected/ total number of rows) *100
                              tableOutput("result") #Display for the table for the number of cases per year-month
                              ),
                 mainPanel(DT::dataTableOutput("datatable"), #Display for the data table
                           #Using DT:: above forces R to use the dataTable funtions from the DT package. This reduces the chances of errors if the 
                           #application is scaled and a package with the same function name is used in this code
                           plotOutput("timePlot")) #Plot display
             )
    )
)
    
# Define server logic required
server <- function(input, output) {
    dataFile <- sheet1
    
    #The functions below collects the selected rows from the UI and also subsets them from the data file.
    #This is set in a reactive block so that it's updated and everything on the page is updated accordingly 
    #when there is a selection.
    filtered_table <- reactive({
        req(input$datatable_rows_selected)
        dataFile[input$datatable_rows_selected, ]  
    })
    
    #The below is the function that renders the data table.
    #Added the filter top so you have the ability to select a particular time or patient id.
    output$datatable <- DT::renderDataTable({
        datatable(dataFile, 
                  filter = 'top', 
                   options = list(pageLength = 10,
                                  autoWidth = T)
                  )
        })
    
    #Function for creating the frequency plot for the time.
    output$timePlot <- renderPlot({
        ggplot(filtered_table(), aes(hour)) +
            geom_bar() 
        #If you want a unique title and labels for the x and y, update the ggplot line
    })
    
    #The next 3 functions are the functions for the number of rows selected, percentage and total cost.
    #All the functions are reactive and update when a row is selected. 
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
    
    #Function for rendering the table output.
    
    #It counts the number of cases in the selected rows for each year-month combination.  
    output$result <- renderTable({
        #Uses the reative table so that the 
        x <- filtered_table() %>% count(month_yr, sort = TRUE)
        
        as.data.frame(x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
