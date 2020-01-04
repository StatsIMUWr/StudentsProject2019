#load source file where functions are defined
library("shiny")
source("functions.R")

#load data without duplicated rows TODO fix data
data <- load_as_dataframe("data/athlete_events.csv")

#ui
ui <- fluidPage("test",
                sliderInput(inputId = "range",
                            label ="Age range:",
                            min = 11, max = 100,
                            value = c(14,18)),
                plotOutput("plot1"))

#server
server <- function(input, output){
  output$plot1 <- renderPlot({plot1(input$range[1], input$range[2])})
}

#run app
shinyApp(ui = ui, server = server)

