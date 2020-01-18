#load source file where functions are defined
library("shiny")
source("functions.R")

ui <- navbarPage("Navbar!",
                tabPanel("Plot 1",
                        sliderInput(inputId = "range",
                                label ="Age range:",
                                min = min(data$Age, na.rm=T), max = max(data$Age, na.rm=T),
                                value = c(14,18)),
                        plotOutput("plot1"))
                )

#server
server <- function(input, output){
  output$plot1 <- renderPlot({plot1(input$range[1], input$range[2])})
}

#run app
shinyApp(ui = ui, server = server)

