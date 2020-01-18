#load source file where functions are defined
library("shiny")
source("functions.R")

#TODO cant be the same as select1 (possible choices: select2), add Medal and Sport?
choices_s2 <- c("Sex") 

ui <- navbarPage("OlympicsVis",
                 #mainPanel
                 mainPanel("Start"),
                 #tabPanel1
                 tabPanel("Plot 1",
                        sliderInput(inputId = "range",
                                label ="Age range:",
                                min = min(data$Age, na.rm=T), max = max(data$Age, na.rm=T),
                                value = c(14,18)),
                        plotOutput("plot1")),
                #tabPanel2
                tabPanel("Plot 2",
                         selectInput(inputId = "select1",
                                     label ="Variable",
                                     choices = colnames(data)),
                         selectInput(inputId = "select2",
                                     label ="Color by",
                                     choices = choices_s2),
                         plotOutput("plot2")),
                
                #tabPanel3
                tabPanel("Summary",
                         selectInput(inputId = "select2",
                                     label ="Test",
                                     choices = colnames(data)),
                         textOutput("test1"))
)

#server
server <- function(input, output){
  output$plot1 <- renderPlot({plot1(input$range[1], input$range[2])})
  output$plot2 <- renderPlot({plot2(input$select1, input$select2)})
  output$test1 <- renderText({summary(data)})
}

#run app
shinyApp(ui = ui, server = server)

