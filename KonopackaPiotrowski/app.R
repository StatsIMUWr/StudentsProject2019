#load source file where functions are defined
library("shiny")
source("functions.R")

ui <- navbarPage("OlympicsVis",
#mainPanel-------------------------------------------------------------------------------------------                 
                 mainPanel(title = "Start"),
#tabPanel1-------------------------------------------------------------------------------------------
                 tabPanel(title = "Plot 1",
                        
                          sliderInput(inputId = "range",
                                    label ="Age range:",
                                    min = min(data_clear$Age, na.rm=T), max = max(data_clear$Age, na.rm=T),
                                    value = c(14,18)),
                        
                          plotOutput("plot1")),
#tabPanel2-------------------------------------------------------------------------------------------
                tabPanel(title = "Plot 2",
                         
                         selectInput(inputId = "select1",
                                     label ="Variable",
                                     choices = colnames(data_clear)),
                         
                         selectInput(inputId = "select2",
                                     label ="Color by",
                                     choices = choices_s2),
                         
                         plotOutput("plot2")),
#tabPanel3----------------------------------------------------------------------------------------------
                tabPanel(title = "Plot 3",
                         checkboxGroupInput(inputId = "tab4_sport", 
                                            h3("Sport:"), 
                                            choices = list("Choice 1" = 1), #TODO generate list of choices
                                            selected = 1),
                         
                         checkboxGroupInput(inputId = "tab4_sex", 
                                            h3("Sex:"), 
                                            choices = list("Male" = "Male", 
                                                           "Female" = "Female"),
                                            selected = list("Male", "Female")),
                         
                         checkboxGroupInput(inputId = "tab4_medal", 
                                            h3("Medals:"), 
                                            choices = list("Gold" = "Gold", 
                                                           "Silver" = "Silver"),
                                            selected = "Gold"),
                         
                         checkboxGroupInput(inputId = "tab4_country", 
                                            h3("Medals:"), 
                                            choices = list("Germany" = "Germany"),
                                            selected = "Germany"),
                         
                         sliderInput(inputId = "tab4_range",
                                     label ="Age range:",
                                     min = min(data_clear$Age, na.rm=T), max = max(data_clear$Age, na.rm=T),
                                     value = c(14,18)),
                         
                         selectInput(inputId = "tab4_select",
                                     label ="Color by",
                                     choices = list("Sex", "Medal", "Age_Group", "Country", "Sport")),
                         
                         plotOutput("plot3")),
               
#tabPanel4-------------------------------------------------------------------------------------------
                tabPanel(title = "Summary",
                         
                         sliderInput(inputId = "tab4_range",
                                     label ="Age range:",
                                     min = min(data_clear$Age, na.rm=T), max = max(data_clear$Age, na.rm=T),
                                     value = c(14,18)),
                         
                         textOutput("test1"))
)

#server-------------------------------------------------------------------------------------------------
server <- function(input, output){
  output$plot1 <- renderPlot({plot1(input$range[1], input$range[2])})
  output$plot2 <- renderPlot({plot2(input$select1, input$select2)})
  output$test1 <- renderText({summary(data_clear)})
  output$plot3 <- renderPlot({plot3(input$tab4_select, 
                                    input$tab4_range[1], input$tab4_range[2],
                                    input$tab4_color, 
                                    input$tab4_sport,
                                    input$tab4_country,
                                    input$tab4_sex)})
}

#run app--------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

