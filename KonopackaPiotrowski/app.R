#load source file where functions are defined
library("shiny")
source("functions.R")

#ui-----------------------------------------------------------------------------------------------
#define choices:
tab2_choices <- c("Sex", "Decade", "Age_Group", "City", "Sport", "Medal", "Season")
allowed_color_by <- c("Medal", "Sex")

ui <- navbarPage("OlympicsVis",
#mainPanel-------------------------------------------------------------------------------------------                 
                 mainPanel(title = "Start"),
#tabPanel1-------------------------------------------------------------------------------------------
                 tabPanel(title = "Plot 1",
                        
                          sliderInput(inputId = "tab1_range",
                                    label ="Age range:",
                                    min = min(data_clear$Age, na.rm=T), max = max(data_clear$Age, na.rm=T),
                                    value = c(14,18),
                                    step = 1),
                        
                          plotOutput("plot1")),
#tabPanel2-------------------------------------------------------------------------------------------
                tabPanel(title = "Plot 2",
                         
                         selectInput(inputId = "tab2_select1",
                                     label ="Variable",
                                     choices = tab2_choices),
                         
                         selectInput(inputId = "tab2_select2",
                                     label ="Color by",
                                     choices = allowed_color_by),
                         
                         plotOutput("plot2")),
#tabPanel3----------------------------------------------------------------------------------------------
                tabPanel(title = "Plot 3",
                         checkboxGroupInput(inputId = "tab3_sport", 
                                            h3("Sport:"), 
                                            choices = list("Choice 1" = 1), 
                                            selected = 1),
                         
                         checkboxGroupInput(inputId = "tab3_sex", 
                                            h3("Sex:"), 
                                            choices = list("Male" = "Male", 
                                                           "Female" = "Female"),
                                            selected = list("Male", "Female")),
                         
                         checkboxGroupInput(inputId = "tab3_medal", 
                                            h3("Medals:"), 
                                            choices = list("Gold" = "Gold", 
                                                           "Silver" = "Silver"),
                                            selected = "Gold"),
                         
                         checkboxGroupInput(inputId = "tab3_country", 
                                            h3("Medals:"), 
                                            choices = list("Germany" = "Germany"),
                                            selected = "Germany"),
                         
                         sliderInput(inputId = "tab3_range",
                                     label ="Age range:",
                                     min = min(data_clear$Age, na.rm=T), max = max(data_clear$Age, na.rm=T),
                                     value = c(14,18)),
                         
                         selectInput(inputId = "tab3_select",
                                     label ="Color by",
                                     choices = list("Sex", "Medal", "Age_Group", "Country", "Sport")),
                         
                         plotOutput("plot3")),
               
#tabPanel4-------------------------------------------------------------------------------------------
                tabPanel(title = "Plot 4",
                         
                         sliderInput(inputId = "tab4_range",
                                     label ="Age range:",
                                     min = min(data_clear$Age, na.rm=T), max = max(data_clear$Age, na.rm=T),
                                     value = c(14,18),
                                     step = 1),
                         
                         plotOutput("plot4"))
)

#server-------------------------------------------------------------------------------------------------
server <- function(input, output){
  output$plot1 <- renderPlot({plot1(data_clear, input$tab1_range[1], input$tab1_range[2])})
  output$plot2 <- renderPlot({plot2(data_clear, input$tab2_select1, input$tab2_select2)})
  output$plot3 <- renderPlot({plot3(data_clear, input$tab3_select, 
                                    input$tab3_range[1], input$tab4_range[2],
                                    input$tab3_color, 
                                    input$tab3_sport,
                                    input$tab3_country,
                                    input$tab3_sex)})
  output$plot4 <- renderPlot({plot4(data_clear, input$tab4_range[1], input$tab4_range[2])})
}

#run app--------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

