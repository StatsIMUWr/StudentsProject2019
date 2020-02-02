ui <- fluidPage(
  titlePanel("Credit risk"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("one variable",
                           selectInput(inputId = "parametr", 
                                       label = "Choose parametr to analize",
                                       choices = parameters),
                           sliderInput(inputId = "age_range",
                                       label = "Choose age:",
                                       min = 19,
                                       max = 75,
                                       value = c(19,75),
                                       step = 1),
                           selectInput(inputId = "color", 
                                       label = "Choose color",
                                       choices = vector_color),
                           selectInput(inputId = "position", 
                                       label = "Choose position",
                                       choices = vector_position),
                           selectInput(inputId = "font",
                                       label = "Choose font",
                                       choices = vector_fonts),
                           numericInput(inputId = "step",
                                        label = "Choose step in age range:",
                                        value = 5),
                           numericInput(inputId = "amount",
                                        label = "Choose step in amount range:",
                                        value = 2000),
                           downloadButton(outputId = "downloadPlot", label = "Download Plot")
                  ),
                  tabPanel("two variables",
                           selectInput(inputId = "parametr1", 
                                       label = "Choose parametr to analize",
                                       choices = parameters),
                           selectInput(inputId = "parametr2", 
                                       label = "Choose second parametr to analize",
                                       choices = parameters),
                           sliderInput(inputId = "age_range1",
                                       label = "Choose age:",
                                       min = 19,
                                       max = 75,
                                       value = c(19,75),
                                       step = 1),
                           selectInput(inputId = "color1", 
                                       label = "Choose color",
                                       choices = vector_color),
                           selectInput(inputId = "position1", 
                                       label = "Choose position",
                                       choices = vector_position),
                           selectInput(inputId = "font1",
                                       label = "Choose font",
                                       choices = vector_fonts),
                           numericInput(inputId = "step1",
                                        label = "Choose step in age range:",
                                        value = 5),
                           numericInput(inputId = "amount1",
                                        label = "Choose step in amount range:",
                                        value = 2000)
                  ),
                  tabPanel("tables",
                           selectInput(inputId = "len",
                                       label = "Choose length of table",
                                       choices = c(10, 20, 50, 100, 200, 500, 1000)),
                           checkboxGroupInput(inputId = "show_vars", 
                                              label = "Columns in credit data to show:",
                                              parameters, 
                                              selected = parameters)
                  )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("plots",
                 plotOutput("plot", height = "660px")
        ),
        tabPanel("tables", 
                 fluidRow(
                   column(width = 2,
                          selectInput(inputId = "purpose",
                                      label = "Choose purpose",
                                      choices = c("All", levels(credit_data_factor$Purpose)))
                   ),
                   column(width = 2,
                          selectInput(inputId = "credit",
                                      label = "Choose credit amount",
                                      choices = c("All", levels(credit_data_factor$Credit_amount)))
                   ),
                   column(width = 2,
                          selectInput(inputId = "saving",
                                      label = "Choose saving account",
                                      choices = c("All", levels(credit_data_factor$Savings_account_and_bonds)))
                   ),
                   column(width = 2,
                          selectInput(inputId = "age",
                                      label = "Choose age:",
                                      choices = c("All", levels(credit_data_factor$Age)))
                   ),
                   column(width = 2,
                          selectInput(inputId = "job",
                                      label = "Choose job:",
                                      choices = c("All", levels(credit_data_factor$Job)))
                   ),
                   column(width = 2,
                          selectInput(inputId = "decision",
                                      label = "Choose decision:",
                                      choices = c("All", levels(credit_data_factor$Decision)))
                   )
                 ),
                 tableOutput("table")
        )
      )
    )
  )
)



