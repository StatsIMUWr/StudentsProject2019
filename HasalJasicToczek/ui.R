ui <- fluidPage(
  column(4, titlePanel("Credit risk"), 
         selectInput(inputId = "parametr", 
         label = "Choose parametr to analize",
         choices = colnames(credit_data)),
         selectInput(inputId = "parametr2", 
                     label = "Choose second parametr to analize",
                     choices = colnames(credit_data)),
           sliderInput(inputId = "range",
                       label = "Choose age:",
             min = 18,
             max = 80,
             value = c(18,80),
             step = 1)),
                                                  
  
  column(8,tabsetPanel(
    tabPanel("1 variable", plotOutput("one_variable")),
    tabPanel("2 variables", plotOutput("two_variables"))
  )))