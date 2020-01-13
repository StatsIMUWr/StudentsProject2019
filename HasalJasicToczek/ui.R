ui <- fluidPage(
  column(4, titlePanel("Credit risk"), selectInput(inputId = "parametr", 
                                                         label = "Choose parametr to analize",
                                                         choices = colnames(data))),
  
  column(8,tabsetPanel(
    tabPanel("1 variable", plotOutput("one_variable")),
    tabPanel("2 variable", plotOutput("2_variable"))
    )))
