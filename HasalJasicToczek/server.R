
server <- function(input, output) {
  
  output$one_variable <- renderPlot({
    #age_range(20, 40, 2)
    plot_one_attribute(input$parametr)
    
  })
  output$two_variables <- renderPlot({
    plot_two_attributes(input$parametr, input$parametr2)
  })
}