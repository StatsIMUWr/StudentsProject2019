server <- function(input, output) {
   
  output$one_variable <- renderPlot({
    credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
    plot_one_attribute(credit_data_age, input$parametr)
  })
  output$two_variables <- renderPlot({
    credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
    plot_two_attributes(credit_data_age, input$parametr, input$parametr2)
  })
}
