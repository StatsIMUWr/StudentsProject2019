server <- function(input, output) {
  output$one_variable <- renderPlot({
    credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
    credit_data_age <- partition_credit_amount(credit_data_age, input$amount)
    plot_one_attribute(credit_data_age, input$parametr, input$position, input$color, input$font)
    
  })
  output$two_variables <- renderPlot({
    credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
    credit_data_age <- partition_credit_amount(credit_data_age, input$amount)
    plot_two_attributes(credit_data_age, input$parametr, input$parametr2, input$position, input$color, input$font)
  })
}