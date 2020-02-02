server <- function(input, output){
  
  
  output$plot <- renderPlot({
    if(input$tabset == "one variable") {
      credit_data_age <- age_range(input$age_range[1], input$age_range[2], input$step)
      credit_data_age <- part_credit_amount(credit_data_age, input$amount)
      plot_one_attribute(credit_data_age, input$parametr, input$position, input$color, input$font)
    } else {
      credit_data_age <- age_range(input$age_range1[1], input$age_range1[2], input$step1)
      credit_data_age <- part_credit_amount(credit_data_age, input$amount1)
      plot_two_attributes(credit_data_age, input$parametr1, input$parametr2, input$position1, input$color1, input$font1)
    }
  })
  
  
  output$table <- renderTable({
    data <- credit_data_factor[1:input$len, input$show_vars, drop = FALSE]
    if(input$purpose != "All") {
      data <- data[data[, "Purpose"] == input$purpose, ] #zmienilem na parameter
    }
    if (input$credit != "All") {
      data <- data[data[, "Credit_amount"] == input$credit, ]
    }
    if (input$age != "All") {
      data <- data[data[, "Age"] == input$age, ]
    }
    if (input$saving != "All") {
      data <- data[data[, "Savings_account_and_bonds"] == input$saving, ]
    }
    if (input$job != "All") {
      data <- data[data[, "Job"] == input$job, ]
    }
    if (input$decision != "All") {
      data <- data[data[, "Decision"] == input$decision, ]
    }
    data
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$parametr, "png", sep = ".")
    },
    content = function(filename) {
      credit_data_age <- age_range(input$age_range[1], input$age_range[2], input$step)
      credit_data_age <- part_credit_amount(credit_data_age, input$amount)
      ggsave(file = filename, plot_one_attribute(credit_data_age, input$parametr, input$position, input$color, input$font))
    }
  )
}
