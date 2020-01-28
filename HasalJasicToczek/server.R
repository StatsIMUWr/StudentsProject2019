server <- function(input, output){
  output$plot <- renderPlot({
    credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
    credit_data_age <- partition_credit_amount(credit_data_age, input$amount)
    if(input$tabset == "one variable"){
      plot_one_attribute(credit_data_age, input$parametr, input$position, input$color, input$font)
    } else{
      plot_two_attributes(credit_data_age, input$parametr, input$parametr2, input$position, input$color, input$font)
    }
  })
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste(input$parametr, "png", sep = ".")
    },
    content = function(filename){
      credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
      credit_data_age <- partition_credit_amount(credit_data_age, input$amount)
      ggsave(file =filename,plot_one_attribute(credit_data_age, input$parametr, input$position, input$color, input$font))
    }
  )
  
}
