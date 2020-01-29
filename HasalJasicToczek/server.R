server <- function(input, output){
  output$plot <- renderPlot({
    
    if(input$tabset == "one variable"){
      credit_data_age <- age_range(input$age_range[1],input$age_range[2], input$step)
      credit_data_age <- partition_credit_amount(credit_data_age, input$amount)
      plot_one_attribute(credit_data_age, input$parametr, input$position, input$color, input$font)
    } else{
      credit_data_age <- age_range(input$age_range1[1],input$age_range1[2], input$step1)
      credit_data_age <- partition_credit_amount(credit_data_age, input$amount1)
      plot_two_attributes(credit_data_age, input$parametr1, input$parametr2, input$position1, input$color1, input$font1)
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
