server <- function(input, output) {
  
  output$one_variable <- renderPlot({
   draw_plot(input$parametr)
  })
  
}