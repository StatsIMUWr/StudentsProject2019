server <- function(input, output) {
  output$contents <- renderTable({
    req(input$data)
    tryCatch({
      df <- read.csv(input$data$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  output$greeks <- renderDT({
    req(input$data)
    tryCatch({
      df <- read.csv(input$data$datapath)
    })
    computeGreeks(
      spot = input$spot,
      strike = input$strike,
      r = input$interest_rate,
      sigma = zwroty(
        data = df,
        start_date = as.character(input$our_start_date),
        end_date = as.character(input$our_end_date),
        T_ = 1
      )[[3]],
      0,
      1
    )
  })
  
  output$plot <- renderPlot({
    #req(input$data)
    tryCatch({
      df <- read.csv(input$data$datapath)
    })
    plot_data(
      data_path = input$data$datapath,
      n_sim = input$nsim,
      start_date = as.character(input$our_start_date),
      end_date = as.character(input$our_end_date),
      input_date = as.character(input$our_input_date)
    )
  })
  
  output$plot_future <- renderPlot({
    req(input$data)
    tryCatch({
      df <- read.csv(input$data$datapath)
    })
    plot_quantiles(
      data_path = input$data$datapath,
      n_sim = input$nsim,
      start_date = as.character(input$our_start_date),
      end_date = as.character(input$our_end_date),
      input_date = as.character(input$our_input_date)
    )
  })
}

