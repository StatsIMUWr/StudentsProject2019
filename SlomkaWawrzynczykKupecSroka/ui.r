ui <- fluidPage(fluidRow(
  # App title ----
  titlePanel("Wczytywanie danych"),
  column(
    width = 3,
    fileInput(
      "data",
      "Wybór pliku CSV",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    tags$hr(),
    tags$hr(),
    radioButtons(
      "disp",
      "Wyświetl",
      choices = c(Head = "head",
                  All = "all"),
      selected = "head"
    )
    
  ),
  column(width = 9,
         tabsetPanel(
           tabPanel("Tabela"),
           tabPanel("Wykresy"),
           tabPanel("Wskaźniki greckie")
         ))
),
fluidRow(titlePanel("Podgląd danych"),
         tableOutput("contents")))

ui <- navbarPage(
  "Projekt - dane giełdowe",
  tabPanel(
    "Wczytywanie danych",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "data",
          "Wybór pliku CSV",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        tags$hr(),
        tags$hr(),
        radioButtons(
          "disp",
          "Wyświetl",
          choices = c(Head = "head",
                      All = "all"),
          selected = "head"
        ),
        #poniżej inputy dla dat
        
        dateInput(
          "our_start_date",
          "Proszę podać datę początku wykresu",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "pl",
          width = NULL
        ),
        
        dateInput(
          "our_end_date",
          "Proszę podać datę początku symulacji",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "pl",
          width = NULL
        ),
        
        dateInput(
          "our_input_date",
          "Proszę podać datę końca symulacji",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "pl",
          width = NULL
        )
        
      ),
      mainPanel(tableOutput("contents"))
    )
  ),
  tabPanel("Wykres",
           sidebarLayout(
             sidebarPanel(
               numericInput(
                 "nsim",
                 "Ilość symulacji:",
                 min = 0,
                 value = 100
               )
             ),
             mainPanel(plotOutput("plot"), plotOutput("plot_future"))
           )),
  tabPanel("Wskaźniki greckie",
           sidebarLayout(
             sidebarPanel(
               tags$hr(),
               sliderInput(
                 "interest_rate",
                 "Stopa procentowa:",
                 min = 0,
                 max = 0.1,
                 value = 0.05
               ),
               numericInput(
                 "strike",
                 "Wartość strike:",
                 value = 0
               ),
               numericInput(
                 "spot",
                 "Wartość spot:",
                 value = 0
               ),
               tags$hr()
             ),
             mainPanel(DTOutput("greeks"))
           ))
  
)
