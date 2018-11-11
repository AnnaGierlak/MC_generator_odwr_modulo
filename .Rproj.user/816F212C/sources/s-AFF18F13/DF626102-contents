library(shiny)
library(shinythemes)
source("./generator.R")
 

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Generator odwrotnosci modulo"),
  
  wellPanel(
    
    fluidRow(
      column(2,
             numericInput('a', "Wybierz parametr a", value = 1)),
      column(2, 
             numericInput('b', "Wybierz parametr b", value = 1)),
      column(2, 
             numericInput('m', "Wybierz parametr m", value = 13)),
      column(2, 
             numericInput('n', "Podaj dlugosc ciagu", value = 20)),
      column(2, 
             numericInput('x0', "Podaj wartosc x_0", value = 0))
    ),
    fluidRow(
      column(2,
             actionButton('generate', 'Generuj!', class = "btn-info")),
      column(2, offset = 6,
             actionButton('clear', 'Wyczysc'))
    )
  ),
  mainPanel( 
    uiOutput('ciag'), 
    tags$br(), tags$br(),
    downloadButton('save', 'Zapisz ciag', value = T))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ciag <- eventReactive(input$generate, {
    if(numbers::isPrime(input$m)) {
      odp <- mod_inv_generator(input$x0, input$a, input$b, input$m, input$n)
      odp <- formatSeries(odp)
    }
    else odp <- "<font color='#ba464e' size = '8'>m nie jest liczbą pierwszą!</font>"
    HTML(odp)
  })
  
  output$ciag <- renderUI({
    ciag()
  })
  
  output$save <-downloadHandler(
    filename = function() {
      paste0('results_',input$x0,'_',input$a,'_',input$b,'_',input$m,'_',input$n,'.csv')
    },
    content = function(file) {
      write.csv(ciag(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$clear, {
    updateNumericInput(session, 'a', value = 1)
    updateNumericInput(session, 'b', value = 1)
    updateNumericInput(session, 'm', value = 13)
    updateNumericInput(session, 'n', value = 20)
    updateNumericInput(session, 'x0', value = 0)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

