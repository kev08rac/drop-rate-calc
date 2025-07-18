### Drop Rate Calculator
### Kevin Anderson

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('head.avif');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-attachment: fixed;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: #e0e0f0;
        padding-top: 20px;
        padding-bottom: 40px;
        position: relative;
      }
      body::before {
        content: '';
        position: fixed;
        top: 0; left: 0; right: 0; bottom: 0;
        background-color: rgba(30, 30, 47, 0.7);  /* overlay for readability */
        z-index: -1;
      }
      .container-custom {
        max-width: 900px;
        margin: auto;
      }
      .panel {
        background-color: #2a2a3dcc;  /* add some transparency */
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0px 0px 10px #00000055;
      }
      .btn-primary {
        background-color: #5c6bc0;
        border-color: #3949ab;
      }
      .btn-primary:hover {
        background-color: #7986cb;
      }
      #result {
        font-size: 20px;
        font-weight: bold;
        margin-top: 20px;
        background-color: #33334dcc;
        padding: 15px;
        border-radius: 8px;
        border: 1px solid #5c6bc0;
      }
    ")),
    
    # user can press enter key to update the text
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        if (e.which == 13) {
          $('#calc').click();
        }
      });
    "))
  ),
  
  div(class = "container-custom",
      titlePanel("Drop Chance Calculator"),
      
      fluidRow(
        column(
          width = 6,
          div(class = "panel",
              numericInput("probability", 
                           "% Drop Chance:", 
                           value = 5, 
                           min = 0, 
                           max = 100, 
                           step = 0.1),
              numericInput("attempts", 
                           "Number of attempts:", 
                           value = 20, 
                           min = 1, 
                           step = 1),
              actionButton("calc", "Calculate!", class = "btn-primary")
          )
        ),
        column(
          width = 6,
          div(class = "panel",
              HTML("<p><em>This calculator uses the formula: 
                <code>1 - (1 - p)^n</code>, where <code>p</code> is the drop rate 
                and <code>n</code> is the number of attempts.</em></p>"),
              textOutput("result")
          )
        )
      )
  ),
  
  div(style = "
      max-width: 900px; 
      margin: 20px auto 40px auto;  
      font-size: 24px; 
      text-align: center;",
      tags$a(href = "https://github.com/kev08rac/drop-rate-calc", 
             target = "_blank",
             "View on GitHub")
  )
)


server <- function(input, output) {
  result_text <- reactiveVal("")
  
  observeEvent(input$calc, {
    p <- input$probability / 100 # percent drop
    n <- input$attempts # number of attempts
    
    # check for invalid numbers like zero and negatives
    if (p <= 0 || n <= 0) {
      result_text("You need at least one attempt and a non-zero drop chance.")
      return()
    }
    
    # if user enters 100% drop rate
    if (p >= 1) {
      result_text(
        paste0("With a 100% drop chance, you're guaranteed a drop in one attempt - duh.")
      )
      return()
    }
    
    # if user enters a non-whole number for number of attempts
    if (n <= 0 || n != floor(n)) {
      result_text("Number of attempts must be a whole number greater than 0.")
      return()
    }
    
    prob_not_zero <- 1 - (1 - p)^n
    percent_result <- round(prob_not_zero * 100, 2)
    
    result_text(
      paste0("After ", n, " attempts with a ", input$probability, 
             "% drop chance per try, your odds of getting at least one drop are ", 
             percent_result, "%")
    )
  })
  
  output$result <- renderText({
    result_text()
  })
}

shinyApp(ui = ui, server = server)
