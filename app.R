# Load packages ----
library(shiny)
library(HDMT)
# stored input data ----
data("snp_input")
data("exercise_input")


# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("HDMT"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(""),
      selectInput("var", 
                  label = "Choose an example input",
                  choices = c("SNP input", "Exercis input"),
                  selected = "SNP input"),
      tags$hr(),
      fileInput("file1", "Or choose a CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      radioButtons("radio", "Option",
                   choices = list("exact" = 0, "approximation" = 1),selected = 0)
      ),
      
      
    
    mainPanel(
      h4(textOutput("help"),align="center"),
      br(),
      h4(textOutput("caption"),align = "center"),
      plotOutput("plot")
    )
  )
)


# Server logic ----
server <- function(input, output, session) {

  # values$upload_state is used to check where the data loaded from
  values <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$file1, {
    values$upload_state <- 'loadfile'
  })
  
  observeEvent(input$var, {
    values$upload_state <- 'loadexample'
  })
  
  
  data1 <- reactive({switch(input$var,
                                "SNP input" = snp_input,
                                "Exercis input" = exercise_input)})
  data2 <- reactive({req(input$file1)
    df <- as.matrix(read.csv(input$file1$datapath,header = F)) })
  
  output$help <- renderText("You can load input from the examples, or your file. An input is a matrix containing two columns of p-values for candidate mediators. See James Y. Dai, Janet L. Stanford, Michael LeBlanc. A multiple-testing procedure for high-dimensional mediation hypotheses, Journal of the American Statistical Association, 2020, DOI: 10.1080/01621459.2020.1765785.")
  output$caption <- renderText({
    if (values$upload_state =="loadexample")
    {
      "Corrected q-q plot, example"
    }else if (values$upload_state =="loadfile")
    {
      paste0("Corrected q-q plot, ",input$file1$name)
    }
  })  
  
  output$plot <- renderPlot({

    if (values$upload_state =="loadexample")
      Corrected_qqplot(data1(),input$radio)
    else if (values$upload_state =="loadfile")
      Corrected_qqplot(data2(),input$radio)
  })
}

# Run app ----
shinyApp(ui, server)