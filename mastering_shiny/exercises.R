### Chapter 1

## Exercise 1

library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    # req(input$name)
    paste0("Hello ", input$name)
  })
}

shinyApp(ui, server)

## Exercise 2

library(shiny)

ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  "then x times 5 is",
  textOutput("product")
)

server <- function(input, output, session) {
  x <- reactive({
    input$x
  })
  
  output$product <- renderText({
    x() * 5
  })
}

shinyApp(ui, server)

## Exercise 3

library(shiny)

ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 50, value = 5),
  "then, x times y is",
  textOutput("product")
)

server <- function(input, output, session) {
  x <- reactive({
    input$x
  })
  
  y <- reactive({
    input$y
  })
  
  output$product <- renderText({
    x() * y()
  })
}

shinyApp(ui, server)

## Exercise 4

library(shiny)

ui <- fluidPage(
  sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
  sliderInput("y", label = "and y is", min = 1, max = 50, value = 5),
  "then, (x * y) is", textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"),
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  x <- reactive({
    input$x
  })
  
  y <- reactive({
    input$y
  })
  
  product <- reactive({
    x() * y()
  })
  
  output$product <- renderText({
    product()
  })
  
  output$product_plus5 <- renderText({
    product() + 5
  })
  
  output$product_plus10 <- renderText({
    product() + 10
  })
}

shinyApp(ui, server)

## Excercise 5

library(shiny)
library(ggplot2)

datasets <- c("economics", "faithfuld", "seals")

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = datasets),
  verbatimTextOutput("summary"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$plot <- renderPlot({
    plot(dataset())
  }, res = 96)
}

shinyApp(ui, server)

### Chapter 2.2

## Exercise 1

library(shiny)

ui <- fluidPage(
  textInput("name", NULL, placeholder = "Your name")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

## Exercise 2

library(shiny)

ui <- fluidPage(
  sliderInput("deliver", "When should we deliver?", 
              min = as.Date("2020-09-16"), 
              max = as.Date("2020-09-23"), 
              value = as.Date("2020-09-17"))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

## Exercise 3

library(shiny)

ui <- fluidPage(
  sliderInput("number", "Choose a number.", 
              min = 0, max = 100, value = 0, step = 5,
              animate = TRUE)
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

## Exercise 4

library(shiny)

ui <- fluidPage(
  selectInput("state", "Where do you live?",
              choices = list("Name" = state.name,
                             "Abbreviation" = state.abb))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

### Chapter 2.2

## Exercise 1

library(shiny)

ui <- fluidPage(
  verbatimTextOutput("summary"),
  textOutput("text"),
  verbatimTextOutput("t.test"),
  verbatimTextOutput("lm")
)

server <- function(input, output, session) {
  output$summary <- renderPrint(summary(mtcars))
  output$text <- renderText("Good morning!")
  output$t.test <- renderPrint(t.test(1:5, 2:6))
  output$lm <- renderPrint(str(lm(mpg ~ wt, data = mtcars)))
}

shinyApp(ui, server)

## Exercise 2

library(shiny)

ui <- fluidPage(
  plotOutput("plot", height = "300px", width = "700px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:5), res = 96,
                            alt = "Scatterplot of five random numbers.")
}

shinyApp(ui, server)

## Exercise 3

ui <- fluidPage(
  dataTableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderDataTable(
    mtcars, options = list(pageLength = 5,
                           searching = FALSE,
                           ordering = FALSE,
                           lengthChange = FALSE))
}

shinyApp(ui, server)

## Exercise 4

library(reactable)

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderReactable(reactable(mtcars))
}

shinyApp(ui, server)

### Chapter 3

## Exercise 1

library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, session) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

shinyApp(ui, server1)

server2 <- function(input, output, session) {
  greeting <- reactive(paste0("Hello ", input$name))
  output$greeting <- renderText(greeting())
}

shinyApp(ui, server2)

server3 <- function(input, output, session) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

shinyApp(ui, server3)

## Exercise 3

library(shiny)

df <- mtcars

ui <- fluidPage(
  selectInput("var", NULL, choices = colnames(df)),
  verbatimTextOutput("range")
)

server <- function(input, output, session) {
  var <- reactive(df[[input$var]])
  col_range <- reactive(range(var(), na.rm = TRUE))
  output$range <- renderPrint(col_range())
}

shinyApp(ui, server)