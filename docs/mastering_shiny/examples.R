### Chapter 1

library(shiny)

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })

  output$summary <- renderPrint({
    summary(dataset())
  })

  output$table <- renderTable({
    dataset()
  })
}

shinyApp(ui, server)

### Chapter 2

## Section 2.1

library(shiny)
animals <-  c("dog", "cat", "mouse", "bird", "other", "I hate animals")

ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10,20), min = 0, max = 100),
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?"),
  ui <- fluidPage(
    selectInput("state", "What's your favourite state?", 
                datasets::state.name),
    radioButtons("animal", "What's your favourite animal?", animals)
  ),
  radioButtons("rb", "Choose one:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry", "happy", "sad")
  ),
  selectInput(
    "state", "What's your favourite state?", state.name,
    multiple = TRUE
  ),
  ui <- fluidPage(
    checkboxGroupInput("animal", "What animals do you like?", animals)
  ),
  checkboxInput("cleanup", "Clean up?", value = TRUE),
  checkboxInput("shutdown", "Shutdown?"),
  fileInput("upload", NULL),
  actionButton("click", "Click me!"),
  actionButton("drink", "Drink me!", icon = icon("cocktail")),
  fluidRow(
    actionButton("click", "Click me!", class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success")
  ),
  fluidRow(
    actionButton("eat", "Eat me!", class = "btn-block")
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)

## Section 2.3

library(shiny)

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code"),
  tableOutput("static"),
  dataTableOutput("dynamic"),
  plotOutput("plot", width = "400px")
)

server <- function(input, output, session) {
  output$text <- renderText({ 
    "Hello friend!" 
  })
  output$code <- renderPrint({ 
    summary(1:10) 
  })
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
  output$plot <- renderPlot(plot(1:5), res = 96)
}

shinyApp(ui, server)

### Chapter 3 

library(ggplot2)

freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  
  ggplot(df, aes(x, color = g)) +
    geom_freqpoly(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

x1 <- rnorm(100, mean = 0, sd = 0.5)
x2 <- rnorm(200, mean = 0.15, sd = 0.9)

freqpoly(x1, x2)
cat(t_test(x1, x2))

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(4,
           "Distribution 1",
           numericInput("n1", label = "n", value = 1000, min = 1),
           numericInput("mean1", label = "B5", value = 0, step = 0.1),
           numericInput("sd1", label = "O", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Distribution 2",
           numericInput("n2", label = "n", value = 1000, min = 1),
           numericInput("mean2", label = "B5", value = 0, step = 0.1),
           numericInput("sd2", label = "O", value = 0.5, min = 0.1, step = 0.1)
    ),
    column(4,
           "Frequency Polygon",
           numericInput("binwidth", label = "Bin width", value = 0.1, min = 0.1, step = 0.1),
           sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9,
           plotOutput("hist")
    ),
    column(3,
           verbatimTextOutput("ttest")
    )
  )
)

server <- function(input, output, session) {
  
  x1 <- reactive(rnorm(n = input$n1, mean = input$mean1, sd = input$sd1))
  x2 <- reactive(rnorm(n = input$n2, mean = input$mean2, sd = input$sd2))
  
  
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
}

shinyApp(ui, server)

library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9,
           plotOutput("hist")
    )
  )
)

server <- function(input, output, session) {
  #timer <- reactiveTimer(500)
  x1 <- reactive({
    #timer()
    rpois(n = input$n, lambda = input$lambda1)
    })
  x2 <- reactive({
    #timer()
    rpois(n = input$n, lambda = input$lambda2)
    })
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0,40))
  }, res = 96)
}

server <- function(input, output, session) {
  x1 <- eventReactive(input$simulate, {
    rpois(n = input$n, lambda = input$lambda1)
  })
  x2 <- eventReactive(input$simulate, {
    rpois(n = input$n, lambda = input$lambda2)
  })
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0,40))
  }, res = 96)
}

shinyApp(ui, server)

### Chapter 4

library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss")
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name))
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

selected <- injuries %>% filter(prod_code == 649)
selected %>% count(location, wt = weight, sort = TRUE)
selected %>% count(body_part, wt = weight, sort = TRUE)

summary <- selected %>% 
  count(age, sex, wt = weight)
summary %>%
  ggplot(aes(age, n, color = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4)
summary %>%
  ggplot(aes(age, rate, color = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")

selected %>%
  sample_n(10) %>%
  pull(narrative)

prod_codes <- setNames(products$prod_code, products$title)

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(
    column(6, selectInput("code", "Product", choices = prod_codes))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  output$diag <- renderTable(
    #selected() %>% count(diag, wt = weight, sort = TRUE) 
    selected() %>% count_top(diag), width = "100%"
  )
  output$body_part <- renderTable(
    #selected() %>% count(body_part, wt = weight, sort = TRUE)
    selected() %>% count_top(body_part), width = "100%"
  )
  output$location <- renderTable(
    #selected() %>% count(location, wt = weight, sort = TRUE)
    selected() %>% count_top(location), width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}

shinyApp(ui, server)
