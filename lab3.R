# Load required packages
library(shiny)
library(ggplot2)

# Load your data from the current directory and store it in 'df'
df <- read.csv("./youtube_UTF_8.csv")

# User Interface
ui <- fluidPage(
  titlePanel("YouTube Statistics Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Choose x variable:", names(df)),
      selectInput("y_var", "Choose y variable:", c("None", names(df)))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$x_var == "Country" && input$y_var == "None") {
      ggplot(df, aes_string(x = input$x_var)) +
        geom_bar() +
        xlab(input$x_var)
    } else if (input$x_var == "Abbreviation" && input$y_var == "category") {
      ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
        geom_count() +
        xlab(input$x_var) +
        ylab(input$y_var)
    } else {
      if (input$y_var != "None") {
        ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point() +
          xlab(input$x_var) +
          ylab(input$y_var)
      } else {
        ggplot(df, aes_string(x = input$x_var)) +
          geom_histogram(binwidth = 30) +
          xlab(input$x_var)
      }
    }
  })
}

# Create Shiny App
shinyApp(ui, server)
