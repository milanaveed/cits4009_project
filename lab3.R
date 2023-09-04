# Load required packages
library(shiny)
library(ggplot2)

# Load your data from the current directory and store it in 'df'
df <- read.csv("./youtube_UTF_8.csv")

# Define x and y variable lists
 x_variables <- c("None","Country",)

# User Interface
ui <- fluidPage(
  titlePanel("YouTube Statistics Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Choose x variable:", names(df)),
      selectInput("y_var", "Choose y variable:", c("None", "density")),
      conditionalPanel(
        condition = "input.x_var == 'lowest_monthly_earnings' && input.y_var == 'density' || 
                     input.x_var == 'highest_monthly_earnings' && input.y_var == 'density'",
        sliderInput("binwidth", "Binwidth:", min = 10, max = 100, value = 30)
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Create a list of names of available plots
  plots <- c("Number of Channels by Country",
             "Category VS Country (Channels)", 
             "Category VS Created Year (Channels)",
             "Category VS Country (Subscribers)",
             "Number of Channels by Category",
             "Number of Uploads VS Subscribers",
             "Number of Uploads VS Video Views",
             "Distribution of Lowest Monthly Earnings",
             "Distribution of Highest Monthly Earnings",
             "Highest Yearly Earnings by Category (by Median)",
             "Highest Yearly Earnings by Country (Top 20 by Median)",
             "Highest Yearly Earnings in Space",
             "None"
             )
  
  # Define a function to check the plot type
  determine_type <- function() {
    if (input$x_var == "Country" && input$y_var == "Number") {
      return(plots[1])
    } else if (input$x_var == "Country" &&
               input$y_var == "Category") {
      return(plots[2])
    } else if (input$x_var == "Created year" &&
               input$y_var == "Category" &&
               input$dimension == "Number of Channels") {
      return(plots[3])
    } else if (input$x_var == "Created year" &&
               input$y_var == "Category" &&
               input$dimension == "Number of Subscribers") {
      return(plots[4])
    } else if (input$x_var == "Category" &&
               input$y_var == "Number") {
      return(plots[5])
    } else if (input$x_var == "Number of uploads" &&
               input$y_var == "Number of subscribers") {
      return(plots[6])
    } else if (input$x_var == "Number of uploads" &&
               input$y_var == "Number of Video Views") {
      return(plots[7])
    } else if (input$x_var == 'Lowest monthly earnings' &&
               input$y_var == 'Density') {
      return(plots[8])
    } else if (input$x_var == 'Highest monthly earnings' &&
               input$y_var == 'Density') {
      return(plots[9])
    } else if (input$x_var == 'Highest yearly earnings' &&
               input$y_var == 'Category') {
      return(plots[10])
    } else if (input$x_var == 'Highest yearly earnings' &&
               input$y_var == 'Country') {
      return(plots[11])
    } else if (input$x_var == 'Longitude' &&
               input$y_var == 'Latitude') {
      return(plots[12])
    } else
      return(plots[13])
  }
  
  # Determine the plot type
  plot_type <- determine_type()
  
  observe({
    if ((input$x_var == 'lowest_monthly_earnings' && input$y_var == 'density') || 
        (input$x_var == 'highest_monthly_earnings' && input$y_var == 'density')) {
      bin_min <- 3000
      bin_max <- 6000
      updateSliderInput(session, "binwidth", min = bin_min, max = bin_max, value = (bin_max + bin_min) / 2)
    }
  })
  
  output$plot <- renderPlot({
    if (input$x_var == "Country" && input$y_var == "None") {
      ggplot(df, aes_string(x = input$x_var)) +
        geom_bar() +
        xlab(input$x_var)
    } else if (input$x_var == "Country" && input$y_var == "Density") {
      ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
        geom_count() +
        xlab(input$x_var) +
        ylab(input$y_var)
    } else if ((input$x_var == 'lowest_monthly_earnings' && input$y_var == 'density') || 
               (input$x_var == 'highest_monthly_earnings' && input$y_var == 'density')) {
      ggplot(df, aes_string(x = input$x_var)) +
        geom_histogram(binwidth = input$binwidth) +
        xlab(input$x_var) +
        ylab('Density')
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
