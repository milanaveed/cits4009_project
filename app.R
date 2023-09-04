# Load required packages
library(shiny)
library(ggplot2)

# Load your data from the current directory and store it in 'df'
df <- read.csv("./youtube_UTF_8.csv")

# Define x and y variable lists
x_variables <-
  c(
    "None",
    "Country",
    "Created year",
    "Category",
    "Number of uploads",
    "Lowest monthly earnings",
    "Highest monthly earnings",
    "Highest yearly earnings",
    "Longitude"
  )

determine_y <- function(x){
  switch(x,
         "None" = c("None"),
         "Country" = c("Number","Category (number of channels)"),
         "Created year" = c("Category (number of channels)",
                            "Category (number of subscribers)"),
         "Category" = c("Number"),
         "Number of uploads"=c("Number of subscribers","Number of Video Views"),
         "Lowest monthly Earnings" = c("Density"),
         "Highest monthly Earnings" = c("Density"),
         "Highest yearly Earnings" = c("Category","Country"),
         "Longitude" = c("Latitude")
  )
}

# User Interface
ui <- fluidPage(
  titlePanel("YouTube Statistics Plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Choose x variable:", x_variables),
      selectInput("y_var", "Choose y variable:", "None"),
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
  plots <- c(
    "Number of Channels by Country",
    "Category VS Country (Channels)",
    "Category VS Created Year (Channels)",
    "Category VS Country (Subscribers)",
    "Number of Channels by Category",
    "Number of Uploads VS Subscribers",
    "Number of Uploads VS Video Views",
    "Distribution of Lowest Monthly Earnings(Log-scale)",
    "Distribution of Highest Monthly Earnings(Log-scale)",
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
               input$y_var == "Category (number of channels)") {
      return(plots[2])
    } else if (input$x_var == "Created Year" &&
               input$y_var == "Category (number of channels)") {
      return(plots[3])
    } else if (input$x_var == "Created Year" &&
               input$y_var == "Category (number of subscribers)") {
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
    } else if (input$x_var == 'Lowest monthly Earnings' &&
               input$y_var == 'Density') {
      return(plots[8])
    } else if (input$x_var == 'Highest monthly Earnings' &&
               input$y_var == 'Density') {
      return(plots[9])
    } else if (input$x_var == 'Highest yearly Earnings' &&
               input$y_var == 'Category') {
      return(plots[10])
    } else if (input$x_var == 'Highest yearly Earnings' &&
               input$y_var == 'Country') {
      return(plots[11])
    } else if (input$x_var == 'Longitude' &&
               input$y_var == 'Latitude') {
      return(plots[12])
    } else
      return(plots[13])
  }
  
  observe({
    # Determine the plot type
    plot_type <- determine_type()
    
    updateSelectInput(session, "y_var", choices=determine_y(input$x_var))
    
    if (plot_type=="Distribution of Lowest Monthly Earnings(Log-scale)") {
      updateSliderInput(
        session,
        "binwidth",
        min = 0.1,
        max = 0.8,
        value = 0.1
      )
    }else if (plot_type=="Distribution of Highest Monthly Earnings(Log-scale)") {
      updateSliderInput(
        session,
        "binwidth",
        min = 0.1,
        max = 0.8,
        value = 0.1
      )
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
        # ggplot(df, aes_string(x = input$x_var)) +
        #   geom_histogram(binwidth = 30) +
        #   xlab(input$x_var)
      }
    }
  })
}

# Create Shiny App
shinyApp(ui, server)
