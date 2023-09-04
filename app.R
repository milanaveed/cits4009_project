# Load required packages
library(shiny)
library(ggplot2)
library(gridExtra)
library(GGally)
library(dplyr)
library(ggthemes)
library(lubridate)
library(ca)
library(ggmap)
library(sp)
library(maptools)
library(maps)

# Load data from the current directory
df <- read.csv("./youtube_UTF_8.csv")

# Define color theme
my_color <- "#2061F2"
color_theme <- theme_few() + # Theme based on S. Few's "Practical Rules for Using Color in Charts"
  theme(plot.title = element_text(color = my_color),
        plot.margin = margin(10, 20, 10, 20)) +
  theme(strip.text.x = element_text(size = 14, colour = "#202020"))

#------------------------ Data cleaning starts----------------------------
# Dealing with missing values and invalid values
df <- mutate(df,
             created_year = ifelse(created_year < 2005, NA, created_year))
df <- mutate(df,
             category = ifelse(category=="nan", NA, category))
df <- mutate(df,
             Country = ifelse(Country=="nan", NA, Country))
df <- mutate(df,
             uploads = ifelse(uploads==0, NA, uploads))
df <- mutate(df,
             video.views = ifelse(video.views<1000000, NA, video.views))
df$Country.fix <- ifelse(is.na(df$Country),
                         "Missing", df$Country)
df$category.fix <- ifelse(is.na(df$category),
                          "Uncategorised", df$category)

# Add isBad indicators to columns Country, category and uploads
df$Country_isBad <- is.na(df$Country)
df$category_isBad <- is.na(df$category)
df$uploads_isBad <- is.na(df$uploads)
df$video.views_isBad <- is.na(df$video.views)

# Removing rows of NA in column created_year and video.views
df <- subset(df, !is.na(df$created_year) & !is.na(df$video.views))

#------------------------ Data cleaning ends----------------------------

# Define x variable lists
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

# Define a function to get the available y options
determine_y <- function(x){
  switch(x,
         "None" = c("None"),
         "Country" = c("Number","Category (number of channels)"),
         "Created year" = c("Category (number of channels)",
                            "Category (number of subscribers)"),
         "Category" = c("Number"),
         "Number of uploads"=c("Number of subscribers","Number of video views"),
         "Lowest monthly earnings" = c("Density"),
         "Highest monthly earnings" = c("Density"),
         "Highest yearly earnings" = c("Category","Country"),
         "Longitude" = c("Latitude")
  )
}

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
determine_type <- function(x, y) {
  if (x == "Country" && y == "Number") {
    return(plots[1])
  } else if (x == "Country" &&
             y == "Category (number of channels)") {
    return(plots[2])
  } else if (x == "Created year" &&
             y == "Category (number of channels)") {
    return(plots[3])
  } else if (x == "Created year" &&
             y == "Category (number of subscribers)") {
    return(plots[4])
  } else if (x == "Category" &&
             y == "Number") {
    return(plots[5])
  } else if (x == "Number of uploads" &&
             y == "Number of subscribers") {
    return(plots[6])
  } else if (x == "Number of uploads" &&
             y == "Number of video views") {
    return(plots[7])
  } else if (x == 'Lowest monthly earnings' &&
             y == 'Density') {
    return(plots[8])
  } else if (x == 'Highest monthly earnings' &&
             y == 'Density') {
    return(plots[9])
  } else if (x == 'Highest yearly earnings' &&
             y == 'Category') {
    return(plots[10])
  } else if (x == 'Highest yearly earnings' &&
             y == 'Country') {
    return(plots[11])
  } else if (x == 'Longitude' &&
             y == 'Latitude') {
    return(plots[12])
  } else
    return(plots[13])
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
        sliderInput("binwidth", "Binwidth:", min = 10, max = 100, value = 30),
        sliderInput("x_limit", "X-Axis Limit:", min = 0, max = 10000, value = c(0, 10000)),
        sliderInput("y_limit", "Y-Axis Limit:", min = 0, max = 10000, value = c(0, 10000))
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  observe({
    updateSelectInput(session, "y_var", choices=determine_y(input$x_var))
    
    # Update binwidth and x y coordiante limit accordingly
    if (plot_type=="Distribution of Lowest Monthly Earnings(Log-scale)") {
      updateSliderInput(
        session,
        "binwidth",
        min = 0.1,
        max = 0.8,
        value = 0.1
      )
      updateSliderInput(
        session,
        "x_limit",
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
      updateSliderInput(
        session,
        "x_limit",
        min = 0.1,
        max = 0.8,
        value = 0.1
      )
    }
  })
  
  # Define functions for plotting
  g1 <- function(df, x_var) {
    # Filter the dataset using the isBad indicator
    df1 <- subset(df, df$Country_isBad == FALSE)
    # Sort the data
    dfsums <- table(df1$Country)
    # Convert the table object into a data frame
    countryf <- as.data.frame(dfsums)
    # Define the column names
    colnames(countryf) <- c("country", "count")
    # Sort the data frame
    countryf <- countryf %>% arrange(desc(countryf$count))
    # Get unique country names
    unique_countries <- unique(countryf$country)
    # Select the first 25 unique country names
    selected_countries <- unique_countries[1:25]
    # Get all rows of the top 25 countries with the largest amount of famous channels
    top_25_country <- subset(countryf, countryf$country %in% selected_countries)
    # Sort the subset
    top_25_country <- transform(top_25_country,
                                country = reorder(country, count))
    # Plot the bar chart
    ggplot(top_25_country, aes_string(x = country, y = count)) +
      geom_bar(stat = "identity", fill = my_color, alpha = 0.60) +
      coord_flip() +
      ggtitle("Number by Country (Top 25)") +
      xlab(x_var)+
      color_theme
  }
  
  output$plot <- renderPlot({
    # Determine the plot type
    plot_type <- determine_type(input$x_var, input$y_var)
    
    if (plot_type == "Number of Channels by Country") {
      g1(df, input$x_var)
      print(g1)
      
    } else {
      
    }
  })
}

# Create Shiny App
shinyApp(ui, server)

# } else if (plot_type == "Category VS Country (Channels)") {
#   ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
#     geom_count() +
#     xlab(input$x_var) +
#     ylab(input$y_var)
# } else if ((input$x_var == 'lowest_monthly_earnings' && input$y_var == 'density') || 
#            (input$x_var == 'highest_monthly_earnings' && input$y_var == 'density')) {
#   ggplot(df, aes_string(x = input$x_var)) +
#     geom_histogram(binwidth = input$binwidth) +
#     xlab(input$x_var) +
#     ylab('Density')
# } else {
#   if (input$y_var != "None") {
#     ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
#       geom_point() +
#       xlab(input$x_var) +
#       ylab(input$y_var)
