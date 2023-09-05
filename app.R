# Load required packages
library(shiny)
library(ggplot2)
library(gridExtra)
library(GGally)
library(dplyr)
library(bslib)

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
color_theme <-
  theme_few() + # Theme based on S. Few's "Practical Rules for Using Color in Charts"
  theme(plot.title = element_text(color = my_color, size=24),
        plot.margin = margin(10, 20, 10, 20),
        axis.text=element_text(size=14),
        axis.title=element_text(size=17, colour = "#202020"))

#------------------------ Data cleaning starts----------------------------
# Dealing with missing values and invalid values
df <- mutate(df,
             created_year = ifelse(created_year < 2005, NA, created_year))
df <- mutate(df,
             category = ifelse(category == "nan", NA, category))
df <- mutate(df,
             Country = ifelse(Country == "nan", NA, Country))
df <- mutate(df,
             uploads = ifelse(uploads == 0, NA, uploads))
df <- mutate(df,
             video.views = ifelse(video.views < 1000000, NA, video.views))
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
df <- subset(df,!is.na(df$created_year) & !is.na(df$video.views))

#------------------------ Data cleaning ends------------------------------

# Define x variable lists
x_variables <-
  c(
    'Country',
    'Created year',
    'Category',
    'Number of uploads',
    'Lowest monthly earnings',
    'Highest monthly earnings',
    'Highest yearly earnings',
    'Longitude'
  )

# Define a function to get the available y options
determine_y <- function(x) {
  switch(
    x,
    'Country' = c('Category'),
    'Created year' = c('Category (number of channels)'),
    'Category' = c('Number'),
    'Number of uploads' = c('Number of subscribers', 'Number of video views'),
    'Lowest monthly earnings' = c('Density'),
    'Highest monthly earnings' = c('Density'),
    'Highest yearly earnings' = c('Category', 'Country'),
    'Longitude' = c('Latitude')
  )
}

# User Interface
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {

      }
      h1 {
        font - family:'Yusei Magic', sans - serif;
      }
      .shiny - input - container {
        color:#474747;
      }"))
  ),
  titlePanel('YouTube Statistics Plots'),
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  sidebarLayout(
    sidebarPanel(
      selectInput('x_var', 'Choose x variable:', x_variables),
      selectInput('y_var', 'Choose y variable:', 'Category'),
      conditionalPanel(
        condition = "input.x_var == 'Country'",
        radioButtons('rb1', 'Choose the third dimension:', c('Number of channels', 'Number of subscribers'))
      ),
      conditionalPanel(
        condition = "input.x_var == 'Number of uploads' &&
 input.y_var == 'Number of subscribers'",
        sliderInput('x_limit0','X-axis limit:',min = 0,max = 3e+05,value = 2.5e+05,step = 5e+03),
        sliderInput('y_limit0','Y-axis limit:',min = 0,max = 2.5e+08,value = 1.5e+08,step = 5e+06)
      ),
      conditionalPanel(
        condition = "input.x_var == 'Lowest monthly earnings' &&
 input.y_var == 'Density'",
        sliderInput('binwidth','Binwidth:',min = 0.1,max = 0.8,value = 0.1,step = 0.05),
        sliderInput('x_limit','X-axis limit:',min = 0,max = 800000,value = 500000,step = 100000),
        sliderInput('y_limit','Y-axis limit:',min = 0,max = 1,value = 0.8,step = 0.05)
      ),
      conditionalPanel(
        condition = "input.x_var == 'Highest monthly earnings' &&
 input.y_var == 'Density'",
        sliderInput('binwidth2','Binwidth:',min = 0.1,max = 0.8,value = 0.1,step = 0.05),
        sliderInput('x_limit2','X-axis limit:',min = 0,max = 10000000,value = 6000000,step = 500000),
        sliderInput('y_limit2','Y-axis limit:',min = 0,max = 1,value = 0.8,step = 0.05)
      )
    ),
    mainPanel(plotOutput('plot'))
  ))


# Server logic
server <- function(input, output, session) {
  # Theme customization
  #bs_themer(gfonts = TRUE, gfonts_update = FALSE)
  
  observe({
    updateSelectInput(session, 'y_var', choices = determine_y(input$x_var))
  })
  
  # Define functions for plotting
  g1 <- function(df) {
    # Filter the dataset using the isBad indicator
    df1 <-
      subset(df, df$Country_isBad == FALSE &
               df$category_isBad == FALSE)
    
    # Select the first 15 unique country names
    selected_countries <- unique_countries[1:15]
    
    # Get all rows of the top 15 countries with the largest amount of famous channels
    top_15_country <-
      subset(df1, df1$Country %in% selected_countries)
    
    # Group country and category, then count the occurrences of each combination
    df_grouped <- top_15_country %>%
      group_by(Abbreviation, category.fix) %>%
      summarise(count = n())
    
    ggplot(df_grouped, aes(x = Abbreviation, y = category.fix)) +
      geom_count(
        aes(size = df_grouped$count, group = category.fix),
        color = my_color,
        alpha = 0.7
      ) +
      scale_size_area(max_size = 15, name = 'Channels') +
      ggtitle('Category VS Country (Number of Channels)') +
      xlab('Country') +
      ylab('Category') +
      color_theme
  }
  
  g2 <- function(df) {
    # Filter the dataset using the isBad indicator
    df1 <- subset(df, df$category_isBad == FALSE)
    
    # Group country and category, then count the occurrences of each combination
    df_grouped1 <- df1 %>%
      group_by(created_year, category.fix) %>%
      summarise(count = n())
    
    ggplot(df_grouped1, aes(x = created_year, y = category.fix)) +
      geom_count(
        aes(size = df_grouped1$count, group = category.fix),
        color = my_color,
        alpha = 0.7
      ) +
      scale_size_area(max_size = 12, name = 'Channels') +
      ggtitle('Category VS Created Year (Number of Channels)') +
      xlab('Created year') +
      ylab('Category') +
      color_theme
  }
  
  g3 <- function(df) {
    # Filter the dataset using the isBad indicator
    df1 <-
      subset(df, df$Country_isBad == FALSE &
               df$category_isBad == FALSE)
    
    # Select the first 15 unique country names
    selected_countries <- unique_countries[1:15]
    
    # Get all rows of the top 15 countries with the largest amount of famous channels
    top_15_country <-
      subset(df1, df1$Country %in% selected_countries)
    
    # Group country and category, then count the sum of subcribers for each combination
    df_grouped2 <- top_15_country %>%
      group_by(Abbreviation, category.fix) %>%
      summarise(count = sum(subscribers))
    
    ggplot(df_grouped2, aes(x = Abbreviation, y = category.fix)) +
      geom_count(
        aes(size = df_grouped2$count, group = category.fix),
        color = my_color,
        alpha = 0.7
      ) +
      scale_size_area(max_size = 15, name = 'Subscribers') +
      ggtitle('Category VS Country (Number of Subscribers)') +
      xlab('Country') +
      ylab('Category') +
      color_theme
  }
  
  g4 <- function(df) {
    # Sort the data
    dfsums <- table(df$category.fix)
    
    # Convert the table object into a data frame
    categoryf <- as.data.frame(dfsums)
    
    # Define the column names
    colnames(categoryf) <- c('category', 'count')
    
    # Sort the data frame
    categoryf <- transform(categoryf,
                           category = reorder(category, count))
    # Plot the bar chart
    ggplot(categoryf) +
      geom_bar(
        aes(x = category, y = count),
        stat = 'identity',
        fill = my_color,
        alpha = 0.60
      ) +
      xlab('Category') +
      ylab('Number') +
      coord_flip() +
      ggtitle('Number by Category') +
      color_theme
  }
  
  g5 <- function(df, x_limit, y_limit) {
    # Filter the dataset
    df1 <- subset(df, df$uploads_isBad == FALSE)
    
    p1 <- ggplot(df1, aes(x = uploads, y = subscribers)) +
      geom_point(color = my_color, alpha = 0.3) +
      coord_cartesian(xlim = c(0, x_limit), ylim = c(0, y_limit)) +
      xlab('Number of uploads') +
      ylab('Subscribers') + 
      ggtitle('Number of Uploads VS Subscribers') +
      color_theme
    
    p2 <- ggplot(df1, aes(x = uploads, y = subscribers)) +
      geom_smooth(color = '#303030') +
      coord_cartesian(xlim = c(0, x_limit), ylim = c(-3e+07,y_limit)) +
      xlab('Number of uploads') +
      ylab('Subscribers') +
      color_theme

    grid.arrange(p1, p2, ncol = 1)
  }
  
  g6 <- function(df) {
    # Filter the dataset using the isBad indicators
    df1 <-
      subset(df, df$uploads_isBad == FALSE &
               df$video.views_isBad == FALSE)
    
    p1 <- ggplot(df1, aes(x = uploads, y = video.views)) +
      geom_point(color = my_color, alpha = 0.3) +
      labs(x = '') + # Hide x coordinate label
      ggtitle('Number of Uploads VS Video Views') +
      color_theme
    
    p2 <- ggplot(df1, aes(x = uploads, y = video.views)) +
      geom_smooth() +
      color_theme
    
    grid.arrange(p1, p2, ncol = 1)
  }
  
  g7 <- function(df, bin, x_limit, y_limit) {
    ggplot(df, aes(x = lowest_monthly_earnings)) +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = bin,
                     fill = 'grey') +
      geom_density(color = my_color) +
      scale_x_log10(breaks = c(100, 2000, 25000, 200000)) +
      coord_cartesian(xlim = c(1, x_limit), ylim = c(0, y_limit)) +
      annotate(
        'text',
        x = 500,
        y = 0.5,
        label = paste('The lowest monthly earnings peak', ' at around 25000', sep =
                        '\n')
      ) +
      ggtitle('Distribution of Lowest Monthly Earnings (Log-scale)') +
      color_theme
  }
  
  g8 <- function(df, bin, x_limit, y_limit) {
    ggplot(df, aes(x = highest_monthly_earnings)) +
      geom_histogram(aes(y = after_stat(density)),
                     binwidth = bin,
                     fill = 'grey') +
      geom_density(color = my_color) +
      scale_x_log10(breaks = c(100, 2000, 25000, 400000)) +
      coord_cartesian(xlim = c(1, x_limit), ylim = c(0, y_limit)) +
      annotate(
        'text',
        x = 500,
        y = 0.4,
        label = paste('The highest monthly earnings peak', 'at around 400000', sep =
                        '\n')
      ) +
      ggtitle('Distribution of Highest Monthly Earnings (Log-scale)') +
      color_theme
  }
  
  g9 <- function(df) {
    # Calculate median for each category
    df$median_earnings_category <-
      with(df,
           ave(highest_yearly_earnings, category.fix, FUN = median))
    
    # Create the boxplot
    ggplot(df, aes(
      x = reorder(category.fix, median_earnings_category),
      y = highest_yearly_earnings
    )) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(alpha = 0.2, color = my_color) +
      coord_flip(ylim = c(0, 6e+07)) +
      ggtitle('Highest Yearly Earnings by Category (by Median)') +
      color_theme
  }
  
  g10 <- function(df) {
    # Calculate median for each country
    df$median_earnings_country <-
      with(df, ave(highest_yearly_earnings, Country.fix, FUN = median))
    
    # Sort the data frame
    df_median_earnings_country_des <-
      df %>% arrange(desc(median_earnings_country))
    
    # Get unique country names
    unique_countries <-
      unique(df_median_earnings_country_des$Country)
    
    # Select the first 20 unique country names
    selected_countries <- unique_countries[1:20]
    
    # Get all rows of the top 20 countries by highest yearly earnings
    top_20_country <-
      subset(
        df_median_earnings_country_des,
        df_median_earnings_country_des$Country %in% selected_countries
      )
    
    # Create the boxplot
    ggplot(top_20_country,
           aes(
             x = highest_yearly_earnings,
             y = reorder(Country.fix, median_earnings_country)
           )) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(alpha = 0.3, color = my_color) +
      coord_cartesian(xlim = c(0, 7e+07)) +
      ggtitle('Highest Yearly Earnings by Country (Top 20 by Median)') +
      color_theme
  }
  
  g11 <- function(df) {
    # Data preparation
    visit.x <- df$Longitude
    visit.y <- df$Latitude
    
    mp <- NULL #Create an empty map
    mapworld <-
      borders('world', colour = 'gray50', fill = 'white') #Draw the map
    mp <- ggplot() + mapworld + ylim(-60, 90)
    # Draw the points
    mp2 <-
      mp + geom_point(aes(
        x = visit.x,
        y = visit.y,
        size = df$highest_yearly_earnings
      ),
      color = my_color) +
      scale_size(range = c(4, 13)) +
      ggtitle('Geospatial Information About Highest Yearly Earnings')
    mp3 <- mp2 + color_theme + theme(legend.position = 'none')
    mp3
  }
  
  output$plot <- renderPlot({
    if (input$x_var == 'Country') {
      if (input$rb1 == 'Number of channels') {
        g1(df)
      } else
        g3(df)
    } else if (input$x_var == 'Created year' &&
               input$y_var == 'Category (number of channels)') {
      g2(df)
    } else if (input$x_var == 'Category' &&
               input$y_var == 'Number') {
      g4(df)
    } else if (input$x_var == 'Number of uploads' &&
               input$y_var == 'Number of subscribers') {
      g5(df, input$x_limit0, input$y_limit0)
    } else if (input$x_var == 'Number of uploads' &&
               input$y_var == 'Number of video views') {
      g6(df)
    } else if (input$x_var ==  'Lowest monthly earnings' &&
               input$y_var == 'Density') {
      g7(df, input$binwidth, input$x_limit, input$y_limit)
    } else if (input$x_var ==  'Highest monthly earnings' &&
               input$y_var == 'Density') {
      g8(df, input$binwidth2, input$x_limit2, input$y_limit2)
    } else if (input$x_var ==  'Highest yearly earnings' &&
               input$y_var == 'Category') {
      g9(df)
    } else if (input$x_var ==  'Highest yearly earnings' &&
               input$y_var == 'Country') {
      g10(df)
    } else if (input$x_var == 'Longitude' &&
               input$y_var == 'Latitude') {
      g11(df)
    }
  })
}

# Create Shiny App
shinyApp(ui, server)


