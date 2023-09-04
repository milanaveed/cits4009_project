# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(gridExtra)

# load dataset
df <- read.csv('../data/youtube_UTF_8.csv')


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Global YouTube Statistics 2023"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(sliderInput(
      "bins",
      "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    ),
    selectInput(
      inputId = "x1",
      label = "Choose X:",
      choices = names(df)
    ),
    selectInput(
      inputId = "x2",
      label = "Choose a quality:",
      choices = c(df$quality)
    ),
    selectInput(
      inputId = "x3",
      label = "Choose from quality and pH:",
      choices = c("quality", "pH")
    )),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput(outputId = "distPlot"))
  )
)

# "use if statements to check which x variable is selected and shows bar chart or
# histogram or boxplot accordingly"

# Define server logic required to plot
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
