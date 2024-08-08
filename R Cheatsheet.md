# Lab 1 Syntax
- list all previous commands: ctrl+up arrow
- type ? before the command name to retrieve the help page, e.g. `?list.files`
- no index out of bound exception, so should always make sure that what we are asking for is within the bounds of the vector we're working with.
- to construct a vector containing all numbers 1 through 40 EXCEPT 2 and 10: `x[c(-2,-10)]` or `x[-c(2,10)]`
- matrices contain only one class of data
  
# Lab 2
- create vectors: `seq()`
- combine vectors: `c()`
- `rep(tmp, each = 4)`
- `rep(tmp, times=4)`
- `rep(tmp, length.out=31)`
- `label_vector <- paste("label", 1:30)`
- `fn_vector <- paste0("fn", 1:30)`
  
## Use the rnorm() and sample() functions
Create a 5x10 matrix with 10 NAs scattered around in the matrix, similar to the one shown below. Note that because of the randomness in the sampling process, your matrix will not look exactly the same each time you run your code.

1. create a 5x10 matrix
2. find 10 random locations
3. assign NAs to these locations

```r
matrix.data <- matrix(rnorm(50), nrow=5, ncol=10)
na_positions <- sample(length(matrix_data), 10)
matrix_data[na_positions] <- NA
df <- as.data.frame(matrix_data)
df
```

```r
# Load required libraries
library(shiny)
library(ggplot2)
library(gridExtra)

df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")
# Define the UI
ui <- fluidPage(
  titlePanel("Histogram Visualization"),  
  sidebarLayout(
    sidebarPanel( 
      selectInput("x_var1", "Choose x for Histogram 1:", choices = names(df)),
      selectInput("x_var2", "Choose x for Histogram 2:", choices = names(df))
    ),  
    mainPanel(plotOutput("histogram"))
  )
)

# Define the server
server <- function(input, output){ 
  # Function to generate the histogram plots  
  generate_histograms <- function(df, x_var1, title1, x_var2, title2){
    # TO DO, add your own codes
    p1 <- ggplot(df, aes_string(x=x_var1)) +
      geom_histogram(bins=20,  color="white") + 
      labs(title = title1, x = x_var1, y = "Frequency")
    
    p2 <- ggplot(df, aes_string(x=x_var2)) +
      geom_histogram(bins=20,  color="white") + 
      labs(title = title2, x = x_var2, y = "Frequency")
    
    grid.arrange(p1, p2, ncol=2)
    
  }  
  # Render the side-by-side histograms  
  output$histogram <- renderPlot({
    # TO DO, add your own codes
    generate_histograms(df, input$x_var1, colnames(input$x_var1), input$x_var2, colnames(input$x_var2))
  })
}
# Run the Shiny app
shinyApp(ui = ui, server = server)
```