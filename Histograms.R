# Load libraries
library(shiny)
library(ggplot2)

# Load the iris dataset
data("iris")

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Histogram"),
  sidebarPanel(
    selectInput("feat", "Choose attribute to plot:", 
                choices = names(iris)[-5], selected = names(iris)[-5][[1]]),
    numericInput("bins", "Number of bins:", 30, min = 1),
    selectInput("color", "Choose histogram color:", 
                choices = c("red", "green", "blue", ""), selected = "red")
  ),
  mainPanel(
    plotOutput("hist")
  )
)

# Define the server
server <- function(input, output) {
  
  # Create a function to generate histograms
  generate_histogram <- function(feature, bins, color) {
    ggplot(data = iris, aes(x = iris[[feature]])) +
      geom_histogram(binwidth = (max(iris[[feature]]) - min(iris[[feature]])) / bins, 
                     color = color, fill = color) +
      labs(title = feature, x = feature, y = "Frequency") +
      theme_bw()
  }
  
  # Output histograms
  output$hist <- renderPlot({
    generate_histogram(input$feat, input$bins, input$color)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

