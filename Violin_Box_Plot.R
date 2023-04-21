# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)

# Load the iris dataset
data("iris")

input_choice <- list("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
# Define the UI
ui <- fluidPage(
  titlePanel("Iris Dataset - Boxplot and Violin plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("y_var", "Y-Axis Variable", choices = input_choice),
      helpText("Select which plot to display:"),
      radioButtons("plot", "Plot Type", choices = c("Boxplot", "Violin Plot"), selected = "Boxplot")
    ),
    mainPanel(
      plotlyOutput("plots")
    )
  )
)

#Define the server
server <- function(input, output) {
  
  #Create boxplot or violin plot based on user input
  output$plots <- renderPlotly({
    if(input$plot == "Boxplot") {
      #Boxplot
      p <- ggplot(iris, aes(x = Species, y = !!as.symbol(input$y_var),fill = Species)) +
        geom_boxplot() +
        ggtitle(paste("Boxplot of", input$y_var, "by Species"))
      plotly_build(p)
    } else {
      #Violin plot
      p <- ggplot(iris, aes(x = Species, y = !!as.symbol(input$y_var), fill = Species)) +
        geom_violin(trim = FALSE) +
        ggtitle(paste("Violin Plot of", input$y_var, "by Species"))
      plotly_build(p)
    }
  })
  
}

#Run the app
shinyApp(ui=ui, server=server)