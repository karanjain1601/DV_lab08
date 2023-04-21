library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

# Load the iris dataset
data(iris)

# Define the UI for the application
ui <- fluidPage(
  # Create a sidebar panel for user inputs
  sidebarPanel(
    # Allow user to select x-axis variable
    selectInput("xvar", "X-axis variable:", choices = names(iris)[-5]),
    # Allow user to select y-axis variable
    selectInput("yvar", "Y-axis variable:", choices = names(iris)[-5]),
    # Create checkboxGroupInput for species selection
    checkboxGroupInput("species",
                       label = "Species:",
                       choices = unique(iris$Species),
                       selected = unique(iris$Species)),
    # Create color picker for each species
    tags$label("Select color for each species:"),
    fluidRow(
      lapply(unique(iris$Species), function(s) {
        column(
          width = 5,
          selectInput(paste0("color_", s), paste0("Color for ", s), 
                      choices = c("red", "green", "blue", "yellow", "purple", "orange", "black", "white"),
                      selected = "red")
        )
      })
    ),
    # Create numericInput for point size
    numericInput("point_size", "Point Size:", 1, min = 0, max = 10),
    # Create selectInput for point shape
    selectInput("point_shape", "Point Shape:",
                choices = c("circle", "square", "triangle", "diamond"),
                selected = "circle")
  ),
  # Create a main panel for the plot
  mainPanel(
    # Create a plotOutput for the scatter plot
    plotOutput("scatterplot")
  )
)

# Define the server for the application
server <- function(input, output) {
  # Create a reactive data frame based on user inputs
  filteredData <- reactive({
    iris %>% 
      filter(Species %in% input$species)
  })
  
  # Create a reactive list of colors for each species
  speciesColors <- reactive({
    lapply(unique(input$species), function(s) {
      input[[paste0("color_", s)]]
    })
  })
  
  # Create the scatter plot using ggplot2 and render it in the output
  output$scatterplot <- renderPlot({
    ggplot(data = filteredData(), aes_string(x = input$xvar, y = input$yvar, color = "Species")) +
      geom_point(size = input$point_size, shape = input$point_shape) +
      scale_color_manual(values = speciesColors()) +
      labs(x = input$xvar, y = input$yvar, color = "Species")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

