library(shiny)
library(plotly)

data <- read.csv("data/dataset.csv")

ui <- fluidPage(
  titlePanel("Gasoline visualizer 9000"),
  plotlyOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    plot_ly(data, 
            type = "scatter", 
            mode = "lines") %>%
      add_trace(x = ~Date, 
                y = ~PricePerLitre,
                name = "trace", 
                line = list(color = "gray", dash = "dash")) %>%
      add_markers(x = ~Date, 
                  y = ~PricePerLitre,
                  color = ~City,
                  marker = list(size = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
