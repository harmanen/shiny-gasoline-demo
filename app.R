library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

# Constants
LABEL_LOCALE <- "en_UK"

# Read data, add day and month columns
data <- read.csv("data/dataset.csv") %>%
  mutate(Day = wday(Date,
    label = TRUE,
    locale = LABEL_LOCALE
  )) %>%
  mutate(Month = month(Date,
    label = TRUE,
    locale = LABEL_LOCALE
  ))

ui <- fluidPage(
  titlePanel("Gasoline visualizer 9000"),
  plotlyOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    plot_ly(data,
      type = "scatter",
      mode = "lines"
    ) %>%
      add_trace(
        x = ~Date,
        y = ~PricePerLitre,
        name = "trace",
        line = list(color = "gray", dash = "dash")
      ) %>%
      add_markers(
        x = ~Date,
        y = ~PricePerLitre,
        color = ~City,
        marker = list(size = 10)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
