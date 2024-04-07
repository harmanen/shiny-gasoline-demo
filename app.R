library(shiny)
library(plotly)
library(dplyr)
library(lubridate)

# Constants
LABEL_LOCALE <- "en_UK"

# Define manually so the scales have same horizontal lines
ymin1BoxPlot <- 1
ymax1BoxPlot <- 3
ymin2BoxPlot <- 0
ymax2BoxPlot <- 40

# Helpers
# Wrap with div and add class name
append_css <- function(styles = "", ...) {
  tags$div(class = styles, ...)
}

# Read data, add day and month columns
data <- read.csv("data/dataset.csv") %>%
  mutate(Day = wday(Date,
                    label = TRUE,
                    locale = LABEL_LOCALE)) %>%
  mutate(Month = month(Date,
                       label = TRUE,
                       locale = LABEL_LOCALE))

# App
ui <- fluidPage(
  # Add styles
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "styles.css")
  ),

  append_css(
    "title-container",
    append_css(
      "fit-content",
      titlePanel("Gasoline visualizer 9000")
    )
  ),

  fluidRow(column(
    12, append_css("shadow-box", plotlyOutput("scatterPlot"))
  )),
  fluidRow(column(
    4, append_css("shadow-box", plotlyOutput("boxPlot"))
  ),
  column(
    8, append_css("shadow-box", plotlyOutput("barPlot"))
  ))
)

server <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    plot_ly(data,
            type = "scatter",
            mode = "lines") %>%
      add_trace(
        x = ~ Date,
        y = ~ PricePerLitre,
        name = "trace",
        line = list(color = "gray", dash = "dash")
      ) %>%
      add_markers(
        x = ~ Date,
        y = ~ PricePerLitre,
        color = ~ City,
        marker = list(size = 10)
      ) %>%
      layout(title = "Time series of the price per litre")
  })

  output$boxPlot <- renderPlotly({
    plot_ly(
      data,
      y = ~ PricePerLitre,
      boxpoints = "suspectedoutliers",
      jitter = 0.2,
      name = "Price per litre (€)",
      notched = TRUE,
      type = "box"
    ) %>%
      add_boxplot(
        y = ~ Volume,
        yaxis = "y2",
        boxpoints = "suspectedoutliers",
        jitter = 0.2,
        name = "Volume (l)",
        notched = TRUE
      ) %>%
      layout(
        title = "Box plots",
        showlegend = FALSE,
        yaxis = list(
          title = "",
          range = list(ymin1BoxPlot, ymax1BoxPlot)
        ),
        yaxis2 = list(
          title = "",
          range = list(ymin2BoxPlot, ymax2BoxPlot),
          side = "right",
          overlaying = "y",
          automargin = TRUE
        )
      )
  })

  output$barPlot <- renderPlotly({
    plot_ly(
      data %>%
        group_by(Day) %>%
        summarise(
          n = n(),
          TotalCost = sum(TotalCost),
          Volume = sum(Volume)
        ),
      x = ~ Day,
      y = ~ TotalCost,
      text = ~ n,
      textposition = "outside",
      type = "bar",
      name = "Cost (€)"
    ) %>%
      add_trace(y = ~ Volume,
                text = "",
                # Needed, otherwise ~n is printed here as well
                name = "Volume (l)") %>%
      layout(title = "Distribution of refills (n over blue bar)",
             yaxis = list(title = ""))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
