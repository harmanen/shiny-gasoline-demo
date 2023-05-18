library(shiny)
library(plotly)

data <- read.csv("data/dataset.csv")

ui <- fluidPage(

    titlePanel("Gasoline visualizer 9000"),

    plotlyOutput("plot")
)

server <- function(input, output) {

    output$plot <- renderPlotly({
        plot_ly(data, type = "scatter", mode = "lines+markers") %>%
        add_trace(x = ~Date, y = ~PricePerLitre) %>% 
        layout(showlegend = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
