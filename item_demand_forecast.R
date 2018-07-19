library(shiny)
library(markdown)

ui <- fluidPage(

                    titlePanel("rMarkdown of Mixpace Items Demand Forecast"),
                    fluidRow(
                            column(12,
                                   includeHTML("items_demand_forecast.html")
                            )
                    )
)
            



server <- function(input, output) {
        
}

shinyApp(ui = ui, server = server)