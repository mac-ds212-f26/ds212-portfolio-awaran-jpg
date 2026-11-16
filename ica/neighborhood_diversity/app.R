
library(shiny)
library(tidyverse)
library(sf)
library(plotly)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")

metro_names <- data_by_dist %>%
  pull(metro_name) %>%
  unique() %>%
  sort()

ui <- fluidPage(

  titlePanel("Neighborhood Diversity"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "metro",
        label = "Select a city:",
        choices = metro_names
      ),

      sliderInput(
        inputId = "span",
        label = "Smoothing span:",
        min = 0.1,
        max = 1,
        value = 0.5,
        step = 0.05
      )
    ),

    mainPanel(
      plotlyOutput("scatter"),
      plotOutput("map"),
      plotOutput("barplot")
    )
  )
)


server <- function(input, output) {

  output$scatter <- renderPlotly({

    p <- data_by_dist |>
      filter(metro_name == input$metro) |>
      ggplot(aes(x = distmiles, y = entropy)) +
      geom_point(alpha = 0.5) +
      geom_smooth(
        method = "loess",
        span = input$span,
        se = FALSE
      ) +
      labs(
        title = paste("Neighborhood Diversity in", input$metro),
        x = "Distance to City Hall (miles)",
        y = "Diversity Score"
      ) +
      theme_minimal()

    ggplotly(p)

  })

}


shinyApp(ui = ui, server = server)
