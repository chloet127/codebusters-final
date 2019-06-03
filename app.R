library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")

data <- read.xls("./data/world-happiness.xls")

# scatterplot social support vs happiness for selected country in 2008
ui <- fluidPage(
  titlePanel("World Happiness"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chosenCountry",
                  label = "Select a country:",
                  choices = unique(data$Country.name),
                  multiple = FALSE),
      sliderInput("chosenYears",
                  "Select year range:",
                  min = min(data$Year),
                  max = max(data$Year),
                  value = c(min(data$Year), max(data$Year)),
                  sep = "",
                  step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("socialSupportPlot"))
      )
    )
  )
)

server <- function(input, output) {
  chosen_data <- reactive({
    data %>%
      filter(Country.name %>% input$chosenCountry,
             Year >= input$chosenYears[1],
             Year <= input$chosenYears[2])
  })
  
  output$socialSupportPlot <- renderPlot({
    chosen_data() %>%
      ggplot() +
      geom_line(aes(x = Year, y = Social.support, color = Country.name)) +
      labs(title = "Social support levels over time",
           x = "Year",
           y = "Social support")
  })
}

shinyApp(ui = ui, server = server)
# US map of life expectancy for given year, hover shows actual expectancy