library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")
library("plotly")

data <- read.xls("./data/world-happiness.xls")

# scatterplot social support vs happiness for selected country in 2008
# need to use navbar to make multiple pages
page_one <- tabPanel(
  "World",
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

page_two <- tabPanel(
  "US",
  titlePanel("US Life Expectancy"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("chosenYears",
                  "Select year range:",
                  min = min(data$Year),
                  max = max(data$Year),
                  value= c(min(data$Year), max(data$Year)),
                  sep = "",
                  step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("US", plotOutput("usLife"))
      )
    )
  )
)

ui <- fluidPage(
  navbarPage(
    "Happiness",
    page_one,
    page_two
  )
)

server <- function(input, output) {
  shinytheme("lumen")
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
  
  output$usMap <- renderPlot({
    us_data <- reactive({
      chosen_data %>%
        filter(chosen_data$Country.name == "United States")
    })
    us_data() %>%
      ggplot() +
      geom_line(aes(x = Year, y = Healthy.life.expectancy.at.birth)) +
      labs(title = "US life expectancy over time",
           x = "Year",
           y = "Life Expectancy")
  })
}

shinyApp(ui = ui, server = server)
# US map of life expectancy for given year, hover shows actual expectancy