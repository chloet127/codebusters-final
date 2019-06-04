library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")
library("plotly")
library("stringr")

data <- as.data.frame(read.xls("./data/world-happiness.xls", verbose = FALSE))

page_one <- tabPanel(
  "Social Support",
  titlePanel("World Social Support"),
  
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
      plotOutput("socialSupportPlot")
    )
  )
)

page_two <- tabPanel(
  "Life Expectancy",
  titlePanel("World Life Expectancy"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chosen_Country",
                  label = "Select a country:",
                  choices = unique(data$Country.name),
                  multiple = FALSE),
      sliderInput("chosen_Years",
                  "Select year range:",
                  min = min(data$Year),
                  max = max(data$Year),
                  value= c(min(data$Year), max(data$Year)),
                  sep = "",
                  step = 1)
    ),
    mainPanel(
      plotOutput("lifeExpectancyPlot")
    )
  )
)

ui <- fluidPage(
  theme = shinytheme("journal"),
  navbarPage(
    "Happiness",
    page_one,
    page_two
  )
)

server <- function(input, output) {
  chosen_data <- reactive({
    data %>%
      filter(Country.name == input$chosenCountry,
             data$Year >= input$chosenYears[1],
             data$Year <= input$chosenYears[2])
  })
  
  output$socialSupportPlot <- renderPlot({
    ggplot(data = chosen_data(), 
           aes(x = Year, y = Social.support, color = "Social Support")) +
    geom_line() +
      scale_color_manual(values = "#6699FF") +
    labs(title = "Social support levels over time",
         x = "Year",
         y = "Social support")
  })
  
  chosen_data1 <- reactive({
    data %>%
      filter(Country.name == input$chosen_Country,
             data$Year >= input$chosen_Years[1],
             data$Year <= input$chosen_Years[2])
  })
  output$lifeExpectancyPlot <- renderPlot({
    ggplot(data = chosen_data1(), 
           aes(x = Year, y = Healthy.life.expectancy.at.birth, color = "Life Expectancy")) +
    geom_line() +
      scale_color_manual(values = "#6699FF") +
    labs(title = "Life expectancy over time",
         x = "Year",
         y = "Life Expectancy")
  })
}

shinyApp(ui = ui, server = server)
