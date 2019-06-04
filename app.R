library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")
library("plotly")
library("stringr")
source("chloe-data.R")

data <- as.data.frame(read.xls("./data/world-happiness.xls", verbose = FALSE))

intro_page <- tabPanel(
  "Introduction",
  titlePanel("World Happiness Report"),
  textOutput("introduction")
)

page_happy <- tabPanel(
  "Happiness",
  titlePanel("Map of World Happiness"),
  plotOutput(outputId = "happy", width = "100%", height = "100%")
)

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

conclusion_page <- tabPanel(
  "Conclusion",
  titlePanel("World Happiness Report"),
  textOutput("conclusion")
)

ui <- fluidPage(
  theme = shinytheme("journal"),
  navbarPage(
    intro_page,
    page_happy,
    page_one,
    page_two,
    conclusion_page
  )
)

server <- function(input, output) {
  output$introduction <- renderText({
    paste0("This project seeks to analyze the variables of social support and
           life expectancy in comparison to levels of happiness in the countries
           of the world.  Our data was collected by the United Nations Sustainable
           Development Solutions Network in partnership with the Ernesto Illy 
           Foundation.  The World Happiness Report contains data from 2008 to 2018.  
           We intend to target health care professionals who review related evidence
           pertaining to our selected variables and happiness.  Our audience wants 
           to learn about the different facets which can affect happiness and if 
           social support or life expectancy can have a significant influence.")
  })
  
  output$happy <- renderPlot({
    happy_plot <- gg
    plot(happy_plot)
  })
  
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
  
  output$conclusion <- renderText({
    paste0("Conclusions will go here")
  })
}

shinyApp(ui = ui, server = server)