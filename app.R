library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")
library("plotly")
library("stringr")
library("readxl")
library("rworldmap")
source("chloe-data.R")
source("scatter2.R")

data <- as.data.frame(read.xls("./data/world-happiness.xls", verbose = FALSE))

intro_page <- tabPanel(
  "Introduction",
  titlePanel("World Happiness Report"),
  textOutput("introduction")
)

page_happy <- tabPanel(
  "World Map",
  titlePanel("Map of World Happiness"),
  textOutput("rank"),
  mainPanel(
    plotOutput(outputId = "happy", width = "800px", height = "400px")
  )
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

top_bottom_page <- tabPanel(
  "2018",
  titlePanel("Top 10 / Bottom 10"),
  
  splitLayout(
    plotOutput("SupportHappinessPlot"),
    plotOutput("LifeHappinessPlot")
  )
)

conclusion_page <- tabPanel(
  "Conclusion",
  titlePanel("World Happiness Report"),
  
  verticalLayout(
    textOutput("sum2018"),
    textOutput("sumGraphs"),
    textOutput("conclusion")
  )
)


ui <- fluidPage(
  theme = shinytheme("journal"),
  navbarPage(
    "Happiness",
    intro_page,
    page_happy,
    page_one,
    page_two,
    top_bottom_page,
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
    gg
  })
  
  output$rank <- renderText({
    paste0("The rankings of national happiness are based on a Cantril ladder survey.
           Nationally representative samples of respondents are asked to think of a 
           ladder, with the best possible life for them being a 10, and the worst 
           possible life being a 0. They are then asked to rate their own current 
           lives on that 0 to 10 scale. The report correlates the results with 
           various life factors. The plot below shows data for the most recent 
           rankings for the year 2019, with Finland ranking the highest with 
           a happiness score of 7.7, and South Sudan with the lowest at 2.8.")
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
  
  chosen_data2 <- reactive({
    top_bottom_data %>%
      filter(top_bottom_data$Year == input$chosenYear)
  })
  
  output$SupportHappinessPlot <- renderPlot({
    sup
  })
  
  output$LifeHappinessPlot <- renderPlot({
    p
  })
  
  output$sum2018 <- renderText({
    paste0("For 2018, our plots show that the top 10 happiest countries are much 
           higher in both social support and life expectancy than the bottom 10.  
           There appears to be a positive correlation between social support and 
           happiness score as well as life expectancy and happiness score.  While 
           there are these correlations, it is not possible to say that they 
           cause happiness.")
  })
  
  output$sumGraphs <- renderText({
    paste0("When looking at the line graphs, social support tends to fluctate for 
           many of the countries, likely due to changes in government leadership 
           or war state of the country.  Life expectancy increases in most of the 
           countries--explained by modern advancements in healthcare and technology.")
  })
  
  output$conclusion <- renderText({
    paste0("Overall, most countries improve in both social support and life expectancy, 
           but the happiest countries tend to be higher for those variables.")
  })
}

shinyApp(ui = ui, server = server)
