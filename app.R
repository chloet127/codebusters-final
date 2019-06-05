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

#data <- as.data.frame(read.xls("./data/world-happiness.xls", verbose = FALSE))
data <- read_excel("data/world-happiness.xls")
names(data) <- gsub(" ", ".", names(data))

## Top 10 and Bottom 10 Countries in happiness
top_bottom_data <- filter(data, Country.name %in% 
                          c('Finland', 'Denmark', 'Norway', 'Iceland', 'Netherlands', 
                            'Switzerland', 'Sweden', 'New Zealand', 'Canada', 'Austria',
                            'Haiti', 'Botswana', 'Syria', 'Malawi', 'Yemen', 'Rwanda', 
                            'Tanzania', 'Afghanistan', 'Central African Republic', 
                            'South Sudan'))

intro_page <- tabPanel(
  "Introduction",
  titlePanel("World Happiness Report"),
  textOutput("introduction")
)

page_happy <- tabPanel(
  "World Map",
  titlePanel("Map of World Happiness"),
  plotOutput(outputId = "happy", width = "1200px", height = "400px"),
  textOutput("rank")
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

scatters <- tabPanel(
  "Highs & Lows",
  titlePanel("Top 5 & Bottom 5 Countries"),
  fluidRow(
    column(2,
      tableOutput(outputId = "le_tab")
    ),
    column(2,
      tableOutput(outputId = "ss_tab")
    ),
    column(4,
      plotOutput(outputId = "le_plot", width = "370px", height = "400px")
    ),
    column(4,
      plotOutput(outputId = "ss_plot", width = "460px", height = "400px")
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
    "Happiness",
    intro_page,
    page_happy,
    page_one,
    page_two,
    scatters,
    conclusion_page
  ),
  
  inlineCSS(list(
    "#introduction" = "font-size:30px; color:#25385e",
    "#conclusion" = "font-size:30px; color:#25385e"
  ))
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
  
  output$ss_plot <- renderPlot({
    sup
  })
  
  output$ss_tab <- renderTable({
    ss_table
  })
  
  output$le_plot <- renderPlot({
    p
  })
  
  output$le_tab <- renderTable({
    tb_table
  })
  
  output$rank <- renderText({
    paste0("The rankings of national happiness are based on a Cantril ladder survey.
           Nationally representative samples of respondents are asked to think of a 
           ladder, with the best possible life for them being a 10, and the worst 
           possible life being a 0. They are then asked to rate their own current 
           lives on that 0 to 10 scale. The report correlates the results with 
           various life factors. The plot above shows data for the most recent 
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
      scale_color_manual(labels = input$chosenCountry, values = "#6699FF") +
    labs(title = "Social support levels over time",
         x = "Year",
         y = "Social support") + 
    theme(legend.position="")
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
      scale_color_manual(labels = input$chosen_Country, values = "#6699FF") +
    labs(title = "Life expectancy over time",
         x = "Year",
         y = "Life Expectancy") +
    theme(legend.position="")
  })
  
  chosen_data2 <- reactive({
    top_bottom_data %>%
      filter(top_bottom_data$Year == input$chosenYear)
  })
  
  output$conclusion <- renderText({
    paste0("Overall, most countries, whether top 5 or bottom 5 in happiness, 
           showed increasing life expectancy over the years.  However, the top 
           5 countries still had higher life expectancies.  Social support 
           fluctuated, likely due to changes in government or war state of the 
           country.  Because social support changes throughout the years in 
           all of the countries, it is difficult to draw a conclusion that it 
           directly affects the happiness of the country's citizens.  For life 
           expectancy, although it is generally higher in the top 5 countries, 
           that may be due to other factors such as healthcare or technological 
           advances.  While higher life expectancy correlates with happier 
           countries, it is not possible to say that it causes happiness.")
  })
}

shinyApp(ui = ui, server = server)
