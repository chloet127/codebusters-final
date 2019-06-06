library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")
library("plotly")
library("stringr")
library("wrapr")

happinessdata <- as.data.frame(read.xls("./data/world-happiness.xls"))
## Top 10 and Bottom 10 Countries in happiness
data <- filter(happinessdata, Country.name %in% c('Finland', 'Denmark', 'Norway', 'Iceland', 'Netherlands', 'Switzerland', 'Sweden', 'New Zealand', 'Canada', 'Austria',
               'Haiti', 'Botswana', 'Syria', 'Malawi', 'Yemen', 'Rwanda', 'Tanzania', 'Afghanistan', 'Central African Republic', 'South Sudan'))

## Application Title
"Social Support and Life Expectancy"
titlePanel("Top 10 / Bottom 10")

ui <- sidebarLayout(
  sidebarPanel(
    sliderInput("chosenYears",
                "Select a Range of Years:",
                min = min(happinessdata$Year),
                max = max(happinessdata$Year),
                value = c(min(happinessdata$Year), max(happinessdata$Year)),
                sep = "",
                step = 1)
  ),
  
  mainPanel(
    plotOutput("Top10BottomPlot")
  )
)

server <- function(input, output) {
Year_chosen <- reactive({
  data %>%
    filter(data$Year == input$chosenYears)
})

output$Top10BottomPlot <- renderPlot({
  ggplot(data = Year_chosen() %>% ## NONE NUMERIC ARUGMENT TO BINARY OPERATOR SHOWS EVERY
         ## tried to change x and y names
         aes(x = data$`Social support`, y = data$`Healthy life expectancy at birth`, shape = as.factor(am), color = as.factor(am)) +
         geom_point(size = 3) +
         scale_color_manual(values = "#6699FF") +
         labs(title = "Top 10 / Bottom 10",
              x = "Social Support",
              y = "Life Expectancy"),
)
  ## Hover text
      plot_ly (type = 'scatterplot', mode = 'marker') %>% 
      add_trace(x = c(0:1.0), y = rnorm(100, mean = 50),
                hoverinfo = 'text',
                marker = list(color='green'),
                showlegend = F
    ) %>% 
    layout(
      title = "Top 10 / Bottom 10",
      titlefont = list(
        size = 10
      ),
      showlegend = F,
      xaxis = list(
        zeroline = F
      ),
      yaxis = list(
        hoverformat = '.2f'
      )
  )
})
}
shinyApp(ui = ui, server = server)