library("gdata")
library("shiny")
library("shinythemes")
library("dplyr")
library("ggplot2")
library("plotly")
library("stringr")

happinessdata <- as.data.frame(read.xls("./data/world-happiness.xls", verbose = FALSE))
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
  ggplot(data = Year_chosen(), 
         aes(x = Social.support, y = Healthy.life.expectancy.at.birth, shape = as.factor(am), color = as.factor(am)) +
         geom_point(size = 3) +
         scale_color_manual(values = "#6699FF") +
         labs(title = "Top 10 / Bottom 10",
              x = "Social Support",
              y = "Life Expectancy"))
  
  plot(x, y, main = "Top 10 / Bottom 10",
       xlab = Social.support, ylab = Healthy.life.expectancy.at.birth,
       pch = 19, frame = FALSE) %>%
    add_trace(
      x = c(1:100), 
      y = rnorm(100, mean = 5), 
      marker = list(color='green'),
      hoverinfo = 'y',
      showlegend = F
    ) %>%
    layout(
      title = "Top 10 / Bottom 10",
      titlefont = list(
        size = 10
      ),
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