library(dplyr)

#read in data
data1 <- read_excel("data/world-happiness.xls")
names(data1) <- gsub(" ", ".", names(data1))
world2 <- read_excel("data/world-happiness.xls", sheet = 2)
names(world2) <- gsub(" ", ".", names(world2))

#filter for relevant data + rename columns for easier access
tb <- filter(data1, Country.name %in% 
                            c('Finland', 'Denmark', 'Norway', 'Iceland', 'Netherlands', 
                              'Switzerland', 'Sweden', 'New Zealand', 'Canada', 'Austria',
                              'Haiti', 'Botswana', 'Syria', 'Malawi', 'Yemen', 'Rwanda', 
                              'Tanzania', 'Afghanistan', 'Central African Republic', 
                              'South Sudan')) %>%
      filter(Year == 2018) %>%
      select(Country.name, Healthy.life.expectancy.at.birth) %>% 
      rename(life = Healthy.life.expectancy.at.birth)

ss <- filter(data1, Country.name %in% 
               c('Finland', 'Denmark', 'Norway', 'Iceland', 'Netherlands', 
                 'Switzerland', 'Sweden', 'New Zealand', 'Canada', 'Austria',
                 'Haiti', 'Botswana', 'Syria', 'Malawi', 'Yemen', 'Rwanda', 
                 'Tanzania', 'Afghanistan', 'Central African Republic', 
                 'South Sudan')) %>%
  filter(Year == 2018) %>%
  select(Country.name, Social.support) %>% 
  rename(social = Social.support)

#match happiness scores to countries
world2 <- select(world2, Country, Happiness.score)
tb$happiness <- world2$Happiness.score[ match(tb$Country.name, world2$Country)]
ss$happiness <- world2$Happiness.score[ match(ss$Country.name, world2$Country)]

#plot, with distinctly colored intervals
p <- ggplot(tb, aes(x=life, y=happiness)) +
        geom_point(aes(color = cut(happiness, c(10, 6, 0), size=3))) + 
        xlab("Healthy Life Expectancy at Birth") + 
        ylab("Happiness Score")
p <- p + scale_color_hue(labels = c("Bottom 10", "Top 10"))
p <- p + labs(color = "")

p


sup <- ggplot(ss, aes(x = social, y = happiness)) +
        geom_point(aes(color = cut(happiness, c(10, 6, 0), size=3))) + 
        xlab("Social Support") + 
        ylab("Happiness Score")
sup <- sup + scale_color_hue(labels = c("Bottom 10", "Top 10"))
sup <- sup + labs(color = "")

sup