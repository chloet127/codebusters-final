library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)
library(rworldmap)


world2 <- read_excel("data/world-happiness.xls", sheet = 2)
names(world2) <- gsub(" ", "_", names(world2))
world2$Country[world2$Country == "United States"] = "USA"
world2$Country[world2$Country == "United Kingdom"] = "UK"
world2$Country[world2$Country == "Taiwan Province of China"] = "Taiwan"


world2 <- select(world2, Country, Happiness_score)


map.world <- map_data(map="world")
map.world$happiness <- world2$Happiness_score[ match(map.world$region, world2$Country)]


gg <- ggplot()
gg <- gg + theme(legend.position="right")
gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=happiness))

gg <- gg + scale_fill_gradient(low = "black", high = "#6699FF", guide = "colourbar")
gg <- gg + xlab("Longitude")
gg <- gg + ylab("Latitude")
gg <- gg + coord_equal()
gg
