library(dplyr)
library(ggplot2)
library(shiny)
library(readxl)
library(rworldmap)

#read in dataset, change names of columns to have underscores instead of spaces
world2 <- read_excel("data/world-happiness.xls", sheet = 2)
names(world2) <- gsub(" ", "_", names(world2))

#rename some country names to match the world data
world2$Country[world2$Country == "United States"] = "USA"
world2$Country[world2$Country == "United Kingdom"] = "UK"
world2$Country[world2$Country == "Taiwan Province of China"] = "Taiwan"

#select relevant columns
world2 <- select(world2, Country, Happiness_score)

#create world data, fill in happiness scores to match
map.world <- map_data(map="world")
map.world$happiness <- world2$Happiness_score[ match(map.world$region, world2$Country)]


#create plot with world data and fill based on happiness score
gg <- ggplot()
gg <- gg + theme(legend.position="right")
gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, fill=happiness))
gg <- gg + expand_limits(x = map.world$long, y = map.world$lat)

gg <- gg + scale_fill_gradient(low = "black", high = "#6699FF", 
                              guide = "colourbar", name = "Happiness Score")
gg <- gg + xlab("Longitude")
gg <- gg + ylab("Latitude")
gg <- gg + coord_equal()

