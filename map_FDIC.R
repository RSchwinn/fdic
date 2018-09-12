
complete_df = readRDS("FDIC_geocoded.RDS")
library(ggmap)
library(dplyr)

df = complete_df[,c("lon","lat","asset")]
df = df[complete.cases(df),]

options(scipen = 99)

library(leaflet)

leaflet(complete_df) %>% addTiles() %>%
    addCircles(lng = ~lon, lat = ~lat, weight = 1,
               radius = ~sqrt(asset) * 30, popup = ~name
    )
