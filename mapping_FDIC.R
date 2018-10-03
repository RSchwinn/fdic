# 
# FDIC_df = readRDS("FDIC_df.RDS")
# library(ggmap)
# library(dplyr)
# FDIC_df = FDIC_df %>%
#     mutate(small = ifelse(asset < 10 ^ 7, 1, 0))
# 
# addresses = NULL
# for (i in 1:nrow(FDIC_df)) {
#     new_address = geocode(
#         FDIC_df$full_address[i],
#         output = "latlona",
#         source = "dsk",
#         override_limit = T
#     )
#     if (ncol(new_address) == 3) {
#         addresses = rbind(addresses, new_address)
#     }else{
#         addresses = rbind(addresses, c(NA, NA, NA))
#     }
# }
# FDIC_df = cbind(FDIC_df, addresses)
# 
# saveRDS(FDIC_df, "FDIC_geocoded.RDS")
# 

complete_df = readRDS("FDIC_geocoded.RDS")
library(ggmap)
library(dplyr)


df = complete_df %>%
    filter(lon, lat, asset, name)

df = df[complete.cases(df),]

options(scipen = 99)

library(leaflet)

leaflet(data = df) %>% 
    addTiles() %>%
    addCircles(lng = ~lon, lat = ~lat, weight = 1,
               radius = ~sqrt(asset) * 30, popup = ~paste0(name,"\n",1000*asset)
    )



# ggplot() + coord_map() + geom_point(data=df, aes(x=lon, y=lat, size = asset, alpha = 0.01), color = "steelblue")
# 
#
