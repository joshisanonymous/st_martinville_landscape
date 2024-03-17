# Packages --------------------------------------------------------------------
library(osmdata)

# Map variables----------------------------------------------------------------
town <- opq(getbb("St. Martinville"))
streets <- osmdata_sf(add_osm_feature(town, key = c("highway")))
water <- osmdata_sf(add_osm_feature(town, key = c("water")))
# Subsets
signs_english <- signs[signs$Language == "English", ]
signs_french <- signs[signs$Language == "French", ]
signs_bilingual <- signs[signs$Language == "French-English", ]

# Maps ------------------------------------------------------------------------
map <- ggplot() +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "steelblue") +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE) +
  geom_point(data = signs_english,
             mapping = aes(x = GPSLongitude,
                           y = GPSLatitude),
             color = "yellow",
             inherit.aes = FALSE) +
  geom_point(data = signs_french,
             mapping = aes(x = GPSLongitude,
                           y = GPSLatitude),
             color = "red",
             inherit.aes = FALSE) +
  geom_point(data = signs_bilingual,
             mapping = aes(x = GPSLongitude,
                           y = GPSLatitude),
             color = "orange",
             inherit.aes = FALSE) +
  coord_sf(ylim = c(30.10, 30.15),
           xlim = c(-91.85, -91.80),
           expand = FALSE) +
  theme_void()
