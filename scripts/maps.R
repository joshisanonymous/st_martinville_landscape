# Packages --------------------------------------------------------------------
library(osmdata)

# Map variables----------------------------------------------------------------
sign_size <- 2
sign_colors <- c("#619CFF", "#00BA38", "#F8766D")
town <- opq(getbb("St. Martinville"))
streets <- osmdata_sf(add_osm_feature(town, key = c("highway")))
water <- osmdata_sf(add_osm_feature(town, key = c("water")))
historic_dist <- data.frame(
  x = c(-93.81760416553334, -93.81760416553334, -89.6886555668733, -89.6886555668733),
  y = c(29.24004503400145, 32.81866418073393, 32.81866418073393, 29.24004503400145)
)

# Subsets
signs_english <- signs[signs$Language == "English", ]
signs_french <- signs[signs$Language == "French", ]
signs_bilingual <- signs[signs$Language == "French-English", ]

# Maps ------------------------------------------------------------------------
map <- ggplot() +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "steelblue") +
  geom_sf(data = streets$osm_lines %>%
            filter(highway %in% c("residential","tertiary", "secondary",
                                  "tertiary_link","secondary_link",
                                  "primary_link","trunk_link")),
          inherit.aes = FALSE,
          linewidth = 2,
          color = "darkgrey") +
  geom_sf(data = streets$osm_lines %>%
            filter(highway %in% c("footway","track","path")),
          inherit.aes = FALSE,
          linewidth = 1,
          color = "lightgrey") +
  geom_sf(data = streets$osm_lines %>%
            filter(highway %in% c("primary","trunk")),
          inherit.aes = FALSE,
          linewidth = 3,
          color = "orange") +
  geom_polygon(data = historic_dist, aes(x = x, y = y, fill = "pink")) +
  geom_point(data = signs_english,
             mapping = aes(x = GPSLongitude,
                           y = GPSLatitude),
             fill = sign_colors[1],
             color = "black",
             shape = 21,
             inherit.aes = FALSE,
             size = sign_size) +
  geom_point(data = signs_bilingual,
             mapping = aes(x = GPSLongitude,
                           y = GPSLatitude),
             fill = sign_colors[2],
             color = "black",
             shape = 21,
             inherit.aes = FALSE,
             size = sign_size) +
  geom_point(data = signs_french,
             mapping = aes(x = GPSLongitude,
                           y = GPSLatitude),
             fill = sign_colors[3],
             color = "black",
             shape = 21,
             inherit.aes = FALSE,
             size = sign_size) +
  coord_sf(ylim = c(30.10, 30.15),
           xlim = c(-91.85, -91.80),
           expand = FALSE) +
  geom_text(aes(label = "Saint Martinville,\nLousiana"),
            y = 30.127, x = -91.81, size = 10) +
  theme_void()
map
