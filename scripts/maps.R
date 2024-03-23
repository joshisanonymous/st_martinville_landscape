# Packages --------------------------------------------------------------------
library(osmdata)
library(sf)

# Map variables----------------------------------------------------------------
sign_size <- 2
sign_colors <- c("#619CFF", "#00BA38", "#F8766D")

# Louisiana
louisiana <- opq(getbb("Louisiana")) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = c("4", "6")) |>
  osmdata_sf()
state <- subset(louisiana$osm_multipolygons,
                admin_level == "4" & name == "Louisiana")
parishes <- louisiana$osm_multipolygons |>
  st_filter(state, .predicate=st_within) |>
  subset(admin_level == 6)

# St Martinville
stmartinville <- opq(getbb("St. Martinville"))
city <- osmdata_sf(add_osm_feature(stmartinville, key = "admin_level", value = "8"))
streets_foot <- osmdata_sf(add_osm_feature(
  stmartinville, key = "highway", value = c("footway","track","path")))
streets_reg <- osmdata_sf(add_osm_feature(
  stmartinville, key = "highway", value = c("residential","tertiary", "secondary",
                                            "tertiary_link","secondary_link",
                                            "primary_link","trunk_link")))
streets_main <- osmdata_sf(add_osm_feature(
  stmartinville, key = "highway", value = c("primary","trunk")))
water <- osmdata_sf(add_osm_feature(stmartinville, key = "water"))
# historic_dist <- data.frame(
#   x = c(-93.81760416553334, -93.81760416553334, -89.6886555668733, -89.6886555668733),
#   y = c(29.24004503400145, 32.81866418073393, 32.81866418073393, 29.24004503400145)
# )

# Subsets
signs_english <- signs[signs$Language == "English", ]
signs_french <- signs[signs$Language == "French", ]
signs_bilingual <- signs[signs$Language == "French-English", ]

# Maps ------------------------------------------------------------------------
mapsm <- ggplot() +
  geom_sf(data = city$osm_multipolygons,
          fill = "lightyellow",
          color = "yellow") +
  geom_sf(data = water$osm_multipolygons,
          inherit.aes = FALSE,
          fill = "steelblue") +
  geom_sf(data = streets_foot$osm_lines,
          inherit.aes = FALSE,
          linewidth = 1,
          color = "lightgrey") +
  geom_sf(data = streets_reg$osm_lines,
          inherit.aes = FALSE,
          linewidth = 2,
          color = "darkgrey") +
  geom_sf(data = streets_main$osm_lines,
          inherit.aes = FALSE,
          linewidth = 3,
          color = "orange") +
  # geom_polygon(data = historic_dist, aes(x = x, y = y, fill = "pink")) +
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
  theme_void()

mapsm_norm <- mapsm +
  geom_textbox(aes(label = "Saint Martinville, LA"),
               y = 30.138, x = -91.838, size = 6,
               width = unit(2.25, "inch"),
               fill = "lightgreen") +
  coord_sf(ylim = c(30.10, 30.15),
           xlim = c(-91.85, -91.80),
           expand = FALSE)

mapsm_dists <- mapsm +
  coord_sf(ylim = c(30.117, 30.129),
           xlim = c(-91.836, -91.821),
           expand = FALSE)

mapla <- ggplot() +
  geom_sf(data = state) +
  geom_sf(data = parishes) +
  theme_void()
