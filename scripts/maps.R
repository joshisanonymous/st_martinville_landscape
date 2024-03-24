# Packages --------------------------------------------------------------------
library(osmdata)
library(sf)

# Map variables----------------------------------------------------------------
sign_size <- 3
sign_colors <- c("#619CFF", "#00BA38", "#F8766D")

# Louisiana
louisiana <- opq(getbb("Louisiana")) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = c("4", "6", "8")) |>
  osmdata_sf()
state <- subset(louisiana$osm_multipolygons,
                admin_level == "4" & name == "Louisiana")
parishes <- louisiana$osm_multipolygons |>
  st_filter(state, .predicate=st_within) |>
  subset(admin_level == 6)
acadiana <- subset(parishes,
  name == "Calcasieu Parish" | name == "Cameron Parish" | name == "Jefferson Davis Parish" |
  name == "Vermilion Parish" | name == "Acadia Parish" | name == "Evangeline Parish" |
  name == "Avoyelles Parish" | name == "St. Landry Parish" | name == "Lafayette Parish" |
  name == "Saint Martin Parish" | name == "Iberia Parish" | name == "St. Mary Parish" |
  name == "Assumption Parish" | name == "Iberville Parish" | name == "Pointe Coupee Parish" |
  name == "West Baton Rouge Parish" | name == "Ascension Parish" | name == "St. James Parish" |
  name == "St. John the Baptist Parish" | name == "St. Charles Parish" |
  name == "Lafourche Parish" | name == "Terrebonne Parish")
major_cities <- subset(louisiana$osm_multipolygons, admin_level == "8" &
                       name == "Lafayette" | name == "New Orleans" |
                       name == "Baton Rouge" | name == "St. Martinville")
major_cities_coords <- data.frame(
  "x" = c(-89.62518, -91.81187 + 0.2, -91.97133, -90.99934),
  "y" = c(30.19947, 30.14827, 30.29646, 30.55898)
)

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
historic_dist_all <- signs[signs$Area == "Historic District", ]
historic_dist <- historic_dist_all[
  historic_dist_all$GPSLongitude == max(historic_dist_all$GPSLongitude) |
  historic_dist_all$GPSLongitude == min(historic_dist_all$GPSLongitude) |
  historic_dist_all$GPSLatitude == max(historic_dist_all$GPSLatitude) |
  historic_dist_all$GPSLatitude == min(historic_dist_all$GPSLatitude), ]
cultural_dist_all <- signs[signs$Area == "Cultural District", ]
cultural_dist <- cultural_dist_all[
  cultural_dist_all$GPSLongitude == max(cultural_dist_all$GPSLongitude) |
    cultural_dist_all$GPSLongitude == min(cultural_dist_all$GPSLongitude) |
    cultural_dist_all$GPSLatitude == max(cultural_dist_all$GPSLatitude) |
    cultural_dist_all$GPSLatitude == min(cultural_dist_all$GPSLatitude), ]

# Subsets
signs_english <- signs[signs$Language == "English", ]
signs_french <- signs[signs$Language == "French", ]
signs_bilingual <- signs[signs$Language == "French-English", ]

# Maps ------------------------------------------------------------------------
mapsm <- ggplot() +
  geom_sf(data = city$osm_multipolygons,
          fill = "lightyellow2",
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
  theme_void()

mapsm_norm <- mapsm +
  geom_text(aes(label = "Saint Martinville, LA"),
            y = 30.138, x = -91.838,
            size = 6, fontface = "bold") +
  coord_sf(ylim = c(30.10, 30.15),
           xlim = c(-91.85, -91.80),
           expand = FALSE)

mapsm_norm_dists <- mapsm_norm +
  geom_polygon(data = cultural_dist,
               aes(x = GPSLongitude, y = GPSLatitude),
               fill = "yellow", alpha = 0.5) +
  geom_polygon(data = historic_dist,
               aes(x = GPSLongitude, y = GPSLatitude),
               fill = "tan2", alpha = 0.5)

mapsm_norm_signs <- mapsm_norm_dists +
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
             size = sign_size)

mapsm_dists <- mapsm +
  coord_sf(ylim = c(30.117, 30.129),
           xlim = c(-91.836, -91.821),
           expand = FALSE) +
  geom_polygon(data = cultural_dist,
               aes(x = GPSLongitude, y = GPSLatitude),
               fill = "yellow", alpha = 0.5) +
  geom_polygon(data = historic_dist,
               aes(x = GPSLongitude, y = GPSLatitude),
               fill = "tan2", alpha = 0.5)

mapsm_dists_signs <- mapsm_dists +
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
             size = sign_size)

mapla <- ggplot() +
  geom_sf(data = state) +
  geom_sf(data = parishes) +
  theme_void()

mapacadiana <- mapla +
  geom_sf(data = acadiana,
          fill = "#F8766D") +
  geom_sf(data = major_cities,
          fill = "#00BA38") +
  geom_text(data = major_cities,
    aes(label = name),
    y = major_cities_coords$y + 0.05, x = major_cities_coords$x + 0.175,
    size = 5, fontface = "bold") +
  theme_void()
