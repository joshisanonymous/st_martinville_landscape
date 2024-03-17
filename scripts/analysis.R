# Packages -------------------------------------------------------------------
library(osmdata)
library(ggplot2)
library(car)

# Variables ------------------------------------------------------------------
dir <- "../data/"
file <- "signs.csv"
cols_as_char <- c("Text", "SourceFile1", "SourceFile2")
# Map vars
town <- opq(getbb("St. Martinville"))
streets <- osmdata_sf(add_osm_feature(town, key = c("highway")))
water <- osmdata_sf(add_osm_feature(town, key = c("water")))

# Data -----------------------------------------------------------------------
signs <- read.csv(paste(dir, file, sep = ""), sep = "\t",
                  as.is = cols_as_char, encoding = "UTF-8")
# Order factor levels
signs$Displayer <- factor(signs$Displayer, levels = c(
  "Government", "Non-local Business", "Non-Profit",
  "Religious Group", "Residence", "Local Business"
))
signs$Use <- factor(signs$Use, levels = c(
  "Warning", "Instruction", "Schedule", "Menu", "Transgression",
  "Marketing", "Decoration", "Placing", "Culture"
))
signs$Type <- factor(signs$Type, levels = c(
  "Metal", "Imprint", "Plaque", "Board", "Tag", "Poster",
  "Document", "Graffiti", "Item"
))
signs$Essential <- factor(signs$Essential, levels = c(
  "Yes", "No"
))
# Subsets
signs_english <- signs[signs$Language == "English", ]
signs_french <- signs[signs$Language == "French", ]
signs_bilingual <- signs[signs$Language == "French-English", ]
signs_import_langs <- droplevels(signs[signs$Language == "French" |
                                 signs$Language == "French-English" |
                                 signs$Language == "English", ])
signs_import_langs$Language <- factor(
  signs_import_langs$Language, levels = c("French", "French-English", "English")
)
signs_binomial <- signs
levels(signs_binomial$Language) <- list(
  "Not French" = c("Ambiguous", "English", "Franglais-English", "Latin",
                   "Spanish", "Spanish-English"),
  "French" = c("French", "French-English")
)

# Models ----------------------------------------------------------------------

# Binomial logistic regression, "Not French" as the reference level
# Use and Essential are perfectly linear, so only one is used
model <- glm(Language ~
               Racial.Area + Cultural.District + Historic.District +
               Displayer + Type + Essential,
             data = signs_binomial,
             family = binomial)

# Maps -----------------------------------------------------------------------
ggplot() +
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

# Graphs ----------------------------------------------------------------------
ggplot(signs_import_langs,
       aes(x = Use)) +
  geom_bar(aes(fill = Language),
           position = "dodge")
