## ---- Run ------------------------------------------------------------------
# Packages -------------------------------------------------------------------
library(ggplot2)
library(car)
library(tidyr)
library(dplyr)

# Variables ------------------------------------------------------------------
dir <- "../data/"
file <- "signs.csv"
cols_as_char <- c("Text", "SourceFile1", "SourceFile2")

# Data -----------------------------------------------------------------------
signs <- read.csv(paste(dir, file, sep = ""), sep = "\t",
                  as.is = cols_as_char, encoding = "UTF-8")

# Order factor levels
signs$Area <- factor(signs$Area, levels = c(
  "Regular", "Cultural District", "Historic District"
))
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

# Subset
signs_binomial <- signs
levels(signs_binomial$Language) <- list(
  "Not French" = c("Ambiguous", "English", "Franglais-English", "Latin",
                   "Spanish", "Spanish-English"),
  "French" = c("French", "French-English")
)

# Models ----------------------------------------------------------------------

# Binomial logistic regression, "Not French" as the reference level
# Use and Essential are perfectly linear, so only one is used
model <- glm(Language ~ Racial.Area + Area + Displayer + Type + Essential,
             data = signs_binomial,
             family = binomial)

# Maps -----------------------------------------------------------------------
source("maps.R")

# Graphs ----------------------------------------------------------------------
source("graphs.R")
