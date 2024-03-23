# Variables -------------------------------------------------------------------
essential <- c("bold.italic", "bold.italic", "bold.italic", "bold.italic",
               "plain", "plain", "plain", "bold.italic", "plain")
graph_use_ylim <-  75
graph_disp_ylim <- 100
graph_type_ylim <- 125
graph_race_ylim <- 300
graph_area_ylim <- 300
ylabel <- "Count"

# Subset ----------------------------------------------------------------------
signs_import_langs <- droplevels(signs[signs$Language == "French" |
                                         signs$Language == "French-English" |
                                         signs$Language == "English", ])
signs_import_langs$Language <- factor(
  signs_import_langs$Language, levels = c("French", "French-English", "English")
)

# Graphs ----------------------------------------------------------------------
graph_use <- signs_import_langs %>%
  mutate(Language, Use) %>%
  count(Language, Use, .drop = FALSE) %>%
  ggplot(aes(x = Use, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, graph_use_ylim)) +
  theme(axis.text.x = element_markdown(face = essential)) +
  geom_text(aes(label = ifelse(Language == "English" & n > graph_use_ylim, n, NA),
                y = graph_use_ylim,
                group = Language),
            position = position_dodge(width = 1)) +
  ylab(ylabel) +
  xlab("Use (essential in italics)")

graph_displayer <- signs_import_langs %>%
  mutate(Language, Displayer) %>%
  count(Language, Displayer, .drop = FALSE) %>%
  ggplot(aes(x = Displayer, y = n)) +
    geom_col(aes(fill = Language),
             position = position_dodge(preserve = "single")) +
    coord_cartesian(ylim = c(0, graph_disp_ylim)) +
    geom_text(aes(label = ifelse(Language == "English" & n > graph_disp_ylim, n, NA),
                  y = graph_disp_ylim,
                  group = Language),
              position = position_dodge(width = 1)) +
  ylab(ylabel)

graph_type <- signs_import_langs %>%
  mutate(Language, Type) %>%
  count(Language, Type, .drop = FALSE) %>%
  ggplot(aes(x = Type, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, graph_type_ylim)) +
  geom_text(aes(label = ifelse(Language == "English" & n > graph_type_ylim, n, NA),
                y = graph_type_ylim,
                group = Language),
            position = position_dodge(width = 1)) +
  ylab(ylabel)

graph_race <- signs_import_langs %>%
  mutate(Language, Racial.Area) %>%
  count(Language, Racial.Area, .drop = FALSE) %>%
  ggplot(aes(x = Racial.Area, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, graph_race_ylim)) +
  geom_text(aes(label = ifelse(Language == "English" & n > graph_race_ylim, n, NA),
                y = graph_race_ylim,
                group = Language),
            position = position_dodge(width = 1)) +
  ylab(ylabel)

graph_area <- signs_import_langs %>%
  mutate(Language, Area) %>%
  count(Language, Area, .drop = FALSE) %>%
  ggplot(aes(x = Area, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, graph_area_ylim)) +
  geom_text(aes(label = ifelse(Language == "English" & n > graph_area_ylim, n, NA),
                y = graph_area_ylim,
                group = Language),
            position = position_dodge(width = 1)) +
  ylab(ylabel)
