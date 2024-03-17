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
  coord_cartesian(ylim = c(0, 75))

graph_displayer <- signs_import_langs %>%
  mutate(Language, Displayer) %>%
  count(Language, Displayer, .drop = FALSE) %>%
  ggplot(aes(x = Displayer, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, 100))

graph_type <- signs_import_langs %>%
  mutate(Language, Type) %>%
  count(Language, Type, .drop = FALSE) %>%
  ggplot(aes(x = Type, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, 125))

# Adjust districts to be one column with separate designations for area type
# rather than separate yes-no columns for each area type then facet graphs
graph_race <- signs_import_langs %>%
  mutate(Language, Racial.Area) %>%
  count(Language, Racial.Area, .drop = FALSE) %>%
  ggplot(aes(x = Racial.Area, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, 300))

graph_culture <- signs_import_langs %>%
  mutate(Language, Cultural.District) %>%
  count(Language, Cultural.District, .drop = FALSE) %>%
  ggplot(aes(x = Cultural.District, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, 300))

graph_history <- signs_import_langs %>%
  mutate(Language, Historic.District) %>%
  count(Language, Historic.District, .drop = FALSE) %>%
  ggplot(aes(x = Historic.District, y = n)) +
  geom_col(aes(fill = Language),
           position = position_dodge(preserve = "single")) +
  coord_cartesian(ylim = c(0, 300))