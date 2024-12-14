library(openxlsx)
library(tidyverse)
library(gtools)

df <- read.xlsx('others/theme-analysis/theme-analysis-test.xlsx')

# pivot table so there is a record for each unique pair of juris & theme
df_piv <- df |> 
  pivot_longer(cols = colnames(df),
               names_to = 'theme',
               values_to = 'juris',
               values_transform = as.character) |> 
  mutate(theme = str_trim(theme)) |> 
  drop_na(juris)

df_piv2 <- df_piv |> 
  nest(list_of_juris = juris)

# create all possible unique theme combinations in groups of 3
themes <- unique(df_piv$theme)
grid <- combinations(length(themes), 3, themes) |> 
  as_tibble() |> 
  rowwise() |> 
  mutate(combos = list(c(V1, V2, V3))) |> 
  unite(combo_label, V1, V2, V3, sep = ", ", remove = FALSE)

# initiate main table

# loop through each combo
for(i in 1:length(grid$combos)) {
# for(i in 1:1) {
  print(grid$combos[[i]])
  
  # filter for combo
  df_f <- df_piv2 |> 
    filter(theme %in% grid$combos[[i]]) |> 
    mutate(combo_label = paste(grid$combos[[i]], collapse = ", "))
  print(df_f)
  
  # unpack tibbles (list of jurisdictions per theme)
  df_unnest <- df_f |> 
    unnest(list_of_juris)
  
  df_summary <- df_unnest |> 
    group_by(juris, combo_label) |>
    summarise(num_themes = n()) |> 
    arrange(desc(num_themes))

  # find specific theme each city belongs to
  juris_theme <- df_piv |> 
    filter(theme %in% grid$combos[[i]]) |> 
    nest(theme = theme) |> 
    unnest_wider(theme)
  
  df_join <- df_summary |> 
    left_join(juris_theme, by = 'juris')
  
  # append to main table
}


