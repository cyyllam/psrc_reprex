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

# create all possible theme combinations in groups of 3
themes <- unique(df_piv$theme)
grid <- combinations(13, 3, themes) |> 
  as_tibble() |> 
  rowwise() |> 
  mutate(combos = list(c(V1, V2, V3)))

# for(i in 1:length(grid$combos)) {
for(i in 1:5) {
  print(grid$combos[[i]])
  df_f <- df_piv2 |> 
    filter(theme %in% grid$combos[[i]])
  print(df_f)
}
