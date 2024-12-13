library(openxlsx)
library(tidyverse)

df <- read.xlsx('others/theme-analysis/theme-analysis-test.xlsx')

# pivot table so there is a record for each unique pair of juris & theme
df_piv <- df |> 
  pivot_longer(cols = colnames(df),
               names_to = 'theme',
               values_to = 'juris')

# tally the number of times a juris is flagged
df_sum <- df_piv |> 
  group_by(juris) |>
  summarise(count_of_juris = n(), themes = list(theme)) |> 
  arrange(desc(count_of_juris))

  
