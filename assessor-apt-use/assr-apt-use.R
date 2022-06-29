# Residential “SectionUse” Codes: 300,984,352,348,596,587,351
# Group Quarters “SectionUse” Codes: 982,321,324,424,451,710,589,551,985,782,784,783
# “Other”: Any complex that has neither residential or group quarter use codes

# “Residential”: Any complex that has at least one residential use code in the commercial section table
# “Group Quarters”: Any complex that has no residential use codes but does have at least one qroup quarter use code
# “Other”: Any complex that has neither residential or group quarter use codes

library(tidyverse)
library(openxlsx)

data.dir <- 'J:/Staff/Christy/scripts/assr-apt-use'
data.files <- list(complex = 'Apartment_Complex.xlsx', comm = 'Comm_Blg_Section.xlsx')

# read-in data into a list
dfs <- map(data.files, ~read.xlsx(file.path(data.dir, .x)))

# filter commercial building data frame for two columns
sec_use <- dfs$comm %>% 
  select(PIN, SectionUse)

# create joined data frame
df <- left_join(dfs$complex, sec_use, by = 'PIN')

# section uses by group
res <- c(300,984,352,348,596,587,351)
gq <- c(982,321,324,424,451,710,589,551,985,782,784,783)

# create new intermediate column 'unit_category' and apply criteria with case_when()
df_code <- df %>% 
  select(PIN, ComplexDescr, SectionUse, unit_category) %>% 
  mutate(unit_category = case_when(SectionUse %in% res ~ 'Residential',
                                   SectionUse %in% gq ~ 'Group Quarters',
                                   TRUE ~ 'Other')) %>% 
  mutate(unit_bin = case_when(unit_category == 'Residential' ~ 0,
                              unit_category == 'Group Quarters' ~ 1))

# group by PIN and Complex Name. Sum of numbers will determine label
df_cat <- df_code %>% 
  drop_na(unit_bin) %>% 
  group_by(PIN, ComplexDescr) %>% 
  summarise(sum_unit_bin = sum(unit_bin))

# final one-to-one table
df_complex <- dfs$complex %>% 
  left_join(df_cat, by = c('PIN', 'ComplexDescr')) %>% 
  mutate(complex_category = case_when(sum_unit_bin == 0 ~ 'Residential',
                                      sum_unit_bin > 0 ~ 'Group Quarters',
                                      is.na(sum_unit_bin) ~ 'Other'))






