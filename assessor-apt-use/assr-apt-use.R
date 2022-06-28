# Residential “SectionUse” Codes: 300,984,352,348,596,587,351
# Group Quarters “SectionUse” Codes: 982,321,324,424,451,710,589,551,985,782,784,783
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

# create new column 'unit_category' and apply criteria with case_when()
df_code <- df %>% 
  mutate(unit_category = case_when(SectionUse %in% res ~ 'Residential',
                                   SectionUse %in% gq ~ 'Group Quarters',
                                   TRUE ~ 'Other'))


