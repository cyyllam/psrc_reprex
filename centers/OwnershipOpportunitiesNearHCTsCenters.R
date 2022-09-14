# TITLE: Ownership Opportunities Near HCTs & Centers
# GEOGRAPHIES: PSRC Region & County
# DATA SOURCE: 5YR ACS Data 2016-20
# LAST EDITED: 9.2.2022
# AUTHOR: Eric Clute

library(psrccensus)
library(tidyverse)
library(dplyr)
library(srvyr)
library(openxlsx)

#------------ Collect HH Tenure by Tract ------------
B25003_raw <- get_acs_recs(geography = 'block group',
                              table.names = c('B25003'),
                              years = 2020,
                              counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                              acs.type = 'acs5')

tenure_blockgrp <- B25003_raw %>%
  filter(variable == "B25003_001" | variable == "B25003_002" | variable == "B25003_003") %>%
  mutate(.keep = "none",
         geoid = GEOID,
         geography = census_geography,
         estimate = estimate,
         moe = moe,
         variable = factor(case_when(variable == "B25003_001" ~ "total households",
                                     variable == "B25003_002" ~ "owner",
                                     variable == "B25003_003" ~ "renter",
                                       !is.na(variable) ~"N/A"),
                       levels=c("total households","owner","renter")))

tenure_blockgrp_pivot <- tenure_blockgrp %>% 
  pivot_wider(id_cols = c( 'geography', 'geoid'),
              names_from = 'variable',
              values_from = c('estimate', 'moe'))

#------------ Collect bedroom count by Tenure by Block Group ------------

B25042_raw <- get_acs_recs(geography = 'block group',
                          table.names = c('B25042'),
                          years = 2020,
                          counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                          acs.type = 'acs5')

bedroomcount_blockgrp <- B25042_raw %>%
  mutate(.keep = "none",
         geoid = GEOID,
         name = name,
         variable = variable,
         estimate = estimate,
         moe = moe,
         label = label,
         concept = concept,
         acs_type = acs_type,
         year = year) 

bedroomcount_blockgrp_clean <- bedroomcount_blockgrp %>% 
  mutate(label_copy := label,
         label_content = str_replace_all(label_copy, 'Estimate!!Total:(!!)*', "")) %>% 
  mutate(tenure = ifelse(label_content == "", 'All Tenure', str_extract(label_content, '^(\\w+)')),
         bedrooms = str_extract(label_content, '(?<=!!)(\\d)*(\\s+)*(\\w+)+(\\s\\w+)*')) %>% 
  mutate(bedrooms = ifelse(tenure != 'All Tenure' & is.na(bedrooms), paste('Total Bedrooms'), bedrooms),
         bedrooms = ifelse(tenure == 'All Tenure' & is.na(bedrooms), 'Total Bedrooms', bedrooms)) %>% 
  select(-label_copy, -label_content) 

bedroomcount_blockgrp_pivot <- bedroomcount_blockgrp_clean %>% 
  pivot_wider(id_cols = c('acs_type', 'year', 'geoid', 'name'),
              names_from = c('tenure', 'bedrooms'),
              values_from = c('estimate', 'moe'))


#------------ Collect reported home value by Block Group ------------

B25075_raw <- get_acs_recs(geography = 'block group',
                           table.names = c('B25075'),
                           years = 2020,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = 'acs5')

homevalue_blockgrp <- B25075_raw %>%
  mutate(.keep = "none",
         geoid = GEOID,
         name = name,
         variable = variable,
         estimate = estimate,
         moe = moe,
         label = label,
         concept = concept,
         acs_type = acs_type,
         year = year)

homevalue_blockgrp_clean <- homevalue_blockgrp %>% 
  mutate(homevalue = str_replace_all(label, 'Estimate!!Total:(!!)*', "")) %>% 
  mutate(homevalue = ifelse(homevalue == "", 'Total', homevalue))

homevalue_blockgrp_pivot <- homevalue_blockgrp_clean %>% 
  pivot_wider(id_cols = c('acs_type', 'year', 'geoid', 'name'),
              names_from = c('homevalue'),
              values_from = c('estimate', 'moe'))

#-------------- Write to Excel --------------

write.xlsx(bedroomcount_blockgrp_pivot, "BedroomCountTenure_Blockgrp.xlsx")
write.xlsx(tenure_blockgrp_pivot, "Tenure_Blockgrp.xlsx")
write.xlsx(homevalue_blockgrp_pivot, "HomeValue_Blockgrp.xlsx")
