library(tidyverse)
library(psrccensus)
library(tidycensus)

create_summary_table <- function(year) {
  #---------------------Grab data from Census API------------------------
  mmh_raw<-get_acs_recs(geography = 'county',
                        table.names = c('B25032'),
                        years=year, #2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                        counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                        acs.type = 'acs1')
  
  #---------------------Create custom groupings owner-occupied------------------------
  # The next step is to create the appropriate grouping variable (using pipes for simplicity)
  mmh_coded <- mmh_raw %>% 
    mutate(building_size=factor(case_when(grepl("_003$", variable) ~ "Single Family", 
                                          grepl("_004$|_005$|_006$", variable) ~ "2-4 units",
                                          grepl("_007$|_008$", variable) ~ "5-19 units",
                                          grepl("_011$|_012$", variable) ~ "Mobile Home/Other",
                                          TRUE ~ NA_character_),
                                levels=c("Single Family","2-4 units","5-19 units","Mobile Home/Other"))) %>% 
    mutate(building_size_2 = factor(case_when(grepl("_003$", variable) ~ "Single Family",
                                              grepl("_004$|_005$|_006$|_007$|_008$", variable) ~ "2-19 units",
                                              grepl("_011$|_012$", variable) ~ "Mobile Home/Other",
                                              TRUE ~ NA_character_),
                                    levels=c("Single Family","2-19 units", "Mobile Home/Other")
                                    ))
    
  
  # In this step, you create an aggregate, using the first grouping you created in the last call.
  mmh_agg_owner_01 <- mmh_coded %>% 
    group_by(across(c(name, year, building_size))) %>% 
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE))
  
  # In this step, you create an aggregate, using the second grouping you created.
  mmh_agg_owner_02 <- mmh_coded %>%
    group_by(across(c(name, year, building_size_2))) %>%
    summarize(estimate=sum(estimate, na.rm=TRUE), moe=moe_sum(moe=moe, estimate=estimate, na.rm=TRUE)) %>% 
    filter(building_size_2 == '2-19 units') %>% 
    rename(building_size = building_size_2)
  
  df <- mmh_agg_owner_01 %>% 
    bind_rows(mmh_agg_owner_02)
  
}

# years of interest
years <- 2010:2014

# iterate thru each year and pass it through the function. Store each output in the list.
list_of_all_tables <- map(years, ~create_summary_table(.x))

# View a table in the list
View(list_of_all_tables[[1]])



