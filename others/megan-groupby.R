library(tidyverse)
library(odbc)
library(DBI)
library(tidycensus)

elmer_connection <- dbConnect(odbc::odbc(), driver = "SQL Server", server = "AWS-PROD-SQL\\Sockeye", database = "Elmer", trusted_connection = "yes" )

pums_ospi_elmer <- dbReadTable(elmer_connection, SQL("equity.v_tracker_indicators")) 
dbDisconnect(elmer_connection)


indicator_measure <- 'crowding' 
indicator_title <- 'Overcrowding (>1 person/room)'

data_indicator <- pums_ospi_elmer %>% mutate(indicator_attribute = case_when(indicator_attribute == "More than 1.5 persons per bedroom" | indicator_attribute == "Between 1 and 1.5 person(s) per bedroom" ~ "More than 1 person per bedroom", TRUE~ indicator_attribute))



data_full <- data_indicator %>% dplyr::filter(indicator_type ==indicator_measure) %>% dplyr::filter(focus_type !="Total") %>% filter(focus_attribute !="Total") %>% filter(indicator_attribute !="Total") %>% dplyr::mutate(data_year_yr = format(data_year,format="%Y"))

test <- data_full %>% 
  group_by(data_year, span, county, vulnerability, focus_type, focus_attribute, indicator_type, indicator_attribute, fact_type) %>% 
  summarise(fv = sum(fact_value), moe = moe_sum(fact_value, margin_of_error))

test2 <- data_full %>% 
  group_by(data_year, span, county, vulnerability, focus_type, focus_attribute, indicator_type, indicator_attribute, fact_type) %>% 
  summarise(fv = sum(fact_value), moe = moe_sum(fact_value, margin_of_error))

