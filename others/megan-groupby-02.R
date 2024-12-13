#devtools::install_github("psrc/psrcplot",
#                         force=TRUE)
library(tidyverse)
library(psrccensus)
library(psrcplot)
library(psrctrends)
library(rlang) #required for psrccensus
# library(emmeans) #required for rlang
library(magrittr)
#library(kableExtra)
#library(ggplot2)

# library(summarytools) #freq
# library(vtable) #summary stats
# library(table1)  #nice descriptive summary table
library(scales) #number formatting
# library(ggpubr) #graphing - ggarrange fx
library(forcats) #for factor re-leveling
library(plotly) #for interactive charts

#remove.packages("Rcpp")
#install.packages("Rcpp")
#library(Rcpp)
library(odbc) #connect to ElmerGeo
library(DBI) #connect to ElmerGeo
library(sf)
#library(leaflet)
#library(leafem) #home button, part of leaflet/Rcpp
#library(htmlwidgets) #save visuals as html
#library(raster)
#library(ggspatial)
library(lubridate) #year formatting
library(stringr) #add leading zero's
# library(reshape2) #formatting data
#library(readxl)
library(RColorBrewer)
library(gridExtra)

install_psrc_fonts()
library(showtext) #trying to fix PSRC font issues
library(sysfonts) #required for showtext
library(showtextdb) #required for showtext

library(dplyr)
#install.packages("srvyr")
library(srvyr)


# Introduction
# The data set referenced in this script is generated from the American Community Survey (ACS) Public Use Microdata Sample (PUMS) and the Washington Office of Superintendent of Public Instruction [(OSPI)](https://www.k12.wa.us/data-reporting/data-portal). These data sets provide data at the **person-level**, with the ability to look at indicators by the six equity demographic groups of interest. 

# Access data
## PUMS and OSPI data (Elmer)
# This data set was compiled from PUMS data. 

# connect to Elmer
elmer_connection <- dbConnect(odbc::odbc(),
                              driver = "SQL Server",
                              server = "AWS-PROD-SQL\\Sockeye",
                              database = "Elmer",
                              trusted_connection = "yes"
)

# PUMS/OSPI data set
pums_ospi_elmer <- dbReadTable(elmer_connection, SQL("equity.v_tracker_indicators"))

#dbDisconnect(elmer_connection)

# Looking at the fields in the data set.

# explore the data
head(pums_ospi_elmer)
# view the equity demographic groups
unique(pums_ospi_elmer$focus_type)
# view the different indicators available within the data set
unique(pums_ospi_elmer$indicator_type)
# view the different data years available within the data set
unique(pums_ospi_elmer$data_year)


# Housing Cost Burden

# set up a variable for analysis
indicator_measure <- 'crowding'
# set up a variable for titles
indicator_title <- 'Overcrowding (>1.5 person/room)'


## 1. Explore data
# *In this section we make sure that the data set makes sense.*  

# clean data set
my_columns <- c("indicator_fact_id", "fact_value", "margin_of_error")

data_indicator <- pums_ospi_elmer %>% 
  mutate(indicator_attribute = case_when(indicator_attribute == "One person per bedroom or less" |
                                           indicator_attribute == "Between 1 and 1.5 person(s) per bedroom" ~ "Less than 1.5 person(s) per bedroom",
                                         TRUE~ indicator_attribute)) %>% 
  group_by(across(!c(indicator_fact_id, fact_value, margin_of_error))) %>% 
  summarize(fact_value = sum(fact_value, na.rm = TRUE),
            margin_of_error = tidycensus::moe_sum(margin_of_error, estimate = fact_value, na.rm = TRUE))

data_full <- data_indicator %>% 
  dplyr::filter(indicator_type ==indicator_measure) %>% 
  dplyr::filter(focus_type !="Total") %>% 
  filter(focus_attribute !="Total") %>%
  filter(indicator_attribute !="Total") %>%
  # distinct(indicator_fact_id, .keep_all = TRUE) %>%
  dplyr::mutate(data_year_yr = format(data_year,format="%Y"))%>%
  dplyr::select(-data_year_yr)