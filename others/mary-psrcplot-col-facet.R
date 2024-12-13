library(tidyverse)
library(psrcplot)

source("C:\\Users\\CLam\\github\\equity-tracker\\data-visualization\\equity-tracker-chart-functions.R")
load("T:\\2023September\\Mary\\temp\\forChristy\\test-data-kg.rda")
# Create Facet Column Chart
data_clean_22 <- data_clean %>%
  dplyr::filter(data_year=="2022") #filter on most recent year
# data_clean_test_22 <- data_clean_test %>%
#   dplyr::filter(data_year=="2022") #filter on most recent year

# set variables 
df = data_clean_22
# df = data_clean_test_22
geo = "county"
x = "focus_attribute_ord"
y = "fact_value"
facet = "focus_type_ord"
fill = "focus_attribute_ord"
y_min = 0
y_max = 1
dec = 0
# width ="400px" # original
width ="400px"
# height ="380px" # original
# height ="360px" # too tall
# height ="340px" # too tall
# height = "320px" # too tall
height = "300px"
esttype = "percent"
color = "blues"
title = "Kindergarten Readiness"
subtitle = "percentage of students demonstrating readiness in 6 of 6 domains*"
source = "Office of the Superintendent of Public Instruction (OSPI) <br> No 2022 data available for students with limited English proficiency in Kitsap County <br> *Social-Emotional, Physical, Language, Cognitive, Literacy, and Math"

column_chart <- equity_tracker_column_facet(df = df,
                                            geo = geo,
                                            x = x,
                                            y = y,
                                            facet = facet,
                                            title = title,
                                            y_min = y_min,
                                            y_max = y_max,
                                            dec = dec,
                                            esttype = esttype,
                                            color = color,
                                            # width = width,
                                            # height = height,
                                            num_colors = 2,
                                            color_rev = TRUE)
