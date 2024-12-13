library(psrcplot)
library(dplyr)

df <- mode_share_example_data %>%
  filter(Category=="Population by Race" & Year==2020) %>%
  filter(Race !="Total")
my_facet <- static_facet_column_chart(t = df,
                                      x = "Geography",
                                      y = "share",
                                      fill = "Geography",
                                      facet = "Race",
                                      moe = 'share_moe',
                                      ncol = 4,
                                      scales = "fixed",
                                      color = "pgnobgy_5",
                                      title = "Population by Race 2020",
                                      subtitle = "For counties in the Central Puget Sound Region",
                                      source = paste("Source: ACS 5-Year Estimates, table B03002",
                                                     "for King, Kitsap, Pierce and Snohomish counties.",
                                                     sep = "\n"))
