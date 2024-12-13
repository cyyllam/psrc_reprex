library(dplyr)
library(echarts4r)

interactive_facet_column_chart <- function(t, x, y, facet, ncol=3, nrows = 2, title=NULL, subtitle=NULL){
  t |> 
    group_by({{facet}}) |> 
    e_charts_(x) |> 
    e_bar_(serie=y, legend = FALSE) |> 
    e_facet(cols=ncol,
            rows = nrows,
            margin_trbl = c(b = 10)
            ) |>
    e_title(title, subtitle) |>
    e_legend(show = TRUE, bottom = '0')

}

df <- psrcplot::mode_share_example_data %>%
  filter(Category=="Population by Race" & Year==2020) %>%
  filter(Race !="Total")

interactive_facet_column_chart(df, 
                               x="Geography", 
                               y="share", 
                               ncol=4, 
                               facet=Race, 
                               title="Population by Race 2020", 
                               subtitle="For counties in the Central Puget Sound Region"
                               )

df |> 
  group_by(Race) |> 
  e_charts(Geography) |> 
  e_bar(serie=share, name='Share', legend = TRUE) |> 
  e_facet(cols=4,rows=2,legend_pos = "top", legend_space = 12) |>
  e_title("Population by Race 2020", "For counties in the Central Puget Sound Region") #|>




#----------------------------------------------------------------------------------------------
group_size <- 20
n_groups <- 13
df2 <- data.frame("day" = rep(1:group_size, times=n_groups), 
                 "temperature" = runif(group_size * n_groups, 10, 40),
                 "location" = rep(LETTERS[1:n_groups], each=group_size))
df2 |> 
  group_by(location) |> 
  e_charts(day) |> 
  e_line(temperature) |> 
  e_facet(rows = 4, cols=4, legend_pos = "top", legend_space = 12) 
