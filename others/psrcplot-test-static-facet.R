library(openxlsx)
library(here)

source(here('others/psrcplot-static-facet.R'))
df <- read.xlsx("C:/Users/CLam/github/reprex/others/cov_emp_22_chart.xlsx")

static_facet_column_chart(
  t = df,
  x = "STANDARD_JOBSECTOR",
  y = "share",
  fill = "STANDARD_JOBSECTOR",
  pos = "dodge",
  facet = "DIS",
  est = "percent",
  ncol = 2,
  scales = "fixed",
  dec = 0,
  color = "pgnobgy_10",
  title = "Title",
  subtitle = "Subtitle",
  source = "",
  alt = NULL,
)