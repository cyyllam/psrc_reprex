# individual histogram plots by building type have the same number of bins and bin size 
# - stepping up by 100 - up to say 2000, when all the rest are just counted in that 
# last category, like "2000 and up"

library(tidyverse)
library(data.table)
library(openxlsx2)

by23_data_path <- "T:/2024October/Mark"
by_data_file <- "by23_blds_sqft_per_job.csv"
blds_table <- fread(file.path(by23_data_path, by_data_file))

# Test one btid ----

df <- blds_table |> filter(building_type_id == '13')

# my_breaks <- cut(df$sqft_per_job, c(seq(0, 2000, by = 100), Inf)) %>% unique()

d.cut <- cut(df$sqft_per_job, c(seq(0, 2000, 100), Inf))
hist(as.numeric(d.cut), xaxt='n', xlab='SqFt per Job',
     col=1, breaks=0:21, border=0, main='', cex.axis=0.8, las=1)
axis(1, at=0:21, labels=c(seq(0, 2000, 100), '>2000'), cex.axis=0.8)
box()


# ggplot(blds_table, aes(x = sqft_per_job, fill=building_type_id))+  
#   geom_histogram(color = "black",binwidth = 100) +  
#   facet_wrap(~building_type_id, scales = "fixed")