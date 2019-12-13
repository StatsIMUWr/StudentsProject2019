#setwd("~/R/StudentsProject2019/KonopackaPiotrowski")

#load source file where functions are defined
source("functions.R")

#load libraries that may be useful
libs <- list("conflicted", "dplyr", "lubridate", "tidyr", "stringr", "data.table", "ggplot2")
load_libraries(libs)

#load data without duplicated rows
data <- load_as_dataframe("data/athlete_events.csv")

#find all NA's
is_NA_dtf <- as_tibble(is.na.data.frame(data))
is_NA_sum <- colSums(NA_dtf)

#height, weight first plot TODO add saving plots
clear_weight <- !is_NA_dtf$Weight
clear_height <- !is_NA_dtf$Height
no_NA_body <- data[clear_weight & clear_height, ]
ggplot(no_NA_body, aes(x = Weight, y = Height, color = Sex)) +
  geom_point(size = 0.5)

#end of script
cat("OK")

  

