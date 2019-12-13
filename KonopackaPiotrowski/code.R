#setwd("~/R/StudentsProject2019/KonopackaPiotrowski")

#load source file where functions are defined
source("functions.R")

#load libraries that may be useful
libs <- list("conflicted", "dplyr", "lubridate", "tidyr", "stringr", "data.table", "ggplot2")
load_libraries(libs)

#load data without duplicated rows
data <- load_as_dataframe("data/athlete_events.csv")

#end of script
cat("OK")

  

