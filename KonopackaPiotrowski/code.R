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

#weight vs height for athletes over 18 depending on sex
plot1 <- function(){
  #height, weight first plot
  clear_age <- !is_NA_dtf$Age
  clear_weight <- !is_NA_dtf$Weight
  clear_height <- !is_NA_dtf$Height
  no_NA_body <- data[clear_weight & clear_height & clear_age, ]
  adults <- no_NA_body[no_NA_body$Age > 17, ]
  
  #plot
  ggplot(adults, aes(x = Weight, y = Height, color = Sex)) + 
    geom_point(size = 0.2, alpha = 0.9) + 
    scale_color_manual("Sex", values = c(F = "sienna3", M = "navy")) + 
    labs(title = "Weight vs height for athletes over 18 depending on sex", 
         x = "weight [kg]", y = "height [cm]")
}
plot1()

#end of script
cat("OK")

  

