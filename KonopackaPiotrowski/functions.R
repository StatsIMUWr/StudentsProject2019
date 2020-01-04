#load libraries that may be useful
libs <- list("conflicted", "dplyr", "lubridate", "tidyr", "stringr", "data.table", "ggplot2")
load_libraries(libs)

#needs data.table library. Loads data and removes duplicated rows.
load_as_dataframe <- function(path){
  data <- fread(file = path, 
                data.table = FALSE,
                blank.lines.skip = TRUE,
                check.names = TRUE,
                stringsAsFactors = TRUE)

#remove duplicated rows
  rows1 <- dim(data)[1]
  data <- distinct(data)
  rows2 <- dim(data)[1]
  cat("Removed duplicated rows:", rows1 - rows2, "\n")
  head(data, 3)
  return (data)
}

#installs all listed libraries (and required packages) at once
load_libraries <- function(libs){
  install.packages("easypackages")
  library("easypackages")
  packages(libs)
  libraries(libs)
}

#find all NAs
is_NA_dtf <- as_tibble(is.na.data.frame(data))
is_NA_sum <- colSums(is_NA_dtf)

#boolean vectors showing where variables aren't NAs
clear_age <- !is_NA_dtf$Age
clear_weight <- !is_NA_dtf$Weight
clear_height <- !is_NA_dtf$Height
medals_only <- !is_NA_dtf$Medal

#weight vs height for athletes in age between ... ...
plot1 <- function(low, high){
  no_NA_body <- data[clear_weight & clear_height & clear_age, ]
  range <- low <= no_NA_body$Age & no_NA_body$Age <= high
  age_str <- paste("in age between ", as.character(low), "and", as.character(high))
  
  ggplot(no_NA_body[range, ], aes(x = Weight, y = Height, color = Sex)) + 
    geom_point(size = 0.2, alpha = 0.9) + 
    scale_color_manual("Sex", values = c(F = "sienna3", M = "navy")) +
    labs(title = "Weight and height of athletes", subtitle = age_str, x = "weight [kg]", y = "height [cm]")
}





