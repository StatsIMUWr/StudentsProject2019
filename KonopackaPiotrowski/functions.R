#LIBRARIES--------------------------------------------------------------------------------------------
load_libraries <- function(libs) {
  install.packages("easypackages")
  library("easypackages")
  packages(libs)
  libraries(libs)
}
libs <-
  list(
    "plotly",
    "conflicted",
    "dplyr",
    "lubridate",
    "tidyr",
    "stringr",
    "data.table",
    "ggplot2"
  )
load_libraries(libs)
conflict_prefer("filter", "dplyr")

#LOAD AND CLEAR DATA-----------------------------------------------------------------------------------------
#this function loads data without duplicated
load_as_dataframe <- function(path) {
  data <- fread(
    file = path,
    data.table = FALSE,
    blank.lines.skip = TRUE,
    check.names = TRUE,
    stringsAsFactors = TRUE
  )
  
  #remove duplicated rows
  rows1 <- dim(data)[1]
  data <- distinct(data)
  rows2 <- dim(data)[1]
  cat("Removed duplicated rows:", rows1 - rows2, "\n")
  head(data, 3)
  return (data)
}
data_NA <- load_as_dataframe("data/athlete_events.csv")

#find all NAs
is_NA_dtf <- as_tibble(is.na.data.frame(data_NA))
is_NA_sum <- colSums(is_NA_dtf)

#boolean vectors showing where variables aren't NAs
clear_age <- !is_NA_dtf$Age
clear_weight <- !is_NA_dtf$Weight
clear_height <- !is_NA_dtf$Height
medals_only <- !is_NA_dtf$Medal

#create dataframe without NA age, height and weight (NA medals allowed)
data_clear <- data_NA[clear_weight & clear_height & clear_age,]

#add decade column
Decade <- factor((data_clear$Year %/% 10) * 10)
data_clear <- cbind(data_clear, Decade)

#add age groups
#TODO fix NA Age_Group
intervals <- c(11, 13, 15, 17, 19, 21, 23, 27, 29, 31, 36, 41, 46, 50, 60, 71)
Age_Group <- factor(cut(data_clear$Age, intervals))
data_clear <- cbind(data_clear, Age_Group)

#FUNCTIONS---------------------------------------------------------------------------------
data_select <- function(sex, country, sport){
  data_clear %>% 
    filter(Sport %in% sport & Team %in% country & Sex %in% sex) -> ds
  return(ds)
}

data_age_range <- function(data, low, high){
  data %>% 
    filter(Age >= low & Age <= high) -> ds
  return(ds)
}

#PLOTS-----------------------------------------------------------------------------------------------
#scatter plot: Weight vs height for athletes aged between ...
plot1 <- function(data, low, high) {
  #subtitle text
  age_str <- paste("aged between", as.character(low), "and", as.character(high))
  #plot
  ggplot(data = data_age_range(data, low, high), 
         aes(x = Weight, y = Height, color = Sex)) +
    geom_point(size = 0.2, alpha = 0.9) +
    scale_color_manual("Sex", values = c(F = "darkorchid1", M = "navy")) +
    labs(
      title = "Weight and height of athletes",
      subtitle = age_str,
      x = "Weight [kg]",
      y = "Height [cm]"
    )
}

#histogram: How many observations of given variable colored by...
#TODO change position, grid, color
plot2 <- function(variable, color_by) {
  ggplot(data_clear, aes_string(x = variable, fill = color_by)) +
    geom_bar() +
    scale_fill_manual("Sex", values = c(F = "darkorchid1", M = "navy")) +
    labs(title = "Number of observations in our database", 
         subtitle = paste("Chosen variable: ", variable),
         x = "Value", y = "Number of observations")
}

#line plot: Mean weight and height over decades in different countries, sports,
plot3 <- function(variable, low, high, color_by, #next arguments are lists
           filter_sport, filter_country, filter_sex) {
    #filter dataframe
    ds <- data_select(filter_sex, filter_sport, filter_country)
    #plot
    ggplot(ds, aes(x = "Decade", y = variable)) +
      geom_line(group = color_by)
  }

#facets: Weight vs height aged between ... for different sports
plot4 <- function(data, low, high) {
  #subtitle text
  age_str <- paste("Weight vs height of athletes aged between", as.character(low), "and", as.character(high))
  #background data
  data[ , names(data) != "Sport"] %>% data_age_range(low, high) -> bg
  #plot
  ggplot(data = data, aes(x = Weight, y = Height), size = 0.2, alpha = 0.2) +
    geom_point(data = bg, color = "grey") +
    geom_point(size = 0.1, aes(color = Sex)) +
    scale_color_manual("Sex", values = c(F = "darkorchid1", M = "navy")) +
    facet_wrap(~ Sport) +
    labs(title = age_str) +
    theme(axis.title.x = element_blank(), axis.text.y = element_blank())
}







