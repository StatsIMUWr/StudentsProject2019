library(shiny)
library(ggplot2)
library(ggthemes)

credit_data <- read.table("german.data")
colnames(credit_data) <- c("Account_status",
                           "Duration_in_months",
                           "Credit_history",
                           "Puprose",
                           "Credit_amount",
                           "Savings_account_and_bonds",
                           "Present_employment_since",
                           "Rate",
                           "Status_and_Sex",
                           "Debtors_and_guarantors",
                           "Present_residence_since",
                           "Property",
                           "Age",
                           "Other_installment_plans",
                           "Housing",
                           "Existing_credit",
                           "Job",
                           "Maintenance",
                           "Telephone",
                           "Foreign_worker",
                           "Decision")



# Zmiana decyzji 
positive <- which(credit_data$Decision == 1)
negative <- which(credit_data$Decision == 2)
credit_data$Decision[positive] = "Granted"
credit_data$Decision[negative] = "Not granted"


vector_factor_col <-c("Account_status",
                     "Credit_history",
                     "Puprose")

# "Savings_account_and_bonds",
# "Present_employment_since",
# "Status_and_Sex",
# "Debtors_and_guarantors",
# "Present_residence_since",
# "Property",
# "Other_installment_plans",
# "Housing",
# "Existing_credit",
# "Job",
# "Maintenance",
# "Telephone",
# "Foreign_worker")
# credit_data <- change_factor_names('Account_status', c('<0DM', '[0,200]DM', '> 200DM', 'none'))
new_names <- list(c('<0DM', '[0,200]DM', '> 200DM', 'none'),
               c('none/all paid back duly',
                 'all paid back duly(in this bank',
                 'all paid back duly(until now)',
                 'delay in past',
                 'critical/other credits existing'),
               c('car(new)',
                 'car(used)',
                 'furniture/equpment',
                 'radio/tv',
                 'domestic appliances',
                 'repairs',
                 'education',
                 'retraining',
                 'business',
                 'other'))
# change_factor_names <- function(i) {
#   levels(credit_data[[vector_factor_col[i]]]) <- new_names[i]
#   credit_data
# }
# 
# sapply(c(1:3), change_factor_names)

# Zmiana wieku na przedzialy wieku.
age_range <- function(st_age, nd_age, by) {
  credit_data_age <- credit_data[credit_data$Age >= st_age & credit_data$Age <= nd_age,]
  credit_data_age$Age <- cut(credit_data_age$Age, breaks = seq(st_age, nd_age, by))
  credit_data_age
}

# Zmiana kwoty na przedzialy kwoty
partition_credit_amount <-function(credit_data_age, range_amount) {
  credit_data_age$Credit_amount <- cut(credit_data_age$Credit_amount, breaks = seq(0, 20000, range_amount))
  credit_data_age
}



plot_one_attribute <- function(credit_data_age,col,fill_stack_dodge, dark2_set1_pastel2 ) {
  ggplot(credit_data_age, aes_string( x = col, fill = 'Decision')) + 
    geom_bar(position = fill_stack_dodge) + 
    scale_fill_brewer(palette = dark2_set1_pastel2) +
    theme_stata() +
    labs(title = paste("Decision about credit depending on", col)) +
    theme(text=element_text(size=15)) 
}

plot_two_attributes <- function(credit_data_age, col1, col2, fill_stack_dodge, dark2_set1_pastel2) {
  ggplot(credit_data_age, aes_string( x = col1, fill = col2)) + 
    geom_bar(position = fill_stack_dodge ) +
    scale_fill_brewer(palette = dark2_set1_pastel2) +
    theme_stata() +
    #theme(axis.title = element_text(color = 'blue')) +
    facet_wrap(vars(credit_data_age$"Decision")) +
    labs(title = paste("Decision about credit depending on", col1, 'and', col2)) +
    theme(text=element_text(size=15)) 
}

vector_fill <- c('stack', 'dodge')
vector_color <- c('Dark2', 'Pastel2')

plot_one_attribute(credit_data, 'Credit_history', 'dodge', 'Dark2')
plot_two_attributes(credit_data,'Job', 'Account_status', 'stack', 'Pastel2')
