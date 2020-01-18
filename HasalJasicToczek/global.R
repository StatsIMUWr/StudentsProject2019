library(ggplot2)
# Wczytanie danych
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

# Zmiana wieku na przedziały wieku.
partition_age <- function(range_age){
  cut(credit_data$Age, breaks = seq(18,80,range_age))
}
credit_data$Age <- partition_age(5)

# Zmiana decyzji 
positive <- which(credit_data$Decision == 1)
negative <- which(credit_data$Decision == 2)
credit_data$Decision[positive] = "Granted"
credit_data$Decision[negative] = "Not granted"

# Zmiana kwoty na przedziały kwoty
partition_credit_amount <-function(range_amount){
  cut(credit_data$Credit_amount, breaks = seq(0,20000,range_amount))
}
credit_data$Credit_amount <- partition_credit_amount(2000) #przedziały w dziwnej postaci

changing_factor_names <- function(col, new_names){
  levels(credit_data$col) <- new_names
}

#sapply(colnames(data), changing_factor_names, c(c('<0DM', '[0,200]DM'))

plot_one_attribute <- function(col){
  ggplot(credit_data, aes_string( x = col, fill = 'Decision')) + 
    geom_bar(position = "dodge") + 
    labs(title = paste("Decision about credit depending on", col))
}

plot_two_attributes <- function(col1, col2){
  ggplot(credit_data, aes_string( x = col1, fill = col2, color = 'Decision')) + 
    geom_bar(position = "dodge") + 
    #facet_wrap(~)
    labs(title = paste("Decision about credit depending on", col1, 'and', col2))
  #grubość ramek i inne kolory ramek niż kolumn
}

plot_one_attribute('Credit_amount')
plot_two_attributes('Job', 'Account_status')

