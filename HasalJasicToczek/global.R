library(shiny)
library(ggplot2)
library(ggthemes)
library(plyr)
library(stringr)
library(extrafont)

font_import()
loadfonts(device = 'win')

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

parameters <- colnames(credit_data)
names(parameters) <- str_replace_all(colnames(credit_data), pattern = '_', replacement = ' ')



# Zmiana decyzji 
positive <- which(credit_data$Decision == 1)
negative <- which(credit_data$Decision == 2)
credit_data$Decision[positive] = "Granted"
credit_data$Decision[negative] = "Not granted"


vector_factor_col <-c("Account_status",
                      "Credit_history",
                      "Puprose",
                      "Savings_account_and_bonds",
                      "Present_employment_since",
                      "Status_and_Sex",
                      "Debtors_and_guarantors",
                      #"Present_residence_since",
                      "Property",
                      "Other_installment_plans",
                      "Housing",
                      #"Existing_credit",
                      "Job",
                      #"Maintenance",
                      "Telephone",
                      "Foreign_worker")

# credit_data <- change_factor_names('Account_status', c('<0DM', '[0,200]DM', '> 200DM', 'none'))
new_names <- list(c('< 0 DM', '[0,200] DM', '> 200 DM', 'none'),
                  c('none/all paid back duly',
                    'all paid back duly(in this bank)',
                    'all paid back duly(until now)',
                    'delay in past',
                    'critical/other credits existing'),
                  c('car(new)',
                    'car(used)',
                    'furniture/equipment',
                    'radio/tv',
                    'domestic appliances',
                    'repairs',
                    'education',
                    'retraining',
                    'business',
                    'other'),
                  c('< 100 DM',
                    '[100,500) DM',
                    '[500,1000) DM',
                    '>= 1000 DM',
                    'unknown/no saving account'),
                  c('uneployment',
                    '< 1 year',
                    '[1,4) years',
                    '[4,7) years',
                    '>= 7 years'),
                  c('male: divorced/separated',
                    'female: divorced/separated/married',
                    'male: single',
                    'male: married/widowed'),
                  c('none',
                    'co-applicant',
                    'guantor'),
                  #
                  c('real estate',
                    'building society savings agreement/life insurance',
                    'car or other',
                    'unknown/no property'),
                  c('bank',
                    'stores',
                    'none'),
                  c('rent',
                    'own',
                    'for free'),
                  #
                  c('unemployed/ unskilled - non-resident',
                    'officialunskilled - resident',
                    'skilled employee / official',
                    'management/ self-employed/ \n
                    highly qualified employee/ officer'),
                  #
                  c('none',
                    'yes, registered under the customers name'),
                  c('yes', 'no'))

for(k in 1:length(new_names)) {
  credit_data[,vector_factor_col[k]] <-  mapvalues(credit_data[,vector_factor_col[k]],
                                                   from = levels(credit_data[,vector_factor_col[k]]),
                                                   to = new_names[[k]])
}


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



plot_one_attribute <- function(credit_data_age,col,fill_stack_dodge, dark2_set1_pastel2, cfont) {
  ggplot(credit_data_age, aes_string( x = col, fill = 'Decision')) + 
    geom_bar(position = fill_stack_dodge) + 
    scale_fill_brewer(palette = dark2_set1_pastel2) +
    theme_stata() +
    labs(title = paste("Decision about credit depending on", str_replace_all(col, '_', ' '))) +
    theme(text=element_text(size=15), axis.text.x = element_text(angle = 15, vjust = 0.6)) +
    xlab(str_replace_all(col, '_', ' '))
}

plot_two_attributes <- function(credit_data_age, col1, col2, fill_stack_dodge, dark2_set1_pastel2, cfont) {
  ggplot(credit_data_age, aes_string( x = col1, fill = col2)) + 
    geom_bar(position = fill_stack_dodge ) +
    scale_fill_brewer(palette = dark2_set1_pastel2) +
    theme_stata() +
    facet_wrap(vars(credit_data_age$"Decision")) +
    labs(title = paste("Decision about credit depending on", str_replace_all(col1, '_', ' '),
                       'and', str_replace_all(col2, '_', ' '))) +
    theme(text=element_text(size=15), axis.text.x = element_text(angle = 15, vjust = 0.6))  +
    xlab(str_replace_all(col1, '_', ' '))
}

vector_position <- c('fill', 'stack', 'dodge')
names(vector_position) <- c('Proportion', 'Count together', 'Count in row')
vector_color <- c('Set3','Set1', 'Dark2', 'Pastel2')
names(vector_color) <- c('Default', 'Normal', 'Dark', 'Pastel')
vector_fonts <- c('serif', 'Bahnschrift', 'Comic Sans MS', 'Javanese Text')

plot_one_attribute(credit_data, 'Credit_history', 'dodge', 'Dark2', 'serif')
plot_two_attributes(credit_data, 'Account_status', 'Job', 'stack', 'Pastel2', 'serif')
