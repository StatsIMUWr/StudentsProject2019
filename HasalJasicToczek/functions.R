library(ggplot2)
# Wczytanie danych
data <- read.table("german.data")
colnames(data) <- c("Account_status",
                    "Duration_in_month",
                    "Credit_history",
                    "Puprose",
                    "Credit_amount",
                    "Savings_account/bonds",
                    "Present_employment since",
                    "Rate",
                    "Status/Sex",
                    "Debtors/guarantors",
                    "Present_residence_since",
                    "Property",
                    "Age",
                    "Other_installment_plans",
                    "Housing",
                    "Existing_credit_at_this_bank",
                    "Job",
                    "Maintenance",
                    "Telephone",
                    "Foreign_worker",
                    "Decision")

# Zmiana wieku na przedziały wieku.
new_age <- c(1:1000)
i=1
for(k in data$Age){
  if (k <= 20) new_age[i] = "<20"
  if (k>20 && k <= 25) new_age[i] = "20-25"
  if (k>25 && k <= 30) new_age[i] = "25-30"
  if (k>30 && k <= 35) new_age[i] = "30-35"
  if (k>35 && k <= 40) new_age[i] = "35-40"
  if (k>40 && k <= 50) new_age[i] = "40-50"
  if (k>50 && k <= 60) new_age[i] = "50-60"
  if (k>60) new_age[i] = ">60"
  i=i+1
}
data$Age <- new_age

# Zmiana decyzji 
jedyneczki <- which(data$Decision == 1)
dwojeczki <- which(data$Decision == 2)
data$Decision[jedyneczki] = "Granted"
data$Decision[dwojeczki] = "Not granted"

# Zmiana kwoty na przedziały kwoty
new_amount <- c(1:1000)
i=1
for(k in data$Credit_amount){
  if (k <= 1000) new_amount[i] = " <1000"
  if (k>1000 && k <= 1500) new_amount[i] = " 1000-1500"
  if (k>1500 && k <= 2000) new_amount[i] = " 1500-2000"
  if (k>2000 && k <= 2500) new_amount[i] = " 2000-2500"
  if (k>2500 && k <= 3000) new_amount[i] = " 2500-3000"
  if (k>3000 && k <= 3500) new_amount[i] = " 3000-3500"
  if (k>3500 && k <= 4000) new_amount[i] = " 3500-4000"
  if (k>4000 && k <= 5000) new_amount[i] = " 4000-5000"
  if (k>5000 && k <= 7500) new_amount[i] = " 5000-7500"
  if (k>7500 && k <= 10000) new_amount[i] = " 7500-10000"
  if (k>10000) new_amount[i] = ">10000"
  i=i+1
}
data$Credit_amount <- new_amount

draw_plot <- function(col){
  ggplot(data, aes(col, fill = Decision)) + 
    geom_bar(position = "dodge") + 
    labs(title = "Decyzja przyznania kredytu w zależności od col", x = "col")
}

draw_plot2 <- function(col1, col2){
  ggplot(data, aes(col1, fill=col2, color = Decision)) + 
    geom_bar(position = "dodge") + 
    labs(title = "Decyzja przyznania kredytu w zależności od col1 i col2", x = "col1")
    #grubość ramek i inne kolory ramek niż kolumn
}

draw_plot(data$Credit_amount)
draw_plot2(data$Job, data$Account_status)
