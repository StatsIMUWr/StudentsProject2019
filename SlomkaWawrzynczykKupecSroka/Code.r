library(readr)
library(ggplot2)

### Pobieranie danych o gie³dzie z za³o¿eniem, ¿e ostatni wiersz to ostatni dzieñ, który bêdziemy symulowaæ.
stock_data <- read_csv("wig20_dane_historyczne.csv")

### Funkcja zwaracaj¹ca zwroty oraz dryf i zmiennoœæ wyliczon¹ na podstawie zwrotów

zwroty <- function(start_date, end_date, T_)
{
  nr_row_start_date <- which(stock_data$Data == start_date)
  nr_row_end_date <- which(stock_data$Data == end_date)
  dt <- T_/length(stock_data$Data[nr_row_start_date:nr_row_end_date]) #deltat
  zwrot <- rep(0, times = (length(stock_data$Data[nr_row_start_date:nr_row_end_date]) - 1))
  for (i in 1:length(zwrot)) 
  {
    zwrot[i] = (stock_data$Zamkniecie[nr_row_start_date + i] - stock_data$Zamkniecie[nr_row_start_date + i - 1])/stock_data$Zamkniecie[nr_row_start_date + i - 1]
  }
  dryf <- mean(zwrot)/dt
  zmiennosc <- sqrt(1/dt)*sd(zwrot)
  return(list(zwroty, dryf, zmiennosc))
}

dryf <- zwroty("2016-01-04", "2016-12-30", 1)[[2]]
zmiennosc <- zwroty("2016-01-04", "2016-12-30", 1)[[3]]

#macierz symulacji akcji
macierzAkcjaSim <-function(S_0, mu, sigma, n_days, n_sim, T_){
  mean = mu*(T_/n_days) - (sigma^2*(T_/n_days))/2
  sd = sigma*sqrt(T_/n_days)
  baz = rnorm(n_days*n_sim, mean, sd)
  tr = matrix(exp(baz),n_days, n_sim)
  tr = rbind(rep(1, times = n_sim), tr)
  tr = S_0 * apply(tr,2,cumprod)
  t(tr)
}

### Wartoœæ indeksu od którego zaczynamy symulowaæ trajektorie
stock_value <- as.numeric(stock_data[which(stock_data$Data == "2017-01-02"), "Zamkniecie"])

### Symulacje
n_sim <- 100 #iloœæ symulacji
simulations <- macierzAkcjaSim(stock_value, dryf, zmiennosc, 249, n_sim, 1)


### Wykres symulowanych trajektorii oraz historycznej trajektorii
dat <- vector("list", n_sim)
p <- ggplot()
time <- as.Date(stock_data$Data[which(stock_data$Data == "2017-01-02"):nrow(stock_data)])
for (i in seq(n_sim)) {
  dat[[i]] <- data.frame(t = time, s = simulations[i,])
  p <- p + geom_line(data = dat[[i]], mapping = aes(x = t, y = s), col = "black", alpha = 0.25)
} 
plot1 <- p + geom_line(data = stock_data[which(stock_data$Data == "2016-01-04"):nrow(stock_data),],
                       mapping = aes(x = as.Date(Data), 
                                     y = Zamkniecie),
                       col = "red") + labs(title = "Symulacje przysz³ych trajektorii WIG20",x = "czas", y = "wartoœæ")

ggsave("sym_z_hist.png", width = 6, height = 4)

### Wykres symulowanych trajektorii, hist trajktorii oraz przedzia³y kwantylowe
p <- p + geom_line(data = stock_data[which(stock_data$Data == "2017-01-02"):nrow(stock_data),],
                   mapping = aes(x = as.Date(Data), 
                                 y = Zamkniecie),
                   col = "red",
                   size = 0.5)
dat_all <- do.call(rbind, dat)

quant <- data.frame(time)
for (i in 1:250) {
  quant[i,"q.0.1"] <- quantile(dat_all[which(dat_all$t == time[i]),"s"], probs = 0.1, names = FALSE)
  quant[i,"q.0.25"] <- quantile(dat_all[which(dat_all$t == time[i]),"s"], probs = 0.25, names = FALSE)
  quant[i,"q.0.5"] <- quantile(dat_all[which(dat_all$t == time[i]),"s"], probs = 0.5, names = FALSE)
  quant[i,"q.0.75"] <- quantile(dat_all[which(dat_all$t == time[i]),"s"], probs = 0.75, names = FALSE)
  quant[i,"q.0.9"] <- quantile(dat_all[which(dat_all$t == time[i]),"s"], probs = 0.9, names = FALSE)
}

p <- p + geom_line(quant, mapping = aes(x = time, y = q.0.1, colour = "blue"), size = 1) +
  geom_line(quant, mapping = aes(x = time, y = q.0.25, colour = "gold"), size = 1) +
  geom_line(quant, mapping = aes(x = time, y = q.0.5, colour = "chartreuse3"), size = 1) +
  geom_line(quant, mapping = aes(x = time, y = q.0.75, colour = "gold"), size = 1) +
  geom_line(quant, mapping = aes(x = time, y = q.0.9, colour = "blue"), size = 1)


p <- p + labs(title = "Kwantyle symulacji przysz³ych trajektorii WIG20",x = "czas", y = "wartoœæ") +
  scale_colour_manual(name = 'Przedzia³y kwantylowe', breaks = c('gold', 'chartreuse3',"blue"),
                      values = c('gold', 'chartreuse3',"blue"), labels = c("25%-75%",'50%','10%-90%'))

ggsave("sym_z_kwant.png", width = 6, height = 4)

### Gie³dowe wskaŸniki greckie

d1 <- function(spot, strike, r, sigma, t, T_){
  asd = log(spot / strike)  + (r + sigma * sigma / 2) * (T_ - t)
  asd = asd / (sigma * sqrt(T_ - t))
  asd
}
d2 <- function(spot, strike, r, sigma, t, T_){
  return (d1(spot, strike, r, sigma, t, T_) - sigma * sqrt(T_ - t))
}

Delta <- function(spot, strike, r, sigma, t, T_, payoffType){
  if(payoffType == 'call'){
    return(pnorm(d1(spot, strike, r, sigma, t, T_)))
  }
  if(payoffType == 'put'){
    return(vanillaDelta(spot, strike, r, sigma, t, T_, 'call') - 1)
  }
}

Gamma <-function(spot, strike, r, sigma, t, T_, payoffType){
  if(payoffType == 'call'){
  return(dnorm(d2(spot, strike, r, sigma, t, T_))/(spot * sigma * sqrt(T_ - t)))}
}

Theta <-function(spot, strike, r, sigma, t, T_, payoffType){
  d1 = d1(spot, strike, r, sigma, t,T_)
  d2 = d2(spot, strike, r, sigma, t,T_)
  if(payoffType == 'call'){
    return(-sigma * spot *pnorm(d1)/(2 * sqrt(T_ - t))-r * strike* exp(-(T_-t)*r)*dnorm(d2))}
  if(payoffType == 'put'){
    return(-sigma * spot *pnorm(-d1)/(2 * sqrt(T_ - t))+r * strike* exp(-(T_-t)*r)*dnorm(-d2))}
}

Vega <-function(spot, strike, r, sigma, t, T_, payoffType){
    return(spot * sqrt(T_ - t) * pnorm(d1))
}

Rho <-function(spot, strike, r, sigma, t, T_, payoffType){
  if(payoffType == 'call'){
  return(spot * (T_ - t) * sigma * exp(-(T_-t)*r) * dnorm(d2))}
  if(payoffType == 'put'){
  return(-spot * (T_ - t) * sigma * exp(-(T_-t)*r) * dnorm(-d2))}
}

#cena
vanillaPrice <- function(spot, strike, r, sigma, t, T_, payoffType){
  discountFactor = exp(-r * (T_ - t))
  d1 = d1(spot, strike, r, sigma, t,T_)
  d2 = d2(spot, strike, r, sigma, t,T_)
  if(payoffType == 'call'){
    return(spot * pnorm(d1) - strike * discountFactor * pnorm(d2))}
  if(payoffType == 'put'){
    return(- spot * pnorm(-d1) + strike * discountFactor * pnorm(-d2))}
  }
  