install.packages('lubridate')
install.packages("readxl")
install.packages("MASS")
install.packages("fitdistrplus")
library(fitdistrplus)
library(MASS)
library(lubridate)
library(readxl)
dane <- read_excel("C:/Users/kuba/Desktop/kredyty/projekt4/dane.xlsx")

dane$Date <- as.Date(dane$Date, format = "%d/%m/%Y")
dane$period <- paste(year(dane$Date), " Q", quarter(dane$Date), sep = "")



table(dane$`Business Line`)
table(dane$`Risk Category`)

table(dane_AM$`Risk Category`)
#wybieramy do swojego projektu linie biznesowa AM - Zarządzanie aktywami na zlecenie (Asset Management) 
#zawierajaca dwie kategorie ryzyka "EDPM" oraz "IF"
#utworzenie ramki danych dla kategori AM
dane_AM <- subset(dane, dane$`Business Line` == "AM")

#urtworzenie ramek danych z czestoscia strat obu kategori ryzyka
dane_EDPM <- subset(dane_AM, dane_AM$`Risk Category` == "EDPM")
dane_IF <- subset(dane_AM, dane_AM$`Risk Category` == "IF")

c_strat_EDPM  <- c_strat_IF[,1].frame(period = unique(dane_EDPM$period), n = NA)
c_strat_EDPM <- sapply(c_strat_EDPM$period, function(p) sum(dane_EDPM$period == p))
c_strat_EDPM <- as.c_strat_IF[,1].frame(c_strat_EDPM)

c_strat_IF  <- c_strat_IF[,1].frame(period = unique(dane_IF$period), n = NA)
c_strat_IF <- sapply(c_strat_IF$period, function(p) sum(dane_IF$period == p))
c_strat_IF <- as.c_strat_IF[,1].frame(c_strat_IF)



#Modelowanie rozkładu częstości strat
#w rozkladzie poissona var=mean
var(c_strat_EDPM[,1])
mean(c_strat_EDPM[,1])
fit_EDPM_poisson <- fitdist(c_strat_EDPM[,1], "pois")
lambda_edpm <- fit_EDPM_poisson$estimate
#test chi kwadrat
p_value_edpm <- gofstat(fit_EDPM_poisson)

# Przedstawienie rozkładu danych
hist(c_strat_EDPM[,1], breaks = "FD", freq = FALSE, main = "Rozkład danych", xlab = "Wartość", ylab = "Gęstość")
x <- seq(0, max(c_strat_EDPM[,1]), by = 1)
poisson_pdf <- dpois(x, lambda_edpm)
lines(x, poisson_pdf, type = "h", lwd = 2, col = "blue")
legend("topright", legend = c("Dane", "Rozkład Poissona"), col = c("black", "blue"), lwd = c(1, 2))

#p value wieksze od 0,05 wiec brak podstaw do odrzucenia h0, rozklady sa zgodne 
var(c_strat_IF[,1])
mean(c_strat_IF[,1])
fit_IF_poisson <- fitdist(c_strat_IF[,1], "pois")
lambda_if <- fit_IF_poisson$estimate
#chi kwadrat
p_value_if <- gofstat(fit_IF_poisson)
#p value wieksze od 0,05 wiec brak podstaw do odrzucenia h0, rozklady sa zgodne
# Przedstawienie rozkładu danych
hist(c_strat_IF[,1], breaks = "FD", freq = FALSE, main = "Rozkład danych", xlab = "Wartość", ylab = "Gęstość")
x <- seq(0, max(c_strat_IF[,1]), by = 1)
poisson_pdf <- dpois(x, lambda_if)
lines(x, poisson_pdf, type = "h", lwd = 2, col = "blue")
legend("topright", legend = c("Dane", "Rozkład Poissona"), col = c("black", "blue"), lwd = c(1, 2))

#Modelowanie dotkliwości strat
EDPM <- subset(dane_AM,dane_AM$`Risk Category` == "EDPM")
IF <- subset(dane_AM,dane_AM$`Risk Category` == "IF")
rozklad_EDPM <- fitdist(EDPM$LOSS, distr = "weibull")
rozklad_IF <- fitdist(IF$LOSS, distr = "weibull")

# Dopasowanie rozkładu Weibulla do danych
fit_EDPM <- fitdist(EDPM$LOSS, "weibull")
# Wyświetlenie podsumowania dopasowania
summary(fit_EDPM)
# Test dopasowania rozkładu Weibulla
gof_test_EDPM <- gofstat(fit_EDPM, fitnames = "weibull")
# Wyświetlenie wyników testu dopasowania
print(gof_test_EDPM)

# Dopasowanie rozkładu Weibulla do danych
fit_IF <- fitdist(IF$LOSS, "weibull")
# Wyświetlenie podsumowania dopasowania
summary(fit_IF)
# Test dopasowania rozkładu Weibulla
gof_test_IF <- gofstat(fit_IF, fitnames = "weibull")
# Wyświetlenie wyników testu dopasowania
print(gof_test_IF)
#Na podstawie tych wyników można stwierdzić, że dane mają kształt zbliżony do rozkładu Weibulla. 
#Estymowane parametry kształtu i skali wskazują na charakterystykę rozkładu Weibulla. 
#Dodatkowo, niskie wartości statystyk dopasowania (Kolmogorova-Smirnova, Cramera-von Misesa, 
#Andersona-Darlinga) sugerują, że dane są zgodne z rozkładem Weibulla.
#Niższe wartości AIC i BIC wskazują również na lepsze dopasowanie. 


# Wygenerowanie próbek z rozkładu Weibulla na podstawie dopasowanych parametrów
n <- length(EDPM$LOSS)
weibull_samples <- rweibull(n, shape = fit_EDPM$estimate[1], scale = fit_EDPM$estimate[2])

# Utworzenie histogramu dla danych
hist(EDPM$LOSS, breaks = "FD", freq = FALSE, col = "lightblue",
     main = "Porównanie rozkładu Weibulla z danymi",
     xlab = "Wartości", ylab = "Częstość")

# Dodanie krzywej dla rozkładu Weibulla
curve(dweibull(x, shape = fit_EDPM$estimate[1], scale = fit_EDPM$estimate[2]),
      add = TRUE, col = "red", lwd = 2)

# Rysowanie gęstości dla wygenerowanych próbek z rozkładu Weibulla
lines(density(weibull_samples)$x, density(weibull_samples)$y,
      col = "blue", lwd = 2)

# Legenda
legend("topright", legend = c("Dane", "Rozkład Weibulla", "Próbki Weibulla"),
       col = c("lightblue", "red", "blue"), lty = 1, lwd = 2)

# Wygenerowanie próbek z rozkładu Weibulla na podstawie dopasowanych parametrów
n <- length(IF$LOSS)
weibull_samples <- rweibull(n, shape = fit$estimate[1], scale = fit_IF$estimate[2])

# Utworzenie histogramu dla danych
hist(IF$LOSS, breaks = "FD", freq = FALSE, col = "lightblue",
     main = "Porównanie rozkładu Weibulla z danymi",
     xlab = "Wartości", ylab = "Częstość")

# Dodanie krzywej dla rozkładu Weibulla
curve(dweibull(x, shape = fit_IF$estimate[1], scale = fit_IF$estimate[2]),
      add = TRUE, col = "red", lwd = 2)

# Rysowanie gęstości dla wygenerowanych próbek z rozkładu Weibulla
lines(density(weibull_samples)$x, density(weibull_samples)$y,
      col = "blue", lwd = 2)

# Legenda
legend("topright", legend = c("Dane", "Rozkład Weibulla", "Próbki Weibulla"),
       col = c("lightblue", "red", "blue"), lty = 1, lwd = 2)



#w obu przypadkach dane pasuja do rozkladow 

