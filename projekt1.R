
install.packages('lubridate')
install.packages('ggplot2')
install.packages('distr')
library(lubridate)
library(ggplot2)
library(distr)
dane <- waluty_2012_2018[]
usd <- waluty_2012_2018[,1:2]
n <- nrow(usd)
log_USD<- -100*log(usd[-1,2]/usd[-n,2])

#---------------------METODA HISTORYCZNA------------------------------------
metoda_historyczna <- function(strata){
  
  var <- sapply(1:(length(strata)-499), function(i){
    quantile(strata[i:(i+499)], 0.99)
  })
  
  es <- sapply(1:(length(strata)-499), function(i){
    w <- strata[i:(i+499)]
    mean(w[which(w>quantile(w,0.99))])
  })
  
  liczba_przekroczen <- sapply(1:(length(strata)-499), function(i){
    sum(strata[i:(i+499)]>var[i])
  })
  
  data.frame(X=ymd(dane[501:n,1]),VAR=var, ES=es, Liczba_przekroczen=liczba_przekroczen)
}
#----------------------------METODA HISTORYCZNA Z WAGAMI-------------------------------
metoda_historyczna_wagi <- function(strata){
  
  q <- 0.995
  waga <- q^(500-c(1:500))*(1-q)/(1-q^500)
  
  var <- sapply(1:(length(strata)-499), function(i){
    d <- DiscreteDistribution(supp = strata[i:(i+499)],prob = waga)
    qd <- q(d)
    qd(0.99)
  })
  
  es <- sapply(1:(length(strata)-499), function(i){
    straty <- data.frame(Strata=strata[i:(i+499)],Waga=waga)
    straty <- straty[order(-straty$Strata),]
    
    e <- straty[which(straty$Strata>=var[i]),]
    e$w <- e$Waga/0.01
    e[nrow(e),"w"] <- 1-sum(e[-nrow(e),"w"])
    sum(e$Strata*e$w) 
  })
  
  liczba_przekroczen <- sapply(1:(length(strata)-499), function(i){
    sum(strata[i:(i+499)]>var[i])
  })
  
  wyniki <- data.frame(X=ymd(dane[501:n, 1]),VAR=var, ES=es, Liczba_przekroczen=liczba_przekroczen)
  wyniki
}


#------------------------------METODA Z WYKORZYSTANIEM EWMA---------------------------------------------------
metoda_heteroskedastycznosc_EWMA <- function(strata, lambda){
  
  EWMA <- var <- es <- liczba_przekroczen <-  0
  
  EWMA[1] <- sd(strata[1:500])
  for(i in 1:(length(strata))){
    EWMA[i+1] <- sqrt(lambda*(EWMA[i]^2)+(1-lambda)*(strata[i]^2))
  }
  
  for(i in 1:(length(strata)-499)){
    strata_EWMA <- 0
    ewma <- EWMA[i:(i+500)]
    st <- strata[i:(i+499)]
    for(j in 1:500){
      strata_EWMA[j] <- st[j]*(ewma[501]/ewma[j])
    }
    var[i] <- quantile(strata_EWMA, 0.99)
    es[i] <- mean(strata_EWMA[which(strata_EWMA>quantile(strata_EWMA,0.99))])
    liczba_przekroczen[i] <- sum(strata[i:(i+499)]>var[i])
  }
  
  wyniki <- data.frame(X=ymd(dane[501:n,1]),VAR=var, ES=es, Liczba_przekroczen=liczba_przekroczen)
  wyniki
}

#---------------------------------------------ANALIZA WYNIKOW-----------------------------------------------
x <- metoda_historyczna(log_USD)
x <- metoda_historyczna_wagi(log_USD)

x<- metoda_heteroskedastycznosc_EWMA(log_USD, 0.01)
#--------------------------

results <- metoda_historyczna(log_USD)
plot(results$VAR, type='l', col='red', xlab='Days', ylab='Exchange Rate', main='VaR and ES Estimates')
lines(results$ES, col='blue')
legend('topright', legend=c('VaR', 'ES'), col=c('red', 'blue'), lty=1)

df <- data.frame(Days = seq_along(results$VAR), VAR = results$VAR, ES = results$ES)
ggplot(df, aes(x = Days)) +
  geom_line(aes(y = VAR, color = "VaR")) +
  geom_line(aes(y = ES, color = "ES")) +
  labs(x = "Days", y = "Exchange Rate", title = "VaR and ES Estimates") +
  scale_color_manual(name = "", values = c("red", "blue")) +
  theme_minimal()

suma_wykroczen <- sum(results$Liczba_przekroczen)





num_exceptions <- 5

# compute the expected number of exceptions
expected_num_exceptions <- length(log_USD) * 0.01

# compute the p-value for the lower-tail test
p_lower <- pbinom(num_exceptions, length(usd), 0.01)

# compute the p-value for the upper-tail test
p_upper <- pbinom(length(usd) - num_exceptions, length(usd), 1 - 0.01)

# perform the test of realized values
if (p_lower > 0.05 && p_upper > 0.05) {
  message("The number of exceptions is not too small or too large.")
} else if (p_lower <= 0.05) {
  message("The number of exceptions is too small.")
} else {
  message("The number of exceptions is too large.")
}

# return the results
#return(list(var = var, num_exceptions = num_exceptions, expected_num_exceptions = expected_num_exceptions, p_lower = p_lower, p_upper = p_upper))
}

