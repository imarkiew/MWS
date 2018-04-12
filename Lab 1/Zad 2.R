# setwd(...)
library(plyr)

# Zad 2
# a), b)

# Wczytywanie pliku katastrofy.csv
X <- read.csv("./katastrofy.csv", sep = ",", head = TRUE)[, c(1, 11)]
# Nadawanie wartości 0 wartościom NA
X[is.na(X)] <- 0
# Wyciągnięcie roku z daty i nadpisanie jej
X[, 1] <- substring(X[, 1], 7, 10)
# Zliczenie wypadków w każdym roku 
accidents <-count(X[, 1])
# zmiana typu kolumny z factor na character
accidents <- data.frame(lapply(accidents, as.character), stringsAsFactors=FALSE)
# Suma ofira w każdym roku
fatalities <- aggregate(X[, 2], list(X[, 1]), sum)  

# rysowanie rozkładu wypadków w czasie
plot(accidents[, 1], accidents[, 2], type = "p", main = "Rozkład wypadków lotniczych w czasie", xlab = "rok", ylab = "liczba wypadków")
# wygładzanie metodą cubic smoothing spline 
smoothed_accidents <- smooth.spline(accidents, spar = 0.5)
lines(smoothed_accidents, col="red")

# rysowanie rozkładu śmiertelności w czasie
plot(fatalities[, 1], fatalities[, 2], type = "p", main = "Rozkład śmiertelności w czasie", xlab = "rok", ylab = "liczba zgonów")
# wygładzanie metodą cubic smoothing spline 
smoothed_fatalities <- smooth.spline(fatalities, spar = 0.5)
lines(smoothed_fatalities, col="red")