# setwd(...)

# Zad 2
# a)

# wczytanie i podział danych
temperature <- read.table("tempciala", header = T, sep = ',')[, 1:2]
mens_temperature <- temperature$temperatura[which(temperature$plec == 1)]
womens_temperature <- temperature$temperatura[which(temperature$plec == 2)]

# nośnik
q = seq(0.01, 0.99, 0.01)

mens_temperature_mean = mean(mens_temperature)
mens_temperature_variance = var(mens_temperature)
mens_temperature_quantile = quantile(mens_temperature, q)
mens_temperature_norm = qnorm(q, mean = mens_temperature_mean, sd = sqrt(mens_temperature_variance))
qqplot(mens_temperature_quantile, mens_temperature_norm, xlim = c(35.7, 38), ylim = c(35.7, 38), xlab = 'kwantyle z próbki', ylab = 'kwantyle rozkladu normalnego', col = "black")
mens_temperature_simulation = rnorm(n = 99, m = mens_temperature_mean, sd = sqrt(mens_temperature_variance))
mens_temperature_simulation_mean = mean(mens_temperature_simulation)
mens_temperature_simulation_variance = var(mens_temperature_simulation)
mens_temperature_simulation_quantile = quantile(mens_temperature_simulation, q);
mens_temperature_simulation_norm = qnorm(q, mean = mens_temperature_simulation_mean, sd = sqrt(mens_temperature_simulation_variance))
par(new = TRUE)
qqplot(mens_temperature_simulation_quantile, mens_temperature_simulation_norm, col = "green", xlim = c(35.7, 38), ylim = c(35.7, 38), axes = FALSE, ann = FALSE)
abline(0, 1, col = "red")
title("Funkcja kwantyl - kwantyl dla mężczyzn")
legend(x = "topleft", 1, lty = 1, legend = c("syntetyczne", "próbka", "y = x"), col = c("green", "black", "red"))

womens_temperature_mean = mean(womens_temperature)
womens_temperature_variance = var(womens_temperature)
womens_temperature_quantile = quantile(womens_temperature, q)
womens_temperature_norm = qnorm(q, mean = womens_temperature_mean, sd = sqrt(womens_temperature_variance))
qqplot(womens_temperature_quantile, womens_temperature_norm, xlim = c(35.7, 38), ylim = c(35.7, 38), xlab = 'kwantyle z próbki', ylab = 'kwantyle rozkladu normalnego')
womens_temperature_simulation = rnorm(n = 99, m = womens_temperature_mean, sd = sqrt(womens_temperature_variance))
womens_temperature_simulation_mean = mean(womens_temperature_simulation)
womens_temperature_simulation_variance = var(womens_temperature_simulation)
womens_temperature_simulation_quantile = quantile(womens_temperature_simulation, q);
womens_temperature_simulation_norm = qnorm(q, mean = womens_temperature_simulation_mean, sd = sqrt(womens_temperature_simulation_variance))
par(new = TRUE)
qqplot(womens_temperature_simulation_quantile, womens_temperature_simulation_norm, col = "green", xlim = c(35.7, 38), ylim = c(35.7, 38), axes = FALSE, ann = FALSE)
abline(0, 1, col = "red")
title("Funkcja kwantyl - kwantyl dla kobiet")
legend(x = "topleft", 1, lty = 1, legend = c("syntetyczne", "próbka", "y = x"), col = c("green", "black", "red"))

# b)

# testy normalności Shapiro-Wilka 
shapiro.test(mens_temperature)
shapiro.test(womens_temperature)

# przybliżenie przez t-Studenta
t.test(mens_temperature, mu = 36.6)
t.test(womens_temperature, mu = 36.6)