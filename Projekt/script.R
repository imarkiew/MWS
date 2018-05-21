# setwd(...)

# metryka
metric <- function(X)
{
  wins_home <- X[1]
  losses_home <- X[2]
  wins_road <- X[3]
  losses_road <- X[4]
  0.5*((wins_home - losses_home)/(wins_home + losses_home) - (wins_road - losses_road)/(wins_road + losses_road))
}

# rysowanie histogramów danych wejściwoych
plot_hist <-function(atlantic, pacific, main_, xlab_)
{
  breaks_supp <- -0.5:(max(max(atlantic + 1), max(pacific + 1)) + 0.5)
  hist(atlantic, breaks = breaks_supp, freq = FALSE, col = rgb(1, 1, 0, 0.7), main = main_, xlab = xlab_, ylab = "częstość")
  hist(pacific, breaks = breaks_supp, freq = FALSE, col = rgb(0, 1, 1, 0.4), add=T)
  legend(x = "topright", 1, lty = 1, legend = c("dywizja atlantycka", "dywizja pacyficzna"), col = c(rgb(1,1,0,0.7), rgb(0,1,1,0.4))) 
}

####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################

# wczytanie danych
X_atlantic <- read.csv("./Data/AtlanticDivision.csv", sep = ",", head = TRUE)
X_pacific <- read.csv("./Data/PacificDivision.csv", sep = ",", head = TRUE)

# rysowanie histogramów danych wejściowych
plot_hist(X_atlantic$WinsHome, X_pacific$WinsHome, "Histogram liczby wygranych w domu", "liczba wygranych")
plot_hist(X_atlantic$LossesHome, X_pacific$LossesHome, "Histogram liczby przegranych w domu", "liczba przegranych")
plot_hist(X_atlantic$WinsRoad, X_pacific$WinsRoad, "Histogram liczby wygranych na wyjeździe", "liczba wygranych")
plot_hist(X_atlantic$LossesHome, X_pacific$LossesHome, "Histogram liczby przegranych w na wyjeździe", "liczba przegranych")

# obliczenie metryki dla obu grup
atlantic_metric <- apply(X_atlantic, 1, metric)
pacific_metric <- apply(X_pacific, 1, metric)

# histogramy metryk
hist(atlantic_metric, breaks = 20, freq = FALSE, col = "green", main = "Histogram metryki dla dywizji atlantycznej", xlab = "wartość miary", ylab = "częstoś")
hist(pacific_metric, breaks = 20, freq = FALSE, col = "red", main = "Histogram metryki dla dywizji pacyficznej", xlab = "wartość miary", ylab = "częstoś")

# test t Welcha
t.test(atlantic_metric, pacific_metric, alternative="two.sided", var.equal=FALSE)

# testy normalności Shapiro-Wilka 
shapiro.test(atlantic_metric)
shapiro.test(pacific_metric)

# test Manna-Whitneya-Wilcoxona (test sum rang Wilcoxona)
wilcox.test(atlantic_metric, pacific_metric, paired = FALSE)