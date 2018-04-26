# setwd(...)
library(boot)

# Zad 1
# a)
# Wczytywanie pliku 
X <- read.table("./skrety", sep = "", head = FALSE)
# estymacja parametru
lambda_ <- mean(X[, 1])
# zliczanie wystąpień poszczególnych wydarzeń
data <- aggregate(X[, 1], list(X[, 1]), length)
# określenie nośnika 
n = 0:(length(data[, 1]) - 1)
# rozkład Poissona
poisson <- dpois(n, lambda = lambda_)
# histogram 
hist(X[, 1], breaks = -0.5:(max(X[, 1] + 1) - 0.5), freq = FALSE, col = "blue", main = "Histogram i estymajcje gęstości funkcji prawdopodobieństwa", xlab = "liczba skrętów w prawo", ylab = "gęstość / częstoś")
# rozkład masy
points(n, poisson, col = "red", lwd = 4)
# interpolacja liniowa
lines(n, poisson, col = "green", lwd = 3)
legend(x = "topright", 1, lty=1, legend=c("interpolowany rozkład", "masy rozkładu Poissona"), col=c("green", "red")) 

# b)
# funkcja generująca populacje i wyliczajaca średnią
mean_ <-function(x, indices)
{
  xx <- x[indices, 1]
  mean(xx)
}
lambdas <- boot(X, statistic = mean_, R = 1000)
std_dev <- sd(lambdas$t)