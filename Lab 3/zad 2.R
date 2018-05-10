# setwd(...)

# Zad 2
n <- 20
t_mean <- 5.1

# a)
support_1 <-seq(0, 1, 0.01)
EX_1 <- 0.5
VX_1 <- 1
alpha_1 <- (EX_1)^2/VX_1
beta_1 <- VX_1/EX_1
priori_1 <- dgamma(support_1, shape = alpha_1, scale = beta_1)
raw_posteriori_1 <- function(x)
{
  dgamma(x, shape = alpha_1, scale = beta_1)*(x^n)*exp(-x*n*t_mean)
}
const_1 <- integrate(raw_posteriori_1, lower = 0, upper = Inf)$value 
posteriori_1 <- raw_posteriori_1(support_1)/const_1
plot(support_1, priori_1, ylim = c(0, 9), type = 'l', col = "blue", xlab = 'lambda', ylab = 'f(lambda)')
lines(support_1, posteriori_1, col = "red")
legend(x = "topright", 1, lty = 1, legend = c("a priori", "a posteriori"), col = c("blue", "red"))
title("E(X) = 0,5 ; Var(X) = 1")
# Znalezienie lambdy, dla której pstwo posteriori jest największe
max_prob_1 <- support_1[which.max(posteriori_1)]

# b)
support_2 <-seq(0, 1, 0.01)
EX_2 <- 10
VX_2 <- 20
alpha_2 <- (EX_2)^2/VX_2
beta_2 <- VX_2/EX_2
priori_2 <- dgamma(support_2, shape = alpha_2, scale = beta_2)
raw_posteriori_2 <- function(x)
{
  dgamma(x, shape = alpha_2, scale = beta_2)*(x^n)*exp(-x*n*t_mean)
}
const_2 <- integrate(raw_posteriori_2, lower = 0, upper = Inf)$value 
posteriori_2 <- raw_posteriori_2(support_2)/const_2
plot(support_2, priori_2, ylim = c(0, 9), type = 'l', col = "blue", xlab = 'lambda', ylab = 'f(lambda)')
lines(support_2, posteriori_2, col = "red")
legend(x = "topright", 1, lty = 1, legend = c("a priori", "a posteriori"), col = c("blue", "red"))
title("E(X) = 10 ; Var(X) = 20")
max_prob_2 <- support_2[which.max(posteriori_2)]