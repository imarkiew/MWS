# setwd(...)

# Zad 1
# założony rozkład priori - beta(2, 2)
alpha_priori <- 2
beta_priori <- 2
x <- seq(0, 1, 0.01)
priori <- dbeta(x, alpha_priori, beta_priori)
plot(x, priori, ylim = c(0, 6), type = 'l', col = "blue", xlab = 'p', ylab = 'f(p)')
title("Rozkład a priori i a posteriori parametru \"p\"")

# seria 20 rzutów
first_throws <- c(0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1)
# pierwszy rozkład a posteriori
alpha_1_posteriori <- alpha_priori + sum(first_throws)
beta_1_posteriori <- beta_priori + length(first_throws) - sum(first_throws)
posteriori_1 <- dbeta(x, alpha_1_posteriori, beta_1_posteriori)
lines(x, posteriori_1, col = "green", xlab = 'p', ylab = 'f(p)')
EX_1 = alpha_1_posteriori / (alpha_1_posteriori + beta_1_posteriori)
VX_1 = alpha_1_posteriori * beta_1_posteriori / (((alpha_1_posteriori + beta_1_posteriori)^2)*(alpha_1_posteriori + beta_1_posteriori + 1))

# seria 40 rzutów
second_throws <- as.vector(rbind(first_throws, c(1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0))) 
# drugi rozkład a posteriori
alpha_2_posteriori <- alpha_priori + sum(second_throws)
beta_2_posteriori <- beta_priori + length(second_throws) - sum(second_throws)
posteriori_2 <- dbeta(x, alpha_2_posteriori, beta_2_posteriori)
lines(x, posteriori_2, col = "red", xlab = 'p', ylab = 'f(p)')
EX_2 = alpha_2_posteriori / (alpha_2_posteriori + beta_2_posteriori)
VX_2 = alpha_2_posteriori * beta_2_posteriori / (((alpha_2_posteriori + beta_2_posteriori)^2)*(alpha_2_posteriori + beta_2_posteriori + 1))

legend(x = "topright", 1, lty = 1, legend = c("a priori", "a posteriori 20", "a posteriori 40"), col = c("blue", "green", "red"))