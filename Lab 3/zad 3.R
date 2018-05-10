# setwd(...)

# Zad 2

n = 100
# ilość zepsutych urządzeń
broken = 0 
theta = broken/n
support = seq(0, 1, 0.01)

# a)
alpha_1_priori <- 1
beta_1_priori <- 1
priori_1 <- dbeta(support, alpha_1_priori, beta_1_priori)
alpha_1_posteriori <- alpha_1_priori + broken
beta_1_posteriori <- beta_1_priori + n - broken
posteriori_1 <- dbeta(support, alpha_1_posteriori, beta_1_posteriori)
plot(support, priori_1, ylim = c(0, 15), type = 'l', col = "blue", xlab = 'theta', ylab = 'f(theta)')
lines(support, posteriori_1, col = "red", xlab = 'p', ylab = 'f(theta)')
title("Rozkład a priori i a posteriori parametru \"theta\" dla alpha = 1, beta = 1")
legend(x = "topright", 1, lty = 1, legend = c("a priori", "a posteriori"), col = c("blue", "red"))
EX_1 = alpha_1_posteriori / (alpha_1_posteriori + beta_1_posteriori)
VX_1 = alpha_1_posteriori * beta_1_posteriori / (((alpha_1_posteriori + beta_1_posteriori)^2)*(alpha_1_posteriori + beta_1_posteriori + 1))
# p, dla którego pstwo a posteriori jest największe 
max_prob_1 <- support[which.max(posteriori_1)]

# b)
alpha_2_priori <- 0.5
beta_2_priori <- 5
priori_2 <- dbeta(support, alpha_2_priori, beta_2_priori)
alpha_2_posteriori <- alpha_2_priori + broken
beta_2_posteriori <- beta_2_priori + n - broken
posteriori_2 <- dbeta(support, alpha_2_posteriori, beta_2_posteriori)
plot(support, priori_2, ylim = c(0, 15), type = 'l', col = "blue", xlab = 'theta', ylab = 'f(theta)')
lines(support, posteriori_2, col = "red", xlab = 'theta', ylab = 'f(theta)')
title("Rozkład a priori i a posteriori parametru \"theta\" dla alpha = 0.5, beta = 5")
legend(x = "topright", 1, lty = 1, legend = c("a priori", "a posteriori"), col = c("blue", "red"))
EX_2 = alpha_2_posteriori / (alpha_2_posteriori + beta_2_posteriori)
VX_2 = alpha_2_posteriori * beta_2_posteriori / (((alpha_2_posteriori + beta_2_posteriori)^2)*(alpha_2_posteriori + beta_2_posteriori + 1))
max_prob_2 <- support[which.max(posteriori_2)]