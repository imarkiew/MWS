# setwd(...)

# Zad 3
v <- c(0, 33.0, 33.0, 49.1, 65.2, 78.5, 93.0)
y <- c(0, 4.7, 4.1, 10.3, 22.3, 34.4, 44.4)

model_1 <- lm(formula = y ~ v - 1)

y_sqrt <- sqrt(y)

model_2 <- lm(formula = y_sqrt ~ v - 1)

plot(v, y)
abline(model_1, col = "red", lwd = 2)
grid() 
title("Regresja liniowa y ~ v")
legend("topleft", 1, lty = c(0, 1), c("oryginalne punkty", "regresja"), pch = c("o", ""), col = c("black", "red"))

plot(v, y_sqrt)
abline(model_2, col = "red", lwd = 2)
grid() 
title("Regresja liniowa sqrt(y) ~ v")
legend("topleft", 1, lty = c(0, 1), c("oryginalne punkty", "regresja"), pch = c("o", ""), col = c("black", "red"))
