# setwd(...)

# Zad 1
los_1 <- read.table("./los1.txt", header = FALSE)[, 1]
los_2 <- read.table("./los2.txt", header = FALSE)[, 1]

# a)
los_1_mean <- mean(los_1)
los_2_mean <- mean(los_2)

los_diff <- los_1_mean - los_2_mean

# b)
los_1_var <- var(los_1)
los_2_var <- var(los_2)
los_1_length <- length(los_1)
los_2_length <- length(los_2)
los_var = ((los_1_length - 1)*los_1_var + (los_2_length - 1)*los_2_var)/(los_1_length + los_2_length - 2)

# c)
error_sd <- sqrt(los_var*(1/los_1_length + 1/los_2_length))

# e)
means_test <- t.test(los_1, los_2, var.equal = TRUE)