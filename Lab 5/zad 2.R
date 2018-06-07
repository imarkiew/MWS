# setwd(...)

# Zad 2
t <- read.table("./lozyska.txt", sep = ",", header = TRUE)
t_1 <- t$Typ.I
t_2 <- t$Typ.II

# a)
t_test <- t.test(t_1, t_2, var.equal = FALSE)

# b)
wilcox_test <- wilcox.test(t_1, t_2)

# d)
# bootstrap nieparametryczny
iterations = 1e6;
t_1_boot <- sample(t_1, iterations, replace = TRUE)
t_2_boot <- sample(t_2, iterations, replace = TRUE)

boot_probability <- sum(t_1_boot > t_2_boot)/iterations