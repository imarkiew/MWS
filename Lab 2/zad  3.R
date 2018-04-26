# setwd(...)

# Zad 3
# a)
X <- read.table("./norm", sep = "", head = FALSE)

n = length(X[, 1])
avg <- mean(X[, 1])
S <- sqrt(sum((X[, 1] - avg)^2) / (n - 1))

gammas <- c(0.9, 0.95, 0.99)

avg_confidence_intervals <- list()
variance_confidence_intervals <- list()
for(i in 1:length(gammas))
{
  avg_confidence_intervals[[i]] <- c(avg - (S/sqrt(n))*qt((1 + gammas[i])/2, n - 1), avg + (S/sqrt(n))*qt((1 + gammas[i])/2, n - 1))
  variance_confidence_intervals[[i]] <- c(((n - 1)*S^2)/(qchisq(1 - (1 - gammas[i])/2, n - 1)), ((n - 1)*S^2)/(qchisq((1 - gammas[i])/2, n - 1)))
}