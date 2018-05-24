# setwd(...)

# Zad 1
suicides_per_month <- c(1867, 1789, 1944, 2094, 2097, 1981, 1887, 2024, 1928, 2032, 1978, 1859)
days_per_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
number_of_months <- 12
number_of_days_per_year <- sum(days_per_month)
suicides_per_year = sum(suicides_per_month)
probability_of_suicide_per_day = 1/number_of_days_per_year
probability_of_suicide_per_month = days_per_month*probability_of_suicide_per_day
T <- sum(((suicides_per_month - suicides_per_year*probability_of_suicide_per_month)^2)/(suicides_per_year*probability_of_suicide_per_month))
alpha <- 0.9
stats_90 <- qchisq(1 - alpha, number_of_months - 1)
