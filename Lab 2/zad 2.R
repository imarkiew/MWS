# setwd(...)

# Zad 2
# b)
X <- read.table("./fotony", sep = "", head = FALSE)
# pierwszy moment
m_1=mean(X[, 1])
# drugi moment
m_2=mean(X[, 1]^2)
# metoda momentów
mm_alpha <- (m_1^2)/(m_2 - m_1^2) 
mm_beta <- (m_2 - m_1^2)/m_1

# metoda największej wiarygodności
const <- -log(m_1) + mean(log(X[, 1]))
equation <- function(alpha)
{
  y = log(alpha) - digamma(alpha) + const
}

mle_alpha <- uniroot(equation, lower = 1e-6, upper=3)$root
mle_beta <- m_1/mle_alpha

# a), c)
hist(X[, 1], breaks = 100, freq = FALSE, col = "blue", main = "Histogram i estymajcje gęstości funkcji prawdopodobieństwa", xlab = "czas miedzy kolejnymi rejestracjami fotonów", ylab = "gęstość / częstoś")
x = seq(min(X[, 1]), max(X[, 1]),  length.out = 100)
lines(x, dgamma(x, shape = mm_alpha, scale = mm_beta), col = "green", lwd = 3)
lines(x, dgamma(x, shape = mle_alpha, scale = mle_beta), col = "red", lwd = 3)
legend(x = "topright", 1, lty=1, legend=c("metoda momentów", "metoda największej wiarygodności"), col=c("green", "red")) 

# d)
bootstrap_mm_alphas <- vector()
bootstrap_mm_betas <- vector()
bootstrap_mle_alphas <- vector()
bootstrap_mle_betas <- vector()

for(i in 1:1000)
{
  x_mm <- rgamma(length(X[, 1]), shape = mm_alpha, scale = mm_beta)
  bootstrap_m_1 <- mean(x_mm)
  bootstrap_m_2 <- mean(x_mm^2)
  
  bootstrap_mm_alphas[i] <- (bootstrap_m_1^2)/(bootstrap_m_2 - bootstrap_m_1^2) 
  bootstrap_mm_betas[i] <- (bootstrap_m_2 - bootstrap_m_1^2)/bootstrap_m_1
  
  const <- -log(bootstrap_m_1) + mean(log(x_mm))
  bootstrap_mle_alphas[i] <- uniroot(equation, lower = 1e-6, upper=3)$root
  bootstrap_mle_betas[i] <- bootstrap_m_1/bootstrap_mle_alphas[i]
}

std_dev_mm_alpha <- sd(bootstrap_mm_alphas)
std_dev_mm_beta <- sd(bootstrap_mm_betas)
std_dev_mle_alpha <- sd(bootstrap_mle_alphas)
std_dev_mle_beta <- sd(bootstrap_mle_betas)

a = (1 - 0.95)/2
bootstrap_mm_alpha_confidence_interval  <- quantile(bootstrap_mm_alphas, c(a, 1 - a))
bootstrap_mm_beta_confidence_interval  <- quantile(bootstrap_mm_betas, c(a, 1 - a))
bootstrap_mle_alpha_confidence_interval  <- quantile(bootstrap_mle_alphas, c(a, 1 - a))
bootstrap_mle_beta_confidence_interval  <- quantile(bootstrap_mle_betas, c(a, 1 - a))