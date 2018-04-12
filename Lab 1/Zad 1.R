# setwd(...)

# Wczytywanie pliku 
X <- read.table("./notowania/ALIOR.mst", sep = ",", head = TRUE)
# wybór potrzebnej kolumny
X <- X$X.OPEN.
# obliczenie względnych różnic między kolejnymi otwarciami 
X <- (diff(X)*100)/X[1:length(X) - 1]

# Zad 1 
# a)
# histogram 
hist(X, breaks = 100, freq = FALSE, col = "blue", main = "Histogram i estymajcje gęstości funkcji prawdopodobieństwa", xlab = "zmiana kursu otwarcia [%]", ylab = "gęstość / częstoś")
# gęstość empiryczna z kernel density estimation 
lines(density(X), col = "green", lwd = 3)
# obliczenie średniej arytmetycznej - estymator wartości średniej
m = mean(X)
# obliczenie estymaty odchylenia standardowego 
sigma = sd(X)
# określenie nośnika 
x = seq(min(X), max(X),  length.out = 100)
# dorysowanie wyestymowanej, empirycznej funkcji gęstości prawdopodobieństwa
lines(x, dnorm(x, m, sigma), col = "red", lwd = 3)
legend(x = "topleft", 1, lty=1, legend=c("kernel density estimation", "pojedynczy Gauss"), col=c("green", "red"))

# b) 
names <- c("ALIOR", "PZU", "MBANK")
# lista może mieć różne typy i długości danych
val_list = list()
for(i in 1:length(names))
{
  path = paste("./notowania/", names[i], ".mst", sep = "")
  values <- read.table(path, sep = ",", head = TRUE)[, 3]
  val_list[[i]] <- (diff(values)*100)/values[1:length(values) - 1]
}

boxplot(val_list, names = names, main = "Wykresy pudełkowe zmian kursów otwarcia", xlab = "spółka", ylab = "cena otwarcia [%]", col = "red")


