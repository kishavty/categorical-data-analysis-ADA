library(binom)
library(ggplot2)
data <- read.csv2(file="choroba.csv", header=TRUE)
data

liczba_chorych <- 0

#Pętla for do zliczania jedynki
for (el in data$CHORY_ZD) {
  if (el == 1) {
    liczba_chorych <- liczba_chorych + 1
  }
}
print(liczba_chorych)


conf_intervals <- binom.confint(x = liczba_chorych, n = 196, conf.level = 0.95, methods = "all")
interval_lengths <- numeric(length(conf_intervals))  

for (i in 1:nrow(conf_intervals)) {
  interval_lengths[i] <- conf_intervals$upper[i] - conf_intervals$lower[i]
}

result <- data.frame(Method = conf_intervals[1], ci_length = interval_lengths)
print(result)

names <- c("agresti-coull", "asymptotic", "bayes", "cloglog", "exact", "logit", "probit", "profile", "lrt", "prop.test", "wilson")


ggplot(result, aes(x = 1:11, y = ci_length, label = names)) +
  geom_point() + 
  geom_text(hjust = 0.3, vjust = -0.5) +
  labs(title = "Porównanie długości przedziałów ufności",
       x = "Metody",
       y = "Długość przedziału ufności") + 
  theme(axis.text.x = element_text(angle = 1, hjust = 1), 
        plot.title = element_text(hjust = 0.5))
