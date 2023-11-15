library(binom)
library(ggplot2)

n <- 30
p <- seq(0, 1, 0.05)

MC <- 50
methods <- c("exact", "asymptotic", "bayes")
DF <- data.frame(Method = character(),
                      N = numeric(),
                      P = numeric(),
                      Coverage = numeric(),
                      Length = numeric(),
                      stringsAsFactors = FALSE)

for (i in p) {
  for (method in methods) {
    coverage <- numeric()
    ci_length <- numeric()
    for (step in 1:MC) {
      
      x <- rbinom(MC, n, i)
      ci <- binom.confint(x, n = n, conf.level = 0.95, method = method) #confidence interval
      
      check <- (ci$lower <= i) & (i <= ci$upper) #check if in ci
      
      if (any(check)) {
        ci_length[step] <- ci$upper[check] - ci$lower[check]
        coverage[step] <- check
      }
    }
    
    df <- data.frame(
      Method = method,
      N = n,
      P = i,
      Coverage = sum(coverage)/MC,
      Length = mean(ci_length),
      stringsAsFactors = FALSE
    )
    DF <- rbind(DF, df)
  }
}


print(results)

ggplot(DF, aes(x = P, y = Coverage, group = Method, color = Method)) +
  geom_line(size = 1.2) +
  facet_wrap(~N) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "NA") +
  geom_vline(xintercept = c(0.3, 0.5, 0.8), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) + 
  labs(x = "p", y = "Pokrycie") +
  annotate("text", x = 0.08, y = 0.955, label = "Poziom ufności 0.95", size = 3)

ggplot(DF, aes(x = P, y = Length, group = Method, color = Method)) +
  geom_line(size = 1.2) +
  facet_wrap(~N) +
  geom_vline(xintercept = c(0.3, 0.5, 0.8), linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(x = "p", y = "Długość przedziału ufności")
