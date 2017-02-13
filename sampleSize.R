# Correction factor
R <- function(r, p, rho) {
  l1 <- (1 + (r - 1) * rho) / r
  l2 <- (p * rho^2) / (1 + (p - 1) * rho)
  return(l1 - l2)
}

# Sample size calculator
n <- function(Delta, r, p, rho, alpha = 0.05, beta = 0.8) {
  f <- (qnorm(1 - alpha/2) + qnorm(beta))^2
  R.int <- R(r, p, rho)
  print(paste("f =", round(f, 2)))
  print(paste("R =", round(R.int, 2)))
  n <- (2 / Delta^2) * R.int * f
  print(paste("Sample size per group required =", ceiling(n)))
  invisible(n)
}

# Examples
n(0.4, 1, 1, 0.7)
n(0.4, 2, 1, 0.7)
n(0.4, 3, 1, 0.7)

# Overall plot
rp <- expand.grid(1:8, 1:8)
colnames(rp) <- c("r", "p")
rp$n <- apply(rp, 1, function(x) n(0.4, x[1], x[2], 0.7))
rp$n <- rp$n / rp$n[1]

library(ggplot2)
library(scales)
ggplot(aes(x = factor(r), y = n, colour = factor(p)), data = rp) +
  geom_line(aes(group = factor(p))) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    colour = "v (Number of baseline measurements)",
    y = "w (Number of follow-up measurements)",
    x = "Sample size relative to a trial with v = 1 and w = 1"
  ) +
  theme(legend.position = "bottom")
