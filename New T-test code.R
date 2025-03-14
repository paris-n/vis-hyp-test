library(ggplot2)
library(tidyverse)

N <- 50#1000  
n1 <- 10    
n2 <- 10
sigma1 <- 1
sigma2 <- 1    
alpha <- 0.05

mu_star_range <- c(0,0.5,1,1.5,2)

G <- expand.grid(rep = 1:N, mu_star = mu_star_range)
G_shuffle <- G[sample(nrow(G)),]
decisions <- numeric(nrow(G_shuffle))

for(i in seq_len(nrow(G_shuffle))){
  
  mu_star = G_shuffle$mu_star[i]
  
  X <- rnorm(n1, mean = 0, sd = sqrt(sigma1))
  
  Y <- rnorm(n2, mean = mu_star, sd = sqrt(sigma2))
  
  t_test <- t.test(X, Y, var.equal = TRUE)
  
  if (t_test$p.value < alpha) {
    
    decisions[i] <- 1
  } else {
    
    decisions[i] <- 0
  }
  
}

G_shuffle$decisions <- decisions
power_data <- G_shuffle %>% 
  group_by(mu_star) %>% 
  summarise(power = mean(decisions))

ggplot(power_data, aes(x = mu_star, y = power)) +
  
  geom_line(color = "blue") +
  
  geom_hline(yintercept = alpha, color = "red", linetype = "dashed") +
  
  labs(title = "Power Curve", x = expression(mu), y = "Power") +
  
  theme_minimal(base_size = 14) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim=c(0,1))
