library(ggplot2)
library(tidyverse)

N <- 50  
n <- 30    
alpha <- 0.05   

df_range <- c(1,5,10,20,30)  

set.seed(123)
G <- expand.grid(rep = 1:N, df = df_range)

G_shuffle <- G[sample(nrow(G)),] 

decisions <- numeric(nrow(G_shuffle))  

for (i in seq_len(nrow(G_shuffle))) {
  
  df_value <- G_shuffle$df[i]  
  
  X <- rt(n, df = df_value)
  
  shapiro_test <- shapiro.test(X)
  
  if (shapiro_test$p.value < alpha) {
    
    decisions[i] <- 1
    
  } else {
    
    decisions[i] <- 0
  }
}

G_shuffle$decisions <- decisions
power_data <- G_shuffle %>%
  group_by(df) %>%
  summarise(power = mean(decisions))

ggplot(power_data, aes(x = df, y = power)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_hline(yintercept = alpha, color = "red", linetype = "dashed") +
  labs(title = "Power Curve of S-W Normality Test",
       x = "Degrees of Freedom (t-distribution)",
       y = "Power (Rejection Rate)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0,1))
