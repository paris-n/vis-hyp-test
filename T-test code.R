library(ggplot2)
library(tidyverse)

N <- 50#1000  
n1 <- 10    
n2 <- 10
sigma1 <- 1
sigma2 <- 1    
alpha <- 0.05

mu_star_range <- c(0,0.5,1,1.5,2)#seq(-2.5,2.5,by=0.1)#-10:10

#power <- numeric(length(mu_star_range))

G <- expand.grid(rep = 1:N, mu_star = mu_star_range)
G_shuffle <- G[sample(nrow(G)),]
decisions <- numeric(nrow(G_shuffle))

for(i in seq_len(nrow(G_shuffle))){

#for (mu_star_i in seq_along(mu_star_range)) {
  
#  decisions <- numeric(N)
  
#  mu_star = mu_star_range[mu_star_i]
  
#  for (i in 1:N) {
   
  mu_star = G_shuffle$mu_star[i]
    
    X <- rnorm(n1, mean = 0, sd = sqrt(sigma1))
    
    Y <- rnorm(n2, mean = mu_star, sd = sqrt(sigma2))
    
    t_test <- t.test(X, Y, var.equal = TRUE)
    
    if (t_test$p.value < alpha) {
      
      decisions[i] <- 1
    } else {
      
      decisions[i] <- 0
    }

    ####
    #create line-up figure
    #get target image id
    #choosen_image_id <- readLines(n=1)
    #decision_lineup[i] <- as.numeric(choosen_image_id) == target_image_id    
    
#  }
#     power[mu_star_i] <- mean(decisions)
}

G_shuffle$decisions <- decisions
power_data <- G_shuffle %>% 
  group_by(mu_star) %>% 
  summarise(power = mean(decisions))


#  power_data <- data.frame(mu_star = mu_star_range, power = power)

  ggplot(power_data, aes(x = mu_star, y = power)) +
    
    geom_line(color = "blue") +
    
    geom_hline(yintercept = alpha, color = "red", linetype = "dashed") +
    
    labs(title = "Power Curve", x = expression(mu), y = "Power") +
    
    theme_minimal(base_size = 14) +
    
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(ylim=c(0,1))


