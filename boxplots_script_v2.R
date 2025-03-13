library(ggplot2)
library(tidyverse)
library(nullabor)  # For lineup protocol

# Parameters
N <- 50
n1 <- 10
n2 <- 10
sigma1 <- 1
sigma2 <- 1    
alpha <- 0.05

mu_star_range <- c(0, 0.5, 1, 1.5, 2)  

if(!file.exists("G_Shuffle.csv")){
  G <- expand.grid(rep=1:N,mu=mu_star_range)
  set.seed(1)
  G_shuffle <- G[sample(nrow(G)),]
  
  G_shuffle$seed <- sample(1:(nrow(G)*10), size=nrow(G), replace = FALSE)
  G_shuffle$done <- FALSE
  G_shuffle <- as_tibble(G_shuffle)
  write_csv(G_shuffle, file = "G_Shuffle.csv")
}else{
   G_shuffle <- read_csv("G_Shuffle.csv")
}

results_file <- "lineup_results.csv"


if (file.exists(results_file)) {
  results <- read_csv(results_file, col_types = cols(
    trial = col_integer(),
    mu_star = col_double(),
    correct_answer = col_integer(),
    user_response = col_integer(),
    correct = col_logical()
  ))
  trial_counter <- max(results$trial) + 1  # Continue numbering trials
  cat("\n✅ Loaded previous results. Starting from Trial", trial_counter, "\n")
} else {
  results <- tibble(
    trial = integer(),
    mu_star = numeric(),
    correct_answer = integer(),
    user_response = integer(),
    correct = logical()
  )
  trial_counter <- 1
  cat("\n🚀 No previous results found. Starting fresh.\n")
}


run_lineup_test <- function(trial_number, mu_star=0, seed=123) {
  
  set.seed(seed)
  
  #mu_star <- sample(mu_star_range, 1)
  
  X <- rnorm(n1, mean = 0, sd = sqrt(sigma1))
  Y <- rnorm(n2, mean = mu_star, sd = sqrt(sigma2))
  
  
  null_sets <- replicate(19, {
    X_null <- rnorm(n1, mean = 0, sd = sqrt(sigma1))
    Y_null <- rnorm(n2, mean = 0, sd = sqrt(sigma2))
    tibble(value = c(X_null, Y_null), sample_id = rep(c("X", "Y"), c(n1, n2)))
  }, simplify = FALSE)
  
 
  real_set <- tibble(value = c(X, Y), sample_id = rep(c("X", "Y"), c(n1, n2)))
  
 
  all_sets <- bind_rows(null_sets, real_set, .id = "set_id")
  
  shuffled_ids <- sample(1:20)  
  all_sets$set_id <- rep(shuffled_ids, each=nrow(all_sets) / 20)  
  
  correct_answer <- shuffled_ids[20]  
  
  
  plot_lineup <- function(data) {
    ggplot(data, aes(x = sample_id, y = value)) +
      geom_boxplot(aes(fill = as.factor(set_id)), alpha = 0.6) +  
      facet_wrap(~set_id, ncol = 5) +  
      labs(x = "Group", y = "Value", fill = "Group") + 
      theme_minimal(base_size = 14) +
      theme(plot.title = element_blank(),  
            axis.title.x = element_text(size = 10), 
            axis.title.y = element_text(size = 10),
            axis.text.x = element_text(size = 8), 
            axis.text.y = element_text(size = 8),
            legend.position = "none")  # No legend
  }
  
  print(plot_lineup(all_sets))
  
  
  repeat {
    user_guess <- suppressWarnings(as.integer(readline("\nEnter the number of the panel you think is real (1-20): ")))
    
    if (!is.na(user_guess) && user_guess >= 1 && user_guess <= 20) {
      break  # Valid input, exit loop
    }
    
    cat("⚠️ Invalid input! Please enter a number between 1 and 20.\n")
  }
  
  
  return(tibble(trial = trial_number, mu_star = mu_star, correct_answer = correct_answer,
                user_response = user_guess, correct = (user_guess == correct_answer)))
}


continue_testing <- TRUE
#G_shuffle <- read_csv("G_shuffle.csv")
while (continue_testing) {
  
  
  repeat {
    num_trials <- suppressWarnings(as.integer(readline("\nHow many tests do you want to take this time? (Enter a number): ")))
    
    if (!is.na(num_trials) && num_trials > 0) {
      break 
    }
    
    cat("⚠️ Invalid input! Please enter a valid positive number.\n")
  }
  
  last <- sum(G_shuffle$done)
  for (i in 1:num_trials) {
    new_result <- run_lineup_test(trial_counter, 
                                  mu_star = G_shuffle$mu[last+i], 
                                  seed = G_shuffle$seed[last+i])
    G_shuffle$done[last+i] <- TRUE
    results <- bind_rows(results, new_result)
    trial_counter <- trial_counter + 1
    if(all(G_shuffle$done)) stop("COMPLETE!!!")
  }
  
  write_csv(G_shuffle, file = "G_Shuffle.csv")
  write_csv(results, results_file)
  
  cat("\n✅ Test batch completed. Your results have been saved.\n")
  
  
  check_input <- TRUE
  while(check_input){
    continue_input <- readline(prompt = "Do you want to continue testing? (yes/no): ")
    if (tolower(continue_input) %in% c("n","no")) {
      continue_testing <- FALSE
      check_input <- FALSE
    }else if(tolower(continue_input) %in% c("y","yes")){
      continue_testing <- TRUE
      check_input <- FALSE
    }else{
     cat("Invalid response, try again.\n")
    }
  }
}



power_data <- results %>%
  group_by(mu_star) %>%
  summarise(power = mean(correct))  

# Plot the power curve
ggplot(power_data, aes(x = mu_star, y = power)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = alpha, color = "red", linetype = "dashed") +
  labs(title = "User-Based Power Curve", x = expression(mu), y = "Power") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 1))

# Print final results
print(results)

cat("\n🎯 All tests are finished. You can restart this script later, and it will continue from where you left off!\n")
