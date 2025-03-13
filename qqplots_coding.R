library(ggplot2)
library(tidyverse)

# Parameters
N <- 50   
n <- 30   
alpha <- 0.05   

df_range <- c(1, 5, 10, 20, 30)  # Different degrees of freedom for t-distribution

if (!file.exists("G_Shuffle_Normality.csv")) {
  G <- expand.grid(rep = 1:N, df = df_range)
  set.seed(1)
  G_shuffle <- G[sample(nrow(G)), ]  
  G_shuffle$done <- FALSE
  G_shuffle$seed <- sample(1:(nrow(G) * 10), size = nrow(G), replace = FALSE)
  write_csv(G_shuffle, file = "G_Shuffle_Normality.csv")
} else {
  G_shuffle <- read_csv("G_Shuffle_Normality.csv")
}

results_file <- "normality_results.csv"

# Load or create results file
if (file.exists(results_file)) {
  results <- read_csv(results_file, col_types = cols(
    trial = col_integer(),
    df = col_double(),
    p_value = col_double(),
    correct_answer = col_integer(),
    user_response = col_integer(),
    correct = col_logical()
  ))
  trial_counter <- max(results$trial) + 1
  cat("\n✅ Loaded previous results. Starting from Trial", trial_counter, "\n")
} else {
  results <- tibble(
    trial = integer(),
    df = numeric(),
    p_value = numeric(),
    correct_answer = integer(),
    user_response = integer(),
    correct = logical()
  )
  trial_counter <- 1
  cat("\n🚀 No previous results found. Starting fresh.\n")
}

run_qq_lineup_test <- function(trial_number, df_value, seed=321) {
  
  set.seed(seed)
  
  X <- rt(n, df = df_value)  
  
  shapiro_test <- shapiro.test(X)
  p_value <- shapiro_test$p.value
  decision <- ifelse(p_value < alpha, 1, 0)  # 1 = reject normality, 0 = fail to reject
  
  null_sets <- replicate(19, rt(n, df = 30), simplify = FALSE)
  
  real_set <- X
  
  all_sets <- bind_rows(
    tibble(value = unlist(null_sets), sample_id = rep(1:19, each = n)),
    tibble(value = real_set, sample_id = 20)
  )
  
  shuffled_ids <- sample(1:20) 
  all_sets$sample_id <- rep(shuffled_ids, each = n)
  
  correct_answer <- shuffled_ids[20] 
  
  plot_qq_lineup <- function(data) {
    ggplot(data, aes(sample = value)) +
      stat_qq() +
      stat_qq_line() +
      facet_wrap(~ sample_id, ncol = 5) +
      labs(title = "QQ-Plot Lineup Test") +
      theme_minimal()
  }
  
  print(plot_qq_lineup(all_sets))
  
  repeat {
    user_guess <- suppressWarnings(as.integer(readline("\nEnter the number of the panel you think is real (1-20): ")))
    if (!is.na(user_guess) && user_guess >= 1 && user_guess <= 20) {
      break
    }
    cat("⚠️ Invalid input! Please enter a number between 1 and 20.\n")
  }
  
  return(tibble(trial = trial_number, df = df_value, p_value = p_value, correct_answer = correct_answer, user_response = user_guess,
                correct = (user_guess == correct_answer)))
}

continue_testing <- TRUE

while (continue_testing) {
  
  repeat {
    num_trials <- suppressWarnings(as.integer(readline("\nHow many normality tests do you want to take this time? (Enter a number): ")))
    if (!is.na(num_trials) && num_trials > 0) break
    cat("⚠️ Invalid input! Please enter a valid positive number.\n")
  }
  
  last <- sum(G_shuffle$done)
  for (i in 1:num_trials) {
    new_result <- run_qq_lineup_test(trial_counter, 
                                     df_value = G_shuffle$df[last + i], 
                                     seed = G_shuffle$seed[last + i])
    
    G_shuffle$done[last + i] <- TRUE
    results <- bind_rows(results, new_result)
    trial_counter <- trial_counter + 1
    
    if (all(G_shuffle$done)) stop("COMPLETE!!!")
  }
  
  write_csv(G_shuffle, file = "G_Shuffle_Normality.csv")
  write_csv(results, results_file)
  
  cat("\n✅ Test batch completed. Your results have been saved.\n")
  
  continue_input <- readline(prompt = "Do you want to continue testing? (yes/no): ")
  if (tolower(continue_input) != "yes") {
    continue_testing <- FALSE
  }
}


power_data_normality <- results %>%
  group_by(df) %>%
  summarise(power = mean(correct))  

# Plot the power curve for normality test
ggplot(power_data_normality, aes(x = df, y = power)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = alpha, color = "red", linetype = "dashed") +
  labs(title = "Power Curve for Normality Test", x = "Degrees of Freedom", y = "Power") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 1))

print(results)
cat("\n🎯 Normality tests are finished. You can restart later, and it will continue where you left off!\n")
