library(ggplot2)

file_path <- "/Users/brandongoblirsch/Documents/school2024/MATH574 - Bayesian Stats/Mod6_Homework/data.csv"
data <- read_csv(file_path)

data_t <- t(data[, -1])
colnames(data_t) <- data$category
data_t <- as.data.frame(data_t)
head(data_t)

n_iterations <- 10000  # Number of iterations
n_causes <- ncol(data_t)  # Number of causes of death (columns in transposed data)
n_years <- nrow(data_t)  # Number of years (rows in transposed data)

# weakly informative prior hyperparameters
mu_0_prior <- 0
tau_2_prior <- 1
alpha_prior <- 2  # Shape parameter for inverse-gamma prior on variance
beta_prior <- 1  # Scale parameter for inverse-gamma prior on variance

# variables to store posterior samples
mu_samples <- matrix(0, nrow = n_iter, ncol = n_causes)  # Each column is mu_i for a cause
mu_0_samples <- numeric(n_iter)  # Global mean
tau_2_samples <- numeric(n_iter)  # Variance of the means

# initial values for the first iteration
mu_0_samples[1] <- 0
tau_2_samples[1] <- 1
sigma_2_samples[1] <- 1

for (iter in 2:n_iter) {
  # Step 1: Sample mu_i for each cause
  for (i in 1:n_causes) {
    n_i <- n_years  # Number of observations for each cause
    y_bar_i <- mean(data_t[, i])  # Mean of data for cause i
    
    # Posterior mean and variance for mu_i
    mu_i_var <- 1 / (n_i / sigma_2_samples[iter - 1] + 1 / tau_2_samples[iter - 1])
    mu_i_mean <- mu_i_var * (n_i * y_bar_i / sigma_2_samples[iter - 1] + mu_0_samples[iter - 1] / tau_2_samples[iter - 1])
    
    # Sample from the normal distribution
    mu_samples[iter, i] <- rnorm(1, mu_i_mean, sqrt(mu_i_var))
  }
  
  # Step 2: Sample mu_0 (global mean)
  mu_0_mean <- mean(mu_samples[iter, ])
  mu_0_var <- max(tau_2_samples[iter - 1] / n_causes, 1e-6)  # Prevent invalid variance
  mu_0_samples[iter] <- rnorm(1, mu_0_mean, sqrt(mu_0_var))
  
  # Step 3: Sample tau^2 (variance of the means)
  alpha_tau <- alpha_prior + n_causes / 2
  beta_tau <- beta_prior + sum((mu_samples[iter, ] - mu_0_samples[iter])^2) / 2
  tau_2_samples[iter] <- 1 / rgamma(1, alpha_tau, beta_tau)
  
  # Step 4: Sample sigma^2 (variance of the data)
  alpha_sigma <- alpha_prior + n_causes * n_years / 2
  beta_sigma <- beta_prior + sum(sapply(1:n_causes, function(i) sum((data_t[, i] - mu_samples[iter, i])^2))) / 2
  sigma_2_samples[iter] <- 1 / rgamma(1, alpha_sigma, beta_sigma)
}


burn_in <- 2000
mu_samples <- mu_samples[(burn_in+1):n_iter, ]
mu_0_samples <- mu_0_samples[(burn_in+1):n_iter]
tau_2_samples <- tau_2_samples[(burn_in+1):n_iter]
sigma_2_samples <- sigma_2_samples[(burn_in+1):n_iter]

# Plot trace plots to check convergence
plot(mu_0_samples, type = "l", main = "Trace Plot for mu_0")
plot(sigma_2_samples, type = "l", main = "Trace Plot for sigma^2")

# Posterior mean estimates
posterior_mu_0 <- mean(mu_0_samples)
posterior_sigma_2 <- mean(sigma_2_samples)
posterior_tau_2 <- mean(tau_2_samples)

posterior_summary <- apply(mu_samples, 2, function(x) {
  mean_val <- mean(x)
  ci_lower <- quantile(x, 0.025)
  ci_upper <- quantile(x, 0.975)
  return(c(mean = mean_val, lower_ci = ci_lower, upper_ci = ci_upper))
})

# Convert to a data frame for easy viewing
posterior_summary_df <- as.data.frame(t(posterior_summary))
colnames(posterior_summary_df) <- c("Posterior Mean", "Lower 95% CI", "Upper 95% CI")
rownames(posterior_summary_df) <- colnames(data_t)  # Use cause names as row names

# View the summary
posterior_summary_df

# Predictive distribution for a new (6th) cause
predictive_mu_6 <- rnorm(1000, mean = posterior_mu_0, sd = sqrt(posterior_tau_2))
predictive_y_6 <- rnorm(1000, mean = predictive_mu_6, sd = sqrt(posterior_sigma_2))

predictive_mean <- mean(predictive_y_6)
predictive_ci <- quantile(predictive_y_6, c(0.025, 0.975))

list(predictive_mean = predictive_mean, lower_95_ci = predictive_ci[1], upper_95_ci = predictive_ci[2])

# Plot the predictive distribution
hist(predictive_y_6, main = "Predictive Distribution for Hypothetical 6th Cause", xlab = "Annual Deaths")

sigma_2_samples <- numeric(n_iter)  # Variance of the data