# Load necessary libraries
library(tidyverse)
library(rstanarm)
library(bayesplot)


file_path <- "/Users/brandongoblirsch/Documents/school2024/MATH574 - Bayesian Stats/StudentsPerformance.csv"

# Read the CSV file
data <- read_csv(file_path)

# View the first few rows of the data
head(data)

# Summary statistics of the dataset
summary(data)

# Check for missing values
colSums(is.na(data))

######################
## Rename Variables ##
######################

names(data)[names(data) == "reading score"] <- "reading"
names(data)[names(data) == "writing score"] <- "writing"
names(data)[names(data) == "math score"] <- "math"
names(data)[names(data) == "parental level of education"] <- "parental_ed"
names(data)[names(data) == "race/ethnicity"] <- "race"
names(data)[names(data) == "test preparation course"] <- "test_prep"

# Convert categorical variables to factors
data$lunch <- factor(data$lunch, levels = c("standard", "free/reduced"))
data$test_prep <- factor(data$test_prep, levels = c("none", "completed"))

##################
## Lunch Status ##
##################
math_model <- stan_glm(math ~ lunch, data = data, family = gaussian(), 
                       prior = normal(0, 1), prior_intercept = normal(0, 1))

reading_model <- stan_glm(reading ~ lunch, data = data, family = gaussian(), 
                          prior = normal(0, 1), prior_intercept = normal(0, 1))

writing_model <- stan_glm(writing ~ lunch, data = data, family = gaussian(), 
                          prior = normal(0, 1), prior_intercept = normal(0, 1))

# Summarize the results to examine the effect of lunch status
summary(math_model)
summary(reading_model)
summary(writing_model)

post_math <- as.matrix(math_model)
post_reading <- as.matrix(reading_model)
post_writing <- as.matrix(writing_model)

# Plot credible intervals for lunch status
mcmc_intervals(post_math, pars = "lunchfree/reduced") + 
  xlab("Estimated Effect") + ggtitle("Math - Effect of Lunch Status")

mcmc_intervals(post_reading, pars = "lunchfree/reduced") + 
  xlab("Estimated Effect") + ggtitle("Reading - Effect of Lunch Status")

mcmc_intervals(post_writing, pars = "lunchfree/reduced") + 
  xlab("Estimated Effect") + ggtitle("Writing - Effect of Lunch Status")

##########################################
## Test Prep & Lunch Status Interaction ##
##########################################

# Fit Bayesian models for each subject
math_model <- stan_glm(math ~ lunch * test_prep, data = data, family = gaussian(), prior = normal(0, 1), prior_intercept = normal(0, 1))
reading_model <- stan_glm(reading ~ lunch * test_prep, data = data, family = gaussian(), prior = normal(0, 1), prior_intercept = normal(0, 1))
writing_model <- stan_glm(writing ~ lunch * test_prep, data = data, family = gaussian(), prior = normal(0, 1), prior_intercept = normal(0, 1))

# Summarize results
summary(math_model)
summary(reading_model)
summary(writing_model)

# Perform model diagnostics including trace plots and 
# effective sample size to confirm MCMC chains are mixing properly
post_math <- as.matrix(math_model, pars = "lunchfree/reduced:test_prepcompleted")
post_reading <- as.matrix(reading_model, pars = "lunchfree/reduced:test_prepcompleted")
post_writing <- as.matrix(writing_model, pars = "lunchfree/reduced:test_prepcompleted")

posterior_math <- as.array(math_model)
posterior_reading <- as.array(reading_model)
posterior_writing <- as.array(writing_model)

mcmc_trace(posterior_math, pars = "lunchfree/reduced:test_prepcompleted") +
  ggtitle("Trace Plot for Math Model - Lunch and Test Prep Interaction")

mcmc_trace(posterior_reading, pars = "lunchfree/reduced:test_prepcompleted") +
  ggtitle("Trace Plot for Reading Model - Lunch and Test Prep Interaction")

mcmc_trace(posterior_writing, pars = "lunchfree/reduced:test_prepcompleted") +
  ggtitle("Trace Plot for Writing Model - Lunch and Test Prep Interaction")

rhat_math <- rhat(math_model)
rhat_reading <- rhat(reading_model)
rhat_writing <- rhat(writing_model)
print(rhat_math)
print(rhat_reading)
print(rhat_writing)
summary(math_model)$stan_summary
summary(reading_model)$stan_summary
summary(writing_model)$stan_summary

# Plot credible intervals for each subject area
mcmc_intervals(
  post_math,
  prob = 0.95,
  prob_outer = 0.95,
  point_est = "mean"
) + ggtitle("Math - 95% Credible Interval for\n Lunch and Test Prep Interaction")

mcmc_intervals(
  post_reading,
  prob = 0.95,
  prob_outer = 0.95,
  point_est = "mean"
) + ggtitle("Reading - 95% Credible Interval for\n Lunch and Test Prep Interaction")

mcmc_intervals(
  post_writing,
  prob = 0.95,
  prob_outer = 0.95,
  point_est = "mean"
) + ggtitle("Writing - 95% Credible Interval for\n Lunch and Test Prep Interaction")


############
## Gender ##
############
math_model <- stan_glm(math ~ gender, data = data, family = gaussian(), 
                       prior = normal(0, 1), prior_intercept = normal(0, 1))

reading_model <- stan_glm(reading ~ gender, data = data, family = gaussian(), 
                          prior = normal(0, 1), prior_intercept = normal(0, 1))

writing_model <- stan_glm(writing ~ gender, data = data, family = gaussian(), 
                          prior = normal(0, 1), prior_intercept = normal(0, 1))

# Summarize the results to examine the effect of gender
summary(math_model)
summary(reading_model)
summary(writing_model)

post_math <- as.matrix(math_model)
post_reading <- as.matrix(reading_model)
post_writing <- as.matrix(writing_model)

# Plot credible intervals for gender
mcmc_intervals(post_math, pars = "gendermale") + 
  xlab("Estimated Effect") + ggtitle("Math - Effect of Gender")

mcmc_intervals(post_reading, pars = "gendermale") + 
  xlab("Estimated Effect") + ggtitle("Reading - Effect of Gender")

mcmc_intervals(post_writing, pars = "gendermale") + 
  xlab("Estimated Effect") + ggtitle("Writing - Effect of Gender")


####################################
## Test Prep & Gender Interaction ##
####################################

data$gender <- factor(data$gender, levels = c("male", "female"))

math_model <- stan_glm(math ~ gender * test_prep, data = data, family = gaussian(), 
                       prior = normal(0, 1), prior_intercept = normal(0, 1))
reading_model <- stan_glm(reading ~ gender * test_prep, data = data, family = gaussian(), 
                          prior = normal(0, 1), prior_intercept = normal(0, 1))
writing_model <- stan_glm(writing ~ gender * test_prep, data = data, family = gaussian(), 
                          prior = normal(0, 1), prior_intercept = normal(0, 1))

# Summarize the results to examine the interaction term
summary(math_model)
summary(reading_model)
summary(writing_model)

# Perform model diagnostics including trace plots and 
# effective sample size to confirm MCMC chains are mixing properly
post_math <- as.matrix(math_model, pars = "lunchfree/reduced:test_prepcompleted")
post_reading <- as.matrix(reading_model, pars = "lunchfree/reduced:test_prepcompleted")
post_writing <- as.matrix(writing_model, pars = "lunchfree/reduced:test_prepcompleted")

posterior_math <- as.array(math_model)
posterior_reading <- as.array(reading_model)
posterior_writing <- as.array(writing_model)

mcmc_trace(posterior_math, pars = "lunchfree/reduced:test_prepcompleted") +
  ggtitle("Trace Plot for Math Model - Lunch and Test Prep Interaction")

mcmc_trace(posterior_reading, pars = "lunchfree/reduced:test_prepcompleted") +
  ggtitle("Trace Plot for Reading Model - Lunch and Test Prep Interaction")

mcmc_trace(posterior_writing, pars = "lunchfree/reduced:test_prepcompleted") +
  ggtitle("Trace Plot for Writing Model - Lunch and Test Prep Interaction")

rhat_math <- rhat(math_model)
rhat_reading <- rhat(reading_model)
rhat_writing <- rhat(writing_model)
print(rhat_math)
print(rhat_reading)
print(rhat_writing)
summary(math_model)$stan_summary
summary(reading_model)$stan_summary
summary(writing_model)$stan_summary

# Extract posterior samples and plot the credible intervals for the interaction term
post_math <- as.matrix(math_model)
post_reading <- as.matrix(reading_model)
post_writing <- as.matrix(writing_model)

# Plot credible intervals for the interaction term
mcmc_intervals(post_math, pars = "gendermale:test_prepcompleted") + 
  xlab("Estimated Effect") + ggtitle("Math - Gender and Test Prep Interaction")

mcmc_intervals(post_reading, pars = "gendermale:test_prepcompleted") + 
  xlab("Estimated Effect") + ggtitle("Reading - Gender and Test Prep Interaction")

mcmc_intervals(post_writing, pars = "gendermale:test_prepcompleted") + 
  xlab("Estimated Effect") + ggtitle("Writing - Gender and Test Prep Interaction")
