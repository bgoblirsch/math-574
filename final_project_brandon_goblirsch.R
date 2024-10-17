library(rstanarm)
library(tidytuesdayR)
library(ggplot2)

### Start of TidyTuesday Import instructions

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

ratings <- tuesdata$ratings
details <- tuesdata$details

### End of TidyTuesday import instructions 

head(ratings, 20)
head(details)

# there are no missing values in the parameters of interest
colSums(is.na(games)) 

summary(details$maxplaytime)
summary(details$minplaytime)
summary(details$playingtime)

# log transformation for play time variables
details <- details %>%
  mutate(log_minplaytime = log(minplaytime + 1),
         log_maxplaytime = log(maxplaytime + 1),
         log_playingtime = log(playingtime + 1))

games <- merge(ratings, details, by = "id")

bayes_lm <- stan_glm(average ~ log_minplaytime + log_maxplaytime + minplayers + maxplayers,
                     data = games,
                     family = gaussian(),
                     prior = normal(0, 10), # Weakly informative priors
                     chains = 4, iter = 2000)

summary(bayes_lm)

posterior <- posterior_interval(bayes_lm, prob = 0.95)
print(posterior)

# Predictions
short_game_data <- data.frame(log_minplaytime = log(10), log_maxplaytime = log(20), 
                            minplayers = 2, maxplayers = 6)
predicted_rating <- posterior_predict(bayes_lm, newdata = short_game_data)
cat("Predicted rating:", mean(predicted_rating), "\n")

long_game_data <- data.frame(log_minplaytime = log(180), log_maxplaytime = log(3000), 
                              minplayers = 2, maxplayers = 6)
predicted_rating <- posterior_predict(bayes_lm, newdata = long_game_data)
cat("Predicted rating:", mean(predicted_rating), "\n")


# Plot posterior distributions of the regression coefficients
plot(bayes_lm, pars = c("log_minplaytime", "log_maxplaytime", "minplayers", "maxplayers"), 
     prob = 0.95) +
  ggtitle("Posterior Distributions of Regression Coefficients")

# PPC Plot to chekc model's goodness of fit
y_rep <- posterior_predict(bayes_lm)
ppc_dens_overlay(y = games$average, yrep = y_rep[1:200, ])
