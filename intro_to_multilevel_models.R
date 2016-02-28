library(lme4)
library(arm)
library(ggplot2)
library(reshape2)

# Introduction to Random Effect / Multi Level Models.
#----------------------------------------------------

# Basic random intercept only example, shows how random intercept models control
# for variation

# X and Y are totally independent
estiamte_lm_random_classes <- function(N, N_classes) {
  X <- factor(sample(1:N_classes, N, replace=TRUE))
  y <- rnorm(N)
  # Linear model gives a wide range of parameter estiamtes to the factors
  M <- lm(y ~ X)
  coef(M)
}

replicate_models <- function(N, N_classes, estimator, N_replicates) {
  model_coefs <- replicate(N_replicates, estimator(N, N_classes))
  model_coefs_melted <- data.frame(
    model_id = rep(1:N_replicates, each=N_classes),
    coef = as.vector(model_coefs)
  )
  model_coefs_melted
}

model_coefs_melted <- replicate_models(10000, 100, estiamte_lm_random_classes, 25)
ggplot(data=model_coefs_melted, aes(x=coef)) +
  geom_histogram() +
  facet_wrap(~model_id) +
  labs(title="Parameter Estimates in Null Linear Model")

# Estimate a linear mixed effects model instead
estiamte_lmer_random_classes <- function(N, N_classes) {
  df <- data.frame(
    x = factor(sample(1:N_classes, N, replace=TRUE)),
    y = rnorm(N)
  )
  # Linear model gives a wide range of parameter estiamtes to the factors
  M <- lmer(y ~ (1 | x), data = df)
  ranef(M)$x[, 1] # Dont include intercept
}

model_coefs_melted <- replicate_models(10000, 100,
                                      estiamte_lmer_random_classes, 25)
ggplot(data=model_coefs_melted, aes(x=coef)) +
  geom_histogram() +
  facet_wrap(~model_id) +
  labs(title="Parameter Estimates in Null Random Intercept Model")

# You can also estiamte a lmer model with fixed effects
estimate_lmer_with_fixed <- function(N, N_classes) {
  df <- data.frame(
    t = runif(N),
    x = factor(sample(1:N_classes, N, replace=TRUE))
  )
  df$y <- df$t + rnorm(N)
  M <- lmer(y ~ t + (1 | x), data = df)
  M
}


set.seed(154)
N_replicates <- 25
N_classes <- 100
lmers_fixed_effects <- replicate(
  N_replicates,
  fixef(estimate_lmer_with_fixed(10000, N_classes))
)

# Evaluate the fixed effects part of the model on a grid of points on [0, 1]
N_grid_points <- 4
grid_matrix <- cbind(rep(1, N_grid_points), seq(0, 1, length = N_grid_points))
fixef_preds <- grid_matrix %*% lmers_fixed_effects
fixef_preds_melted <- data.frame(
  model_id = factor(rep(1:N_replicates, each = N_grid_points)),
  x = rep(seq(0, 1, length = N_grid_points), N_replicates),
  fix_effects_preds = as.vector(fixef_preds)
)
ggplot(data = fixef_preds_melted, aes(x = x, y = fix_effects_preds)) +
  geom_line(aes(group = model_id), alpha = .25) +
  geom_abline(intercept = 0, slope = 1, color="red") +
  labs(title="Fix Effect Estiamtes in Null Random Intercept Model")


# Plot the random effect distribution from the same model
set.seed(154)
lmers_rand_effects <- replicate(
  N_replicates,
  ranef(estimate_lmer_with_fixed(10000, N_classes))$x[,1]
)
rand_effects_melted <- data.frame(
  model_id = rep(1:N_replicates, each = N_classes),
  coef = as.vector(lmers_rand_effects)
)
ggplot(data=rand_effects_melted, aes(x=coef)) +
  geom_histogram() +
  facet_wrap(~model_id) +
  labs(title="Random effects in Null Random Intercept Model")