library(lme4)
library(arm)
library(ggplot2)
library(reshape2)

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
