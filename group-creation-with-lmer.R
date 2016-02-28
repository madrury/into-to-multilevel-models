library(lme4)
library(arm)
library(ggplot2)
library(reshape2)

# We will construct a model where each class has an associated parameter, and
# each class-subclass combination also has a parameter.  The effect of the
# combined class-subclass combination will be the sum of the effects.
N_class <- 9
N_subclass <- 25

class_parameters <- structure(
                      rnorm(N_class, sd = 3),
                      names = as.character(1:N_class)
)
subclass_parameters <- structure(
  rnorm(N_class*N_subclass),
  # Check the transpose, to put things in an inuitive order
  names = as.vector(t(outer(1:N_class, 1:N_subclass, paste, sep=".")))
)

# The efective parameter for a class-subclass combination is the sum of the
# parameters for each
get_class_name <- function(subclass_name) strsplit(subclass_name, ".", fixed = TRUE)[[1]][1]
effective_parameters <- lapply(
  names(subclass_parameters),
  function(x) subclass_parameters[x] + class_parameters[get_class_name(x)]
)
names(effective_parameters) <- names(subclass_parameters)

parameters_df <- data.frame(
  effective_parameter = unlist(effective_parameters),
  class = rep(names(class_parameters), each = N_subclass),
  subclass = names(subclass_parameters)
)

ggplot(data = parameters_df, aes(x = effective_parameter)) +
  geom_histogram(aes(fill = class)) +
  labs(title = "Distrribution of True Class-Subclass Parameters")


# Create some fake data with a two-heirachy structure to mimic make-model.
N_obs <- 200000

class <- as.character(sample(1:N_class, N_obs, replace = TRUE))
subclass <- as.character(sample(1:N_subclass, N_obs, replace = TRUE))
class_subclass <- paste(class, subclass, sep = ".")
y <- class_parameters[class] + subclass_parameters[class_subclass] + rnorm(N_obs, mean = 0, sd = .1)

class_subclass_df <- data.frame(
  class  = class,
  subclass = subclass,
  y = y
)

M <- lmer(y ~ (1 | class) + (1 | class:subclass), data = class_subclass_df)

# The class means are recovered well
class_means_df <- data.frame(
  class_id = names(class_parameters),
  true_class_means = class_parameters,
  est_class_means = fixef(M) + ranef(M)$class[, 1],
  # The intercept and class means are assumed to vary independently here.
  upper_se = fixef(M) + ranef(M)$class[, 1] +
             sqrt(se.coef(M)$fixef^2 + se.coef(M)$class[, 1]^2),
  lower_se = fixef(M) + ranef(M)$class[, 1] -
             sqrt(se.coef(M)$fixef^2 + se.coef(M)$class[, 1]^2)
)

ggplot(data = class_means_df) +
  geom_point(aes(x = class_id, y = est_class_means), color = "blue") +
  geom_pointrange(aes(x = class_id, y = est_class_means,
                      ymax = upper_se, ymin = lower_se), color = "blue",
                  alpha = .5) +
  geom_point(aes(x = class_id, y = true_class_means)) +
  labs(title = "Estiamted vs. Actual Class Means")

