# https://easystats.github.io/bayestestR/articles/example1.html

library(rstanarm)
library(bayestestR)
library(insight)


# Fitting the model
model <- lm(Sepal.Length ~ Petal.Length, data = iris)
summary(model)

insight::get_parameters(model)


library(ggplot2) # Load the package

# The ggplot function takes the data as argument, and then the variables
# related to aesthetic features such as the x and y axes.
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_point() + # This adds the points
  geom_smooth(method = "lm") # This adds a regression line


model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris)

# Extracting the posterior
posteriors <- insight::get_parameters(model)

head(posteriors) # Show the first 6 rows
nrow(posteriors) # Size (number of rows)

model <- stan_glm(Sepal.Length ~ Petal.Length, data = iris, chains = 2, iter = 1000, warmup = 250)

nrow(insight::get_parameters(model)) # Size (number of rows)

ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density(fill = "orange")


# Describing the Posterior
mean(posteriors$Petal.Length)
median(posteriors$Petal.Length)
map_estimate(posteriors$Petal.Length)

ggplot(posteriors, aes(x = Petal.Length)) +
  geom_density(fill = "orange") +
  # The mean in blue
  geom_vline(xintercept = mean(posteriors$Petal.Length), color = "blue", size = 1) +
  # The median in red
  geom_vline(xintercept = median(posteriors$Petal.Length), color = "red", size = 1) +
  # The MAP in purple
  geom_vline(xintercept = map_estimate(posteriors$Petal.Length), color = "purple", size = 1)

range(posteriors$Petal.Length)
hdi(posteriors$Petal.Length, ci = 0.89)


#################################################
# A linear model with a categorical predictor
#################################################

# Data preparation and model fitting
library(dplyr)

# We keep only rows for which feed is meatmeal or sunflower
data <- filter(chickwts, feed %in% c("meatmeal", "sunflower"))

model <- stan_glm(weight ~ feed, data = data)


# Posterior description
posteriors <- insight::get_parameters(model)

ggplot(posteriors, aes(x = feedsunflower)) +
  geom_density(fill = "red")

median(posteriors$feedsunflower)

hdi(posteriors$feedsunflower)


# ROPE Percentage
rope(posteriors$feedsunflower, range = c(-20, 20), ci = 0.89)

rope_value <- 0.1 * sd(data$weight)
rope_range <- c(-rope_value, rope_value)
rope_range

rope_value <- rope_range(model)
rope_value

rope(posteriors$feedsunflower, range = rope_range, ci = 0.89)


# Probability of Direction (pd)
n_positive <- posteriors %>%
  filter(feedsunflower > 0) %>% # select only positive values
  nrow() # Get length

n_positive / nrow(posteriors) * 100

p_direction(posteriors$feedsunflower)

pd <- 97.82
onesided_p <- 1 - pd / 100
twosided_p <- onesided_p * 2
twosided_p

summary(lm(weight ~ feed, data = data))


# All with one function
describe_posterior(model, test = c("p_direction", "rope", "bayesfactor"))
