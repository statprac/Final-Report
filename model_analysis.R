library(tidyverse)

# Read the main analysis dataset
analysis <- read_csv("project datasets/analysis_data_final.csv")

# Check the data
glimpse(analysis)
summary(analysis)

# Set reference groups: 2012 and Live Phone
analysis <- analysis |>
  mutate(
    year = factor(year, levels = c(2012, 2016)),
    mode = factor(
      mode,
      levels = c("Live Phone", "Internet", "IVR/Online")
    )
  )

table(analysis$year, analysis$mode)

# Model 1: additive model
model_add <- lm(
  absolute_error ~ Undecided + days_to_election + mode + year,
  data = analysis
)

# Model 2: associations can differ by year
model_interaction <- lm(
  absolute_error ~ year * (Undecided + days_to_election + mode),
  data = analysis
)

summary(model_add)
summary(model_interaction)

# Compare the two nested models
anova(model_add, model_interaction)

# Check diagnostic plots for the interaction model
par(mfrow = c(2, 2))
plot(model_interaction)
par(mfrow = c(1, 1))

# Try a model with a transformed response
model_log <- lm(
  log1p(absolute_error) ~
    year * (Undecided + days_to_election + mode),
  data = analysis
)

par(mfrow = c(2, 2))
plot(model_log)
par(mfrow = c(1, 1))

summary(model_log)

# Check the largest Cook's distances
head(sort(cooks.distance(model_log), decreasing = TRUE), 10)

# Inspect the three largest Cook's distances for the log model
analysis[c(96, 219, 299),
         c("state", "year", "pollster", "mode", "absolute_error")]

model_log_additive <- lm(
  log1p(absolute_error) ~ Undecided + days_to_election + mode + year,
  data = analysis
)

anova(model_log_additive, model_log)

# Fit the same model using a square-root transformation (Final model)
model_sqrt <- lm(
  sqrt(absolute_error) ~ year * (Undecided + days_to_election + mode),
  data = analysis
)

# Check diagnostics
par(mfrow = c(2, 2))
plot(model_sqrt)
par(mfrow = c(1, 1))

summary(model_sqrt)

head(sort(cooks.distance(model_sqrt), decreasing = TRUE), 5)

# Inspect the five largest Cook's distances for the square-root model
analysis[c(96, 7, 299, 219, 237),
         c("state", "year", "pollster", "mode", "absolute_error")]

model_sqrt_additive <- lm(
  sqrt(absolute_error) ~ Undecided + days_to_election + mode + year,
  data = analysis
)

par(mfrow = c(2, 2))
plot(model_sqrt_additive)
par(mfrow = c(1, 1))

anova(model_sqrt_additive, model_sqrt)

#| echo: false

library(tidyverse)

# Predict the model outcome for each survey mode and year
plot_data <- expand_grid(
  year = levels(analysis$year),
  mode = levels(analysis$mode),
  Undecided = median(analysis$Undecided, na.rm = TRUE),
  days_to_election = median(analysis$days_to_election, na.rm = TRUE)
) |>
  mutate(
    year = factor(year, levels = levels(analysis$year)),
    mode = factor(mode, levels = levels(analysis$mode))
  )

# Add predictions and 95% confidence intervals
predictions <- predict(
  model_sqrt,
  newdata = plot_data,
  interval = "confidence"
)

plot_data <- bind_cols(plot_data, as_tibble(predictions))

# Plot the predictions

ggplot(plot_data, aes(x = mode, y = fit, colour = year, group = year)) +
  geom_point(size = 3)+
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.12,
  ) +
  labs(
    title = "Predicted polling error by survey mode and year",
    subtitle = "Undecided voters: 5%; days to election: 26",
    x = "Survey mode",
    y = "Predicted square root of absolute polling error",
    colour = "Election year",
    caption = "Bars show 95% confidence intervals."
  ) +
  theme_minimal()
