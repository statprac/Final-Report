# Person B: Data preparation and sensitivity analysis
# Run from the project root containing clean-poll-data.R and data/raw/.
# Uses the existing EDA cleaning script and the three original CSV files.

library(tidyverse)

# 1. Read official results, then run our original EDA cleaning script.
# clean-poll-data.R uses result_1976_2024_raw in its final checks,
# so define it before calling source().
result_1976_2024_raw <- read_csv(
  "data/raw/1976-2024-president.csv", show_col_types = FALSE
)
source("clean-poll-data.R")

# polls_clean and unusual_totals are created by clean-poll-data.R.
# No separate cleaned CSV files need to be supplied.

# 2. Calculate the actual Democratic minus Republican vote margin.
# This follows the method in our EDA report.
actual_results <- result_1976_2024_raw |>
  filter(year %in% c(2012, 2016),
         party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) |>
  group_by(year, state, party_simplified) |>
  summarise(votes = sum(candidatevotes),
            totalvotes = first(totalvotes), .groups = "drop") |>
  mutate(vote_share = 100 * votes / totalvotes) |>
  select(year, state, party_simplified, vote_share) |>
  pivot_wider(names_from = party_simplified, values_from = vote_share) |>
  mutate(actual_margin = DEMOCRAT - REPUBLICAN,
         state = str_replace_all(str_to_lower(state), " ", "-"),
         state = if_else(state == "district-of-columbia",
                         "washington-dc", state)) |>
  select(year, state, actual_margin)

# 3. Calculate each poll's absolute error.
polls <- polls_clean |>
  mutate(poll_margin = case_when(
    year == 2012 ~ Obama - Romney,
    year == 2016 ~ Clinton - Trump
  )) |>
  left_join(actual_results, by = c("year", "state")) |>
  mutate(absolute_error = abs(poll_margin - actual_margin))

# 4. Select polls for the analysis.
sample_90 <- polls |> filter(between(days_to_election, 0, 90))
valid <- sample_90 |>
  filter(!is.na(absolute_error), !is.na(sample_size), sample_size > 0)
complete <- valid |> filter(!is.na(Undecided))
selected <- complete |>
  filter(mode %in% c("Live Phone", "Internet", "IVR/Online"))

# EDA flagged records with reported totals above 102%; keep them
# in the sensitivity sample and exclude them from the main sample.
main_data <- selected |>
  filter(!poll_key %in% unusual_totals$poll_key)
sensitivity_data <- selected

# Use the same reference categories for both regressions.
main_data <- main_data |>
  mutate(year = factor(year, levels = c(2012, 2016)),
         mode = factor(mode, levels = c("Live Phone", "Internet", "IVR/Online")))
sensitivity_data <- sensitivity_data |>
  mutate(year = factor(year, levels = c(2012, 2016)),
         mode = factor(mode, levels = c("Live Phone", "Internet", "IVR/Online")))

# Print counts so we can confirm they still match our earlier results.
print(tibble(
  stage = c("EDA polls", "Within 90 days", "Valid", "Undecided reported",
            "Three modes", "Main sample"),
  records = c(nrow(polls), nrow(sample_90), nrow(valid), nrow(complete),
              nrow(selected), nrow(main_data))
))
print(selected |>
        filter(poll_key %in% unusual_totals$poll_key) |>
        select(poll_key, year, state))

# Save the final dataset for Person A and the sensitivity dataset.
write_csv(main_data, "analysis_data_final.csv")
write_csv(sensitivity_data, "analysis_data_with_unusual.csv")

# 5. Sensitivity check: use the same formula with and without flagged polls.
# Person A will confirm the final model formula.
model_formula <- absolute_error ~ year * (Undecided + days_to_election + mode)
model_main <- lm(model_formula, data = main_data)
model_sensitivity <- lm(model_formula, data = sensitivity_data)

summary(model_main)
summary(model_sensitivity)

# Compare ordinary OLS estimates and p-values.
main_coef <- as.data.frame(summary(model_main)$coefficients) |>
  rownames_to_column("term") |>
  select(term, main_estimate = Estimate, main_p = `Pr(>|t|)`)
sensitivity_coef <- as.data.frame(summary(model_sensitivity)$coefficients) |>
  rownames_to_column("term") |>
  select(term, sensitivity_estimate = Estimate,
         sensitivity_p = `Pr(>|t|)`)

comparison <- inner_join(main_coef, sensitivity_coef, by = "term") |>
  mutate(estimate_change = sensitivity_estimate - main_estimate)
print(comparison)
write_csv(comparison, "sensitivity_comparison.csv")
