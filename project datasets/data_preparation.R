# Data preparation and sensitivity check

library(tidyverse)

# 1. Read the combined cleaned polling data and election results.
polls <- read_csv(
  "project datasets/clean/state_polls_combined_clean.csv",
  show_col_types = FALSE
)
elections <- read_csv(
  "project datasets/raw/1976-2024-president.csv",
  show_col_types = FALSE
)

# Check that each state-poll has a unique identifier.
sum(is.na(polls$poll_key)) # Count missing poll keys

sum(duplicated(polls$poll_key)) # Count duplicate poll keys

# 2. Count each major candidate's votes across all party labels.
# This includes votes reported on extra ballot lines, especially in New York.
elections <- elections |>
  filter(year %in% c(2012, 2016)) |>
  mutate(
    state = str_replace_all(str_to_lower(state), " ", "-"),
    state = if_else(state == "district-of-columbia", "washington-dc", state),
    candidate_group = case_when(
      year == 2012 & candidate == "OBAMA, BARACK H." ~ "Democrat",
      year == 2012 & candidate == "ROMNEY, MITT" ~ "Republican",
      year == 2016 & candidate == "CLINTON, HILLARY" ~ "Democrat",
      year == 2016 & candidate == "TRUMP, DONALD J." ~ "Republican",
      TRUE ~ NA_character_
    )
  )

# Check Democrat and Republican records with no candidate name
unknown_candidate <- elections |>
  filter(
    is.na(candidate),
    party_simplified %in% c("DEMOCRAT", "REPUBLICAN")
  ) |>
  select(year, state, party_simplified, candidatevotes)

print(unknown_candidate)

# Each state-year should have one total-votes value.
vote_totals <- elections |>
  group_by(year, state) |>
  summarise(total_votes = first(totalvotes),
            number_of_totals = n_distinct(totalvotes), .groups = "drop")

# Count state-year records with different total-vote values
print(sum(vote_totals$number_of_totals != 1))

# Count state-year records with missing total votes
print(sum(is.na(vote_totals$total_votes)))

# Count state-year records with zero or negative total votes
print(sum(vote_totals$total_votes <= 0, na.rm = TRUE))

candidate_votes <- elections |>
  filter(!is.na(candidate_group)) |>
  group_by(year, state, candidate_group) |>
  summarise(votes = sum(candidatevotes), .groups = "drop") |>
  pivot_wider(names_from = candidate_group, values_from = votes)

actual_results <- candidate_votes |>
  left_join(vote_totals, by = c("year", "state")) |>
  mutate(actual_margin = 100 * (Democrat - Republican) / total_votes) |>
  select(year, state, actual_margin)

# Count duplicate state-year election results
print(sum(duplicated(actual_results[c("year", "state")])))

# Count missing actual margins
print(sum(is.na(actual_results$actual_margin)))

# 3. Join each poll to its state-year election result.
polls <- polls |>
  left_join(actual_results, by = c("year", "state")) |>
  mutate(
    poll_margin = case_when(
      year == 2012 ~ Obama - Romney,
      year == 2016 ~ Clinton - Trump
    ),
    absolute_error = abs(poll_margin - actual_margin)
  )
# Count polls without a matching election result
print(sum(is.na(polls$actual_margin)))

# 4. Flag polls whose reported response percentages add to more than 102%.
# Missing categories are ignored when adding reported values.
# A high total may reflect rounding or reporting differences, so we do not indicate that it is an error.
polls <- polls |>
  mutate(
    reported_total = case_when(
      year == 2012 ~ rowSums(across(c(Obama, Romney, Undecided, Other)), na.rm = TRUE),
      year == 2016 ~ rowSums(across(c(Clinton, Trump, Undecided, Other, Johnson, McMullin)), na.rm = TRUE)
    ),
    unusual_total = reported_total > 102
  )

# 5. Select polls from the final 90 days.
# Exclude polls with missing or non-positive sample size.
# After data exploration, we agree to include 3 modes: Live Phone, Internet and IVR/Online polls into the model. 
# Furthermore, we exclude the two flagged polls from the main sample
# and include them in a sensitivity check.

sample_90 <- polls |> filter(between(days_to_election, 0, 90))
valid <- sample_90 |>
  filter(!is.na(absolute_error), !is.na(sample_size), sample_size > 0)
# Polls that do not report undecided share are excluded; this may affect results.
complete <- valid |> filter(!is.na(Undecided))
selected <- complete |>
  filter(mode %in% c("Live Phone", "Internet", "IVR/Online")) |>
  mutate(
    year = factor(year, levels = c(2012, 2016)),
    mode = factor(mode, levels = c("Live Phone", "Internet", "IVR/Online"))
  )

main_data <- selected |> filter(!unusual_total)
sensitivity_data <- selected
unusual_records <- selected |>
  filter(unusual_total) |>
  select(poll_key, year, state, pollster, mode,
         Obama, Romney, Clinton, Trump, Undecided, Johnson, McMullin, Other,
         reported_total)

sample_counts <- tibble(
  stage = c("Cleaned polls", "Within 90 days", "Valid error and sample size",
            "Undecided reported", "Three modes", "Main sample"),
  remaining = c(nrow(polls), nrow(sample_90), nrow(valid), nrow(complete),
                nrow(selected), nrow(main_data))
) |>
  mutate(excluded_at_step = lag(remaining) - remaining)

print(sample_counts)
print(unusual_records, width = Inf)
print(table(main_data$year, main_data$mode))

# Save datasets
write_csv(main_data, "analysis_data_final.csv")
write_csv(sensitivity_data, "analysis_data_with_unusual.csv")
write_csv(sample_counts, "sample_selection_counts.csv")
write_csv(unusual_records, "unusual_records.csv")

# 6. Sensitivity check: compare estimates with and without the two flagged polls.
# Keep the other sample rules and model formula the same in both analyses.
# final model once it is agreed; use the same formula for both datasets.
model_formula <- absolute_error ~ year * (Undecided + days_to_election + mode)
model_main <- lm(model_formula, data = main_data)
model_with_unusual <- lm(model_formula, data = sensitivity_data)

# Check whether the two models have the same coefficient names
print(identical(
  names(coef(model_main)),
  names(coef(model_with_unusual))
))

# Count coefficients that could not be estimated
print(sum(is.na(coef(model_main))))
print(sum(is.na(coef(model_with_unusual))))

comparison <- tibble(
  term = names(coef(model_main)),
  main_estimate = unname(coef(model_main)),
  with_unusual_estimate = unname(coef(model_with_unusual))
) |>
  mutate(estimate_change = with_unusual_estimate - main_estimate)

print(summary(model_main))
print(summary(model_with_unusual))
print(comparison)
write_csv(comparison, "sensitivity_comparison.csv")

# Polls may not be independent when they share a state or pollster. This may
# affect standard errors and p-values; discuss this limitation in the report.
# These observational results describe the selected polls and two elections;
# they do not establish causation or necessarily apply to future elections.
