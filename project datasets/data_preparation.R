# Person B — Data Preparation and Sensitivity Check
# Input files (put in the same working directory as this script):
#   state_polls_2012.csv, state_polls_2016.csv, 1976-2024-president.csv
# Outputs:
#   analysis_data_final.csv (n=1219)
#   analysis_data_with_unusual.csv (n=1221)
#   sample_selection_counts.csv, unusual_records.csv,
#   sensitivity_comparison.csv

library(tidyverse)
library(sandwich)
library(lmtest)

polls12 <- read_csv('state_polls_2012.csv', show_col_types = FALSE)
polls16 <- read_csv('state_polls_2016.csv', show_col_types = FALSE)
elections <- read_csv('1976-2024-president.csv', show_col_types = FALSE)

# Calculate a major-party margin from official state returns.
# Use candidate names because NY and other states report multiple ballot lines.
election_results <- elections |>
  filter(year %in% c(2012, 2016)) |>
  mutate(
    statekey = str_replace_all(str_to_lower(state), ' ', '-'),
    statekey = if_else(statekey == 'district-of-columbia',
                       'washington-dc', statekey),
    candidate_upper = str_to_upper(coalesce(candidate, '')),
    candidate_group = case_when(
      year == 2012 & str_detect(candidate_upper, 'OBAMA') ~ 'DEM',
      year == 2012 & str_detect(candidate_upper, 'ROMNEY') ~ 'REP',
      year == 2016 & str_detect(candidate_upper, 'CLINTON') ~ 'DEM',
      year == 2016 & str_detect(candidate_upper, 'TRUMP') ~ 'REP',
      TRUE ~ 'OTHER'
    )
  ) |>
  group_by(year, statekey) |>
  summarise(
    dem_votes = sum(candidatevotes[candidate_group == 'DEM'], na.rm = TRUE),
    rep_votes = sum(candidatevotes[candidate_group == 'REP'], na.rm = TRUE),
    total_votes = first(totalvotes),
    .groups = 'drop'
  ) |>
  mutate(actual_margin = 100 * (dem_votes - rep_votes) / total_votes)

prepare_polls <- function(raw, yr) {
  election_day <- as.Date(if (yr == 2012) '2012-11-06' else '2016-11-08')
  dem <- if (yr == 2012) 'Obama' else 'Clinton'
  rep <- if (yr == 2012) 'Romney' else 'Trump'
  response_cols <- if (yr == 2012) {
    c('Obama', 'Romney', 'Undecided', 'Other')
  } else {
    c('Trump', 'Clinton', 'Undecided', 'Other', 'Johnson', 'McMullin')
  }
  # Unreported response categories are omitted from the *flagging total*;
  # undecided missingness itself is excluded later, not set to zero.
  raw |>
    mutate(
      year = yr,
      statekey = str_match(poll_info, '^\\d{4}-(.*?)-president')[, 2],
      statekey = if_else(statekey == 'washington-d-c', 'washington-dc', statekey),
      end_date = as.Date(end_date),
      days_to_election = as.numeric(election_day - end_date),
      poll_margin = as.numeric(.data[[dem]]) - as.numeric(.data[[rep]]),
      Undecided = as.numeric(Undecided),
      sample_size = as.numeric(sample_size),
      reported_total = rowSums(
        across(all_of(response_cols),
               ~ suppressWarnings(as.numeric(.x))),
        na.rm = TRUE
      )
    )
}

polls <- bind_rows(prepare_polls(polls12, 2012),
                   prepare_polls(polls16, 2016))

joined <- polls |>
  left_join(election_results |>
              select(year, statekey, actual_margin),
            by = c('year', 'statekey'), relationship = 'many-to-one') |>
  mutate(absolute_error = abs(poll_margin - actual_margin),
         cluster = paste0(statekey, '_', year))

# Check matching and sequentially track exclusions.
print(paste('Unmatched rows:', sum(is.na(joined$actual_margin))))
sample_90 <- joined |> filter(between(days_to_election, 0, 90))
valid <- sample_90 |>
  filter(!is.na(absolute_error), !is.na(sample_size), sample_size > 0)
complete <- valid |> filter(!is.na(Undecided))
selected <- complete |>
  filter(mode %in% c('Live Phone', 'Internet', 'IVR/Online'))
unusual <- selected |> filter(reported_total > 102)
primary <- selected |> filter(reported_total <= 102)

sample_counts <- tibble(
  stage = c('Original polls', 'Within 90 days', 'Valid sample and error',
            'Complete undecided', 'Three survey modes', 'Final sample'),
  remaining = c(nrow(polls), nrow(sample_90), nrow(valid),
                nrow(complete), nrow(selected), nrow(primary))
) |>
  mutate(excluded_at_step = lag(remaining) - remaining)
print(sample_counts)
print(unusual |> select(poll_id, year, statekey, mode, reported_total))

# Ensure identical factor levels in primary and sensitivity samples.
format_analysis <- function(data) {
  data |>
    mutate(
      year = factor(year, levels = c(2012, 2016)),
      mode = factor(mode, levels = c('Live Phone', 'Internet', 'IVR/Online')),
      cluster = factor(cluster)
    )
}
analysis <- format_analysis(primary)
sensitivity_data <- format_analysis(selected)

# Expected counts for the supplied project datasets; fail visibly if different.
stopifnot(identical(sample_counts$remaining,
                    c(2438L, 1763L, 1762L, 1561L, 1221L, 1219L)))
stopifnot(nrow(unusual) == 2L)
stopifnot(sum(is.na(joined$actual_margin)) == 0L)

write_csv(analysis, 'analysis_data_final.csv')
write_csv(sensitivity_data, 'analysis_data_with_unusual.csv')
write_csv(sample_counts, 'sample_selection_counts.csv')
write_csv(unusual, 'unusual_records.csv')

# Sensitivity check: keep model specification and clusters unchanged.
# Confirm the covariance/test method with Person A and the tutor.
fit_and_extract <- function(df) {
  fit <- lm(absolute_error ~ year * (Undecided + days_to_election + mode), data = df)
  cov_mat <- vcovCL(fit, cluster = df$cluster, type = 'HC1')
  G <- nlevels(df$cluster)
  test <- coeftest(fit, vcov. = cov_mat, df = G - 1)
  tibble(term = rownames(test), estimate = unname(test[, 1]),
         std_error = unname(test[, 2]), p_value = unname(test[, 4]))
}
main_result <- fit_and_extract(analysis)
with_unusual_result <- fit_and_extract(sensitivity_data)
comparison <- main_result |>
  rename(main_estimate = estimate, main_se = std_error, main_p = p_value) |>
  inner_join(
    with_unusual_result |>
      rename(with_unusual_estimate = estimate,
             with_unusual_se = std_error, with_unusual_p = p_value),
    by = 'term'
  ) |>
  mutate(coefficient_change = with_unusual_estimate - main_estimate)
print(comparison)
write_csv(comparison, 'sensitivity_comparison.csv')
