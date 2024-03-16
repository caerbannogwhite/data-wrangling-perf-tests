# Example 01
# Given a dataset with N subject that reports conditions (from cond_a to
# cond_j, 10 conditions in total) and body temperature (temp_C), classify
# the subjects are as follows:
#
# - If the subject has 1 or more conditions with a value greater than 1, or
#   has a body temperature greater than 37.5, classify the subject as the
#   number of conditions with a value greater than 1 plus the body temperature
#   greater than 37.5.
#
# - If the subject has 6 or more conditions with a value less than or equal to
#   1, classify the subject as "R" (Recovering).
#
# - Otherwise, classify the record as "M" (Missing).
#
# Conditions and temperature allow for missing values so, given a record,
# the sum of the conditions with a value greater than 1 (strong conditions)
# and the sum of the conditions with a value less than or equal to 1 (mild
# conditions) may not add up to 10.
# The same applies to the temperature, which may be missing.

rm(list = ls())
library(tidyverse)

example01 <- function(N, D) {
  fever_threshold_C <- 37.5

  conditions <- c(
    "cond_a", "cond_b", "cond_c", "cond_d", "cond_e",
    "cond_f", "cond_g", "cond_h", "cond_i", "cond_j"
  )

  # Generate the data for the example 01
  subj_num <- N
  days_num <- D
  rows_num <- subj_num * days_num

  subjects <- str_c("SUBJ", sample(1:subj_num, subj_num, replace = FALSE))

  cond_values <- c(rep(0, 6), rep(1, 4), 2, 2, 3, rep(NA, 6))
  fever_values <- c(36.1 + 0.3 * 0:20, NA)

  example01_data <- tibble(expand.grid(
    subj_id = subjects,
    day = 1:days_num
  )) %>%
    mutate(
      cond_a = sample(cond_values, rows_num, replace = TRUE),
      cond_b = sample(cond_values, rows_num, replace = TRUE),
      cond_c = sample(cond_values, rows_num, replace = TRUE),
      cond_d = sample(cond_values, rows_num, replace = TRUE),
      cond_e = sample(cond_values, rows_num, replace = TRUE),
      cond_f = sample(cond_values, rows_num, replace = TRUE),
      cond_g = sample(cond_values, rows_num, replace = TRUE),
      cond_h = sample(cond_values, rows_num, replace = TRUE),
      cond_i = sample(cond_values, rows_num, replace = TRUE),
      cond_j = sample(cond_values, rows_num, replace = TRUE),
      temp_C = sample(fever_values, rows_num, replace = TRUE),
    )

  write_csv(example01_data, str_c("example01_data_", rows_num, ".csv"))

  # First implementation of the example in R
  example01_r_baseline <- function(tab) {
    tab %>%
      rowwise() %>%
      mutate(
        strong_cond_num = sum(across(all_of(conditions), ~ .x > 1), na.rm = TRUE),
        mild_cond_num = sum(across(all_of(conditions), ~ .x <= 1), na.rm = TRUE),
      ) %>%
      mutate(
        strong_cond_num = strong_cond_num + (
          !is.na(temp_C) & (temp_C >= fever_threshold_C)
        ),
        mild_cond_num = mild_cond_num + (
          !is.na(temp_C) &
            (temp_C < fever_threshold_C)
        ),
        classification = case_when(
          strong_cond_num > 0 ~ as.character(strong_cond_num),
          mild_cond_num >= 6 ~ "R",
          TRUE ~ "M"
        ),
      ) %>%
      ungroup()
  }

  # Second implementation
  example01_r_improved <- function(tab) {
    tab %>%
      rowwise() %>%
      mutate(
        cond_a_strong = cond_a > 1,
        cond_b_strong = cond_b > 1,
        cond_c_strong = cond_c > 1,
        cond_d_strong = cond_d > 1,
        cond_e_strong = cond_e > 1,
        cond_f_strong = cond_f > 1,
        cond_g_strong = cond_g > 1,
        cond_h_strong = cond_h > 1,
        cond_i_strong = cond_i > 1,
        cond_j_strong = cond_j > 1,
        temp_hight = !is.na(temp_C) & (temp_C >= fever_threshold_C),
        temp_low = !is.na(temp_C) & (temp_C < fever_threshold_C),
      ) %>%
      mutate(
        strong_cond_num = sum(
          cond_a_strong, cond_b_strong, cond_c_strong, cond_d_strong,
          cond_e_strong, cond_f_strong, cond_g_strong, cond_h_strong,
          cond_i_strong, cond_j_strong,
          temp_hight,
          na.rm = TRUE
        ),
        mild_cond_num = sum(
          !cond_a_strong, !cond_b_strong, !cond_c_strong, !cond_d_strong,
          !cond_e_strong, !cond_f_strong, !cond_g_strong, !cond_h_strong,
          !cond_i_strong, !cond_j_strong,
          temp_low,
          na.rm = TRUE
        ),
      ) %>%
      mutate(
        classification = case_when(
          strong_cond_num > 0 ~ as.character(strong_cond_num),
          mild_cond_num >= 6 ~ "R",
          TRUE ~ "M"
        ),
      ) %>%
      ungroup()
  }

  # Second implementation
  example01_r_take_2 <- function(tab) {
    tab %>%
      mutate(
        cond_a_strong = cond_a > 1,
        cond_b_strong = cond_b > 1,
        cond_c_strong = cond_c > 1,
        cond_d_strong = cond_d > 1,
        cond_e_strong = cond_e > 1,
        cond_f_strong = cond_f > 1,
        cond_g_strong = cond_g > 1,
        cond_h_strong = cond_h > 1,
        cond_i_strong = cond_i > 1,
        cond_j_strong = cond_j > 1,
        temp_hight = !is.na(temp_C) & (temp_C >= fever_threshold_C),
        temp_low = !is.na(temp_C) & (temp_C < fever_threshold_C),
      ) %>%
      rowwise() %>%
      mutate(
        strong_cond_num = sum(
          cond_a_strong, cond_b_strong, cond_c_strong, cond_d_strong,
          cond_e_strong, cond_f_strong, cond_g_strong, cond_h_strong,
          cond_i_strong, cond_j_strong,
          temp_hight,
          na.rm = TRUE
        ),
        mild_cond_num = sum(
          !cond_a_strong, !cond_b_strong, !cond_c_strong, !cond_d_strong,
          !cond_e_strong, !cond_f_strong, !cond_g_strong, !cond_h_strong,
          !cond_i_strong, !cond_j_strong,
          temp_low,
          na.rm = TRUE
        ),
      ) %>%
      ungroup() %>%
      mutate(
        classification = case_when(
          strong_cond_num > 0 ~ as.character(strong_cond_num),
          mild_cond_num >= 6 ~ "R",
          TRUE ~ "M"
        ),
      )
  }

  example01_r_take_3 <- function(tab) {
    tab %>%
      rowwise() %>%
      mutate(
        strong_cond_num = sum(across(all_of(conditions), ~ .x > 1), na.rm = TRUE),
        mild_cond_num = sum(across(all_of(conditions), ~ .x <= 1), na.rm = TRUE),
      ) %>%
      ungroup() %>%
      mutate(
        strong_cond_num = strong_cond_num + (
          !is.na(temp_C) & (temp_C >= fever_threshold_C)
        ),
        mild_cond_num = mild_cond_num + (
          !is.na(temp_C) &
            (temp_C < fever_threshold_C)
        ),
        classification = case_when(
          strong_cond_num > 0 ~ as.character(strong_cond_num),
          mild_cond_num >= 6 ~ "R",
          TRUE ~ "M"
        ),
      )
  }


  # Run the example, time the execution
  run <- function(func, data, times = 5) {
    records <- c()
    for (i in 1:times) {
      records <- c(records, system.time(func(data))["elapsed"])
    }

    list(
      mean = mean(records),
      sd = sd(records)
    )
  }

  # Check the results
  solution_baseline <- example01_r_baseline(example01_data)
  solution_improved <- example01_r_improved(example01_data)
  solution_take_2 <- example01_r_take_2(example01_data)
  solution_take_3 <- example01_r_take_3(example01_data)

  stopifnot(all(solution_baseline$classification == solution_improved$classification))
  stopifnot(all(solution_baseline$classification == solution_take_2$classification))
  stopifnot(all(solution_baseline$classification == solution_take_3$classification))

  solution_baseline %>%
    select(subj_id, day, classification) %>%
    write_csv("example01_solution.csv")

  # Run the example
  list(
    example01_r_baseline = run(example01_r_baseline, example01_data),
    example01_r_improved = run(example01_r_improved, example01_data),
    example01_r_take_2 = run(example01_r_take_2, example01_data),
    example01_r_take_3 = run(example01_r_take_3, example01_data)
  )
}

Ns <- c(64, 128, 256, 512)
Ds <- c(32, 32, 32, 32)


results <- map2(Ns, Ds, example01)

ggplot2::ggplot(
  tibble(
    N = Ns,
    D = Ds,
    example01_r_baseline = map_dbl(results, ~ .x$example01_r_baseline$mean),
    example01_r_improved = map_dbl(results, ~ .x$example01_r_improved$mean),
    example01_r_take_2 = map_dbl(results, ~ .x$example01_r_take_2$mean),
    example01_r_take_3 = map_dbl(results, ~ .x$example01_r_take_3$mean)
  ) %>%
    pivot_longer(
      cols = -c(N, D),
      names_to = "implementation",
      values_to = "time"
    ),
  aes(x = N, y = time, color = implementation)
) +
  geom_point() +
  geom_line() +
  labs(
    title = "Example 01",
    x = "Number of subjects",
    y = "Time (s)",
    color = "Implementation"
  ) +
  theme_minimal()
  
  ggsave("example01.png", width = 6, height = 4, dpi = 300)
