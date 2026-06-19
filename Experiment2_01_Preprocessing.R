# =============================================================================
# M-OSPAN — Experiment 2
# Script 1 of 3: Data Preprocessing
# =============================================================================
#
# Description:
#   Loads raw trial data from three recruitment cohorts (Prolific, Sona #1,
#   Sona #2), each contributing three task conditions (classic ospan, rhythm
#   span, tone/pitch span — arithmetic is the only secondary task in this
#   experiment). Merges Gold-MSI Musical Training scores, applies inclusion
#   criteria, recodes variables, and exports one analysis-ready dataset.
#
# Input files (per cohort: prolific, sona1, sona2):
#   {cohort}_rhythm.csv    — rhythm span trials
#   {cohort}_pitch.csv     — tone/pitch span trials
#   {cohort}_classic.csv   — classic ospan trials
#   {cohort}_gold.csv      — Gold-MSI questionnaire responses
#
# Output:
#   exp2_clean_data.csv    — trial-level dataset ready for analysis
#
# =============================================================================

library(tidyverse)

# Gold-MSI Musical Training subscale items
gold_msi_items <- c("questionnaire.mrik.MT_01.quantised", "questionnaire.mrik.MT_02.quantised",
                     "questionnaire.mrik.MT_03.quantised", "questionnaire.mrik.MT_04.quantised",
                     "questionnaire.mrik.MT_05.quantised", "questionnaire.mrik.MT_06.quantised",
                     "questionnaire.mrik.MT_07.quantised")

analysis_cols <- c("Participant.Private.ID", "Reaction.Time", "Trial.Number", "order.2nlx",
                    "Attempt", "Correct", "Response.Type", "Response", "length",
                    "block", "MT_mean")

# -----------------------------------------------------------------------------
# [1] Load and combine raw trial data by cohort
# -----------------------------------------------------------------------------

# Helper: load rhythm/pitch/classic files for one cohort, tag with condition
# (block), and coerce RT to numeric
load_cohort_trials <- function(file_map) {
  file_map %>%
    imap(function(path, block_label) {
      read.csv(path) %>%
        mutate(Reaction.Time = as.numeric(Reaction.Time), block = block_label)
    }) %>%
    bind_rows()
}

# Helper: extract participant-level Musical Training score from one cohort's
# Gold-MSI file
load_cohort_gold <- function(path) {
  read.csv(path) %>%
    mutate(MT_mean = rowMeans(across(all_of(gold_msi_items)), na.rm = TRUE)) %>%
    select(Participant.Private.ID, MT_mean)
}

# File names are not uniform across cohorts (Sona cohort 2 appends "1" to
# the task name rather than the cohort prefix), so each cohort's file map is
# listed explicitly rather than constructed from a single naming pattern
cohorts <- list(
  prolific = list(
    trials = c(rhythm = "prolific_rhythm.csv", tone = "prolific_pitch.csv", classic = "prolific_classic.csv"),
    gold   = "prolific_gold.csv"
  ),
  sona1 = list(
    trials = c(rhythm = "sona_rhythm.csv", tone = "sona_pitch.csv", classic = "sona_classic.csv"),
    gold   = "sona_gold.csv"
  ),
  sona2 = list(
    trials = c(rhythm = "sona_rhythm1.csv", tone = "sona_pitch1.csv", classic = "sona_classic1.csv"),
    gold   = "sona_gold1.csv"
  )
)

cohort_trials <- map(cohorts, ~ load_cohort_trials(.x$trials))
cohort_gold   <- map(cohorts, ~ load_cohort_gold(.x$gold))

cohort_ns <- map_int(cohort_trials, ~ n_distinct(.x$Participant.Private.ID))
print(data.frame(Cohort = names(cohort_ns), n_participants = cohort_ns))

all_data <- bind_rows(cohort_trials) %>%
  left_join(bind_rows(cohort_gold), by = "Participant.Private.ID")

cat("Total participants (all cohorts):", n_distinct(all_data$Participant.Private.ID), "\n")

# -----------------------------------------------------------------------------
# [2] Filter trials and recode variables
# -----------------------------------------------------------------------------

# Classic ospan primary task (free letter recall) is submitted via Enter Key
# rather than a SAME/DIFFERENT response, so both response formats are kept
df <- all_data %>%
  filter(length != "practice") %>%
  mutate(Reaction.Time = as.numeric(Reaction.Time),
         Trial.Number   = as.numeric(Trial.Number)) %>%
  select(any_of(analysis_cols)) %>%
  rename(order = order.2nlx) %>%
  filter(Response.Type == "Enter Key" | Response %in% c("SAME", "DIFFERENT", "TRUE", "FALSE")) %>%
  mutate(
    block = factor(block, levels = c("classic", "rhythm", "tone")),
    # Primary (storage) task performance: SAME/DIFFERENT responses, or
    # free-recall responses submitted via Enter Key (classic ospan)
    task_performance = case_when(
      Response %in% c("SAME", "DIFFERENT") | Response.Type == "Enter Key" ~ Correct,
      TRUE ~ NA_real_
    ),
    # Secondary (processing) task performance: arithmetic TRUE/FALSE responses
    math_performance = case_when(
      Response %in% c("TRUE", "FALSE") ~ Correct,
      TRUE ~ NA_real_
    )
  )

# -----------------------------------------------------------------------------
# [3] Participant summary
# -----------------------------------------------------------------------------

cat("Total participants in cleaned dataset:", n_distinct(df$Participant.Private.ID), "\n")

condition_counts <- df %>% group_by(block) %>% summarise(n = n_distinct(Participant.Private.ID), .groups = "drop")
print(condition_counts)

# -----------------------------------------------------------------------------
# [4] Export
# -----------------------------------------------------------------------------

write.csv(df, "exp2_clean_data.csv", row.names = FALSE)
cat("Exported: exp2_clean_data.csv\n")
