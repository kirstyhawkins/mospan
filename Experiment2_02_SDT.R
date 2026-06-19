# =============================================================================
# M-OSPAN — Experiment 2
# Script 2 of 3: Signal Detection Theory (SDT) Indices
# =============================================================================
#
# Description:
#   Computes signal detection indices (d', criterion c) for the m-ospan
#   primary task (SAME/DIFFERENT trials only) in the rhythm and tone
#   conditions. Classic ospan is excluded, as its primary task is free
#   letter recall rather than a SAME/DIFFERENT judgement.
#
#   Ground truth (whether a trial was actually SAME or DIFFERENT) is derived
#   from Correct and Response, since trial-level ground truth was not
#   exported by the testing platform. SAME is treated as the signal-present
#   response; a log-linear correction (Hautus, 1995) is applied to hit and
#   false-alarm rates to avoid undefined z-scores at floor/ceiling. This
#   mirrors the Experiment 1 SDT procedure (Script 2) for methodological
#   consistency across the two-experiment paper.
#
# Input:
#   exp2_clean_data.csv   — from Script 1
#
# Output:
#   exp2_sdt_data.csv     — one row per participant x condition (rhythm, tone)
#
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# [1] SDT helper (differencing model with log-linear correction)
# -----------------------------------------------------------------------------

# hits = correctly identified SAME trials; fa = incorrectly reported SAME on
# a DIFFERENT trial. n_signal/n_noise are derived from Correct x Response
# rather than a stored ground-truth column.
compute_sdt <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_signal = sum(Correct == 1 & Response == "SAME") + sum(Correct == 0 & Response == "DIFFERENT"),
      n_noise  = sum(Correct == 0 & Response == "SAME") + sum(Correct == 1 & Response == "DIFFERENT"),
      hits     = sum(Correct == 1 & Response == "SAME"),
      fa       = sum(Correct == 0 & Response == "SAME"),
      .groups  = "drop"
    ) %>%
    mutate(
      HR_adj  = (hits + 0.5) / (n_signal + 1),
      FAR_adj = (fa   + 0.5) / (n_noise  + 1),
      dprime  = (qnorm(HR_adj) - qnorm(FAR_adj)) / sqrt(2),
      c_bias  = -(qnorm(HR_adj) + qnorm(FAR_adj)) / 2
    )
}

# -----------------------------------------------------------------------------
# [2] Compute SDT indices for rhythm and tone conditions
# -----------------------------------------------------------------------------

clean_data <- read.csv("exp2_clean_data.csv")

sdt_data <- clean_data %>%
  filter(block %in% c("rhythm", "tone"), Response %in% c("SAME", "DIFFERENT")) %>%
  compute_sdt(group_vars = c("Participant.Private.ID", "block", "MT_mean")) %>%
  rename(ID = Participant.Private.ID, MT = MT_mean) %>%
  mutate(condition_factor = factor(block, levels = c("rhythm", "tone")))

cat("SDT — participant x condition rows:", nrow(sdt_data), "\n")
print(count(sdt_data, block))

# -----------------------------------------------------------------------------
# [3] Export
# -----------------------------------------------------------------------------

write.csv(sdt_data, "exp2_sdt_data.csv", row.names = FALSE)
cat("Exported: exp2_sdt_data.csv\n")
