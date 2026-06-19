# =============================================================================
# M-OSPAN — Experiment 1
# Script 2 of 3: Signal Detection Theory (SDT) Indices
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# [1] 
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
      HR_adj = (hits + 0.5) / (n_signal + 1),
      FAR_adj = (fa   + 0.5) / (n_noise  + 1),
      dprime  = (qnorm(HR_adj) - qnorm(FAR_adj)) / sqrt(2),
      c_bias  = -(qnorm(HR_adj) + qnorm(FAR_adj)) / 2
    )
}

# -----------------------------------------------------------------------------
# [2] Rhythm version
# -----------------------------------------------------------------------------

rhythm_data <- read.csv("exp1_rhythm_clean.csv")

rhythm_sdt <- rhythm_data %>%
  filter(task.version == "rhythm", Response %in% c("SAME", "DIFFERENT")) %>%
  rename(secondary_task = Task.Name) %>%
  compute_sdt(group_vars = c("Participant.Private.ID", "secondary_task", "MT_mean", "musicianship_sr"))

cat("Rhythm SDT — participant x condition rows:", nrow(rhythm_sdt), "\n")

# -----------------------------------------------------------------------------
# [3] Pitch version
# -----------------------------------------------------------------------------

tone_data <- read.csv("exp1_tone_clean.csv")

tone_sdt <- tone_data %>%
  filter(task.version == "tone", Response %in% c("SAME", "DIFFERENT")) %>%
  rename(secondary_task = Task.Name) %>%
  compute_sdt(group_vars = c("Participant.Private.ID", "secondary_task", "MT_mean", "musicianship_sr"))

cat("Tone SDT — participant x condition rows:", nrow(tone_sdt), "\n")

# -----------------------------------------------------------------------------
# [4] Export
# -----------------------------------------------------------------------------

write.csv(rhythm_sdt, "exp1_rhythm_sdt.csv", row.names = FALSE)
write.csv(tone_sdt,   "exp1_tone_sdt.csv",   row.names = FALSE)

cat("Exported: exp1_rhythm_sdt.csv, exp1_tone_sdt.csv\n")
