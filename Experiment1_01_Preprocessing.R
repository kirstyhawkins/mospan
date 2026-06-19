# =============================================================================
# M-OSPAN — Experiment 1
# Script 1 of 3: Data Preprocessing
# =============================================================================

library(dplyr)

# Gold-MSI Musical Training subscale items (used for both task versions)
gold_msi_items <- c("goldmsi32.quantised", "goldmsi33.quantised", "goldmsi14.quantised",
                     "goldmsi35.quantised", "goldmsi36.quantised", "goldmsi37.quantised",
                     "goldmsi27.quantised")

# -----------------------------------------------------------------------------
# [1] Load and combine raw trial data for one task version
# -----------------------------------------------------------------------------

# load verbal/spatial/maths/classic files for a version, tag the
# classic file, drop practice trials, coerce RT, and bind into one frame
load_task_version <- function(verbal_path, spatial_path, maths_path, classic_path, classic_label) {
  classic <- read.csv(classic_path)
  classic$Task.Name <- classic_label

  files <- list(verbal = read.csv(verbal_path),
                spatial = read.csv(spatial_path),
                maths   = read.csv(maths_path),
                classic = classic)

  files <- lapply(files, function(df) df[df$length != "practice", ])
  files <- lapply(files, function(df) {
    select(df, c(Participant.Private.ID, Task.Name, Reaction.Time, Trial.Number,
                 Correct, Response, Response.Type))
  })

  all_data <- bind_rows(files$verbal, files$maths, files$spatial, files$classic)
  all_data$Reaction.Time <- as.numeric(all_data$Reaction.Time)

  # Keep classic ospan free-recall responses (Enter Key) alongside the
  # SAME/DIFFERENT (primary task) and TRUE/FALSE (arithmetic) responses
  all_data %>%
    filter(Response.Type == "Enter Key" | Response %in% c("SAME", "DIFFERENT", "TRUE", "FALSE"))
}

# merge self-reported musicianship and Gold-MSI Musical Training score
add_demographics <- function(df, demo_path, gold_path) {
  demo <- read.csv(demo_path)
  demo <- select(demo, c(Participant.Private.ID, musician))
  names(demo)[names(demo) == "musician"] <- "musicianship_sr"

  gold <- read.csv(gold_path)
  gold$MT_mean <- rowMeans(gold[, gold_msi_items], na.rm = TRUE)
  gold <- select(gold, c(Participant.Private.ID, MT_mean))

  df <- merge(df, demo, by = "Participant.Private.ID", all.x = TRUE)
  merge(df, gold, by = "Participant.Private.ID", all = TRUE)
}

# -----------------------------------------------------------------------------
# [2] Rhythm task version
# -----------------------------------------------------------------------------

rhythm_alldata <- load_task_version("rhythm_verbal.csv", "rhythm_spatial.csv",
                                     "rhythm_maths.csv", "classic.csv", "classic_r")

# Flag which trials belong to the music m-ospan vs the classic ospan
rhythm_alldata$task.version <- ifelse(
  rhythm_alldata$Task.Name %in% c("rhythm_verbal", "rhythm_spatial", "rhythm_maths"), "music_r",
  ifelse(rhythm_alldata$Task.Name == "classic_r", "classic_r", NA)
)

rhythm_alldata <- add_demographics(rhythm_alldata, "demographics.csv", "gold_r.csv")

cat("Rhythm version — participants:", n_distinct(rhythm_alldata$Participant.Private.ID), "\n")

# -----------------------------------------------------------------------------
# [3] Tone task version
# -----------------------------------------------------------------------------

tone_alldata <- load_task_version("tone_verbal.csv", "tone_spatial.csv",
                                   "tone_maths.csv", "classic_t.csv", "classic_t")

tone_alldata$task.version <- ifelse(
  tone_alldata$Task.Name %in% c("tone_verbal", "tone_spatial", "tone_maths"), "music_t",
  ifelse(tone_alldata$Task.Name == "classic_t", "classic_t", NA)
)

tone_alldata <- add_demographics(tone_alldata, "demo.csv", "gold.csv")

cat("Tone version — participants:", n_distinct(tone_alldata$Participant.Private.ID), "\n")

# -----------------------------------------------------------------------------
# [4] Export
# -----------------------------------------------------------------------------

write.csv(rhythm_alldata, "rhythm_alldata.csv", row.names = FALSE)
write.csv(tone_alldata,   "tone_alldata.csv",   row.names = FALSE)

cat("Exported: rhythm_alldata.csv, tone_alldata.csv\n")
