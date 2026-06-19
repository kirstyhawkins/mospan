# =============================================================================
# M-OSPAN — Experiment 1
# Script 3 of 3: Analysis
# =============================================================================
#

library(tidyverse)
library(brms)
library(gt)
library(patchwork)

# -----------------------------------------------------------------------------
# [0] APA plot theme
# -----------------------------------------------------------------------------

theme_apa <- theme_classic() +
  theme(
    text            = element_text(size = 12),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 10),
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 10),
    strip.text      = element_text(size = 11),
    plot.title      = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  )

secondary_task_colours <- c("#4E79A7", "#F28E2B", "#59A14F")

# -----------------------------------------------------------------------------
# [1] Load preprocessed data
# -----------------------------------------------------------------------------

rhythm_data <- read.csv("exp1_rhythm_clean.csv")
tone_data   <- read.csv("exp1_tone_clean.csv")
rhythm_sdt  <- read.csv("exp1_rhythm_sdt.csv")
tone_sdt    <- read.csv("exp1_tone_sdt.csv")

# -----------------------------------------------------------------------------
# [2] Person-level aggregates: primary task proportion correct
# -----------------------------------------------------------------------------

aggregate_primary <- function(df, version) {
  df %>%
    filter(task.version == version, Response %in% c("SAME", "DIFFERENT")) %>%
    rename(secondary_task = Task.Name) %>%
    group_by(Participant.Private.ID, secondary_task, MT_mean, musicianship_sr) %>%
    summarise(prop_correct = mean(Correct, na.rm = TRUE), .groups = "drop")
}

rhythm_primary_agg <- aggregate_primary(rhythm_data, "rhythm")
tone_primary_agg   <- aggregate_primary(tone_data,   "tone")

# -----------------------------------------------------------------------------
# [3] Descriptive statistics
# -----------------------------------------------------------------------------

desc_primary <- bind_rows(
  rhythm_primary_agg %>% mutate(Study = "Rhythm"),
  tone_primary_agg   %>% mutate(Study = "Tone")
) %>%
  group_by(Study, secondary_task) %>%
  summarise(M  = round(mean(prop_correct, na.rm = TRUE), 2),
            SD = round(sd(prop_correct,   na.rm = TRUE), 2),
            N  = n(), .groups = "drop")

gt(desc_primary, groupname_col = "Study") %>%
  tab_header(title = "Table 1",
             subtitle = "Primary Task Proportion Correct by Task Version and Secondary Task Condition") %>%
  cols_label(secondary_task = "Secondary Task") %>%
  tab_options(table.font.size = 12, heading.align = "left")

desc_sdt <- bind_rows(
  rhythm_sdt %>% mutate(Study = "Rhythm"),
  tone_sdt   %>% mutate(Study = "Tone")
) %>%
  group_by(Study, secondary_task) %>%
  summarise(M_dprime  = round(mean(dprime, na.rm = TRUE), 2),
            SD_dprime = round(sd(dprime,   na.rm = TRUE), 2),
            M_c       = round(mean(c_bias, na.rm = TRUE), 2),
            SD_c      = round(sd(c_bias,   na.rm = TRUE), 2),
            .groups = "drop")

gt(desc_sdt, groupname_col = "Study") %>%
  tab_header(title = "Table 2",
             subtitle = "SDT Measures (d' and c) by Secondary Task Condition") %>%
  cols_label(secondary_task = "Secondary Task",
             M_dprime = "M d'", SD_dprime = "SD d'",
             M_c = "M c",       SD_c = "SD c") %>%
  tab_options(table.font.size = 12, heading.align = "left")

# -----------------------------------------------------------------------------
# [4] RQ1: Convergent validity (m-ospan vs classic ospan)
# -----------------------------------------------------------------------------

load_classic_accuracy <- function(path) {
  read.csv(path) %>%
    filter(length != "practice", !Response %in% c("TRUE", "FALSE", "solved", "")) %>%
    mutate(Correct = as.numeric(Correct)) %>%
    group_by(Participant.Private.ID) %>%
    summarise(classic_acc = mean(Correct, na.rm = TRUE), .groups = "drop")
}

rhythm_classic_agg <- load_classic_accuracy("classic.csv")
tone_classic_agg   <- load_classic_accuracy("classic_t.csv")

run_rq1 <- function(primary_agg, classic_agg, study_label) {
  mospan_mean <- primary_agg %>%
    group_by(Participant.Private.ID) %>%
    summarise(mospan_acc = mean(prop_correct, na.rm = TRUE), .groups = "drop")

  merged <- inner_join(mospan_mean, classic_agg, by = "Participant.Private.ID")
  cat(study_label, "RQ1 matched participants:", nrow(merged), "\n")

  list(data = merged %>% mutate(Study = study_label), test = cor.test(merged$mospan_acc, merged$classic_acc))
}

rq1_rhythm <- run_rq1(rhythm_primary_agg, rhythm_classic_agg, "Rhythm")
rq1_tone   <- run_rq1(tone_primary_agg,   tone_classic_agg,   "Tone")

cat("RQ1 Rhythm: r =", round(rq1_rhythm$test$estimate, 3), "p =", round(rq1_rhythm$test$p.value, 3),
    "95% CI [", round(rq1_rhythm$test$conf.int, 3), "]\n")
cat("RQ1 Tone:   r =", round(rq1_tone$test$estimate, 3),   "p =", round(rq1_tone$test$p.value, 3),
    "95% CI [", round(rq1_tone$test$conf.int, 3), "]\n")

p_rq1 <- bind_rows(rq1_rhythm$data, rq1_tone$data) %>%
  ggplot(aes(x = classic_acc, y = mospan_acc)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.8) +
  facet_wrap(~ Study) +
  labs(x = "Classic OSpan (Proportion Correct)",
       y = "M-OSpan (Proportion Correct)",
       title = "Figure 1. Convergent Validity: M-OSpan vs Classic OSpan") +
  theme_apa

ggsave("fig1_rq1_convergent_validity.png", p_rq1, width = 9, height = 4.5, dpi = 300)

# -----------------------------------------------------------------------------
# [5] RQ2: Storage-processing correlations
# -----------------------------------------------------------------------------

extract_secondary_acc <- function(verbal_path, spatial_path, clean_data) {
  verbal_sec <- read.csv(verbal_path) %>%
    filter(length != "practice", Response %in% c("WORD", "NON-WORD")) %>%
    mutate(Correct = as.numeric(Correct), secondary_task = "verbal") %>%
    group_by(Participant.Private.ID, secondary_task) %>%
    summarise(secondary_acc = mean(Correct, na.rm = TRUE), .groups = "drop")

  spatial_sec <- read.csv(spatial_path) %>%
    filter(length != "practice", grepl("\\.png$", Response)) %>%
    mutate(Correct = as.numeric(Correct), secondary_task = "spatial") %>%
    group_by(Participant.Private.ID, secondary_task) %>%
    summarise(secondary_acc = mean(Correct, na.rm = TRUE), .groups = "drop")

  arithmetic_sec <- clean_data %>%
    filter(Task.Name == "arithmetic", Response %in% c("TRUE", "FALSE")) %>%
    mutate(Correct = as.numeric(Correct), secondary_task = "arithmetic") %>%
    group_by(Participant.Private.ID, secondary_task) %>%
    summarise(secondary_acc = mean(Correct, na.rm = TRUE), .groups = "drop")

  bind_rows(verbal_sec, spatial_sec, arithmetic_sec)
}

rhythm_secondary_agg <- extract_secondary_acc("rhythm_verbal.csv", "rhythm_spatial.csv", rhythm_data)
tone_secondary_agg   <- extract_secondary_acc("tone_verbal.csv",   "tone_spatial.csv",   tone_data)

run_rq2 <- function(primary_agg, secondary_agg, sdt_df, study_label) {
  merged_acc <- inner_join(primary_agg, secondary_agg,
                            by = c("Participant.Private.ID", "secondary_task"))
  merged_sdt <- inner_join(sdt_df %>% select(Participant.Private.ID, secondary_task, dprime),
                            secondary_agg, by = c("Participant.Private.ID", "secondary_task"))

  lapply(unique(merged_acc$secondary_task), function(cond) {
    r_acc <- cor.test(merged_acc$secondary_acc[merged_acc$secondary_task == cond],
                       merged_acc$prop_correct[merged_acc$secondary_task == cond])
    r_sdt <- cor.test(merged_sdt$secondary_acc[merged_sdt$secondary_task == cond],
                       merged_sdt$dprime[merged_sdt$secondary_task == cond])
    data.frame(Study = study_label, secondary_task = cond,
               r_acc    = round(r_acc$estimate, 3), p_acc    = round(r_acc$p.value, 3),
               r_dprime = round(r_sdt$estimate, 3), p_dprime = round(r_sdt$p.value, 3))
  }) %>% bind_rows()
}

rq2 <- bind_rows(
  run_rq2(rhythm_primary_agg, rhythm_secondary_agg, rhythm_sdt, "Rhythm"),
  run_rq2(tone_primary_agg,   tone_secondary_agg,   tone_sdt,   "Tone")
)

gt(rq2, groupname_col = "Study") %>%
  tab_header(title = "Table 3",
             subtitle = "RQ2: Storage-Processing Correlations by Secondary Task Condition") %>%
  cols_label(secondary_task = "Secondary Task",
             r_acc    = "r (Prop. Correct)", p_acc    = "p",
             r_dprime = "r (d')",            p_dprime = "p") %>%
  tab_options(table.font.size = 12, heading.align = "left")

# -----------------------------------------------------------------------------
# [6] RQ3: Secondary task type effects on primary task performance
# -----------------------------------------------------------------------------

# Beta regression cannot accommodate proportions of exactly 0 or 1; squish
# towards the centre using the actual number of trials per cell (n = 9)
squish <- function(p, n = 9) (p * (n - 1) + 0.5) / n

set_secondary_task_factor <- function(df) {
  df %>% mutate(secondary_task = factor(secondary_task, levels = c("arithmetic", "spatial", "verbal")))
}

rhythm_sdt_f  <- set_secondary_task_factor(rhythm_sdt)
tone_sdt_f    <- set_secondary_task_factor(tone_sdt)
rhythm_beta   <- rhythm_primary_agg %>% set_secondary_task_factor() %>% mutate(prop_sq = squish(prop_correct))
tone_beta     <- tone_primary_agg   %>% set_secondary_task_factor() %>% mutate(prop_sq = squish(prop_correct))

# Shared priors and sampler control settings for all RQ3/RQ4 models.
# exponential(1) on sd keeps random-effect variance away from zero;
# adapt_delta = 0.99 reduces divergent transitions given the small samples.
wi_priors_gaussian <- c(
  prior(normal(0, 1),   class = b),
  prior(normal(0, 1),   class = Intercept),
  prior(exponential(1), class = sd),
  prior(exponential(1), class = sigma)
)

wi_priors_beta <- c(
  prior(normal(0, 1),   class = b),
  prior(normal(0, 1),   class = Intercept),
  prior(exponential(1), class = sd)
)

stan_ctrl <- list(adapt_delta = 0.99, max_treedepth = 15)

# Single helper for all brms calls in RQ3/RQ4: differ only in formula, data,
# family, priors, and the cache filename. Models are cached to .rds via
# `file`; delete cached files before re-running if priors or data change.
fit_brms_model <- function(formula, data, family, prior, file) {
  brm(
    formula = formula, data = data, family = family, prior = prior,
    chains = 4, iter = 4000, warmup = 1000, cores = 4, seed = 42,
    control = stan_ctrl, file = file
  )
}

# --- Beta regression: proportion correct ~ secondary task ---
m_beta_r <- fit_brms_model(prop_sq ~ secondary_task + (1 | Participant.Private.ID),
                            rhythm_beta, Beta(), wi_priors_beta, "m_beta_rhythm")
m_beta_t <- fit_brms_model(prop_sq ~ secondary_task + (1 | Participant.Private.ID),
                            tone_beta,   Beta(), wi_priors_beta, "m_beta_tone")

# --- Gaussian: d' ~ secondary task ---
m_dprime_r <- fit_brms_model(dprime ~ secondary_task + (1 | Participant.Private.ID),
                              rhythm_sdt_f, gaussian(), wi_priors_gaussian, "m_dprime_rhythm")
m_dprime_t <- fit_brms_model(dprime ~ secondary_task + (1 | Participant.Private.ID),
                              tone_sdt_f,   gaussian(), wi_priors_gaussian, "m_dprime_tone")

# --- Gaussian: c ~ secondary task ---
m_c_r <- fit_brms_model(c_bias ~ secondary_task + (1 | Participant.Private.ID),
                         rhythm_sdt_f, gaussian(), wi_priors_gaussian, "m_c_rhythm")
m_c_t <- fit_brms_model(c_bias ~ secondary_task + (1 | Participant.Private.ID),
                         tone_sdt_f,   gaussian(), wi_priors_gaussian, "m_c_tone")

# Summarise — focus on posterior estimates and 95% credible intervals
summary(m_beta_r);   summary(m_beta_t)
summary(m_dprime_r); summary(m_dprime_t)
summary(m_c_r);      summary(m_c_t)

# Check convergence (Rhat should be < 1.01 for all parameters)
plot(m_dprime_r)
plot(m_dprime_t)

# --- RQ3 plots: accuracy, d', and c by secondary task condition ---
relabel_secondary_task <- function(df) {
  df %>% mutate(secondary_task = factor(secondary_task, levels = c("arithmetic", "spatial", "verbal"),
                                         labels = c("Arithmetic", "Spatial", "Verbal")))
}

acc_plot_data <- bind_rows(rhythm_primary_agg %>% mutate(Study = "Rhythm"),
                            tone_primary_agg   %>% mutate(Study = "Tone")) %>%
  relabel_secondary_task()

sdt_plot_data <- bind_rows(rhythm_sdt %>% mutate(Study = "Rhythm"),
                            tone_sdt   %>% mutate(Study = "Tone")) %>%
  relabel_secondary_task()

# Shared violin + boxplot layer for the three RQ3 outcome plots
rq3_violin_plot <- function(data, y_var, y_label, title, hline = FALSE) {
  p <- ggplot(data, aes(x = secondary_task, y = .data[[y_var]], fill = secondary_task)) +
    geom_violin(trim = TRUE, alpha = 0.35, colour = NA) +
    geom_boxplot(width = 0.12, outlier.shape = NA, colour = "black") +
    facet_wrap(~ Study) +
    scale_fill_manual(values = secondary_task_colours) +
    labs(x = "Secondary Task", y = y_label, title = title) +
    theme_apa + theme(legend.position = "none")
  if (hline) p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
  p
}

p_acc    <- rq3_violin_plot(acc_plot_data, "prop_correct", "Proportion Correct",
                             "Figure 2. Primary Task Accuracy by Secondary Task Type")
p_dprime <- rq3_violin_plot(sdt_plot_data, "dprime", "d'",
                             "Figure 3. Perceptual Sensitivity (d') by Secondary Task Type", hline = TRUE)
p_c      <- rq3_violin_plot(sdt_plot_data, "c_bias", "c",
                             "Figure 4. Response Bias (c) by Secondary Task Type", hline = TRUE)

ggsave("fig2_rq3_accuracy.png", p_acc,    width = 8, height = 4.5, dpi = 300)
ggsave("fig3_rq3_dprime.png",   p_dprime, width = 8, height = 4.5, dpi = 300)
ggsave("fig4_rq3_cbias.png",    p_c,      width = 8, height = 4.5, dpi = 300)

# -----------------------------------------------------------------------------
# [7] RQ4: Musical training effects on d' and c
# -----------------------------------------------------------------------------

rhythm_sdt_sc <- rhythm_sdt %>% mutate(MT_mean_z = scale(MT_mean)[, 1])
tone_sdt_sc   <- tone_sdt   %>% mutate(MT_mean_z = scale(MT_mean)[, 1])

m_mt_dprime_r <- fit_brms_model(dprime ~ MT_mean_z + (1 | Participant.Private.ID),
                                 rhythm_sdt_sc, gaussian(), wi_priors_gaussian, "m_mt_dprime_rhythm")
m_mt_dprime_t <- fit_brms_model(dprime ~ MT_mean_z + (1 | Participant.Private.ID),
                                 tone_sdt_sc,   gaussian(), wi_priors_gaussian, "m_mt_dprime_tone")
m_mt_c_r <- fit_brms_model(c_bias ~ MT_mean_z + (1 | Participant.Private.ID),
                            rhythm_sdt_sc, gaussian(), wi_priors_gaussian, "m_mt_c_rhythm")
m_mt_c_t <- fit_brms_model(c_bias ~ MT_mean_z + (1 | Participant.Private.ID),
                            tone_sdt_sc,   gaussian(), wi_priors_gaussian, "m_mt_c_tone")

summary(m_mt_dprime_r); summary(m_mt_dprime_t)
summary(m_mt_c_r);      summary(m_mt_c_t)

# Posterior probability that the musical training effect on d' is positive
hypothesis(m_mt_dprime_r, "MT_mean_z > 0")
hypothesis(m_mt_dprime_t, "MT_mean_z > 0")

# Confirm the training effect on response bias is near zero
hypothesis(m_mt_c_r, "MT_mean_z = 0")
hypothesis(m_mt_c_t, "MT_mean_z = 0")

# --- RQ4 plots: d' and c as a function of musical training ---
rq4_plot_data <- bind_rows(rhythm_sdt_sc %>% mutate(Study = "Rhythm"),
                            tone_sdt_sc   %>% mutate(Study = "Tone"))

rq4_scatter_plot <- function(data, y_var, y_label, title) {
  ggplot(data, aes(x = MT_mean_z, y = .data[[y_var]])) +
    geom_point(alpha = 0.5, size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    facet_wrap(~ Study) +
    labs(x = "Musical Training (Gold-MSI, standardised)", y = y_label, title = title) +
    theme_apa
}

p_mt_dprime <- rq4_scatter_plot(rq4_plot_data, "dprime", "d'",
                                 "Figure 5. Musical Training and Perceptual Sensitivity (d')")
p_mt_c      <- rq4_scatter_plot(rq4_plot_data, "c_bias", "c",
                                 "Figure 6. Musical Training and Response Bias (c)")

ggsave("fig5_rq4_mt_dprime.png", p_mt_dprime, width = 8, height = 4.5, dpi = 300)
ggsave("fig6_rq4_mt_cbias.png",  p_mt_c,      width = 8, height = 4.5, dpi = 300)
