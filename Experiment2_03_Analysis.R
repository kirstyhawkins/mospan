# =============================================================================
# M-OSPAN — Experiment 2
# Script 3 of 3: Analysis
# =============================================================================

library(tidyverse)
library(brms)
library(gt)
library(patchwork)
library(knitr)

set.seed(1234)

theme_apa <- theme_classic() +
  theme(
    text            = element_text(size = 12),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 10),
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 10),
    strip.text      = element_text(size = 11),
    plot.title      = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

stan_ctrl <- list(adapt_delta = 0.99, max_treedepth = 15)

# -----------------------------------------------------------------------------
# [1] Load data and compute person-level primary task accuracy
# -----------------------------------------------------------------------------

clean_data <- read.csv("exp2_clean_data.csv")
sdt_data   <- read.csv("exp2_sdt_data.csv")

# Primary task accuracy: SAME/DIFFERENT responses (rhythm, tone) or free
# recall submitted via Enter Key (classic), as flagged by task_performance
# during preprocessing (Script 1)
accuracy_data <- clean_data %>%
  group_by(Participant.Private.ID, block) %>%
  summarise(accuracy = mean(task_performance, na.rm = TRUE),
            MT = mean(MT_mean, na.rm = TRUE), .groups = "drop") %>%
  rename(ID = Participant.Private.ID) %>%
  mutate(
    condition_factor = factor(block, levels = c("classic", "rhythm", "tone")),
    # Beta regression cannot accommodate proportions of exactly 0 or 1
    accuracy_beta = accuracy * (1 - 2e-6) + 1e-6
  )

cat("Primary task accuracy — participant x condition rows:", nrow(accuracy_data), "\n")

# -----------------------------------------------------------------------------
# [2] Primary task accuracy: Bayesian beta regression model comparison
# Nine models of increasing complexity, crossing three fixed-effects
# structures (MT only; condition only; condition x MT interaction) with
# three random-effects structures (none; random intercepts; random
# intercepts and slopes for MT). Both the mean (mu) and precision (phi) of
# the beta distribution are modelled. Compared via LOO-CV.
# -----------------------------------------------------------------------------

prior_mt <- c(
  prior(normal(1, 0.5),   class = Intercept),
  prior(normal(0, 1),     class = b, coef = "MT"),
  prior(normal(0.4, 0.5), class = Intercept, dpar = phi),
  prior(normal(0, 1),     class = b, coef = "MT", dpar = phi)
)

prior_condition <- c(
  prior(normal(1, 0.5),   class = Intercept),
  prior(normal(0, 1),     class = b, coef = "condition_factorrhythm"),
  prior(normal(0, 1),     class = b, coef = "condition_factortone"),
  prior(normal(0.4, 0.5), class = Intercept, dpar = phi)
)

prior_interaction <- c(
  prior(normal(1, 0.5),   class = Intercept),
  prior(normal(0, 1),     class = b, coef = "MT"),
  prior(normal(0, 1),     class = b, coef = "condition_factorrhythm"),
  prior(normal(0, 1),     class = b, coef = "condition_factortone"),
  prior(normal(0.4, 0.5), class = Intercept, dpar = phi),
  prior(normal(0, 1),     class = b, coef = "MT", dpar = phi)
)

# Random-slope variants add an LKJ prior on the intercept-slope correlation
prior_mt_slope          <- prior_mt          + prior(lkj(2), class = cor)
prior_condition_slope   <- prior_condition   + prior(lkj(2), class = cor)
prior_interaction_slope <- prior_interaction + prior(lkj(2), class = cor)

# One row per model in the comparison set. Random-effects structure is
# appended to the fixed-effects formula for both the mean and phi
# sub-models, mirroring the original model specification throughout.
accuracy_model_specs <- tibble::tribble(
  ~model_name, ~mu_formula,                              ~phi_formula,                        ~prior,
  "m1_mt",            "accuracy_beta ~ 1 + MT",                  "phi ~ 1 + MT",                       "prior_mt",
  "m2_condition",      "accuracy_beta ~ 1 + condition_factor",    "phi ~ 1 + condition_factor",         "prior_condition",
  "m3_interaction",    "accuracy_beta ~ 1 + condition_factor*MT", "phi ~ 1 + condition_factor*MT",      "prior_interaction",
  "m4_mt_ri",          "accuracy_beta ~ 1 + MT + (1 | ID)",                  "phi ~ 1 + MT",                  "prior_mt",
  "m5_condition_ri",   "accuracy_beta ~ 1 + condition_factor + (1 | ID)",    "phi ~ 1 + MT",                  "prior_condition",
  "m6_interaction_ri", "accuracy_beta ~ 1 + condition_factor*MT + (1 | ID)", "phi ~ 1 + condition_factor*MT", "prior_interaction",
  "m7_mt_rs",          "accuracy_beta ~ 1 + MT + (1 + MT | ID)",                  "phi ~ 1 + MT",                  "prior_mt_slope",
  "m8_condition_rs",   "accuracy_beta ~ 1 + condition_factor + (1 + MT | ID)",    "phi ~ 1 + condition_factor",   "prior_condition_slope",
  "m9_interaction_rs", "accuracy_beta ~ 1 + condition_factor*MT + (1 + MT | ID)", "phi ~ 1 + condition_factor*MT", "prior_interaction_slope"
)

fit_accuracy_model <- function(model_name, mu_formula, phi_formula, prior_name) {
  brm(
    formula = bf(as.formula(mu_formula), as.formula(phi_formula)),
    data    = accuracy_data,
    family  = Beta(),
    prior   = get(prior_name),
    init    = "0",
    sample_prior = TRUE,
    chains = 4, iter = 5000, warmup = 1000, cores = 4, seed = 1234,
    control = stan_ctrl,
    backend = "cmdstanr",
    file    = paste0("exp2_", model_name)
  )
}

accuracy_models <- accuracy_model_specs %>%
  pmap(function(model_name, mu_formula, phi_formula, prior) {
    m <- fit_accuracy_model(model_name, mu_formula, phi_formula, prior)
    add_criterion(m, criterion = c("loo", "bayes_R2"))
  }) %>%
  set_names(accuracy_model_specs$model_name)

loo_comparison <- do.call(loo_compare, accuracy_models)
kable(loo_comparison, caption = "Table 1. LOO Model Comparison for Primary Task Accuracy")

# Best-fitting model carried forward to results and plots
best_model <- accuracy_models[[rownames(loo_comparison)[1]]]
summary(best_model)

# -----------------------------------------------------------------------------
# [3] Primary task accuracy: model checks and effects plots
# -----------------------------------------------------------------------------

pp_check(best_model, ndraws = 100) +
  labs(x = "Accuracy", y = "Density") +
  theme_apa

p_effects <- plot(conditional_effects(best_model), plot = FALSE)
wrap_plots(p_effects) +
  plot_annotation(title = "Figure 1. Conditional Effects on Primary Task Accuracy", tag_levels = "A") &
  theme_apa

# -----------------------------------------------------------------------------
# [4] SDT indices: musical training and condition effects (rhythm, tone only)
# Gaussian mixed models — d' and c are unbounded continuous measures and are
# NOT modelled with beta regression, unlike primary task accuracy above.
# -----------------------------------------------------------------------------

sdt_data <- sdt_data %>% mutate(condition_factor = factor(block, levels = c("rhythm", "tone")))

prior_dprime <- c(
  prior(normal(1, 1),   class = Intercept),   # moderate sensitivity expected
  prior(normal(0, 0.5), class = b)
)

prior_criterion <- c(
  prior(normal(0, 0.5), class = Intercept),   # unbiased responding expected
  prior(normal(0, 0.5), class = b)
)

m_dprime <- brm(
  dprime ~ 1 + condition_factor * MT + (1 | ID),
  data = sdt_data, family = gaussian(), prior = prior_dprime,
  chains = 4, iter = 5000, warmup = 1000, cores = 4, seed = 1234,
  backend = "cmdstanr", file = "exp2_m_dprime"
)

m_criterion <- brm(
  c_bias ~ 1 + condition_factor * MT + (1 | ID),
  data = sdt_data, family = gaussian(), prior = prior_criterion,
  chains = 4, iter = 5000, warmup = 1000, cores = 4, seed = 1234,
  backend = "cmdstanr", file = "exp2_m_criterion"
)

summary(m_dprime)
summary(m_criterion)

sdt_desc <- sdt_data %>%
  summarise(across(c(dprime, c_bias), list(M = mean, SD = sd, SE = ~ sd(.x) / sqrt(length(.x)))))
print(sdt_desc)

# -----------------------------------------------------------------------------
# [5] Storage-processing correlations
# Arithmetic (secondary task) accuracy vs primary task accuracy, by condition
# -----------------------------------------------------------------------------

arithmetic_data <- clean_data %>%
  filter(!is.na(math_performance)) %>%
  group_by(Participant.Private.ID, block) %>%
  summarise(arithmetic_accuracy = mean(math_performance, na.rm = TRUE), .groups = "drop") %>%
  rename(ID = Participant.Private.ID)

storage_processing <- accuracy_data %>%
  select(ID, block, condition_factor, accuracy) %>%
  inner_join(arithmetic_data, by = c("ID", "block"))

cat("Storage-processing — matched rows:", nrow(storage_processing), "\n")

condition_labels <- c(classic = "Classic (OSpan)", rhythm = "Rhythm Span", tone = "Pitch Span")

storage_processing_cor <- storage_processing %>%
  group_by(condition_factor) %>%
  group_modify(~ {
    test <- cor.test(.x$accuracy, .x$arithmetic_accuracy)
    tibble(n = nrow(.x), r = unname(test$estimate), t_stat = unname(test$statistic), p_value = test$p.value)
  }) %>%
  ungroup() %>%
  mutate(Condition = condition_labels[as.character(condition_factor)]) %>%
  select(Condition, n, r, t_stat, p_value)

gt(storage_processing_cor) %>%
  tab_header(title = "Table 2",
             subtitle = "Storage-Processing Correlations (Primary Task vs Arithmetic Accuracy) by Condition") %>%
  fmt_number(columns = c(r, t_stat), decimals = 2) %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  cols_label(n = "N", r = "r", t_stat = "t", p_value = "p") %>%
  tab_options(table.font.size = 12, heading.align = "left")

p_storage_processing <- ggplot(storage_processing, aes(x = arithmetic_accuracy, y = accuracy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.8) +
  facet_wrap(~ condition_factor, labeller = labeller(condition_factor = condition_labels)) +
  labs(x = "Arithmetic Accuracy (Processing)", y = "Primary Task Accuracy (Storage)",
       title = "Figure 2. Storage-Processing Correlations by Condition") +
  theme_apa

ggsave("fig2_storage_processing.png", p_storage_processing, width = 9, height = 4.5, dpi = 300)
