# 11-correlations.R
# Purpose  Descriptive sprint, the correlational exercise
#          (rebuild/06-descriptive-sprint.md, section 7): two-way
#          fixed-effects regressions of annual clearing on the social-policy
#          variables lagged one year, with controls, municipality and year
#          effects and standard errors clustered by municipality, 2004-2019;
#          the forest rate and the normalized outcome as robustness; a
#          long-difference cross section on the windows of D-2026-10-16-a.
#          Analysis layer; pipeline 01-06 untouched.
# Inputs   _data/final/panel_amz_2000_2020_descriptive.rds        script 09
#          _data/interim/siconfi_expenditure_munic_2000_2024.rds   script 09
#          _data/interim/siconfi_revenue_munic_2000_2024.rds       script 09
#          _scripts/utils/theme_ch4.R                              task 4.4
#          _scripts/utils/screen_siconfi.R                        D-2026-10-16-b
# Outputs  output/tables/tab-corr-twfe-main.csv        social regressors,
#                                                      separate and joint
#          output/tables/tab-corr-twfe-full.csv        every coefficient of
#                                                      every specification
#          output/tables/tab-corr-twfe-robustness.csv  alternative outcomes
#          output/tables/tab-corr-long-difference.csv  cross section
# Status   2026-10-17, sprint day 5. A conditional-correlation exercise, not
#          a candidate identification design, which is why it lives in
#          _scripts/ and not in _scripts/models/. Sample, lag structure and
#          variable scaling: D-2026-10-17-a; groups and windows
#          D-2026-10-16-a; SICONFI filing screen D-2026-10-16-b.
#          Phase 2R, session 2 (2026-10-30): the panel's columns carry the
#          crosswalk names (D-2026-10-30-a); the sample is selected by
#          script 09 (502 biome municipalities, D-2026-10-30-g) and asserted
#          here; the pooled price z-scores left the panel and are built on
#          that sample with z_on() (D-2026-10-30-b). Outputs not
#          regenerated before stage 10.

library(tidyverse)   # magrittr %>% throughout, never the native pipe
library(fixest)

source("_scripts/utils/theme_ch4.R")
source("_scripts/utils/screen_siconfi.R")
source("_scripts/utils/derived_variables.R")

# ---- Constants --------------------------------------------------------------
N_MUNIC_EXPECTED  <- 502L
PANEL_YEARS       <- 2000:2020
CORR_YEARS        <- 2004:2019     # outcome years; D-2026-10-17-a (BF ends
                                   # 2019, D-2026-10-05-b)
CORR_LAG          <- 1L            # regressors at t - 1; D-2026-10-17-a
TRANSFERS_SCALE   <- 100           # BF transfers per capita in hundreds of
                                   # R$ 2024; D-2026-10-17-a
EMP_SCALE         <- 10            # employment rate in units of ten points
BASELINE_YEARS    <- 2004:2007     # D-2026-10-16-a
END_YEARS         <- 2017:2020     # D-2026-10-16-a
BF_END_YEARS      <- 2017:2019     # the end window where BF exists
MIN_BASELINE_REPORTS <- 2L         # D-2026-10-16-a
N_FLAGGED_EXP_EXPECTED <- 16L      # D-2026-10-16-b, as asserted by script 10
N_FLAGGED_REV_EXPECTED <- 26L
N_FLAGGED_SAMPLE_EXPECTED <- 16L

SOCIAL_LABELS <- c(
  bf_cov_l1     = "BF coverage (families / quota)",
  bf_pc_l1      = "BF transfers per capita (hundred R$ 2024)",
  log_health_l1 = "log(1 + health spending per capita)",
  log_educ_l1   = "log(1 + education spending per capita)",
  log_assist_l1 = "log(1 + social assistance spending per capita)",
  emp_l1        = "Formal employment rate (ten points)"
)
SOCIAL_TOTAL <- c(log_social_l1 = "log(1 + social spending per capita)")
CONTROL_LABELS <- c(
  log_gdp_l1      = "log GDP per capita",
  price_crop_l1   = "Crop price index (z, lagged)",
  price_cattle_l1 = "Cattle price index (z, lagged)",
  cattle_l1       = "Cattle per km2",
  listed          = "On the priority list (0/1)"
)
OUTCOME_LABELS <- c(
  y_log_km2 = "log(1 + cleared km2)",
  y_rate    = "Clearing, share of remaining forest",
  y_norm    = "Normalized clearing (z on the own 2000-07 reference)"
)
LD_LABELS <- c(
  d_bf_cov     = "Change in BF coverage (families / quota)",
  d_bf_pc      = "Change in BF transfers per capita (hundred R$ 2024)",
  d_log_health = "Change in log(1 + health spending per capita)",
  d_log_educ   = "Change in log(1 + education spending per capita)",
  d_log_assist = "Change in log(1 + social assistance spending per capita)",
  d_emp        = "Change in formal employment rate (ten points)"
)
LD_TOTAL <- c(d_log_social = "Change in log(1 + social spending per capita)")
LD_CONTROL_LABELS <- c(
  d_log_gdp      = "Change in log GDP per capita",
  d_price_crop   = "Change in crop price index (z)",
  d_price_cattle = "Change in cattle price index (z)",
  d_cattle       = "Change in cattle per km2",
  ever_listed    = "Ever listed (0/1)"
)

# ---- Read -------------------------------------------------------------------
panel     <- read_rds("_data/final/panel_amz_2000_2020_descriptive.rds")
exp_munic <- read_rds("_data/interim/siconfi_expenditure_munic_2000_2024.rds")
rev_munic <- read_rds("_data/interim/siconfi_revenue_munic_2000_2024.rds")
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ---- Assert: input state ----------------------------------------------------
stopifnot(
  n_distinct(panel$geocode) == N_MUNIC_EXPECTED,
  setequal(panel$year, PANEL_YEARS),
  nrow(panel) == N_MUNIC_EXPECTED * length(PANEL_YEARS),
  !anyDuplicated(paste(panel$geocode, panel$year)),
  all(c("env_defor_area_km2", "env_defor_forest_share",
        "env_defor_area_z_bl0007", "sop_bf_fam_quota_ratio",
        "sop_bf_transf_pc_brl24", "lab_emp_formal_pia_rate",
        "ecn_gdp_pc_brl24_log", "mkt_price_crop_idx",
        "mkt_price_cattle_idx", "agr_cattle_per_km2", "enf_ppcdam_listed_dum",
        "exp_health_pc_brl_2024", "exp_education_pc_brl_2024",
        "exp_social_assistance_pc_brl_2024", "exp_social_pc_brl_2024",
        "exp_total_pc_brl_2024", "rev_current_pc_brl_2024") %in% names(panel)),
  all(!is.na(panel$env_defor_area_km2)),
  is.integer(panel$enf_ppcdam_listed_dum)
)

# ---- Transform: price z-scores (D-2026-10-30-b) ----------------------------
# The pooled standardisation of the price indices is sample-dependent and
# left the panel at phase 2R: centred and scaled here on the script's
# sample, every municipality-year of the 502 biome municipalities, which is
# the reference population script 04 used (D-2026-10-29-c); the lagged z
# is the z lagged within municipality on the balanced panel.
panel <- panel %>%
  arrange(geocode, year) %>%
  mutate(mkt_price_crop_idx_z   = z_on(mkt_price_crop_idx),
         mkt_price_cattle_idx_z = z_on(mkt_price_cattle_idx)) %>%
  group_by(geocode) %>%
  mutate(mkt_price_crop_idx_z_lag1   = lag(mkt_price_crop_idx_z),
         mkt_price_cattle_idx_z_lag1 = lag(mkt_price_cattle_idx_z)) %>%
  ungroup()

# ---- Transform: SICONFI filing screen (D-2026-10-16-b) ---------------------
# The same flags and application as script 10: computed on the
# 808-municipality interim files, applied to the exp_* and rev_* columns of
# the panel. Flagged cells are reporting gaps; nothing is corrected.
flags_exp <- siconfi_flags(exp_munic, "exp_total_pc_brl_2024")
flags_rev <- siconfi_flags(rev_munic, "rev_current_pc_brl_2024")
panel <- panel %>%
  siconfi_screen(flags_exp, names(panel)[str_starts(names(panel), "exp_")]) %>%
  siconfi_screen(flags_rev, names(panel)[str_starts(names(panel), "rev_")])
flags_sample <- bind_rows(flags_exp, flags_rev) %>%
  filter(geocode %in% panel$geocode) %>%
  distinct(geocode, year)
stopifnot(
  nrow(flags_exp) == N_FLAGGED_EXP_EXPECTED,
  nrow(flags_rev) == N_FLAGGED_REV_EXPECTED,
  nrow(flags_sample) == N_FLAGGED_SAMPLE_EXPECTED
)

# ---- Transform: regression panel (D-2026-10-17-a) --------------------------
# Lags are positional within municipality on the balanced 2000-2020 panel
# (asserted above), so lag(x, 1) is the previous year's value. The price
# indices enter through the lagged z-scores built above (D-2026-10-30-b)
# and are not lagged again. Listing status is contemporaneous: it is the
# enforcement regime of the outcome year, not a social condition. A function
# absent from a reporting year is a zero (D-2026-10-15-c), so expenditure
# per capita enters as log(1 + x) rather than log(x).
reg <- panel %>%
  arrange(geocode, year) %>%
  group_by(geocode) %>%
  mutate(
    y_log_km2       = log1p(env_defor_area_km2),
    y_rate          = env_defor_forest_share,
    y_norm          = env_defor_area_z_bl0007,
    bf_cov_l1       = lag(sop_bf_fam_quota_ratio, CORR_LAG),
    bf_pc_l1        = lag(sop_bf_transf_pc_brl24, CORR_LAG) / TRANSFERS_SCALE,
    log_health_l1   = lag(log1p(exp_health_pc_brl_2024), CORR_LAG),
    log_educ_l1     = lag(log1p(exp_education_pc_brl_2024), CORR_LAG),
    log_assist_l1   = lag(log1p(exp_social_assistance_pc_brl_2024), CORR_LAG),
    log_social_l1   = lag(log1p(exp_social_pc_brl_2024), CORR_LAG),
    emp_l1          = lag(lab_emp_formal_pia_rate, CORR_LAG) * EMP_SCALE,
    log_gdp_l1      = lag(ecn_gdp_pc_brl24_log, CORR_LAG),
    cattle_l1       = lag(agr_cattle_per_km2, CORR_LAG),
    price_crop_l1   = mkt_price_crop_idx_z_lag1,
    price_cattle_l1 = mkt_price_cattle_idx_z_lag1,
    listed          = enf_ppcdam_listed_dum
  ) %>%
  ungroup() %>%
  filter(year %in% CORR_YEARS)

# One estimation sample for the main table, so that the separate and the
# joint specifications are read on the same municipality-years: complete
# cases on the log outcome, the six social regressors, their total and the
# controls. BF has no 2003 value, so the sample starts with the 2005 outcome.
main_vars <- c("y_log_km2", names(SOCIAL_LABELS), names(SOCIAL_TOTAL),
               names(CONTROL_LABELS))
reg_main <- reg %>% filter(if_all(all_of(main_vars), ~ !is.na(.x)))
stopifnot(
  min(reg_main$year) == min(CORR_YEARS) + CORR_LAG,
  max(reg_main$year) == max(CORR_YEARS),
  all(is.finite(reg_main$log_health_l1)),
  all(is.finite(reg_main$log_educ_l1)),
  all(is.finite(reg_main$log_assist_l1))
)

# ---- Estimate: two-way fixed effects ---------------------------------------
twfe <- function(y, rhs, data) {
  feols(as.formula(paste(y, "~", paste(rhs, collapse = " + "),
                         "| geocode + year")),
        data = data, cluster = ~geocode)
}
tidy_twfe <- function(m, data, specification, outcome, keep) {
  ct <- coeftable(m)
  tibble(
    specification = specification,
    outcome       = OUTCOME_LABELS[[outcome]],
    term          = rownames(ct),
    coefficient   = ct[, "Estimate"],
    se            = ct[, "Std. Error"],
    p_value       = ct[, "Pr(>|t|)"],
    n_obs         = nobs(m),
    n_munic       = unname(m$fixef_sizes["geocode"]),
    years         = paste(range(data$year[obs(m)]), collapse = "-"),
    r2_within     = unname(fitstat(m, "wr2")[[1]])
  ) %>%
    filter(term %in% keep)
}
all_labels <- c(SOCIAL_LABELS, SOCIAL_TOTAL, CONTROL_LABELS)
controls   <- names(CONTROL_LABELS)

separate <- imap_dfr(c(SOCIAL_LABELS, SOCIAL_TOTAL), function(label, var) {
  tidy_twfe(twfe("y_log_km2", c(var, controls), reg_main), reg_main,
            "Separate, with controls", "y_log_km2", keep = c(var, controls)) %>%
    mutate(block = label)
})
m_joint <- twfe("y_log_km2", c(names(SOCIAL_LABELS), controls), reg_main)
joint <- tidy_twfe(m_joint, reg_main, "Joint, with controls", "y_log_km2",
                   keep = c(names(SOCIAL_LABELS), controls)) %>%
  mutate(block = "Joint")

tab_full <- bind_rows(separate, joint) %>%
  mutate(regressor = all_labels[term]) %>%
  select(specification, block, outcome, regressor, coefficient, se, p_value,
         n_obs, n_munic, years, r2_within) %>%
  mutate(across(c(coefficient, se, p_value, r2_within), ~ round(.x, 4)))
save_tab(tab_full, "corr-twfe-full")

tab_main <- bind_rows(
  separate %>% filter(term %in% c(names(SOCIAL_LABELS), names(SOCIAL_TOTAL))),
  joint %>% filter(term %in% names(SOCIAL_LABELS))
) %>%
  mutate(regressor = all_labels[term]) %>%
  select(specification, regressor, coefficient, se, n_obs, n_munic,
         r2_within) %>%
  mutate(across(c(coefficient, se, r2_within), ~ round(.x, 3)))
save_tab(tab_main, "corr-twfe-main")

# Robustness: the joint specification without controls, then with the two
# alternative outcomes on their own complete cases (the normalized outcome
# is NA where the 2000-07 reference sd is degenerate, D-2026-10-12-f).
robust_vars <- c(names(SOCIAL_LABELS), controls)
reg_rate <- reg %>% filter(if_all(all_of(c("y_rate", robust_vars)),
                                  ~ !is.na(.x)))
reg_norm <- reg %>% filter(if_all(all_of(c("y_norm", robust_vars)),
                                  ~ !is.na(.x)))
tab_robust <- bind_rows(
  tidy_twfe(twfe("y_log_km2", names(SOCIAL_LABELS), reg_main), reg_main,
            "Joint, no controls", "y_log_km2", names(SOCIAL_LABELS)),
  tidy_twfe(twfe("y_rate", robust_vars, reg_rate), reg_rate,
            "Joint, with controls", "y_rate", names(SOCIAL_LABELS)),
  tidy_twfe(twfe("y_norm", robust_vars, reg_norm), reg_norm,
            "Joint, with controls", "y_norm", names(SOCIAL_LABELS))
) %>%
  mutate(regressor = all_labels[term]) %>%
  select(outcome, specification, regressor, coefficient, se, n_obs, n_munic,
         years, r2_within) %>%
  mutate(across(c(coefficient, se, r2_within), ~ round(.x, 3)))
save_tab(tab_robust, "corr-twfe-robustness")

# ---- Estimate: long differences --------------------------------------------
# One change per municipality between the windows of D-2026-10-16-a (window
# means; the spending change needs two reported years in each window, 470
# municipalities). BF's end window stops in 2019. Heteroskedasticity-robust
# standard errors (HC1); no fixed effects in a cross section.
window_mean <- function(x, year, years, min_n = 1L) {
  v <- x[year %in% years]
  if (sum(!is.na(v)) < min_n) NA_real_ else mean(v, na.rm = TRUE)
}
ld <- panel %>%
  group_by(geocode) %>%
  summarise(
    ever_listed = as.integer(any(enf_ppcdam_listed_dum == 1L)),
    d_log_defor = log1p(window_mean(env_defor_area_km2, year, END_YEARS)) -
      log1p(window_mean(env_defor_area_km2, year, BASELINE_YEARS)),
    d_bf_cov = window_mean(sop_bf_fam_quota_ratio, year, BF_END_YEARS) -
      window_mean(sop_bf_fam_quota_ratio, year, BASELINE_YEARS),
    d_bf_pc = (window_mean(sop_bf_transf_pc_brl24, year, BF_END_YEARS) -
      window_mean(sop_bf_transf_pc_brl24, year, BASELINE_YEARS)) /
      TRANSFERS_SCALE,
    d_log_health = log1p(window_mean(exp_health_pc_brl_2024, year, END_YEARS,
                                     MIN_BASELINE_REPORTS)) -
      log1p(window_mean(exp_health_pc_brl_2024, year, BASELINE_YEARS,
                        MIN_BASELINE_REPORTS)),
    d_log_educ = log1p(window_mean(exp_education_pc_brl_2024, year, END_YEARS,
                                   MIN_BASELINE_REPORTS)) -
      log1p(window_mean(exp_education_pc_brl_2024, year, BASELINE_YEARS,
                        MIN_BASELINE_REPORTS)),
    d_log_assist = log1p(window_mean(exp_social_assistance_pc_brl_2024, year,
                                     END_YEARS, MIN_BASELINE_REPORTS)) -
      log1p(window_mean(exp_social_assistance_pc_brl_2024, year,
                        BASELINE_YEARS, MIN_BASELINE_REPORTS)),
    d_log_social = log1p(window_mean(exp_social_pc_brl_2024, year, END_YEARS,
                                     MIN_BASELINE_REPORTS)) -
      log1p(window_mean(exp_social_pc_brl_2024, year, BASELINE_YEARS,
                        MIN_BASELINE_REPORTS)),
    d_emp = (window_mean(lab_emp_formal_pia_rate, year, END_YEARS) -
      window_mean(lab_emp_formal_pia_rate, year, BASELINE_YEARS)) * EMP_SCALE,
    d_log_gdp = window_mean(ecn_gdp_pc_brl24_log, year, END_YEARS) -
      window_mean(ecn_gdp_pc_brl24_log, year, BASELINE_YEARS),
    d_price_crop = window_mean(mkt_price_crop_idx_z, year, END_YEARS) -
      window_mean(mkt_price_crop_idx_z, year, BASELINE_YEARS),
    d_price_cattle = window_mean(mkt_price_cattle_idx_z, year, END_YEARS) -
      window_mean(mkt_price_cattle_idx_z, year, BASELINE_YEARS),
    d_cattle = window_mean(agr_cattle_per_km2, year, END_YEARS) -
      window_mean(agr_cattle_per_km2, year, BASELINE_YEARS),
    .groups = "drop"
  )
ld_vars <- c("d_log_defor", names(LD_LABELS), names(LD_TOTAL),
             names(LD_CONTROL_LABELS))
ld_main <- ld %>% filter(if_all(all_of(ld_vars), ~ is.finite(.x)))
ld_fit <- function(rhs, data) {
  feols(as.formula(paste("d_log_defor ~", paste(rhs, collapse = " + "))),
        data = data, vcov = "hetero")
}
tidy_ld <- function(m, specification, keep) {
  ct <- coeftable(m)
  tibble(
    specification = specification,
    term          = rownames(ct),
    coefficient   = ct[, "Estimate"],
    se            = ct[, "Std. Error"],
    p_value       = ct[, "Pr(>|t|)"],
    n_munic       = nobs(m),
    r2            = unname(fitstat(m, "r2")[[1]])
  ) %>%
    filter(term %in% keep)
}
ld_labels <- c(LD_LABELS, LD_TOTAL, LD_CONTROL_LABELS)
ld_controls <- names(LD_CONTROL_LABELS)
tab_ld <- bind_rows(
  imap_dfr(c(LD_LABELS, LD_TOTAL), function(label, var) {
    tidy_ld(ld_fit(c(var, ld_controls), ld_main),
            "Separate, with controls", keep = var)
  }),
  tidy_ld(ld_fit(c(names(LD_LABELS), ld_controls), ld_main),
          "Joint, with controls", keep = names(LD_LABELS))
) %>%
  mutate(regressor = ld_labels[term],
         windows = paste0(min(BASELINE_YEARS), "-", max(BASELINE_YEARS),
                          " to ", min(END_YEARS), "-", max(END_YEARS))) %>%
  select(specification, regressor, coefficient, se, p_value, n_munic, r2,
         windows) %>%
  mutate(across(c(coefficient, se, p_value, r2), ~ round(.x, 3)))
save_tab(tab_ld, "corr-long-difference")

# ---- Assert: output state --------------------------------------------------
outputs <- c(
  "output/tables/tab-corr-twfe-main.csv",
  "output/tables/tab-corr-twfe-full.csv",
  "output/tables/tab-corr-twfe-robustness.csv",
  "output/tables/tab-corr-long-difference.csv"
)
stopifnot(
  all(file.exists(outputs)),
  nrow(tab_main) == length(SOCIAL_LABELS) * 2 + length(SOCIAL_TOTAL),
  all(is.finite(tab_main$coefficient)), all(is.finite(tab_main$se)),
  n_distinct(tab_main$n_obs) == 1L,
  nrow(tab_robust) == length(SOCIAL_LABELS) * 3,
  all(is.finite(tab_robust$coefficient)),
  nrow(tab_ld) == length(LD_LABELS) * 2 + length(LD_TOTAL),
  all(is.finite(tab_ld$coefficient)),
  n_distinct(tab_ld$n_munic) == 1L,
  tab_ld$n_munic[1] <= N_MUNIC_EXPECTED
)
message("TWFE main sample: ", nrow(reg_main), " municipality-years, ",
        n_distinct(reg_main$geocode), " municipalities, ",
        min(reg_main$year), "-", max(reg_main$year),
        ". Long difference: ", nrow(ld_main), " municipalities.")
