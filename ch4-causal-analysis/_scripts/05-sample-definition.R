# 07-at-risk-indicator.R
# Build a baseline (2000-2003) "at-risk" indicator and run positivity / overlap
# diagnostics for the PPCDAm contrast. Nothing from the policy window enters the
# rule: the indicator rests only on pre-treatment characteristics.
#
# SAMPLE IS MODULAR BY ESTIMAND. Two flags are produced and should be used for
# different designs, never collapsed into one global filter:
#   avail_ok                  -> forest-availability floor only. This is the
#                                sample for the DIRECT effect of cash transfers
#                                or employment on deforestation, where you want
#                                the full range of social-policy exposure and
#                                must NOT condition on a deforestation-correlated
#                                gate.
#   at_risk_broad / _narrow   -> forest floor PLUS a baseline-deforestation
#                                frontier gate. This is the sample for the
#                                PPCDAm contrast and the PPCDAm-by-social-policy
#                                triple difference, where positivity requires
#                                controls that are plausibly listable.
#
# Design notes
#  - Availability floor: a municipality needs forest to lose. Barely binds here
#    (almost every unit is forested) but kept for documentation.
#  - Pressure scope: baseline deforestation AREA (not rate). Absolute area is what
#    PPCDAm priority status is assigned on, so it preserves overlap with the
#    treatment; the rate misleads because listed units are large, heavily
#    forested municipalities with big absolute clearings but modest rates.
#  - Agricultural pressure (cattle, crop) is a CONTINUOUS covariate for matching
#    and heterogeneity, not a gate: listed units have lower baseline cattle
#    density, so an OR-gate on it would pull in off-support units.
#  - Treatment for the DIAGNOSTICS is defined to match the analysis WINDOW, not
#    the full panel. A unit first listed in 2017-2020 is a not-yet-treated
#    control in a 2004-2010 design, and the positivity check must treat it as
#    such. Two windows are reported: the main absorbing window (2004-2010, zero
#    exits) and the robustness window (2004-2012).
#
# Input : _data/final_dataset_4.csv
# Output: _data/final_dataset_5.csv         (panel + indicator + treatment cols)
#         _output/at_risk_indicator.csv      (municipality-level lookup)
#         _output/at_risk_balance.csv        (covariate balance, main window)

library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(magrittr)

# ---- Helpers (defined first so any section can run on its own) ---------------
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  # z-score

# ---- Parameters (tune, then report robustness across them) ------------------
FOREST_SHARE_FLOOR <- 0.10        # availability: minimum baseline forest share
DEFOR_CUT_BROAD    <- 1           # km2/yr baseline clearing for the broad scope
DEFOR_CUT_NARROW   <- 5           # km2/yr baseline clearing for the narrow scope
BASE_YEARS         <- 2000:2003   # pre-treatment window for baseline measures
WINDOW_START       <- 2004        # analysis window start (BF data begin)
WINDOW_END_MAIN    <- 2010        # main: absorbing treatment, zero exits
WINDOW_END_ROBUST  <- 2012        # robustness: more cohorts, a few exits


# ---- Read and drop Mojui dos Campos -----------------------------------------
df <- read_csv(file.path("_data", "final_dataset_4.csv"),
               col_types = cols(geocode = col_character(), year = col_integer())) %>%
  filter(geocode != "1504752")

glimpse(df)


# ---- Baseline (2000-2003) municipality summaries ----------------------------
base <- df %>%
  filter(year %in% BASE_YEARS) %>%
  mutate(crop_share_all = crop_soy_munic_area_share + crop_corn_munic_area_share +
           crop_rice_munic_area_share + crop_cotton_munic_area_share)

forest0 <- df %>% filter(year == min(BASE_YEARS)) %>%
  transmute(geocode,
            forest_share_2000 = forest_area_km2 / munic_area_km2,  # a few >1 from
            forest_km2_2000   = forest_area_km2)                   # source boundary mismatch

muni <- base %>%
  group_by(geocode) %>%
  summarise(defor_area_b  = mean(deforestation_area_km2, na.rm = TRUE),
            defor_rate_b  = mean(deforestation_forest_rate, na.rm = TRUE),
            cattle_dens_b = mean(cattle_per_km2, na.rm = TRUE),
            crop_share_b  = mean(crop_share_all, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(forest0, by = "geocode")

muni

# ---- Window-consistent treatment definitions --------------------------------
# treated == listed at any point within [WINDOW_START, window end]. ppcdam_ever
# over the full panel is kept only as a reference column.
treat_by <- function(d, y0, y1, nm) {
  d %>% filter(year >= y0, year <= y1) %>%
    group_by(geocode) %>%
    summarise(!!nm := as.integer(max(ppcdam_list) == 1), .groups = "drop")
}

muni <- muni %>%
  left_join(treat_by(df, WINDOW_START, WINDOW_END_MAIN,   "ppcdam_main"),   by = "geocode") %>%
  left_join(treat_by(df, WINDOW_START, WINDOW_END_ROBUST, "ppcdam_robust"), by = "geocode") %>%
  left_join(df %>% group_by(geocode) %>%
              summarise(ppcdam_ever_full = max(ppcdam_list), .groups = "drop"),
            by = "geocode")

view(muni)

# ---- Continuous pressure scores ---------------------------------------------
# Heavy right skew, so log first, then z-score. Three flavours:
#   pressure_defor : assignment-relevant (absolute clearing), a lagged outcome
#   pressure_agri  : input-based (not an outcome), safe for matching
#   pressure_score : combined frontier-intensity index
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

muni <- muni %>%
  mutate(
    log_defor_area_b  = log1p(defor_area_b),
    log_cattle_dens_b = log1p(cattle_dens_b),
    log_crop_share_b  = log1p(crop_share_b),
    pressure_defor = z(log_defor_area_b),
    pressure_agri  = 0.5 * z(log_cattle_dens_b) + 0.5 * z(log_crop_share_b),
    pressure_score = z(pressure_defor + pressure_agri)
  )

glimpse(muni)

# ---- Availability floor and binary scopes -----------------------------------
muni <- muni %>%
  mutate(
    avail_ok       = forest_share_2000 >= FOREST_SHARE_FLOOR,
    at_risk_broad  = avail_ok & defor_area_b >= DEFOR_CUT_BROAD,
    at_risk_narrow = avail_ok & defor_area_b >= DEFOR_CUT_NARROW
  )

glimpse(muni)
view(muni)

# ---- Diagnostics (run for a given treatment definition) ---------------------
retention <- function(d, treat) {
  data.frame(
    sample  = c("full", "broad", "narrow"),
    n       = c(nrow(d), sum(d$at_risk_broad), sum(d$at_risk_narrow)),
    treated = c(sum(d[[treat]]),
                sum(d[[treat]] & d$at_risk_broad),
                sum(d[[treat]] & d$at_risk_narrow)),
    control = c(sum(d[[treat]] == 0),
                sum(d[[treat]] == 0 & d$at_risk_broad),
                sum(d[[treat]] == 0 & d$at_risk_narrow))
  )
}

# Purpose of the SMD table is NOT to show the restriction balances covariates
# (it does not, and need not). It flags which covariates the matching / weighting
# step inside the at-risk set must condition on. |SMD| > ~0.1 = imbalanced.
bal_covs <- c("forest_share_2000", "defor_area_b", "defor_rate_b",
              "cattle_dens_b", "crop_share_b")

smd_tab <- function(d, covs, treat, sample_label) {
  t <- d[d[[treat]] == 1, ]; c <- d[d[[treat]] == 0, ]
  data.frame(
    sample = sample_label, covariate = covs,
    mean_treated = sapply(covs, function(v) mean(t[[v]], na.rm = TRUE)),
    mean_control = sapply(covs, function(v) mean(c[[v]], na.rm = TRUE)),
    smd = sapply(covs, function(v) {
      s <- sqrt((var(t[[v]], na.rm = TRUE) + var(c[[v]], na.rm = TRUE)) / 2)
      (mean(t[[v]], na.rm = TRUE) - mean(c[[v]], na.rm = TRUE)) / s
    }), row.names = NULL
  )
}

# Positivity: how many CONTROL units sit off the treated common support.
ps_covs <- c("forest_share_2000", "log_defor_area_b",
             "log_cattle_dens_b", "log_crop_share_b")

ps_overlap <- function(d, treat, label) {
  cc <- d[stats::complete.cases(d[ps_covs]), ]
  m  <- glm(reformulate(ps_covs, response = treat), data = cc, family = binomial)
  cc$ps <- predict(m, type = "response")
  tr <- cc$ps[cc[[treat]] == 1]; co <- cc$ps[cc[[treat]] == 0]
  lo <- max(min(tr), min(co)); hi <- min(max(tr), max(co))
  data.frame(
    sample = label, n = nrow(cc), treated = sum(cc[[treat]]),
    ps_support_lo = round(lo, 3), ps_support_hi = round(hi, 3),
    off_support_treated = sum(tr < lo | tr > hi),
    off_support_control = sum(co < lo | co > hi),
    treated_med_ps = round(median(tr), 3), control_med_ps = round(median(co), 3)
  )
}

run_diag <- function(treat, tag) {
  cat("\n############################################################\n")
  cat(sprintf("## Treatment definition: %s  (listed within window)\n", tag))
  cat("############################################################\n")
  cat("\n-- Retention (all treated units should be kept) --\n")
  print(retention(muni, treat), row.names = FALSE)
  cat("\n-- Standardized mean differences (treated vs control) --\n")
  print(bind_rows(
    smd_tab(muni,                         bal_covs, treat, "full"),
    smd_tab(filter(muni, at_risk_broad),  bal_covs, treat, "broad"),
    smd_tab(filter(muni, at_risk_narrow), bal_covs, treat, "narrow")
  ) %>% mutate(across(c(mean_treated, mean_control, smd), ~ round(.x, 3))),
  row.names = FALSE)
  cat("\n-- Propensity overlap for P(treated) --\n")
  print(bind_rows(
    ps_overlap(muni,                         treat, "full"),
    ps_overlap(filter(muni, at_risk_broad),  treat, "broad"),
    ps_overlap(filter(muni, at_risk_narrow), treat, "narrow")
  ), row.names = FALSE)
}

run_diag("ppcdam_main",   sprintf("MAIN %d-%d", WINDOW_START, WINDOW_END_MAIN))

# For the methodological appendix
# Sample-definition diagnostics. The at-risk sample is defined on pre-treatment characteristics averaged over 2000–2003, before the priority-list policy began. Three diagnostics describe how the restriction behaves, each reported for the full sample of 502 municipalities and for two nested restrictions: the broad sample (forest available and baseline annual clearing of at least 1 km²) and the narrow sample (clearing of at least 5 km²). Throughout, a municipality counts as treated if it entered the priority list at any point within the analysis window of 2004–2010; municipalities listed only after the window serve as controls. A parallel set of tables is produced for the 2004–2012 robustness window.
# Retention. This table confirms that the restriction removes only control units and never discards a treated one, since dropping a listed municipality would change the estimand. The column n is the number of municipalities in the sample, treated the number listed within the window, and control the remainder. All 43 listed municipalities survive both restrictions; only never-listed units are removed, from 459 controls in the full sample to 425 in the broad and 365 in the narrow.
# Standardized mean differences. This table compares the baseline characteristics of listed and non-listed municipalities. For each covariate it reports the treated-group mean (mean_treated), the control-group mean (mean_control), and the standardized mean difference (smd), defined as the difference in means divided by the pooled within-group standard deviation. Values near zero indicate similar groups, and absolute values above about 0.1 indicate imbalance that a comparison must address. Listed municipalities have much higher baseline deforestation area (smd near 1.5), are more forested (smd 0.7 to 0.9), and have somewhat lower deforestation rate and cattle density (smd around −0.4 to −0.6). The imbalance does not shrink as the sample narrows, and on some covariates it grows. This is expected and is not the task of the restriction. The restriction secures comparable treatment probability (overlap); the remaining covariate imbalance is removed at the estimation stage by matching or weighting within the at-risk sample. The table therefore identifies the covariates the estimator must condition on: forest share, deforestation area, deforestation rate, and cattle density.
# Propensity overlap. This table assesses common support, the requirement that listed and non-listed municipalities share a comparable range of treatment probability. A municipality's propensity is its predicted probability of being listed, from a logistic regression of listing status on the baseline covariates (forest share, log baseline deforestation area, log cattle density, log crop share). The columns are: n, the municipalities with complete covariates entering the model, three being dropped for missing baseline values; treated, the number listed; ps_support_lo and ps_support_hi, the bounds of the region where treated and control propensities overlap; off_support_treated and off_support_control, the counts of each group falling outside that shared region; and treated_med_ps and control_med_ps, the median propensity in each group. The groups separate sharply: the median listed municipality has a propensity of 0.69 while the median non-listed municipality is near zero. Most controls in the full sample, 410 of the 456 in the model, lie below the treated range, meaning they have almost no probability of being listed and add little to a credible comparison. The restriction reduces this off-support control mass to 377 in the broad sample and 317 in the narrow, while keeping all 43 listed municipalities. A residual 14 listed municipalities lie above the control range; these are extreme-frontier units for which no control offers a close match, and estimates that depend on them rest on extrapolation and are reported with that caveat.
# 
# For the methodological section of the chapter
# The enforcement analysis is restricted to municipalities at risk of frontier deforestation, identified from pre-treatment characteristics measured over 2000–2003: a municipality enters the sample if it retained forest and recorded baseline annual clearing above a minimum area. The restriction serves the positivity requirement of the design. Priority-list municipalities are large, heavily forested frontier units with high absolute baseline deforestation, and most Amazonian municipalities have almost no probability of ever being listed; comparing listed units against these would set them against municipalities that were never at risk of the policy. The restriction removes such off-support controls, cutting their number by roughly a fifth, while retaining every municipality listed within the analysis window, so it improves comparability without altering the treated group or selecting on the outcome. Differences that remain between listed and non-listed municipalities on baseline forest cover, deforestation, and cattle density are handled at the estimation stage by matching within the at-risk sample. A small group of extreme-frontier listed municipalities has no close control match, and estimates are reported with and without them. The area threshold defining the sample is reported in a broad and a narrow version to show that the results do not turn on where the line is drawn.

run_diag("ppcdam_robust", sprintf("ROBUST %d-%d", WINDOW_START, WINDOW_END_ROBUST))

# For the methodological appendix
# Robustness window (2004–2012). These tables repeat the sample-definition diagnostics under the wider analysis window, in which a municipality counts as treated if it entered the priority list at any point between 2004 and 2012. This window adds the cohorts listed in 2011 and 2012, raising the treated group from 43 to 51, at the cost of a few municipalities that later left the list, which is why it serves as a robustness check rather than the main specification. The columns carry the same meanings as in the MAIN-window tables: in the retention table, n, treated, and control are the municipality counts; in the balance table, mean_treated and mean_control are the group means and smd the difference divided by the pooled within-group standard deviation; in the overlap table, n is the municipalities entering the propensity model, ps_support_lo and ps_support_hi bound the shared propensity region, off_support_treated and off_support_control count units outside it, and treated_med_ps and control_med_ps are the group median propensities.
# Retention. All 51 listed municipalities survive both restrictions. Only never-listed controls are removed, from 451 in the full sample to 417 in the broad and 357 in the narrow. As in the main window, the restriction touches controls only.
# Standardized mean differences. The pattern matches the main window. Listed municipalities have far higher baseline deforestation area (smd near 1.4), are more forested (smd 0.74 to 0.92), and have lower deforestation rate and cattle density (smd around −0.4 to −0.6), and the imbalance does not shrink as the sample narrows. One difference from the main window is worth noting: baseline crop share now sits higher in the treated group (smd 0.14 to 0.19, rather than near zero), because the 2011 and 2012 cohorts include more crop-oriented municipalities. Crop share therefore joins forest share, deforestation area, deforestation rate, and cattle density as a covariate the estimator must condition on when this window is used.
# Propensity overlap. The separation between groups is again sharp, with the median listed municipality at a propensity of 0.67 against near zero for controls. Adding the later cohorts widens the lower bound of common support slightly (from 0.088 to 0.035) and lowers the off-support control mass relative to the main window, since the larger and marginally less extreme treated group brings more controls within range. Off-support controls fall from 357 in the full sample to 324 in the broad and 264 in the narrow, while all 51 listed municipalities are retained. The residual block of off-support treated units is 12 here, slightly smaller than the 14 in the main window; these remain extreme-frontier units with no close control, and estimates relying on them are reported with that caveat.
# 
# For the methodological section of the chapter
# As a robustness check, the analysis window is widened to 2004–2012, which adds the priority-list cohorts of 2011 and 2012 and raises the treated group from 43 to 51 municipalities. The sample diagnostics behave as in the main window: the at-risk restriction removes only never-listed controls, cutting the off-support control mass while retaining every listed municipality, and the listed group remains distinct on baseline forest cover, deforestation, and cattle density, with these differences handled by matching at the estimation stage. The wider window differs in one respect, a higher baseline crop share among listed municipalities, reflecting the more crop-oriented profile of the later cohorts, which is accordingly included among the covariates conditioned on. The consistency of the diagnostics across the two windows indicates that the sample definition and the comparability of the treated and control groups do not depend on the precise treatment window or on the area threshold used to draw the sample.

cat("\nNote: a residual block of treated units stays above the control support;",
    "\nthese are extreme-frontier units no control matches. Acknowledge them as",
    "\nhard-to-match rather than extrapolating a counterfactual for them.\n")


# ---- Save -------------------------------------------------------------------
if (!dir.exists("_output")) dir.create("_output")
write_csv(
  bind_rows(
    smd_tab(muni,                         bal_covs, "ppcdam_main", "full"),
    smd_tab(filter(muni, at_risk_broad),  bal_covs, "ppcdam_main", "broad"),
    smd_tab(filter(muni, at_risk_narrow), bal_covs, "ppcdam_main", "narrow")
  ),
  file.path("_output", "at_risk_balance.csv")
)
write_csv(muni, file.path("_output", "at_risk_indicator.csv"))

panel <- df %>%
  left_join(muni %>% select(geocode, forest_share_2000, defor_area_b,
                            pressure_defor, pressure_agri, pressure_score,
                            avail_ok, at_risk_broad, at_risk_narrow,
                            ppcdam_main, ppcdam_robust, ppcdam_ever_full),
            by = "geocode")
write_csv(panel, file.path("_data", "final_dataset_5.csv"))


panel

glimpse(panel)

cat("\nWrote _data/final_dataset_5.csv, _output/at_risk_indicator.csv, _output/at_risk_balance.csv\n")

# ---- Notes ------------------------------------------------------------------
# 1. Sample by estimand: use at_risk_broad/_narrow for the PPCDAm and PPCDAm-by-
#    social-policy designs; use avail_ok (or the full sample) for the direct
#    effect of transfers/employment. Do not impose the frontier gate on the
#    latter.
# 2. Baseline moderators for the triple difference (pre-listing means of BF per
#    capita and employment, e.g. 2004-2007) are NOT built here. They belong in a
#    companion baseline-characteristics step so the moderator is fixed and
#    pre-treatment, never contemporaneous.
# 3. Matching inside the at-risk set: MatchIt + cobalt on ppcdam_main, e.g.
#      m <- matchit(ppcdam_main ~ forest_share_2000 + log_defor_area_b +
#                   log_cattle_dens_b + log_crop_share_b,
#                   data = subset(muni, at_risk_broad), method = "nearest")
#      bal.tab(m)