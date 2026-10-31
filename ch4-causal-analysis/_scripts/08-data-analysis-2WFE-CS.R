# Purpose: stratified Callaway-Sant'Anna estimation of the priority-list effect by
#          baseline Bolsa Família coverage (the chapter's moderation test)
# Inputs:  _data/final/panel_amz_2000_2020_final.csv (script 06; crosswalk names
#          since phase 2R, D-2026-10-30-a)
# Outputs: output/tables/tab-att-strata.csv, output/figures/fig-event-study-strata.png
# Status:  legacy script (August 2026) run as-is for the descriptive sprint;
#          restyle deferred. Phase 2R, session 2 (2026-10-30): column names
#          migrated through _data/aux/crosswalk.csv; the script selects the
#          chapter's sample, the 502 biome municipalities, right after the
#          read (rebuild/tasks.md 1.12, D-2026-10-30-g). Outputs not
#          regenerated before stage 10.

library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(writexl)
library(ggplot2)

# ============================================================================
# moderation-callaway-santanna.R
# Chapter 3 spine. Does the blocklist (PPCDAm priority-municipality list) effect
# on deforestation depend on baseline social-protection provision?
#
# Estimand. ATT of first blocklist entry on the within-municipality normalized
# deforestation increment, estimated SEPARATELY for municipalities with high vs
# low baseline social-protection coverage. The moderation test is the difference
# between the two ATTs. This is the empirical form of H1, H4, and Mechanism 3.
#
# Estimator. Callaway & Sant'Anna (2021) staggered DiD, package `did`.
# Treatment is the ONSET of listing: a municipality is treated from its first
# entry year onward. Inside 2004-2012 exits are negligible (the annual count
# goes 47 -> 45 at the end), so the onset definition is clean. The reversibility
# robustness (de Chaisemartin & D'Haultfoeuille) belongs in a separate script.
#
# Why this moderator. Raw BF per capita correlates with baseline deforestation
# at about -0.28, so it tangles the moderator with treatment selection and
# yields unbalanced strata. The coverage ratio (families served / quota)
# correlates at about 0.02, splits the treated group 28/23, and balances the
# strata on structure. It also measures provision relative to need, which is the
# theoretical construct, rather than transfer volume, which tracks poverty.
#
# Input: _data/final/panel_amz_2000_2020_final.csv (phase 1 panel)
# ============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

library(did)

# ---- Parameters. Change these to run the robustness grid. -------------------
WINDOW_START <- 2002L            # earlier start = more pre-periods for the PT test
WINDOW_END   <- 2012L            # 2010 near-absorbing | 2012 full decline | 2020 extension
SAMPLE_FLAG  <- "smp_atrisk_narrow_dum" # "smp_atrisk_narrow_dum" | "smp_atrisk_broad_dum" | smp_avail_dum 
MODERATOR    <- "bf_cover"       # "bf_cover" (primary) | "bf_resid" | "formal"
SPLIT        <- "median"         # "median" | "tercile" (top vs bottom third)
CONTROL_GRP  <- "notyettreated"   # "nevertreated" | "notyettreated"
BASE_YEARS   <- 2002:2007        # pre-blocklist baseline for the moderator
REGION_FLAG  <- "geo_biome_amazon_dum"  # the chapter's sample (D-2026-10-30-g);
                                        # "geo_legal_amazon_dum": robustness
N_MUNIC_EXPECTED <- 502L

# ---- Load -------------------------------------------------------------------
# idname must be numeric for `did`. geocode is already integer.
df <- read_csv(file.path("_data", "final", "panel_amz_2000_2020_final.csv"),
               col_types = cols(geocode = col_integer(), year = col_integer())) %>%
  filter(.data[[REGION_FLAG]] == 1L)
stopifnot(n_distinct(df$geocode) == N_MUNIC_EXPECTED)

#glimpse(df)

# ---- Treatment cohort: first year on the priority list ----------------------
# enf_ppcdam_listed_dum is the annual (reversible) indicator. g is the first-entry year,
# 0 = never listed. Entries after WINDOW_END are set to 0 so that municipalities
# listed only later act as legitimate not-yet-listed controls inside the window.
cohort <- df %>%
  group_by(geocode) %>%
  summarise(g_first = { y <- year[enf_ppcdam_listed_dum == 1]
  if (length(y)) min(y) else 0L },
  .groups = "drop") %>%
  mutate(g = if_else(g_first > 0 & g_first <= WINDOW_END, g_first, 0L))

#cohort
#print(df, n = 300)

# ---- Baseline moderator and structural covariates (pre-treatment means) -----
baseline <- df %>%
  filter(year %in% BASE_YEARS) %>%
  group_by(geocode) %>%
  summarise(
    bf_pc     = mean(sop_bf_transf_pc_brl24, na.rm = TRUE),
    bf_cover  = mean(sop_bf_fam_quota_ratio,  na.rm = TRUE),
    formal    = mean(lab_emp_formal_pia_rate,       na.rm = TRUE),
    gdp0      = mean(ecn_gdp_pc_brl24_log,  na.rm = TRUE),
    informal0 = mean(lab_informal_share,       na.rm = TRUE),
    agri0     = mean(lab_emp_formal_agri_share,   na.rm = TRUE),
    defor0    = mean(env_defor_area_km2,   na.rm = TRUE),
    forest0   = mean(env_forest_area_km2,          na.rm = TRUE),
    .groups   = "drop"
  )


#baseline

# Residualized social protection: BF per capita net of baseline structure.
# Used only when MODERATOR == "bf_resid". Strips poverty and forest stock out of
# transfer volume so the moderator is closer to provision effort.
res_fit <- lm(bf_pc ~ gdp0 + informal0 + agri0 + log(forest0) + log1p(defor0),
              data = baseline)
baseline$bf_resid <- baseline$bf_pc - predict(res_fit, baseline)

#res_fit

# ---- Build the estimation sample --------------------------------------------
keep_ids <- df %>% filter(.data[[SAMPLE_FLAG]]) %>% distinct(geocode)
samp <- df %>%
  semi_join(keep_ids, by = "geocode") %>%
  left_join(select(cohort, geocode, g), by = "geocode") %>%
  left_join(baseline, by = "geocode") %>%
  filter(year >= WINDOW_START, year <= WINDOW_END)


# ---- High/low split of the moderator on the municipality cross-section -------
# Split is computed once per municipality (not on the stacked panel) so years do
# not weight the cut. Never-listed municipalities fall into a stratum by their
# own baseline value, so each stratum holds its own treated and control units.
xsec <- samp %>% distinct(geocode, .keep_all = TRUE)
mvec <- xsec[[MODERATOR]]
if (SPLIT == "median") {
  thr <- median(mvec, na.rm = TRUE)
  xsec <- xsec %>% mutate(stratum = if_else(.data[[MODERATOR]] >= thr, "high", "low"))
} else {  # top vs bottom tercile, middle third dropped for a sharper contrast
  q <- quantile(mvec, c(1/3, 2/3), na.rm = TRUE)
  xsec <- xsec %>% mutate(stratum = case_when(
    .data[[MODERATOR]] >= q[2] ~ "high",
    .data[[MODERATOR]] <  q[1] ~ "low",
    TRUE ~ NA_character_))
}

#glimpse(xsec)
samp <- samp %>% left_join(select(xsec, geocode, stratum), by = "geocode")
#glimpse(samp)
# ============================================================================
# SETUP EDA
# ============================================================================

# 1) Balance of the two strata. This is the confounding check. If high and low
#    coverage municipalities look alike on structure, the moderation contrast is
#    close to "effect of social protection holding structure fixed". With the
#    coverage ratio they do line up; print it for the appendix.
balance_tbl <- xsec %>%
  filter(!is.na(stratum)) %>%
  group_by(stratum) %>%
  summarise(n = n(), treated = sum(g > 0),
            across(c(bf_cover, formal, gdp0, informal0, agri0, defor0, forest0),
                   ~ round(mean(.x, na.rm = TRUE), 2)),
            .groups = "drop")
cat("\n--- Stratum balance (setup EDA) ---\n"); print(balance_tbl)

# 2) Raw descriptive trajectory by event time and stratum, before any modelling.
#    Shows the visual story the model will then test with controls and inference.
raw_es <- samp %>%
  mutate(etime = if_else(g > 0, year - g, NA_real_),
         grp = case_when(g == 0 ~ "never listed",
                         stratum == "high" ~ "listed, high coverage",
                         stratum == "low"  ~ "listed, low coverage")) %>%
  filter(g == 0 | between(etime, -4, 6)) %>%
  group_by(grp, etime) %>%
  summarise(y = mean(env_defor_area_z_bl0007, na.rm = TRUE), .groups = "drop")

p_raw <- raw_es %>% filter(!is.na(etime)) %>%
  ggplot(aes(etime, y, color = grp)) +
  geom_hline(yintercept = 0, linewidth = .3, color = "grey60") +
  geom_vline(xintercept = -0.5, linetype = 2, linewidth = .3, color = "grey60") +
  geom_line(linewidth = .8) + geom_point(size = 1.6) +
  labs(x = "Years since blocklist entry",
       y = "Mean normalized deforestation (raw)",
       color = NULL,
       title = "Raw trajectories around blocklist entry, by baseline coverage") +
  theme_minimal(base_size = 12)

#ggsave("fig_raw_event_by_stratum.png", p_raw, width = 8, height = 5, dpi = 200)

p_raw
# ============================================================================
# CALLAWAY-SANT'ANNA ESTIMATION, PER STRATUM
# ============================================================================
# Each stratum holds its own treated and never-treated municipalities, so the
# two estimates use disjoint units and are statistically independent. xformla
# conditions the parallel-trends assumption on baseline structure. If a thin
# (g,t) cell makes the doubly-robust step unstable, drop xformla or switch
# est_method to "reg".
run_cs <- function(d) {
  att_gt(yname   = "env_defor_area_z_bl0007",
         tname   = "year",
         idname  = "geocode",
         gname   = "g",
         xformla = ~ gdp0 + informal0 + agri0,
         data    = d,
         control_group = CONTROL_GRP,
         est_method    = "dr",
         base_period   = "universal",
         clustervars   = "geocode",
         allow_unbalanced_panel = FALSE,
         bstrap = TRUE, cband = TRUE)
}

set.seed(2026)   # bootstrap SEs reproducible across runs (sprint 2026-10-14)
cs_high <- run_cs(filter(samp, stratum == "high"))
cs_low  <- run_cs(filter(samp, stratum == "low"))

# Overall ATT per stratum (average over cohorts and post-treatment periods)
att_high <- aggte(cs_high, type = "simple", na.rm = TRUE)
att_low  <- aggte(cs_low,  type = "simple", na.rm = TRUE)

# Dynamic (event-study) per stratum, for pre-trend inspection and the plot
dyn_high <- aggte(cs_high, type = "dynamic", na.rm = TRUE)
dyn_low  <- aggte(cs_low,  type = "dynamic", na.rm = TRUE)

# ---- Moderation test: difference of the two overall ATTs --------------------
# Strata are disjoint sets of municipalities, so the two ATTs are independent and
# the variance of the difference is the sum of the variances.
diff_att <- att_high$overall.att - att_low$overall.att
se_diff  <- sqrt(att_high$overall.se^2 + att_low$overall.se^2)
zstat    <- diff_att / se_diff
cat(sprintf(
  "\n--- Moderation result ---\nATT high coverage = %.3f (se %.3f)\nATT low coverage  = %.3f (se %.3f)\nDifference (high - low) = %.3f (se %.3f), z = %.2f, p = %.3f\n",
  att_high$overall.att, att_high$overall.se,
  att_low$overall.att,  att_low$overall.se,
  diff_att, se_diff, zstat, 2 * pnorm(-abs(zstat))))

# ---- Event-study plot, both strata together --------------------------------
es <- bind_rows(
  tibble(etime = dyn_high$egt, att = dyn_high$att.egt, se = dyn_high$se.egt,
         stratum = "high coverage"),
  tibble(etime = dyn_low$egt,  att = dyn_low$att.egt,  se = dyn_low$se.egt,
         stratum = "low coverage"))

p_es <- ggplot(es, aes(etime, att, color = stratum, fill = stratum)) +
  geom_hline(yintercept = 0, linewidth = .3, color = "grey60") +
  geom_vline(xintercept = -0.5, linetype = 2, linewidth = .3, color = "grey60") +
  geom_ribbon(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
              alpha = .15, color = NA) +
  geom_line(linewidth = .8) + geom_point(size = 1.6) +
  labs(x = "Years since blocklist entry",
       y = "ATT on normalized deforestation",
       color = NULL, fill = NULL,
       title = "Blocklist effect by baseline social-protection coverage") +
  theme_minimal(base_size = 12)

ggsave("output/figures/fig-event-study-strata.png", p_es,
       width = 8, height = 5, dpi = 200)

# ---- Chapter table: stratum ATTs and the moderation difference -------------
n_treated <- xsec %>% filter(g > 0, !is.na(stratum)) %>% count(stratum)
att_tab <- tibble(
  quantity  = c("ATT, high coverage", "ATT, low coverage",
                "Moderation difference (high - low)"),
  estimate  = c(att_high$overall.att, att_low$overall.att, diff_att),
  se        = c(att_high$overall.se,  att_low$overall.se,  se_diff),
  n_treated = c(n_treated$n[n_treated$stratum == "high"],
                n_treated$n[n_treated$stratum == "low"], NA_integer_)
) %>%
  mutate(z = estimate / se,
         p = 2 * pnorm(-abs(z)),
         ci_low  = estimate - 1.96 * se,
         ci_high = estimate + 1.96 * se)
write_csv(att_tab, "output/tables/tab-att-strata.csv")
print(att_tab)

p_es
# ============================================================================
# NEXT, NOT IN THIS SCRIPT
#  - Reversibility: de Chaisemartin & D'Haultfoeuille (package DIDmultiplegtDYN)
#    on the same strata, using enf_ppcdam_listed_dum as the on/off treatment. Compare to
#    the onset-based CS above. Matters most for the 2004-2020 window.
#  - Spatial leakage (SUTVA): Conley standard errors and a border analysis, or a
#    spatial-lag control, to check displacement of clearing to neighbours.
#  - Moderation interpretation: rerun the split within GDP terciles, or use
#    MODERATOR = "bf_resid", to confirm the coverage gradient is not structure.
# ============================================================================

