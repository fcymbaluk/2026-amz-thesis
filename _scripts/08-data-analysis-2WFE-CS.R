# Data analysis - Two Way Fixed Effects - Callaway & Sant'Anna (2021) staggered DiD

rm(list = ls())
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(writexl)
library(ggplot2)



df4 <- read_csv("_data/final_dataset_4.csv")

glimpse(df4)

df6 <- read_csv("_data/final_dataset_6.csv")

glimpse(df6)

glimpse(df) 

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
# Input: _data/final_dataset_6.csv
# ============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

install.packages("did")
library(did)
version

rm(list = ls())

# ---- Parameters. Change these to run the robustness grid. -------------------
WINDOW_START <- 2002L            # earlier start = more pre-periods for the PT test
WINDOW_END   <- 2012L            # 2010 near-absorbing | 2012 full decline | 2020 extension
SAMPLE_FLAG  <- "at_risk_narrow" # "at_risk_narrow" | "at_risk_broad" | avail_ok 
MODERATOR    <- "bf_cover"       # "bf_cover" (primary) | "bf_resid" | "formal"
SPLIT        <- "median"         # "median" | "tercile" (top vs bottom third)
CONTROL_GRP  <- "notyettreated"   # "nevertreated" | "notyettreated"
BASE_YEARS   <- 2002:2007        # pre-blocklist baseline for the moderator

# ---- Load -------------------------------------------------------------------
# idname must be numeric for `did`. geocode is already integer.
df <- read_csv(file.path("_data", "final_dataset_6.csv"),
               col_types = cols(geocode = col_integer(), year = col_integer()))

#glimpse(df)

# ---- Treatment cohort: first year on the priority list ----------------------
# ppcdam_list is the annual (reversible) indicator. g is the first-entry year,
# 0 = never listed. Entries after WINDOW_END are set to 0 so that municipalities
# listed only later act as legitimate not-yet-listed controls inside the window.
cohort <- df %>%
  group_by(geocode) %>%
  summarise(g_first = { y <- year[ppcdam_list == 1]
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
    bf_pc     = mean(bf_transfers_pc_brl_2024, na.rm = TRUE),
    bf_cover  = mean(bf_families_quota_ratio,  na.rm = TRUE),
    formal    = mean(emp_pia_rate_total,       na.rm = TRUE),
    gdp0      = mean(gdp_per_capita_2024_log,  na.rm = TRUE),
    informal0 = mean(informal,                 na.rm = TRUE),
    agri0     = mean(emp_share_agri_workers,   na.rm = TRUE),
    defor0    = mean(deforestation_area_km2,   na.rm = TRUE),
    forest0   = mean(forest_area_km2,          na.rm = TRUE),
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
  summarise(y = mean(defor_norm_ref0007, na.rm = TRUE), .groups = "drop")

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
  att_gt(yname   = "defor_norm_ref0007",
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

# ggsave("fig_cs_event_by_stratum.png", p_es, width = 8, height = 5, dpi = 200)

p_es
# ============================================================================
# NEXT, NOT IN THIS SCRIPT
#  - Reversibility: de Chaisemartin & D'Haultfoeuille (package DIDmultiplegtDYN)
#    on the same strata, using ppcdam_list as the on/off treatment. Compare to
#    the onset-based CS above. Matters most for the 2004-2020 window.
#  - Spatial leakage (SUTVA): Conley standard errors and a border analysis, or a
#    spatial-lag control, to check displacement of clearing to neighbours.
#  - Moderation interpretation: rerun the split within GDP terciles, or use
#    MODERATOR = "bf_resid", to confirm the coverage gradient is not structure.
# ============================================================================

