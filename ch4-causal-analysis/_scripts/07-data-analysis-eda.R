# Data analysis and visualization of the dataset - Exploratory Data Analysis (EDA)

rm(list = ls())
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(writexl)
library(ggplot2)

# =============================================================================
# Block A: context and motivation figures (A1, A2)
# Block D1: within/between variance decomposition
# Plus: type verification and missingness-by-year map
#
# Input : final_dataset_6.csv
# Output: output/figures/*.png, output/tables/*.csv
#
# Run after scripts 01–06. Assumes the type fix (variables as doubles).
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- paths -------------------------------------------------------------------
data_path <- "_data/final_dataset_6.csv"   # adjust if needed
#dir.create("_output/_figures", recursive = TRUE, showWarnings = FALSE)
#dir.create("_output/_tables",  recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path)
df
glimpse(df)



df %>%
  count(ppcdam_list)

table(df$ppcdam_list)

df %>%
  filter(ppcdam_list == 0) %>%
  summarise(n_municipalities = n_distinct(geocode))

df %>% 
  filter(
    at_risk_narrow == TRUE,
    ppcdam_list == 0) %>% 
  summarise(n_municipalities = n_distinct(geocode))

df %>%
  filter(ppcdam_list == 1) %>%
  count(year)

df <- df %>%
  mutate(
    geocode = as.character(geocode)
    )

# =============================================================================
# 0. Type verification and missingness map
# -----------------------------------------------------------------------------
# Why: two known gaps should be visible, not buried. (i) BF variables are
# entirely missing in 2020, which truncates any BF-involved analysis at 2019.
# (ii) GDP is missing in 2000–2001. The heatmap documents the temporal
# structure of the data honestly (census-year labor data, BF from 2004, etc.)
# and preempts referee suspicion about silent sample loss.
# =============================================================================

key_vars <- c("defor_norm_ref0007", "deforestation_area_km2",
              "bf_families_quota_ratio", "bf_transfers_pc_brl_2024",
              "emp_pia_rate_total", "emp_pia_rate_agric",
              "emp_share_low_workers",
              "price_index_crop", "price_index_cattle",
              "gdp_per_capita_2024", "elec_enc", "elec_winner_share")

# Confirm everything is numeric; stop early if the type fix regressed.
stopifnot(all(sapply(df[key_vars], is.numeric)))

miss_by_year <- df |>
  group_by(year) |>
  summarise(across(all_of(key_vars), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(-year, names_to = "variable", values_to = "pct_na")

p_miss <- ggplot(miss_by_year,
                 aes(x = year, y = variable, fill = pct_na)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "grey95", high = "firebrick",
                      name = "% missing") +
  scale_x_continuous(breaks = seq(2000, 2020, 4)) +
  labs(title = "Data availability by variable and year",
       subtitle = "Bolsa Família variables are unavailable in 2020; GDP begins in 2002",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11)

p_miss
# #ggsave("output/figures/fig00_missingness_map.png", p_miss,
#        width = 9, height = 4.5, dpi = 300)

# =============================================================================
# A1. Annotated policy timeline over aggregate deforestation
# -----------------------------------------------------------------------------
# Why: the 2004–2012 decline coincides with PPCDAm phases, the priority list,
# credit conditionality, supply-chain agreements, BF expansion, and a commodity
# cycle. Rather than hiding this, the figure states the identification problem
# explicitly: many policies moved at once, which is precisely why the chapter
# needs municipal-level staggered variation. Aggregate co-movement motivates,
# it does not identify.
# =============================================================================

agg_defor <- df |>
  group_by(year) |>
  summarise(defor_total_km2 = sum(deforestation_area_km2, na.rm = TRUE))

# Compact event set. Deliberately not exhaustive: the point is legibility.
events <- tribble(
  ~year, ~label,                              ~side,
  2003,  "Bolsa Família created",             "social",
  2004,  "PPCDAm Phase I",                    "environmental",
  2006,  "Soy Moratorium",                    "environmental",
  2007,  "MW valorization policy",            "social",
  2008,  "Priority list (1st cohort)\nCredit Res. 3,545", "environmental",
  2009,  "Cattle TACs; PPCDAm II",            "environmental",
  2012,  "Forest Code revision",              "environmental",
  2019,  "Enforcement weakening",             "environmental"
)

y_max <- max(agg_defor$defor_total_km2)

p_a1 <- ggplot(agg_defor, aes(x = year, y = defor_total_km2)) +
  geom_line(linewidth = 0.9, color = "grey20") +
  geom_point(size = 1.6, color = "grey20") +
  geom_vline(data = events, aes(xintercept = year, color = side),
             linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
  geom_text(data = events,
            aes(x = year, y = y_max * 1.02, label = label, color = side),
            angle = 90, hjust = 0, vjust = -0.3, size = 2.7,
            lineheight = 0.85, show.legend = FALSE) +
  scale_color_manual(values = c(environmental = "#1b7837",
                                social = "#762a83"),
                     name = NULL,
                     labels = c("Environmental policy", "Social policy")) +
  scale_y_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0.02, 0.45))) +
  scale_x_continuous(breaks = seq(2000, 2020, 2)) +
  labs(title = "Deforestation in the Legal Amazon and the policy timeline, 2000–2020",
       subtitle = "Sample total of annual PRODES increments (502 municipalities). Dashed lines mark policy events.",
       x = NULL, y = expression("Deforestation (km"^2*")"),
       caption = "The clustering of interventions in 2004–2012 is the identification problem this chapter confronts.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

p_a1
# #ggsave("output/figures/fig01_policy_timeline.png", p_a1,
#        width = 10, height = 6, dpi = 300)

# =============================================================================
# A2. Multi-series trends, small multiples in natural units
# -----------------------------------------------------------------------------
# Why: descriptive co-trajectories of environmental, social, and economic
# series. A common-index version (2004 = 100) was rejected: BF coverage in
# 2004 reflects rollout (mean ratio 0.33 vs ~1.1 in steady state), so any
# common base year turns the rollout into a fake 3.5x expansion. Small
# multiples in natural units avoid the base-year artifact and the dual-axis
# problem at once. The shared shaded band aligns the eye across panels.
# Notes:
#  - BF enters as the quota ratio (coverage relative to administrative need),
#    not transfer volume, which tracks poverty and prices.
#  - Price indices are plotted as stored; verify the 2000 = 1 normalization
#    from script 03 before these enter any regression (stored scales are
#    inconsistent between crop and cattle).
# =============================================================================

series <- df |>
  group_by(year) |>
  summarise(
    `Deforestation (km2, total)`     = sum(deforestation_area_km2, na.rm = TRUE),
    `BF coverage (families/quota)`   = mean(bf_families_quota_ratio, na.rm = TRUE),
    `Formal employment (RAIS/PIA)`   = mean(emp_pia_rate_total, na.rm = TRUE),
    `Crop price exposure index`      = mean(price_index_crop, na.rm = TRUE),
    `Cattle price exposure index`    = mean(price_index_cattle, na.rm = TRUE)
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  mutate(series = factor(series, levels = c(
    "Deforestation (km2, total)", "BF coverage (families/quota)",
    "Formal employment (RAIS/PIA)", "Crop price exposure index",
    "Cattle price exposure index")))

p_a2 <- ggplot(series, aes(x = year, y = value)) +
  annotate("rect", xmin = 2004, xmax = 2012, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  geom_line(linewidth = 0.8, color = "grey15") +
  facet_wrap(~ series, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Environmental, social, and economic trajectories, 2000–2020",
       subtitle = "Sample means/totals across 502 municipalities. Shaded band: the 2004–2012 deforestation decline.",
       x = NULL, y = NULL,
       caption = paste0("BF coverage before 2006 reflects program rollout and is ",
                        "unavailable in 2020. Formal employment: RAIS formal jobs / ",
                        "working-age population (PIA, interpolated between censuses).")) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0),
        strip.text = element_text(face = "bold", size = 9))

p_a2

# #ggsave("output/figures/fig02_series_trends.png", p_a2,
#        width = 10, height = 6.5, dpi = 300)

# =============================================================================
# D1. Within/between variance decomposition
# -----------------------------------------------------------------------------
# Why: estimator-agnostic diagnostic. FE/DiD designs identify from within
# variation only; the moderation design uses the moderator's between
# variation across well-identified stratum ATTs. This table shows, before
# any model, which variables carry which kind of variation.
# Decomposition: total SS = between SS (municipality means around the grand
# mean) + within SS (deviations around own municipality mean).
# =============================================================================

decompose_wb <- function(x, id) {
  d  <- data.frame(x = x, id = id)
  d  <- d[!is.na(d$x), ]
  gm <- mean(d$x)
  mu <- ave(d$x, d$id)                # municipality means
  between <- sum((mu - gm)^2)
  within  <- sum((d$x - mu)^2)
  tibble(between_pct = 100 * between / (between + within),
         within_pct  = 100 * within  / (between + within),
         n_obs = nrow(d),
         n_munic = length(unique(d$id)))
}

decomp_vars <- c("bf_families_quota_ratio", "bf_transfers_pc_brl_2024",
                 "emp_pia_rate_total", "emp_pia_rate_agric",
                 "emp_share_low_workers",
                 "defor_norm_ref0007", "deforestation_area_km2",
                 "deforestation_forest_rate",
                 "elec_enc", "gdp_per_capita_2024",
                 "price_index_crop", "price_index_cattle", "cattle_per_km2")

wb_table <- bind_rows(
  lapply(decomp_vars,
         \(v) decompose_wb(df[[v]], df$geocode) |> mutate(variable = v))
) |>
  select(variable, between_pct, within_pct, n_obs, n_munic) |>
  mutate(across(ends_with("_pct"), \(x) round(x, 1)))
# 
# #write.csv(wb_table, "output/tables/tab_d1_variance_decomposition.csv",
#           row.names = FALSE)
# print(wb_table, n = Inf)

# Interpretation notes (for the chapter text, not the console):
# - bf_families_quota_ratio ~71% between: stable municipal characteristic.
#   Appropriate as a cross-sectional moderator; weak identifying variation
#   for FE direct-effect specifications.
# - emp_pia_rate_agric ~84% between: structural; use baseline values as a
#   predetermined moderator rather than a time-varying regressor.
# - defor_norm_ref0007 ~81% within by construction (z-score on own
#   2000–2007 baseline): suitable outcome for within-based designs.
# - price indices ~93% between: Bartik-type structure; levels reflect fixed
#   crop mix, identification would rest on the small differential-trend
#   component.
# - emp_share_low_workers ~80% within: wage brackets are defined using fixed
#   real thresholds (2024 minimum wage after IPCA deflation), so movements
#   across brackets reflect changes in the real wage distribution of formal
#   employment rather than annual reclassification from changing nominal MW
#   thresholds. Variation may arise from real wage growth, worker turnover,
#   and changes in the occupational or sectoral composition of employment.
#   Treat as a labor-market composition measure, not as a direct MW-policy
#   outcome.


# =============================================================================
# 07b-eda-series.R — Aggregate time series: social policy and population stats
# Extends Block A of 07-eda.R. Run after 07-eda.R (same working directory).
#
# Aggregation rules used throughout:
#   - monetary/area/count TOTALS  -> sum across municipalities
#   - rates, ratios, shares       -> unweighted mean across municipalities
#     (the "average municipality", matching the unit of analysis in the
#     causal design; a population-weighted mean would tell the "average
#     person" story and can be added as a robustness display)
# Known data structure handled below:
#   - BF variables exist 2004–2019 only (2020 gap pending upstream fix)
#   - GDP exists 2002–2020
#   - `informal` is linear interpolation between the 2000/2010/2022 censuses:
#     only census points are displayed as data
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

data_path <- "final_dataset_6.csv"
#dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path)

theme_eda <- theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0),
        legend.position = "bottom")

# =============================================================================
# Fig 03 — Formal employment by wage bracket (requests 1.1, 1.2)
# -----------------------------------------------------------------------------
# Brackets are fixed REAL thresholds (2024 MW, IPCA-deflated wages), so movement 
# across brackets reflects changes in the real wage distribution of formal 
# employment rather than mechanical reclassification from annual MW updates.
# Attribution to MW policy specifically still requires the exposure design;
# these are descriptives of the wage distribution's base.
# Panel A: rates over PIA (do not sum to 1; they sum to emp_pia_rate_total,
#          so lines). Panel B: shares of formal workers (sum to 1, so a
#          stacked area is the natural geometry).
# =============================================================================

rates_long <- df |>
  group_by(year) |>
  summarise(across(c(emp_pia_rate_low, emp_pia_rate_mid, emp_pia_rate_high),
                   \(x) mean(x, na.rm = TRUE))) |>
  pivot_longer(-year, names_to = "bracket", values_to = "value") |>
  mutate(bracket = factor(bracket,
                          levels = c("emp_pia_rate_low", "emp_pia_rate_mid", "emp_pia_rate_high"),
                          labels = c("Low (≤ 1 MW-2024)", "Mid (1–4 MW-2024)", "High (> 4 MW-2024)")))

p3a <- ggplot(rates_long, aes(year, value, color = bracket)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("#d95f02", "#7570b3", "#1b9e77"), name = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(subtitle = "A. Formal jobs / working-age population, by wage bracket",
       x = NULL, y = "Share of PIA") +
  theme_eda

shares_long <- df |>
  group_by(year) |>
  summarise(across(c(emp_share_low_workers, emp_share_mid_workers,
                     emp_share_high_workers), \(x) mean(x, na.rm = TRUE))) |>
  pivot_longer(-year, names_to = "bracket", values_to = "value") |>
  mutate(bracket = factor(bracket,
                          levels = c("emp_share_high_workers", "emp_share_mid_workers",
                                     "emp_share_low_workers"),
                          labels = c("High (> 4 MW-2024)", "Mid (1–4 MW-2024)", "Low (≤ 1 MW-2024)")))

p3b <- ggplot(shares_long, aes(year, value, fill = bracket)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3", "#d95f02"), name = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(subtitle = "B. Composition of formal workers, by wage bracket",
       x = NULL, y = "Share of formal workers") +
  theme_eda

p3a
p3b

# 2k if available; otherwise save separately
# if (requireNamespace("patchwork", quietly = TRUE)) {
#   library(patchwork)
#   p3 <- p3a / p3b +
#     plot_annotation(
#       title = "Formal employment and the real wage distribution, 2000–2020",
#       caption = paste0("Unweighted municipal means. Wage brackets are fixed real ",
#                        "thresholds: multiples of the 2024 minimum wage, wages ",
#                        "IPCA-deflated to 2024 BRL. Source: RAIS / IBGE Census (PIA)."))
#   ggsave("output/figures/fig03_wage_brackets.png", p3,
#          width = 9, height = 7.5, dpi = 300)
# } else {
#   ggsave("output/figures/fig03a_pia_rates.png",  p3a, width = 9, height = 4, dpi = 300)
#   ggsave("output/figures/fig03b_emp_shares.png", p3b, width = 9, height = 4, dpi = 300)
# }

# =============================================================================
# Fig 04 — Total vs agricultural formal employment (requests 2.1, 2.2)
# =============================================================================

agri <- df |>
  group_by(year) |>
  summarise(
    `Formal / PIA: total`          = mean(emp_pia_rate_total, na.rm = TRUE),
    `Formal / PIA: agricultural`   = mean(emp_pia_rate_agric, na.rm = TRUE),
    `Agricultural share of formal` = mean(emp_share_agri_workers, na.rm = TRUE)
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  mutate(panel = ifelse(grepl("share", series),
                        "B. Agricultural share of formal workers",
                        "A. Formal employment rates over PIA"))

p4 <- ggplot(agri, aes(year, value, color = series)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(title = "Total and agricultural formal employment, 2000–2020",
       x = NULL, y = NULL,
       caption = paste0("Unweighted municipal means. Agricultural occupations: ",
                        "unified CBO codes beginning with 6 (RAIS).")) +
  theme_eda

p4
# ggsave("output/figures/fig04_formal_agric.png", p4,
#        width = 9, height = 6.5, dpi = 300)

# =============================================================================
# Fig 05 — Bolsa Família panel (requests 3.1–3.5)
# -----------------------------------------------------------------------------
# Restricted to 2004–2019: values are structurally missing before 2004
# (program rollout) and in 2020 (upstream gap under investigation).
# sum() with na.rm = TRUE would silently return 0 for all-NA years,
# so the filter is not cosmetic.
# =============================================================================

bf <- df |>
  filter(year >= 2004, year <= 2019) |>
  group_by(year) |>
  summarise(
    `Total transfers (billion BRL-2024)` =
      sum(bf_transfers_total_brl_2024, na.rm = TRUE) / 1e9,
    `Avg transfer per family (BRL-2024)` =
      mean(bf_transfers_avg_brl_2024, na.rm = TRUE),
    `Coverage: families / quota` =
      mean(bf_families_quota_ratio, na.rm = TRUE),
    `Transfers per quota family (BRL-2024)` =
      mean(bf_transfers_quota_brl_2024, na.rm = TRUE),
    `Transfers per capita (BRL-2024)` =
      mean(bf_transfers_pc_brl_2024, na.rm = TRUE)
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  mutate(series = factor(series, levels = c(
    "Total transfers (billion BRL-2024)", "Coverage: families / quota",
    "Avg transfer per family (BRL-2024)", "Transfers per quota family (BRL-2024)",
    "Transfers per capita (BRL-2024)")))

p5 <- ggplot(bf, aes(year, value)) +
  geom_line(linewidth = 0.8, color = "#762a83") +
  facet_wrap(~ series, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(2004, 2019, 5)) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Bolsa Família in the sample, 2004–2019",
       x = NULL, y = NULL,
       caption = paste0("Total: sample sum. Other panels: unweighted municipal ",
                        "means. All values IPCA-deflated to 2024 BRL. ",
                        "2004–2005 reflect program rollout; 2020 unavailable.")) +
  theme_eda

p5

# ggsave("output/figures/fig05_bolsa_familia.png", p5,
#        width = 9, height = 7, dpi = 300)

# =============================================================================
# Fig 06 — Informality and GDP (requests 4.1–4.3)
# -----------------------------------------------------------------------------
# `informal` contains only census information (2000, 2010, 2022 anchor);
# intermediate years are linear interpolation. Displaying the interpolated
# line as data would be misleading, so census points are drawn as points
# and the interpolation as a dashed segment, labeled in the caption.
# GDP available from 2002.
# =============================================================================

informal_s <- df |>
  group_by(year) |>
  summarise(value = mean(informal, na.rm = TRUE)) |>
  mutate(census = year %in% c(2000, 2010, 2020))
# 2020 shown as the last interpolated point toward the 2022 census anchor

p6a <- ggplot(informal_s, aes(year, value)) +
  geom_line(linetype = "dashed", color = "grey50", linewidth = 0.6) +
  geom_point(data = subset(informal_s, year %in% c(2000, 2010)),
             size = 3, color = "#d95f02") +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(subtitle = "A. Labor informality (% of workers)",
       x = NULL, y = "%") +
  theme_eda

gdp_s <- df |>
  filter(year >= 2002) |>
  group_by(year) |>
  summarise(
    `B. GDP per capita (BRL-2024)`   = mean(gdp_per_capita_2024, na.rm = TRUE),
    `C. Total GDP (billion BRL-2024)`= sum(gdp_brl_2024, na.rm = TRUE) / 1e9
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value")

p6b <- ggplot(gdp_s, aes(year, value)) +
  geom_line(linewidth = 0.8, color = "grey15") +
  facet_wrap(~ series, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(2002, 2020, 6)) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = NULL, y = NULL) +
  theme_eda

# if (requireNamespace("patchwork", quietly = TRUE)) {
#   library(patchwork)
#   p6 <- p6a / p6b +
#     plot_annotation(
#       title = "Informality and economic activity, 2000–2020",
#       caption = paste0("Informality: census measurements (2000, 2010) shown as ",
#                        "points; dashed line is linear interpolation (2010–2020 ",
#                        "segment anchored to the 2022 Census) and carries no ",
#                        "annual information. GDP: IBGE municipal series, ",
#                        "IPCA-deflated to 2024 BRL, available from 2002."))
#   ggsave("output/figures/fig06_informal_gdp.png", p6,
#          width = 9, height = 7, dpi = 300)
# } else {
#   ggsave("output/figures/fig06a_informal.png", p6a, width = 9, height = 3.5, dpi = 300)
#   ggsave("output/figures/fig06b_gdp.png",      p6b, width = 9, height = 3.5, dpi = 300)
# }

p6a
p6b

# =============================================================================
# Fig 07 — Agricultural expansion and prices (requests 5.1–5.5)
# -----------------------------------------------------------------------------
# Crop panel is a stacked area by crop rather than a single summed line:
# same total, more information (soy dominance, rice decline).
# CAUTION (pending confirmation): major Cerrado-MT soy municipalities
# (e.g., Sorriso) are outside the 502-municipality sample. Totals describe
# the sample, not Legal Amazon agriculture as a whole; the caption must say
# which sample definition applies.
# =============================================================================

crops <- df |>
  group_by(year) |>
  summarise(Soy    = sum(crop_soy_area_planted_km2,    na.rm = TRUE),
            Corn   = sum(crop_corn_area_planted_km2,   na.rm = TRUE),
            Rice   = sum(crop_rice_area_planted_km2,   na.rm = TRUE),
            Cotton = sum(crop_cotton_area_planted_km2, na.rm = TRUE)) |>
  pivot_longer(-year, names_to = "crop", values_to = "km2") |>
  mutate(crop = factor(crop, levels = c("Soy", "Corn", "Rice", "Cotton")))

p7a <- ggplot(crops, aes(year, km2, fill = crop)) +
  geom_area(alpha = 0.85) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(subtitle = expression("A. Planted area, four main crops (km"^2*", sample total)"),
       x = NULL, y = NULL) +
  theme_eda

cattle <- df |>
  group_by(year) |>
  summarise(`B. Cattle herd (million heads, total)` =
              sum(cattle_heads, na.rm = TRUE) / 1e6,
            `C. Cattle density (heads/km², mean)` =
              mean(cattle_per_km2, na.rm = TRUE)) |>
  pivot_longer(-year, names_to = "series", values_to = "value")

p7b <- ggplot(cattle, aes(year, value)) +
  geom_line(linewidth = 0.8, color = "grey15") +
  facet_wrap(~ series, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(x = NULL, y = NULL) +
  theme_eda

prices <- df |>
  group_by(year) |>
  summarise(`Crop price index (z)`   = mean(price_index_crop_z,   na.rm = TRUE),
            `Cattle price index (z)` = mean(price_index_cattle_z, na.rm = TRUE)) |>
  pivot_longer(-year, names_to = "series", values_to = "value")

p7c <- ggplot(prices, aes(year, value, color = series)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("#a6611a", "#018571"), name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(subtitle = "D. Standardized price exposure indices (sample mean)",
       x = NULL, y = "z-score") +
  theme_eda
# 
# if (requireNamespace("patchwork", quietly = TRUE)) {
#   library(patchwork)
#   p7 <- p7a / p7b / p7c +
#     plot_annotation(
#       title = "Agricultural expansion and price exposure, 2000–2020",
#       caption = paste0("PAM (crops), PPM (cattle), World Bank Pink Sheet (prices). ",
#                        "Totals describe the 502-municipality sample; major ",
#                        "Cerrado-MT soy municipalities lie outside it. Pre-2005 ",
#                        "cattle growth partly reflects early-period PPM ",
#                        "measurement noise."))
#   ggsave("output/figures/fig07_agriculture_prices.png", p7,
#          width = 9, height = 10, dpi = 300)
# } else {
#   ggsave("output/figures/fig07a_crops.png",  p7a, width = 9, height = 3.5, dpi = 300)
#   ggsave("output/figures/fig07b_cattle.png", p7b, width = 9, height = 3.5, dpi = 300)
#   ggsave("output/figures/fig07c_prices.png", p7c, width = 9, height = 3.5, dpi = 300)
# }

p7a
p7b
p7c

glimpse(df)
# =============================================================================
# End of aggregate series block. Next: Block B (cross-sectional structure).
# =============================================================================

# =============================================================================
# 07c-eda-combined-trends.R — Fig 08: six-series combined trajectory
# Append to / run after 07b-eda-series.R (same setup and theme).
#
# Series: formal mid-wage employment rate, BF average transfer per family,
# GDP per capita, planted area of the four main crops, cattle density,
# deforestation.
#
# Normalization: index 2004 = 100. 2004 is the first year in which all six
# series exist (BF starts 2004, GDP in 2002) and is the PPCDAm launch year,
# so the reading is "proportional change since enforcement began". Series
# with small 2004 bases (mid-wage rate, crop area) show steep proportional
# growth; that is substance, not artifact.
# Alternative (timing comparison, equal amplitudes): replace the index
# mutate with  value = as.numeric(scale(value))  per series and relabel the
# y-axis as "z-score" — see the commented line below.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
# requires ggrepel for end-of-line labels: install.packages("ggrepel")

# assumes df, theme_eda, output dirs from 07b; otherwise uncomment:
# df <- read.csv("final_dataset_6.csv")
# theme_eda <- theme_minimal(base_size = 11) +
#   theme(plot.caption = element_text(size = 7.5, hjust = 0))

combined <- df |>
  group_by(year) |>
  summarise(
    `Formal low-wage rate (mean)` = mean(emp_pia_rate_low, na.rm = TRUE),
    `Formal mid-wage rate (mean)` = mean(emp_pia_rate_mid, na.rm = TRUE),
    `BF avg transfer/family (mean)` = mean(bf_transfers_avg_brl_2024, na.rm = TRUE),
    `GDP per capita (mean)`       = mean(gdp_per_capita_2024, na.rm = TRUE),
    `Crop area, 4 crops (total)`  = sum(crop_soy_area_planted_km2, na.rm = TRUE) +
      sum(crop_corn_area_planted_km2, na.rm = TRUE) +
      sum(crop_rice_area_planted_km2, na.rm = TRUE) +
      sum(crop_cotton_area_planted_km2, na.rm = TRUE),
    `Cattle density (mean)`       = mean(cattle_per_km2, na.rm = TRUE),
    `Crop price index (mean)`     = mean(price_index_crop, na.rm = TRUE),
    `Cattle price index (mean)`   = mean(price_index_cattle, na.rm = TRUE),
    `Deforestation (total)`       = sum(deforestation_area_km2, na.rm = TRUE)
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  group_by(series) |>
  mutate(index = value / value[year == 2004] * 100) |>
  # z-score alternative: mutate(index = as.numeric(scale(value))) |>
  ungroup() |>
  filter(!is.na(index), !is.nan(index))

# direct labels at line ends (legend with eight series is hard to read).
# All labels are right-aligned at a common x (the panel's last year); series
# ending earlier (BF stops in 2019) get a dotted extension segment from
# their last observation to the label column, so endpoints stay honest
# while labels line up.
x_end <- max(combined$year)

label_df <- combined |>
  group_by(series) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(x_label = x_end)

# Price series use the raw indices, not the _z versions: z-scores cross zero
# (crop z = -0.11 in 2004), so a 2004 = 100 index is undefined for them.
# Indexing also neutralizes the raw indices' inconsistent stored scales,
# which still need fixing in script 03 before any regression use.
pal <- c(`Deforestation (total)`         = "grey15",
         `Formal low-wage rate (mean)`   = "#d95f02",
         `Formal mid-wage rate (mean)`   = "#1b9e77",
         `BF avg transfer/family (mean)` = "#762a83",
         `GDP per capita (mean)`         = "#7570b3",
         `Crop area, 4 crops (total)`    = "#e6ab02",
         `Cattle density (mean)`         = "#a6611a",
         `Crop price index (mean)`       = "#66a61e",
         `Cattle price index (mean)`     = "#e7298a")

p8 <- ggplot(combined, aes(year, index, color = series)) +
  annotate("rect", xmin = 2004, xmax = 2012, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.45) +
  geom_hline(yintercept = 100, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_line(data = subset(combined, series == "Deforestation (total)"),
            linewidth = 1.3) +   # emphasize the outcome
  geom_segment(data = subset(label_df, year < x_end),
               aes(x = year, xend = x_label, y = index, yend = index),
               linetype = "dotted", linewidth = 0.4, alpha = 0.7,
               show.legend = FALSE) +
  ggrepel::geom_text_repel(data = label_df,
                           aes(x = x_label, label = series), direction = "y",
                           hjust = 0, nudge_x = 0.4,
                           size = 2.9, segment.size = 0.2, segment.color = "grey70",
                           box.padding = 0.15, min.segment.length = 0,
                           show.legend = FALSE) +
  scale_color_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = seq(2000, 2020, 4),
                     limits = c(2000, 2028)) +  # room for end labels
  labs(title = "Deforestation against social and economic trajectories, 2000–2020",
       subtitle = "All series indexed to 2004 = 100. Shaded band: the 2004–2012 deforestation decline.",
       x = NULL, y = "Index (2004 = 100)",
       caption = paste0("Rates and per-capita values: unweighted municipal means; ",
                        "areas and deforestation: sample totals. BF series ends in 2019 ",
                        "(2020 unavailable); GDP begins in 2002. The figure is ",
                        "descriptive: agricultural expansion, income growth, welfare ",
                        "provision, and commodity prices all co-move with the ",
                        "deforestation decline, which is the identification problem, ",
                        "not its answer. Price indices: sample means of the ",
                        "exposure-weighted Pink Sheet series.")) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig08_combined_trends.png", p8,
#        width = 10.5, height = 6.5, dpi = 300)

p8


# =============================================================================
# 07d-eda-panel-trends.R — Fig 09: deforestation against grouped trajectories
# Four panels, deforestation repeated in each as the reference series.
#   A. Economic activity: GDP per capita, crop area, cattle density
#   B. Price exposure: crop and cattle price indices
#   C. Formal wages: mid- and low-bracket employment rates
#   D. Bolsa Família: average transfer per family
# Normalization: index 2004 = 100, as in fig08. Self-contained: only needs df.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# df <- read.csv("final_dataset_6.csv")   # uncomment if running standalone
# dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

idx_series <- df |>
  group_by(year) |>
  summarise(
    `Deforestation`         = sum(deforestation_area_km2, na.rm = TRUE),
    `GDP per capita`        = mean(gdp_per_capita_2024, na.rm = TRUE),
    `Crop area (4 crops)`   = sum(crop_soy_area_planted_km2,    na.rm = TRUE) +
      sum(crop_corn_area_planted_km2,   na.rm = TRUE) +
      sum(crop_rice_area_planted_km2,   na.rm = TRUE) +
      sum(crop_cotton_area_planted_km2, na.rm = TRUE),
    `Cattle density`        = mean(cattle_per_km2, na.rm = TRUE),
    `Crop price index`      = mean(price_index_crop,   na.rm = TRUE),
    `Cattle price index`    = mean(price_index_cattle, na.rm = TRUE),
    `Formal mid-wage rate`  = mean(emp_pia_rate_mid, na.rm = TRUE),
    `Formal low-wage rate`  = mean(emp_pia_rate_low, na.rm = TRUE),
    `BF avg transfer/family`= mean(bf_transfers_avg_brl_2024, na.rm = TRUE)
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  group_by(series) |>
  mutate(index = value / value[year == 2004] * 100) |>
  ungroup() |>
  filter(is.finite(index))

# panel assignment; deforestation is duplicated into every panel
panels <- list(
  "A. Economic activity" = c("GDP per capita", "Crop area (4 crops)",
                             "Cattle density"),
  "B. Commodity prices"  = c("Crop price index", "Cattle price index"),
  "C. Formal wages"      = c("Formal mid-wage rate", "Formal low-wage rate"),
  "D. Bolsa Família"     = c("BF avg transfer/family")
)

plot_df <- bind_rows(lapply(names(panels), \(p) {
  bind_rows(
    idx_series |> filter(series %in% panels[[p]]),
    idx_series |> filter(series == "Deforestation")
  ) |> mutate(panel = p)
}))

pal <- c(`GDP per capita`         = "#7570b3",
         `Crop area (4 crops)`    = "#e6ab02",
         `Cattle density`         = "#a6611a",
         `Crop price index`       = "#66a61e",
         `Cattle price index`     = "#e7298a",
         `Formal mid-wage rate`   = "#1b9e77",
         `Formal low-wage rate`   = "#d95f02",
         `BF avg transfer/family` = "#762a83")

p9 <- ggplot() +
  annotate("rect", xmin = 2004, xmax = 2012, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.5) +
  geom_hline(yintercept = 100, linewidth = 0.3, color = "grey60") +
  # reference series first, so colored lines draw on top
  geom_line(data = subset(plot_df, series == "Deforestation"),
            aes(year, index), color = "grey25", linewidth = 1.2) +
  geom_line(data = subset(plot_df, series != "Deforestation"),
            aes(year, index, color = series), linewidth = 0.85) +
  facet_wrap(~ panel, ncol = 2) +
  scale_color_manual(values = pal, name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(title = "Deforestation against social and economic trajectories, 2000–2020",
       subtitle = paste0("All series indexed to 2004 = 100. Thick grey line: ",
                         "deforestation (sample total). Shaded band: 2004–2012 decline."),
       x = NULL, y = "Index (2004 = 100)",
       caption = paste0("Rates and per-capita values: unweighted municipal means; ",
                        "areas and deforestation: sample totals. BF series ends in ",
                        "2019; GDP begins in 2002. Descriptive co-movement only; ",
                        "shared trends motivate, they do not identify.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 9.5),
        plot.caption = element_text(size = 7.5, hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

# ggsave("output/figures/fig09_defor_vs_groups.png", p9,
#        width = 10, height = 7.5, dpi = 300)
p9


#ORRRRRR
# =============================================================================
# 07d-eda-panel-trends.R — Fig 09: deforestation against grouped trajectories
# Four panels, deforestation repeated in each as the reference series.
#   A. Economic activity: GDP per capita, crop area, cattle density
#   B. Price exposure: crop and cattle price indices
#   C. Formal wages: mid- and low-bracket employment rates
#   D. Bolsa Família: average transfer per family
# Normalization: index 2004 = 100, as in fig08. Self-contained: only needs df.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# # df <- read.csv("final_dataset_6.csv")   # uncomment if running standalone
# dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

idx_series <- df |>
  group_by(year) |>
  summarise(
    `Deforestation`         = sum(deforestation_area_km2, na.rm = TRUE),
    `GDP per capita`        = mean(gdp_per_capita_2024, na.rm = TRUE),
    `Crop area (4 crops)`   = sum(crop_soy_area_planted_km2,    na.rm = TRUE) +
      sum(crop_corn_area_planted_km2,   na.rm = TRUE) +
      sum(crop_rice_area_planted_km2,   na.rm = TRUE) +
      sum(crop_cotton_area_planted_km2, na.rm = TRUE),
    `Cattle density`        = mean(cattle_per_km2, na.rm = TRUE),
    `Crop price index`      = mean(price_index_crop,   na.rm = TRUE),
    `Cattle price index`    = mean(price_index_cattle, na.rm = TRUE),
    `Formal low-wage rate`  = mean(emp_pia_rate_low, na.rm = TRUE),
    `BF coverage (families/quota)` = mean(bf_families_quota_ratio, na.rm = TRUE)
  ) |>
  pivot_longer(-year, names_to = "series", values_to = "value") |>
  filter(year <= 2019) |>
  group_by(series) |>
  # BF coverage is indexed to 2006 (post-rollout steady state); the 2004
  # value (mean ratio 0.33) reflects program rollout and would turn a flat
  # coverage trajectory into a spurious ~3.5x expansion. All other series
  # keep the 2004 base.
  mutate(base_yr = ifelse(series == "BF coverage (families/quota)", 2006, 2004),
         index = value / value[year == base_yr[1]] * 100) |>
  ungroup() |>
  filter(is.finite(index))

# panel assignment; deforestation is duplicated into every panel
panels <- list(
  "A. Economic activity" = c("GDP per capita", "Crop area (4 crops)",
                             "Cattle density"),
  "B. Commodity prices"  = c("Crop price index", "Cattle price index"),
  "C. Formal wages"      = c("Formal mid-wage rate", "Formal low-wage rate"),
  "D. Bolsa Família"     = c("BF coverage (families/quota)")
)

plot_df <- bind_rows(lapply(names(panels), \(p) {
  bind_rows(
    idx_series |> filter(series %in% panels[[p]]),
    idx_series |> filter(series == "Deforestation")
  ) |> mutate(panel = p)
}))

pal <- c(`GDP per capita`         = "#7570b3",
         `Crop area (4 crops)`    = "#e6ab02",
         `Cattle density`         = "#a6611a",
         `Crop price index`       = "#66a61e",
         `Cattle price index`     = "#e7298a",
         `Formal mid-wage rate`   = "#1b9e77",
         `Formal low-wage rate`   = "#d95f02",
         `BF coverage (families/quota)` = "#762a83")

p9 <- ggplot() +
  annotate("rect", xmin = 2004, xmax = 2012, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.5) +
  geom_hline(yintercept = 100, linewidth = 0.3, color = "grey60") +
  # reference series first, so colored lines draw on top
  geom_line(data = subset(plot_df, series == "Deforestation"),
            aes(year, index), color = "grey25", linewidth = 1.2) +
  geom_line(data = subset(plot_df, series != "Deforestation"),
            aes(year, index, color = series), linewidth = 0.85) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_color_manual(values = pal, name = NULL) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2019)) +
  labs(title = "Deforestation against social and economic trajectories, 2000–2019",
       subtitle = paste0("Series indexed to 2004 = 100 (BF coverage: 2006 = 100). ",
                         "Thick grey line: deforestation (sample total). ",
                         "Shaded band: 2004–2012 decline."),
       x = NULL, y = "Index (base year = 100)",
       caption = paste0("Rates and ratios: unweighted municipal means; areas and ",
                        "deforestation: sample totals. GDP begins in 2002. BF ",
                        "coverage indexed to 2006 because 2004\u20132005 values ",
                        "reflect program rollout, not steady-state provision. ",
                        "Y-axis ranges differ across panels: compare timing and ",
                        "direction, not visual slopes. Descriptive co-movement ",
                        "only; shared trends motivate, they do not identify.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 9.5),
        plot.caption = element_text(size = 7.5, hjust = 0)) +
  guides(color = guide_legend(nrow = 2))

# ggsave("output/figures/fig09_defor_vs_groups.png", p9,
#        width = 10, height = 7.5, dpi = 300)

p9

# =============================================================================
# 07e-eda-block-b.R — Block B: cross-sectional structure
#   B1. Bivariate choropleth: BF coverage x deforestation decline
#   B2. Moderator credibility: distribution among treated + baseline scatter
#   B3. Aggregate deforestation under alternative sample definitions
#
# Requires: geobr, biscale, cowplot (B1); ggplot2, dplyr, tidyr (all)
#   install.packages(c("geobr", "biscale", "cowplot"))
# geobr downloads geometries on first use (internet required).
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- parameters (robustness-grid switches) -----------------------------------
treatment_var <- "ppcdam_main"   # alternatives: ppcdam_robust, ppcdam_list
mod_years     <- 2006:2007       # moderator window: post-rollout, pre-treatment
post_years    <- 2013:2016       # outcome window for the map
base_years    <- 2000:2007       # baseline window (matches defor_norm_ref0007)

data_path <- "_data/final_dataset_6.csv"
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path) |>
  mutate(geocode = as.numeric(geocode),
         avail_ok = avail_ok %in% c(TRUE, "True", "TRUE"))


# --- municipality-level cross-section -----------------------------------------
cross <- df |>
  group_by(geocode) |>
  summarise(
    ever_treated = max(.data[[treatment_var]], na.rm = TRUE),
    bfq_mod  = mean(bf_families_quota_ratio[year %in% mod_years], na.rm = TRUE),
    bfpc_mod = mean(bf_transfers_pc_brl_2024[year %in% mod_years], na.rm = TRUE),
    post_norm  = mean(defor_norm_ref0007[year %in% post_years], na.rm = TRUE),
    base_defor = mean(deforestation_area_km2[year %in% base_years], na.rm = TRUE),
    at_risk_narrow = any(at_risk_narrow %in% c(TRUE, "True")),
    state = first(state_abbreviation),
    .groups = "drop"
  ) |>
  mutate(decline = -post_norm)   # higher = larger decline vs own baseline

# Data-quality flag: quota-ratio outliers concentrate in MT (14 municipalities
# with bfq_mod > 3, all MT; max ~11.9). Pending upstream check of the quota
# source in script 02. They are retained here; B2 displays them explicitly.
cat("Moderator outliers (bfq_mod > 3):",
    sum(cross$bfq_mod > 3, na.rm = TRUE), "municipalities\n")

# =============================================================================
# B1. Bivariate choropleth: coverage x deforestation decline
# -----------------------------------------------------------------------------
# The map version of the stratified design: does the spatial pattern of
# post-period deforestation decline align with pre-treatment BF coverage?
# 3x3 quantile classes; both dimensions classified on the full 502 sample.
# =============================================================================

library(geobr)
library(biscale)
library(cowplot)
library(sf)

muni_sf <- read_municipality(year = 2010, showProgress = FALSE) |>
  mutate(geocode = as.numeric(code_muni)) |>
  inner_join(cross, by = "geocode")

bi_df <- bi_class(muni_sf, x = bfq_mod, y = decline,
                  style = "quantile", dim = 3)

p_map <- ggplot(bi_df) +
  geom_sf(aes(fill = bi_class), color = "white", linewidth = 0.05,
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(title = "Bolsa Família coverage and deforestation decline",
       subtitle = paste0("Coverage: mean families/quota ratio, ",
                         min(mod_years), "\u2013", max(mod_years),
                         ". Decline: mean normalized deforestation, ",
                         min(post_years), "\u2013", max(post_years),
                         ", relative to the 2000\u20132007 baseline (sign inverted)."),
       caption = paste0("Quantile terciles on both dimensions, 502 municipalities. ",
                        "Descriptive: the spatial coincidence of high coverage and ",
                        "large decline is the pattern the stratified design tests.")) +
  theme_void(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))

legend <- bi_legend(pal = "GrPink", dim = 3,
                    xlab = "Higher coverage",
                    ylab = "Larger decline", size = 7)

p_b1 <- ggdraw() +
  draw_plot(p_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.08, 0.25, 0.25)

# ggsave("output/figures/fig10_bivariate_map.png", p_b1,
#        width = 9, height = 8, dpi = 300)

p_b1
# =============================================================================
# B2. Moderator credibility
# -----------------------------------------------------------------------------
# Panel A: moderator distribution among ever-treated, median split marked.
# Panel B: moderator vs baseline deforestation, quota ratio next to BF per
# capita. The contrast in correlations (~0.09 vs ~-0.31) is the argument:
# the quota ratio does not proxy baseline deforestation pressure, raw
# transfers per capita do.
# =============================================================================

treated <- filter(cross, ever_treated == 1, !is.na(bfq_mod))
med_split <- median(treated$bfq_mod)
cat("Treated municipalities:", nrow(treated),
    "| median split at", round(med_split, 3),
    "| high/low:", sum(treated$bfq_mod > med_split), "/",
    sum(treated$bfq_mod <= med_split), "\n")

p_b2a <- ggplot(treated, aes(bfq_mod)) +
  geom_histogram(bins = 24, fill = "#762a83", alpha = 0.8, color = "white") +
  geom_vline(xintercept = med_split, linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = med_split, y = Inf, vjust = 1.5, hjust = -0.1,
           label = paste0("median = ", round(med_split, 2)), size = 3) +
  labs(subtitle = paste0("A. Moderator among ever-treated municipalities (",
                         treatment_var, ", n = ", nrow(treated), ")"),
       x = paste0("Families/quota ratio, ", min(mod_years), "\u2013",
                  max(mod_years), " mean"),
       y = "Municipalities") +
  theme_minimal(base_size = 11)

scatter_df <- cross |>
  filter(at_risk_narrow, !is.na(base_defor), base_defor > 0) |>
  pivot_longer(c(bfq_mod, bfpc_mod), names_to = "measure", values_to = "mod") |>
  filter(!is.na(mod)) |>
  mutate(measure = factor(measure, levels = c("bfq_mod", "bfpc_mod"),
                          labels = c("Families/quota ratio",
                                     "Transfers per capita (BRL-2024)")))

cors <- scatter_df |>
  group_by(measure) |>
  summarise(r = cor(base_defor, mod), .groups = "drop") |>
  mutate(label = paste0("r = ", sprintf("%.2f", r)))

p_b2b <- ggplot(scatter_df, aes(base_defor, mod)) +
  geom_point(alpha = 0.35, size = 1, color = "grey25") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#d95f02") +
  geom_text(data = cors, aes(label = label), x = Inf, y = Inf,
            hjust = 1.2, vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_x_log10(labels = label_comma()) +
  labs(subtitle = "B. Moderator candidates against baseline deforestation (at-risk narrow sample)",
       x = expression("Mean annual deforestation 2000\u20132007 (km"^2*", log scale)"),
       y = NULL,
       caption = paste0("At-risk narrow sample (n = 399), matching the estimation ",
                        "sample. Pearson r on unlogged values; Spearman \u03c1 = ",
                        "-0.03 (quota ratio) and -0.40 (per capita). Full-sample ",
                        "values are nearly identical (r = 0.09 and -0.31). Ratios ",
                        "above 3 (all MT) are retained pending an upstream ",
                        "quota-data check.")) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0),
        strip.text = element_text(face = "bold", size = 9))

# if (requireNamespace("patchwork", quietly = TRUE)) {
#   library(patchwork)
#   p_b2 <- p_b2a / p_b2b + plot_layout(heights = c(1, 1.3)) +
#     plot_annotation(title = "Moderator credibility: coverage relative to need")
#   ggsave("output/figures/fig11_moderator_credibility.png", p_b2,
#          width = 9, height = 8, dpi = 300)
# } else {
#   ggsave("output/figures/fig11a_moderator_dist.png",    p_b2a, width = 8, height = 4, dpi = 300)
#   ggsave("output/figures/fig11b_moderator_scatter.png", p_b2b, width = 9, height = 4.5, dpi = 300)
# }

p_b2a
p_b2b

# =============================================================================
# B3. Aggregate deforestation under alternative sample definitions
# -----------------------------------------------------------------------------
# Full 502 vs forest floor (avail_ok, 499) vs at-risk broad (468) vs
# at-risk narrow (408). Forest floor barely restricts, so its line will
# track the full sample: that near-identity is itself the point. Divergence
# between at-risk samples and the full sample would indicate results could
# be sensitive to sample choice.
# =============================================================================

sample_series <- bind_rows(
  df |> mutate(sample = "Full (n = 502)"),
  df |> filter(avail_ok)        |> mutate(sample = "Forest floor"),
  df |> filter(at_risk_broad %in% c(TRUE, "True"))  |> mutate(sample = "At risk, broad"),
  df |> filter(at_risk_narrow %in% c(TRUE, "True")) |> mutate(sample = "At risk, narrow")
) |>
  group_by(sample, year) |>
  summarise(defor = sum(deforestation_area_km2, na.rm = TRUE),
            n = n_distinct(geocode), .groups = "drop") |>
  mutate(sample = paste0(sub(" \\(.*", "", sample), " (n = ", n, ")"))

p_b3 <- ggplot(sample_series, aes(year, defor, color = sample)) +
  annotate("rect", xmin = 2004, xmax = 2012, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  labs(title = "Aggregate deforestation under alternative sample definitions",
       subtitle = "Sample totals of annual PRODES increments, 2000–2020",
       x = NULL, y = expression("Deforestation (km"^2*")"),
       caption = paste0("Sample flags are municipality-constant. Near-coincident ",
                        "lines indicate the aggregate trajectory is not an artifact ",
                        "of sample restriction.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig12_sample_definitions.png", p_b3,
#        width = 9.5, height = 5.5, dpi = 300)

p_b3

# =============================================================================
# B4. Normalized-outcome instability and the at-risk restriction
# -----------------------------------------------------------------------------
# The functional-form (Type B) case for running normalized-outcome analyses
# on the at-risk samples: defor_norm_ref0007 scales by each municipality's
# own 2000-2007 baseline variability, which is near zero where there was no
# frontier activity. Those municipalities produce explosive z-scores
# (Chaves-PA reaches |z| ~ 250) that are division artifacts, not signal.
# The figure shows max |normalized outcome| (2008-2019) against baseline
# deforestation, marking municipalities excluded by at_risk_narrow.
# =============================================================================

b4 <- df |>
  group_by(geocode) |>
  summarise(
    base_defor = mean(deforestation_area_km2[year %in% base_years], na.rm = TRUE),
    max_abs_norm = max(abs(defor_norm_ref0007[year %in% 2008:2019]), na.rm = TRUE),
    in_narrow = any(at_risk_narrow %in% c(TRUE, "True")),
    municipality = first(municipality),
    state = first(state_abbreviation),
    .groups = "drop"
  ) |>
  filter(is.finite(max_abs_norm)) |>
  # zero-baseline municipalities are plotted at 0.01 km2 (log scale)
  mutate(base_plot = pmax(base_defor, 0.01),
         status = ifelse(in_narrow, "In at-risk narrow sample",
                         "Excluded by at-risk narrow"),
         lab = ifelse(max_abs_norm > 30,
                      paste0(municipality, " (", state, ")"), NA))

p_b4 <- ggplot(b4, aes(base_plot, max_abs_norm, color = status)) +
  geom_point(alpha = 0.55, size = 1.4) +
  ggrepel::geom_text_repel(aes(label = lab), size = 2.6, na.rm = TRUE,
                           show.legend = FALSE, max.overlaps = 20) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  scale_y_log10() +
  scale_color_manual(values = c("In at-risk narrow sample" = "grey35",
                                "Excluded by at-risk narrow" = "#d95f02"),
                     name = NULL) +
  labs(title = "Why normalized-outcome analyses use the at-risk sample",
       subtitle = paste0("Max |normalized deforestation| 2008\u20132019 against ",
                         "baseline deforestation, 502 municipalities"),
       x = expression("Mean annual deforestation 2000\u20132007 (km"^2*", log scale)"),
       y = "Max |normalized outcome|, log scale",
       caption = paste0("The normalized outcome divides by own-baseline variability; ",
                        "near-zero baselines produce explosive z-scores that are ",
                        "division artifacts. Zero-baseline municipalities plotted at ",
                        "0.01 km\u00b2. Exclusion is functional-form necessity, not ",
                        "outcome-based selection: none of the excluded municipalities ",
                        "was ever blacklisted. High-baseline extremes (e.g. Feij\u00f3) ",
                        "motivate a winsorized-outcome robustness row.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig13_norm_outcome_instability.png", p_b4,
#        width = 9, height = 6, dpi = 300)

p_b4


# =============================================================================
# B5. Rebound map: change in normalized deforestation between windows
# -----------------------------------------------------------------------------
# rebound = mean normalized deforestation (2013-2016) minus (2008-2012).
# Positive (red): deforestation rose relative to the crash period, i.e. the
# decline did not persist. Negative (teal): continued decline. Colored for
# the at-risk narrow sample only; municipalities outside the estimand
# population are drawn in light gray (the normalized measure is unreliable
# where baseline activity was near zero; see B4). Fill capped at |2| SD to
# keep the color scale informative (about 8% of at-risk municipalities
# exceed the cap; they render at the extreme colors).
# =============================================================================

rebound <- df |>
  group_by(geocode) |>
  summarise(
    w_crash = mean(defor_norm_ref0007[year %in% 2008:2012], na.rm = TRUE),
    w_after = mean(defor_norm_ref0007[year %in% 2013:2016], na.rm = TRUE),
    in_narrow = any(at_risk_narrow %in% c(TRUE, "True")),
    .groups = "drop"
  ) |>
  mutate(reb = ifelse(in_narrow, w_after - w_crash, NA_real_))

reb_sf <- read_municipality(year = 2010, showProgress = FALSE) |>
  mutate(geocode = as.numeric(code_muni)) |>
  inner_join(rebound, by = "geocode")

p_b5 <- ggplot(reb_sf) +
  geom_sf(aes(fill = reb), color = "white", linewidth = 0.05) +
  scale_fill_gradient2(low = "#01665e", mid = "grey96", high = "#8e0152",
                       midpoint = 0, limits = c(-2, 2), oob = scales::squish,
                       na.value = "grey85",
                       name = "Change in normalized\ndeforestation (SD)") +
  labs(title = "Persistence and rebound of the deforestation decline",
       subtitle = paste0("Mean normalized deforestation, 2013\u20132016 minus ",
                         "2008\u20132012. Red: rebound; teal: continued decline."),
       caption = paste0("At-risk narrow sample (n = 408) colored; other ",
                        "municipalities in gray (normalized outcome unreliable ",
                        "at near-zero baselines). Fill capped at \u00b12 SD. ",
                        "61% of at-risk municipalities show some rebound.")) +
  theme_void(base_size = 11) +
  theme(legend.position = c(0.15, 0.22),
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig14_rebound_map.png", p_b5,
#        width = 9, height = 8, dpi = 300)

p_b5

# =============================================================================
# End of Block B. Next: Block C (event-time heterogeneity among listed
# municipalities, by cohort, exit status, and moderator stratum).
# =============================================================================

# =============================================================================
# 07f-eda-block-c.R — Block C: heterogeneity among listed municipalities
#   C1. Event-time small multiples by listing cohort, colored by exit status
#   C2. Event-time paths by BF moderator stratum (2008/09 cohorts) with a
#       never-treated reference line
#
# Treatment timing ALWAYS comes from ppcdam_list (time-varying listing);
# ppcdam_main / ppcdam_robust / ppcdam_ever_full are municipality-constant
# cohort-SET flags (main = 2008+2009 cohorts, robust adds 2011/12, full adds
# 2017/18). They must never be passed as timing variables.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# --- parameters ----------------------------------------------------------------
mod_years <- 2006:2007        # moderator window (matches Block B)
et_window <- c(-6, 10)        # event-time display window
y_clip    <- c(-4, 8)         # outcome display clip (~1% of points outside)

# data_path <- "final_dataset_6.csv"
# dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# df <- read.csv(data_path)
last_year <- max(df$year)

# --- cohorts, exit status, event time ------------------------------------------
cohorts <- df |>
  filter(ppcdam_list == 1) |>
  group_by(geocode) |>
  summarise(cohort = min(year),
            listed_last = max(year) == last_year,
            .groups = "drop")
# exit = not listed in the final panel year; 4 municipalities were re-listed
# after exiting, so exit is not absorbing. Cohort = FIRST listing year.

moder <- df |>
  filter(year %in% mod_years) |>
  group_by(geocode) |>
  summarise(bfq = mean(bf_families_quota_ratio, na.rm = TRUE), .groups = "drop")

listed <- df |>
  inner_join(cohorts, by = "geocode") |>
  mutate(et = year - cohort,
         exit_status = ifelse(listed_last, "Listed through end of panel",
                              "Exited the list")) |>
  filter(et >= et_window[1], et <= et_window[2])

# =============================================================================
# C1. Small multiples by cohort, colored by exit status
# -----------------------------------------------------------------------------
# DESCRIPTIVE ONLY. Exit is conditioned on meeting deforestation-reduction
# targets, so comparing exiters to stayers is selection on the outcome by
# construction. The figure documents heterogeneity to be explained; it is
# not an effect comparison.
# =============================================================================

cohort_n <- listed |>
  distinct(geocode, cohort) |>
  count(cohort)

listed <- listed |>
  left_join(cohort_n, by = "cohort") |>
  mutate(cohort_lab = paste0("Cohort ", cohort, " (n = ", n, ")"))

p_c1 <- ggplot(listed, aes(et, defor_norm_ref0007, group = geocode,
                           color = exit_status)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(alpha = 0.45, linewidth = 0.4) +
  facet_wrap(~ cohort_lab, ncol = 3) +
  coord_cartesian(ylim = y_clip) +
  scale_color_manual(values = c("Exited the list" = "#1b9e77",
                                "Listed through end of panel" = "#d95f02"),
                     name = NULL) +
  labs(title = "Deforestation among blacklisted municipalities, by cohort and exit status",
       subtitle = "Normalized deforestation in event time (0 = first listing year)",
       x = "Years since first listing",
       y = "Normalized deforestation (own 2000\u20132007 baseline)",
       caption = paste0("Descriptive. Exit requires meeting deforestation-reduction ",
                        "targets, so exit status is outcome-dependent by construction ",
                        "and differences between groups are not effect estimates. ",
                        "Exit = not listed in ", last_year, "; four municipalities ",
                        "were re-listed after exiting. Cohorts dated by first ",
                        "listing. Display clipped to [", y_clip[1], ", ", y_clip[2],
                        "] (~1% of points outside).")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig15_cohort_exit_paths.png", p_c1,
#        width = 10, height = 7, dpi = 300)

p_c1

# =============================================================================
# C2. Event-time paths by moderator stratum, 2008/09 cohorts, with a
#     never-treated reference
# -----------------------------------------------------------------------------
# Restricted to the ppcdam_main cohort set (2008 + 2009, n = 43): near-uniform
# timing makes the event-time display clean. Median split on the 2006-07
# quota ratio (the Block B split). The never-treated reference (at-risk
# narrow, never listed) is aligned on the 2008 clock, since 36 of 43 treated
# municipalities belong to the 2008 cohort.
#
# INTERPRETATION GUARD: these are raw paths, not counterfactual comparisons.
# In raw means the low-coverage stratum reaches a deeper trough; the C&S
# stratified ATTs (high -0.664 vs low -0.511) net each stratum against
# never-treated trends and cohort weighting, which raw means do not. The
# relevant visual quantity is each stratum's gap to the reference line, and
# even that remains descriptive. Both strata decline before event time 0:
# pre-periods fall in the middle of the 2005-2012 general crash, and listing
# selected on high recent deforestation.
# =============================================================================

main_set <- listed |>
  filter(cohort %in% c(2008, 2009)) |>
  left_join(moder, by = "geocode")

split_val <- median(main_set |> distinct(geocode, bfq) |> pull(bfq), na.rm = TRUE)

main_set <- main_set |>
  mutate(stratum = ifelse(bfq > split_val, "High BF coverage", "Low BF coverage"))

strat_means <- main_set |>
  group_by(stratum, et) |>
  summarise(m = mean(defor_norm_ref0007, na.rm = TRUE), .groups = "drop")

glimpse(main_set)

never <- df |>
  anti_join(cohorts, by = "geocode") |>
  filter(at_risk_narrow %in% c(TRUE, "True")) |>
  mutate(et = year - 2008) |>
  filter(et >= et_window[1], et <= et_window[2]) |>
  group_by(et) |>
  summarise(m = mean(defor_norm_ref0007, na.rm = TRUE), .groups = "drop")

p_c2 <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_line(data = main_set,
            aes(et, defor_norm_ref0007, group = geocode, color = stratum),
            alpha = 0.25, linewidth = 0.35) +
  geom_line(data = strat_means, aes(et, m, color = stratum), linewidth = 1.3) +
  geom_line(data = never, aes(et, m), color = "grey35", linewidth = 1,
            linetype = "longdash") +
  annotate("text", x = max(never$et), y = never$m[which.max(never$et)],
           label = "Never listed\n(at-risk narrow)", hjust = -0.05, size = 2.8,
           color = "grey35", lineheight = 0.9) +
  coord_cartesian(ylim = y_clip, xlim = c(et_window[1], et_window[2] + 2.5)) +
  scale_color_manual(values = c("High BF coverage" = "#762a83",
                                "Low BF coverage"  = "#e7a323"),
                     name = NULL) +
  labs(title = "Listed municipalities by Bolsa Família coverage stratum, 2008/09 cohorts",
       subtitle = paste0("Normalized deforestation in event time. Median split at ",
                         round(split_val, 2), " (2006\u20132007 families/quota ratio). ",
                         "Dashed gray: never-listed reference on the 2008 clock."),
       x = "Years since first listing",
       y = "Normalized deforestation (own 2000\u20132007 baseline)",
       caption = paste0("Raw paths, not counterfactual comparisons: the stratified ",
                        "difference-in-differences contrast is each stratum's gap ",
                        "to never-treated trends, not the strata's raw depths. ",
                        "Pre-listing declines reflect the 2005\u20132012 aggregate ",
                        "crash and selection into listing on recent deforestation. ",
                        "Display clipped to [", y_clip[1], ", ", y_clip[2], "].")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig16_stratum_paths.png", p_c2,
#        width = 10, height = 6.5, dpi = 300)

p_c2

# =============================================================================
# End of Block C. Next: Block D2-D3 (within-demeaned binned scatter; baseline
# covariate balance), Block E (electoral competitiveness by listing status).
# =============================================================================


# =============================================================================
# 07g-eda-block-d.R — Block D: model-agnostic diagnostics (D2, D3)
#   D2. Binned scatter of two-way demeaned deforestation against two-way
#       demeaned BF variables: the model-free analogue of a TWFE direct-
#       effect regression (H2), shown before any model is run.
#   D3. Baseline covariate balance (standardized mean differences) for the
#       two comparisons the design uses:
#         (a) treated 2008/09 cohorts vs never-listed at-risk controls
#         (b) high vs low BF-coverage stratum among the treated
#
# D1 (within/between variance decomposition) lives in 07-eda.R.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

mod_years  <- 2006:2007
base_years <- 2004:2007     # BF-era baseline window for balance table
d2_years   <- 2004:2019     # BF availability window
n_bins     <- 20

#data_path <- "final_dataset_6.csv"
#dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
#dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path) |>
  mutate(geocode = as.numeric(geocode))

glimpse(df)

# =============================================================================
# D2. Two-way demeaned binned scatter (TWFE analogue)
# -----------------------------------------------------------------------------
# Sample: at-risk narrow (normalized outcome), 2004-2019 (BF availability).
# Both variables are demeaned by municipality and year (grand mean added
# back), so the plotted relationship is exactly the variation a TWFE
# regression would use. The slope printed in each panel equals the TWFE
# coefficient on that regressor entered alone.
# READING: slopes are ~ +0.09 (quota ratio) and ~ +0.015/BRL (per capita).
# The within variation here is administrative churn and transfers tracking
# local conditions, not policy experiments; the panel documents that the
# naive direct-effect channel (H2) carries no protective signal, which is
# why the chapter's causal weight rests on moderation.
# =============================================================================

twoway_demean <- function(x, id, t) {
  x - ave(x, id) - ave(x, t) + mean(x)
}

d2 <- df |>
  filter(at_risk_narrow %in% c(TRUE, "True"), year %in% d2_years) |>
  select(geocode, year, defor_norm_ref0007,
         bf_families_quota_ratio, bf_transfers_pc_brl_2024) |>
  drop_na()

d2_long <- d2 |>
  mutate(y_dd = twoway_demean(defor_norm_ref0007, geocode, year),
         x1   = twoway_demean(bf_families_quota_ratio, geocode, year),
         x2   = twoway_demean(bf_transfers_pc_brl_2024, geocode, year)) |>
  pivot_longer(c(x1, x2), names_to = "var", values_to = "x_dd") |>
  mutate(var = factor(var, levels = c("x1", "x2"),
                      labels = c("Families/quota ratio",
                                 "Transfers per capita (BRL-2024)")))

slopes <- d2_long |>
  group_by(var) |>
  summarise(b = cov(x_dd, y_dd) / var(x_dd), .groups = "drop") |>
  mutate(label = paste0("slope = ", sprintf("%+.3f", b)))

binned <- d2_long |>
  group_by(var) |>
  mutate(bin = ntile(x_dd, n_bins)) |>
  group_by(var, bin) |>
  summarise(x = mean(x_dd), y = mean(y_dd),
            se = sd(y_dd) / sqrt(n()), .groups = "drop")

p_d2 <- ggplot(binned, aes(x, y)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_errorbar(aes(ymin = y - 1.96 * se, ymax = y + 1.96 * se),
                width = 0, color = "grey55", linewidth = 0.4) +
  geom_point(size = 1.8, color = "#762a83") +
  geom_smooth(data = d2_long, aes(x_dd, y_dd), method = "lm", se = FALSE,
              linewidth = 0.7, color = "#d95f02") +
  geom_text(data = slopes, aes(label = label), x = Inf, y = Inf,
            hjust = 1.1, vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ var, scales = "free_x") +
  labs(title = "Within-variation relationship between Bolsa Família and deforestation",
       subtitle = paste0("Two-way demeaned (municipality and year), binned into ",
                         n_bins, " equal-count bins. At-risk narrow sample, 2004\u20132019."),
       x = "BF variable, demeaned",
       y = "Normalized deforestation, demeaned",
       caption = paste0("Model-free analogue of a two-way FE regression: the fitted ",
                        "slope equals the TWFE coefficient on the regressor entered ",
                        "alone. The remaining within variation reflects administrative ",
                        "churn and transfers responding to local conditions; the ",
                        "near-zero, slightly positive slopes document that the naive ",
                        "direct channel carries no protective signal. Descriptive, ",
                        "not causal.")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))

# #ggsave("output/figures/fig17_twfe_binned_scatter.png", p_d2,
#        width = 10, height = 5.5, dpi = 300)

p_d2

# =============================================================================
# D3. Baseline covariate balance (standardized mean differences)
# -----------------------------------------------------------------------------
# SMD = (mean_A - mean_B) / sqrt((var_A + var_B) / 2), on 2004-2007 baseline
# means (population: 2004). Two comparisons:
#  (a) treated (2008/09 cohorts) vs never-listed at-risk narrow controls.
#      Large imbalances expected and documented: listing selected on
#      deforestation (SMD ~ +1.8). This motivates a design that nets out
#      unit levels rather than comparing them.
#  (b) high vs low coverage stratum among the treated. This is the
#      comparison the moderation claim leans on. Balanced on baseline
#      deforestation, electoral competition, employment; NOT balanced on
#      agricultural employment share (~ -0.9) and GDP pc (~ -0.6):
#      the moderation contrast is partially confounded with agrarian
#      structure. Motivates covariate-adjusted stratified estimation and
#      an explicit limitation paragraph.
# =============================================================================

first_listed <- df |>
  filter(ppcdam_list == 1) |>
  group_by(geocode) |>
  summarise(cohort = min(year), .groups = "drop")

baseline <- df |>
  filter(year %in% base_years) |>
  group_by(geocode) |>
  summarise(
    `Deforestation (km2/yr)`     = mean(deforestation_area_km2, na.rm = TRUE),
    `Forest area (km2)`          = mean(forest_area_km2, na.rm = TRUE),
    `BF families/quota`          = mean(bf_families_quota_ratio, na.rm = TRUE),
    `BF transfers pc (BRL-2024)` = mean(bf_transfers_pc_brl_2024, na.rm = TRUE),
    `GDP per capita (BRL-2024)`  = mean(gdp_per_capita_2024, na.rm = TRUE),
    `Formal employment / PIA`    = mean(emp_pia_rate_total, na.rm = TRUE),
    `Agric. share of formal`     = mean(emp_share_agri_workers, na.rm = TRUE),
    `Cattle per km2`             = mean(cattle_per_km2, na.rm = TRUE),
    `Electoral competition (ENC)`= mean(elec_enc, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(df |> filter(year == 2004) |>
              transmute(geocode, `Population (2004)` = as.numeric(population)),
            by = "geocode") |>
  left_join(first_listed, by = "geocode") |>
  mutate(at_risk = df |> group_by(geocode) |>
           summarise(a = any(at_risk_narrow %in% c(TRUE, "True"))) |> pull(a))

moder <- df |>
  filter(year %in% mod_years) |>
  group_by(geocode) |>
  summarise(bfq = mean(bf_families_quota_ratio, na.rm = TRUE), .groups = "drop")

smd <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  (mean(a) - mean(b)) / sqrt((var(a) + var(b)) / 2)
}

covars <- c("Deforestation (km2/yr)", "Forest area (km2)", "BF families/quota",
            "BF transfers pc (BRL-2024)", "GDP per capita (BRL-2024)",
            "Formal employment / PIA", "Agric. share of formal",
            "Cattle per km2", "Electoral competition (ENC)", "Population (2004)")

# (a) treated vs never-listed controls, at-risk narrow, later cohorts excluded
bal_a <- baseline |>
  filter(at_risk, is.na(cohort) | cohort %in% c(2008, 2009)) |>
  mutate(g = ifelse(!is.na(cohort), "T", "C"))
smd_a <- sapply(covars, \(v) smd(bal_a[[v]][bal_a$g == "T"],
                                 bal_a[[v]][bal_a$g == "C"]))

# (b) high vs low stratum among treated
bal_b <- baseline |>
  filter(cohort %in% c(2008, 2009)) |>
  left_join(moder, by = "geocode") |>
  mutate(g = ifelse(bfq > median(bfq, na.rm = TRUE), "H", "L"))
smd_b <- sapply(setdiff(covars, "BF families/quota"),
                \(v) smd(bal_b[[v]][bal_b$g == "H"],
                         bal_b[[v]][bal_b$g == "L"]))

smd_tab <- bind_rows(
  tibble(variable = names(smd_a), smd = smd_a,
         comparison = "A. Treated vs never-listed (at-risk narrow)"),
  tibble(variable = names(smd_b), smd = smd_b,
         comparison = "B. High vs low coverage stratum (treated only)")
)
#write.csv(smd_tab, "output/tables/tab_d3_balance_smd.csv", row.names = FALSE)

p_d3 <- ggplot(smd_tab, aes(smd, reorder(variable, smd))) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = c(-0.25, 0.25), linetype = "dotted",
             linewidth = 0.4, color = "grey50") +
  geom_point(size = 2.4, color = "#762a83") +
  facet_wrap(~ comparison, ncol = 2, scales = "free_x") +
  labs(title = "Baseline covariate balance, 2004\u20132007 means",
       subtitle = "Standardized mean differences. Dotted lines: |SMD| = 0.25.",
       x = "Standardized mean difference", y = NULL,
       caption = paste0("Panel A documents selection into listing (levels ",
                        "comparisons are invalid; the design nets out unit levels). ",
                        "Panel B is the comparison the moderation claim uses: ",
                        "balanced on baseline deforestation and competition, ",
                        "imbalanced on agrarian structure and income, motivating ",
                        "covariate-adjusted stratified estimation.")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))
# 
# #ggsave("output/figures/fig18_covariate_balance.png", p_d3,
#        width = 10.5, height = 5.5, dpi = 300)

p_d3

# =============================================================================
# End of Block D. Next: Block E (electoral competitiveness by listing status).
# =============================================================================


# =============================================================================
# 07h-eda-block-e.R — Block E: electoral competition and the blacklist
#   E1. Baseline electoral competition by listing group (pre-listing
#       administration snapshot)
#   E2. Electoral competition over time by listing group
#
# Electoral variables derive from the municipal election and are constant
# within administration blocks. NOTE (pending verification in script 03):
# as stored, values change IN election years (2004 election -> 2004-2007),
# not in the first governing year (2005-2008) as the stated one-lag rule
# implies. The 2006 snapshot used here is mid-administration and safe under
# either convention; treatment-onset uses of electoral covariates are not.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

snap_year <- 2006          # mid-administration of the pre-listing (2004) election
# data_path <- "final_dataset_6.csv"
# dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
# dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path) |>
  mutate(geocode = as.numeric(geocode))

first_listed <- df |>
  filter(ppcdam_list == 1) |>
  group_by(geocode) |>
  summarise(cohort = min(year), .groups = "drop")

groups <- df |>
  distinct(geocode) |>
  left_join(first_listed, by = "geocode") |>
  left_join(df |> group_by(geocode) |>
              summarise(at_risk = any(at_risk_narrow %in% c(TRUE, "True")),
                        .groups = "drop"), by = "geocode") |>
  filter(at_risk) |>
  mutate(grp = case_when(
    cohort %in% c(2008, 2009) ~ "Listed 2008/09 (n = 43)",
    !is.na(cohort)            ~ "Listed later (n = 18)",
    TRUE                      ~ "Never listed (n = 347)"
  ))

# =============================================================================
# E1. Baseline snapshot: ENC and winner share by group
# =============================================================================

e1 <- df |>
  filter(year == snap_year) |>
  select(geocode, elec_enc, elec_winner_share) |>
  mutate(across(c(elec_enc, elec_winner_share), as.numeric)) |>
  inner_join(groups, by = "geocode") |>
  pivot_longer(c(elec_enc, elec_winner_share),
               names_to = "var", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(var = factor(var, levels = c("elec_enc", "elec_winner_share"),
                      labels = c("Effective number of candidates",
                                 "Winner vote share")))

p_e1 <- ggplot(e1, aes(grp, value, color = grp)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 0.9) +
  facet_wrap(~ var, scales = "free_y") +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  labs(title = "Electoral competition before the blacklist, by listing group",
       subtitle = paste0("Values from the 2004 municipal election (snapshot ",
                         snap_year, "). At-risk narrow sample."),
       x = NULL, y = NULL,
       caption = paste0("Listed and never-listed municipalities are ",
                        "indistinguishable at baseline (mean ENC 2.40 vs 2.42; ",
                        "winner share 0.51 in both): listing did not select on ",
                        "local political competition.")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0),
        axis.text.x = element_text(size = 8))

# ggsave("output/figures/fig19_electoral_baseline.png", p_e1,
#        width = 10, height = 5, dpi = 300)

p_e1
# summary table across all electoral variables

e_tab <- df |>
  filter(year == snap_year) |>
  select(geocode, elec_enc, elec_mov, elec_winner_share, elec_n_candidates,
         elec_competitive, elec_uncontested) |>
  mutate(across(-geocode, as.numeric)) |>
  inner_join(groups, by = "geocode") |>
  group_by(grp) |>
  summarise(across(c(elec_enc, elec_mov, elec_winner_share, elec_n_candidates,
                     elec_competitive, elec_uncontested),
                   \(x) round(mean(x, na.rm = TRUE), 3)),
            n = n(), .groups = "drop")
# write.csv(e_tab, "output/tables/tab_e1_electoral_baseline.csv", row.names = FALSE)
# print(e_tab)

e_tab
# =============================================================================
# E2. Electoral competition over time by group
# -----------------------------------------------------------------------------
# Group means of ENC by year (values are administration-constant, so lines
# are step-shaped). Descriptive question: does competition evolve
# differently in listed municipalities after listing? Mechanism 3 (blame
# avoidance) would be consistent with listing raising the political salience
# of enforcement; this figure can only show the coarse trace of that.
# =============================================================================

e2 <- df |>
  select(geocode, year, elec_enc) |>
  mutate(elec_enc = as.numeric(elec_enc)) |>
  inner_join(groups, by = "geocode") |>
  group_by(grp, year) |>
  summarise(m = mean(elec_enc, na.rm = TRUE), .groups = "drop")

p_e2 <- ggplot(e2, aes(year, m, color = grp)) +
  annotate("rect", xmin = 2008, xmax = 2009, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  geom_step(linewidth = 0.9) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2020, 4)) +
  labs(title = "Effective number of candidates over time, by listing group",
       subtitle = "Group means; values constant within administrations. Shaded: 2008/09 listing.",
       x = NULL, y = "Effective number of candidates (mean)",
       caption = paste0("Descriptive. Administration blocks as stored in the data ",
                        "(election-year convention pending verification in ",
                        "script 03).")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig20_electoral_over_time.png", p_e2,
#        width = 9.5, height = 5.5, dpi = 300)

p_e2
# =============================================================================
# End of Block E — EDA blocks A-E complete.
# =============================================================================

# =============================================================================
# 07i-eda-bf-variation.R — Variation in BF coverage and transfers
#   fig21. Baseline distributions by listing group (boxplots)
#   fig22. Map of baseline coverage
#   fig23. Cross-municipal distribution over time (percentile ribbons)
#   fig24. Rank stability of the coverage moderator across windows
#   fig25. Coverage by state (MT outlier diagnostic)
#
# Baseline window: 2006-2007 (post-rollout, pre-listing), matching the
# moderator definition used throughout.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

mod_years <- 2006:2007
# data_path <- "final_dataset_6.csv"
# dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path) |>
  mutate(geocode = as.numeric(geocode))

first_listed <- df |>
  filter(ppcdam_list == 1) |>
  group_by(geocode) |>
  summarise(cohort = min(year), .groups = "drop")

base <- df |>
  filter(year %in% mod_years) |>
  group_by(geocode) |>
  summarise(bfq  = mean(bf_families_quota_ratio, na.rm = TRUE),
            bfpc = mean(bf_transfers_pc_brl_2024, na.rm = TRUE),
            state = first(state_abbreviation), .groups = "drop") |>
  left_join(first_listed, by = "geocode") |>
  left_join(df |> group_by(geocode) |>
              summarise(at_risk = any(at_risk_narrow %in% c(TRUE, "True")),
                        .groups = "drop"), by = "geocode") |>
  mutate(grp = case_when(
    cohort %in% c(2008, 2009) ~ "Listed 2008/09",
    !is.na(cohort)            ~ "Listed later",
    TRUE                      ~ "Never listed"
  ))

# =============================================================================
# fig21. Baseline distributions by listing group
# -----------------------------------------------------------------------------
# READING: coverage (quota ratio) is nearly identical across groups
# (medians 0.96 / 0.88 / 1.00); transfers per capita are sharply graded
# (8.3 vs 16.7 BRL): money follows poverty away from the frontier while
# provision relative to need is a leveled field. Quota-ratio display capped
# at 2.5 (MT outliers extend to 11.9, retained in the data).
# =============================================================================

f21 <- base |>
  filter(at_risk) |>
  pivot_longer(c(bfq, bfpc), names_to = "var", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(var = factor(var, levels = c("bfq", "bfpc"),
                      labels = c("Families/quota ratio",
                                 "Transfers per capita (BRL-2024)")))

p21 <- ggplot(f21, aes(grp, value, color = grp)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
  facet_wrap(~ var, scales = "free_y") +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  # cap only the quota-ratio panel via per-facet limits trick
  ggh4x::facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(0, 2.5)),
    scale_y_continuous())) +
  labs(title = "Bolsa Família at baseline, by listing group",
       subtitle = paste0("Municipality means, ", min(mod_years), "\u2013",
                         max(mod_years), ". At-risk narrow sample."),
       x = NULL, y = NULL,
       caption = paste0("Quota-ratio panel capped at 2.5 for display; values ",
                        "extend to 11.9 (MT outliers, pending upstream check). ",
                        "Coverage is balanced across groups; transfer intensity ",
                        "is roughly twice as high in never-listed municipalities.")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))

# install.packages("ggh4x")
library(ggh4x)

# ggh4x optional: fall back to uniform free scales if not installed

if (!requireNamespace("ggh4x", quietly = TRUE)) {
  p21 <- ggplot(f21, aes(grp, value, color = grp)) +
    geom_boxplot(outlier.shape = NA, width = 0.5) +
    geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
    facet_wrap(~ var, scales = "free_y") +
    scale_color_brewer(palette = "Dark2", guide = "none") +
    labs(title = "Bolsa Família at baseline, by listing group",
         subtitle = paste0("Municipality means, ", min(mod_years), "\u2013",
                           max(mod_years), ". At-risk narrow sample."),
         x = NULL, y = NULL,
         caption = "Install ggh4x to cap the quota-ratio panel at 2.5.") +
    theme_minimal(base_size = 11)
}
# ggsave("output/figures/fig21_bf_by_group.png", p21,
#        width = 10, height = 5, dpi = 300)

p21

# =============================================================================
# fig22. Map of baseline coverage (all 502 municipalities)
# =============================================================================

library(geobr)
library(sf)

map_sf <- read_municipality(year = 2010, showProgress = FALSE) |>
  mutate(geocode = as.numeric(code_muni)) |>
  inner_join(base, by = "geocode")

p22 <- ggplot(map_sf) +
  geom_sf(aes(fill = bfq), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(option = "mako", direction = -1,
                       limits = c(0, 1.5), oob = scales::squish,
                       name = "Families/quota\nratio, 2006\u201307") +
  labs(title = "Bolsa Família coverage relative to quota, 2006\u20132007",
       caption = paste0("Fill capped at 1.5 (values extend to 11.9; MT outliers ",
                        "pending upstream check). Ratio ~1 = serving the ",
                        "administrative quota.")) +
  theme_void(base_size = 11) +
  theme(legend.position = c(0.15, 0.22),
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig22_coverage_map.png", p22,
#        width = 9, height = 8, dpi = 300)

p22

# =============================================================================
# fig23. Cross-municipal distribution over time
# -----------------------------------------------------------------------------
# Median, IQR, and P10-P90 ribbons, 2004-2019. Shows rollout, saturation,
# and whether dispersion compresses over time.
# =============================================================================

f23 <- df |>
  filter(year >= 2004, year <= 2019) |>
  select(year, bf_families_quota_ratio, bf_transfers_pc_brl_2024) |>
  pivot_longer(-year, names_to = "var", values_to = "v") |>
  filter(!is.na(v)) |>
  group_by(var, year) |>
  summarise(p10 = quantile(v, .1), p25 = quantile(v, .25), p50 = median(v),
            p75 = quantile(v, .75), p90 = quantile(v, .9), .groups = "drop") |>
  mutate(var = factor(var,
                      levels = c("bf_families_quota_ratio",
                                 "bf_transfers_pc_brl_2024"),
                      labels = c("Families/quota ratio",
                                 "Transfers per capita (BRL-2024)")))

p23 <- ggplot(f23, aes(year)) +
  geom_ribbon(aes(ymin = p10, ymax = p90), fill = "#762a83", alpha = 0.15) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = "#762a83", alpha = 0.3) +
  geom_line(aes(y = p50), color = "#762a83", linewidth = 1) +
  facet_wrap(~ var, scales = "free_y") +
  scale_x_continuous(breaks = seq(2004, 2019, 5)) +
  labs(title = "Cross-municipal distribution of Bolsa Família over time",
       subtitle = "Median (line), interquartile range (dark), P10\u2013P90 (light). 502 municipalities.",
       x = NULL, y = NULL,
       caption = "2004\u20132005 reflect program rollout; 2020 unavailable.") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig23_bf_distribution_time.png", p23,
#        width = 10, height = 5, dpi = 300)

p23

# =============================================================================
# fig24. Rank stability of the coverage moderator
# -----------------------------------------------------------------------------
# Percentile rank in 2006-07 vs 2013-14 (at-risk narrow). Spearman ~ 0.41:
# the moderator is provision AT TREATMENT ONSET, not a permanent municipal
# trait. Ranks (not levels) so MT outliers cannot distort the display.
# =============================================================================

late <- df |>
  filter(year %in% 2013:2014) |>
  group_by(geocode) |>
  summarise(bfq_late = mean(bf_families_quota_ratio, na.rm = TRUE),
            .groups = "drop")

f24 <- base |>
  filter(at_risk) |>
  inner_join(late, by = "geocode") |>
  filter(!is.na(bfq), !is.na(bfq_late)) |>
  mutate(r_early = percent_rank(bfq), r_late = percent_rank(bfq_late))

rho <- cor(f24$r_early, f24$r_late, method = "spearman")

p24 <- ggplot(f24, aes(r_early, r_late)) +
  geom_abline(linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.35, size = 1, color = "#762a83") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#d95f02") +
  annotate("text", x = 0.02, y = 0.97, hjust = 0, size = 3.3,
           label = paste0("Spearman \u03c1 = ", sprintf("%.2f", rho))) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Rank stability of Bolsa Família coverage across windows",
       subtitle = "Percentile rank of the families/quota ratio: 2006\u201307 vs 2013\u201314. At-risk narrow sample.",
       x = "Coverage rank, 2006\u20132007",
       y = "Coverage rank, 2013\u20132014",
       caption = paste0("Moderate persistence: the stratification captures ",
                        "provision at treatment onset, not a permanent municipal ",
                        "trait. Motivates the continuous-moderator robustness ",
                        "check and cautions the persistence analysis.")) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig24_rank_stability.png", p24,
#        width = 7.5, height = 6.5, dpi = 300)

p24

# =============================================================================
# fig25. Coverage by state (MT outlier diagnostic)
# =============================================================================

p25 <- base |>
  filter(!is.na(bfq)) |>
  mutate(state = reorder(state, bfq, FUN = median)) |>
  ggplot(aes(state, bfq)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, color = "grey30") +
  geom_jitter(aes(color = bfq > 3), width = 0.15, alpha = 0.5, size = 0.9) +
  scale_color_manual(values = c(`FALSE` = "grey55", `TRUE` = "#d95f02"),
                     guide = "none") +
  coord_cartesian(ylim = c(0, 4)) +
  labs(title = "Baseline coverage by state",
       subtitle = paste0("Families/quota ratio, ", min(mod_years), "\u2013",
                         max(mod_years), " means. Orange: ratio > 3."),
       x = NULL, y = "Families/quota ratio",
       caption = paste0("State medians all sit near 1 (RR lowest at 0.78): ",
                        "coverage saturation was near-universal. The extreme ",
                        "values are isolated MT municipalities (up to 11.9, ",
                        "clipped at 4 here), consistent with a quota-source ",
                        "problem for specific municipal codes rather than a ",
                        "statewide pattern.")) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig25_coverage_by_state.png", p25,
#        width = 9, height = 5.5, dpi = 300)


p25

# =============================================================================
# 08-direct-effect-twfe.R — H2 direct-effect regressions (DESCRIPTIVE)
#
# PURPOSE AND INTERPRETATION LIMITS (read before interpreting output):
# These TWFE regressions document the within-municipality association between
# Bolsa Família and normalized deforestation. The within-variation in BF is
# administrative churn, migration response, and benefit-rule changes that
# co-move with local conditions (see EDA D1/D2 and the coverage-listing DiD).
# Coefficients therefore measure CO-MOVEMENT, not effects. They enter the
# chapter as the documented descriptive examination of H2; causal weight in
# the chapter rests on the moderation design.
#
# Expected headline (verified externally): the transfers-per-capita
# association is robustly positive (~ +0.015 per BRL, t ~ 3), consistent
# with transfers tracking frontier booms; the quota-ratio association is
# fragile and collapses under outcome winsorization.
#
# Requires: fixest  (install.packages("fixest"))
# =============================================================================

library(dplyr)
library(fixest)

# data_path <- "final_dataset_6.csv"
# dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path) |>
  mutate(geocode = as.numeric(geocode)) |>
  arrange(geocode, year) |>
  group_by(geocode) |>
  mutate(bfq_l1  = dplyr::lag(bf_families_quota_ratio),
         bfpc_l1 = dplyr::lag(bf_transfers_pc_brl_2024)) |>
  ungroup()

# primary estimation sample: at-risk narrow (normalized outcome), BF years
nar <- df |>
  filter(at_risk_narrow %in% c(TRUE, "True"), year >= 2004, year <= 2019)

# winsorized outcome (1/99 within estimation sample)
q <- quantile(nar$defor_norm_ref0007, c(.01, .99), na.rm = TRUE)
nar <- nar |>
  mutate(defor_norm_w = pmin(pmax(defor_norm_ref0007, q[1]), q[2]))

# =============================================================================
# Main grid: regressor (quota ratio / transfers pc) x timing (t / t-1)
# x outcome (raw / winsorized), plus price-controlled variants.
# All models: municipality + year FE, SEs clustered by municipality.
# =============================================================================

m <- list(
  bfq        = feols(defor_norm_ref0007 ~ bf_families_quota_ratio | geocode + year,
                     data = nar, cluster = ~geocode),
  bfq_lag    = feols(defor_norm_ref0007 ~ bfq_l1 | geocode + year,
                     data = nar, cluster = ~geocode),
  bfq_wins   = feols(defor_norm_w ~ bf_families_quota_ratio | geocode + year,
                     data = nar, cluster = ~geocode),
  bfq_prices = feols(defor_norm_ref0007 ~ bf_families_quota_ratio +
                       price_index_crop_z + price_index_crop_lag1_z +
                       price_index_cattle_z + price_index_cattle_lag1_z |
                       geocode + year,
                     data = nar, cluster = ~geocode),
  bfpc       = feols(defor_norm_ref0007 ~ bf_transfers_pc_brl_2024 | geocode + year,
                     data = nar, cluster = ~geocode),
  bfpc_lag   = feols(defor_norm_ref0007 ~ bfpc_l1 | geocode + year,
                     data = nar, cluster = ~geocode),
  bfpc_wins  = feols(defor_norm_w ~ bf_transfers_pc_brl_2024 | geocode + year,
                     data = nar, cluster = ~geocode),
  bfpc_prices= feols(defor_norm_ref0007 ~ bf_transfers_pc_brl_2024 +
                       price_index_crop_z + price_index_crop_lag1_z +
                       price_index_cattle_z + price_index_cattle_lag1_z |
                       geocode + year,
                     data = nar, cluster = ~geocode)
)

etable(m, cluster = ~geocode, digits = 3,
       file = "_output/_tables/tab_h2_direct_twfe.tex", replace = TRUE)
print(etable(m, cluster = ~geocode, digits = 3))

# =============================================================================
# Alternative outcome row: deforestation rate over forest stock, forest-floor
# sample (avail_ok). The normalized outcome is undefined-in-practice for
# near-zero-baseline municipalities (EDA B4); the rate outcome permits the
# wider sample and checks functional-form dependence.
# =============================================================================

floor_s <- df |>
  filter(avail_ok %in% c(TRUE, "True"), year >= 2004, year <= 2019)

m_alt <- list(
  rate_bfq  = feols(deforestation_forest_rate ~ bf_families_quota_ratio |
                      geocode + year, data = floor_s, cluster = ~geocode),
  rate_bfpc = feols(deforestation_forest_rate ~ bf_transfers_pc_brl_2024 |
                      geocode + year, data = floor_s, cluster = ~geocode)
)
print(etable(m_alt, cluster = ~geocode, digits = 4))
# etable(m_alt, cluster = ~geocode, digits = 4,
#        file = "output/tables/tab_h2_direct_twfe_ratealt.tex", replace = TRUE)

# =============================================================================
# BAD-CONTROL ILLUSTRATION (do not interpret; documented deliberately).
# Adding ppcdam_list conditions on a variable that responds to the outcome
# (listing selects on deforestation) and that itself affects coverage (the
# coverage-listing DiD). Included only to show the estimate's sensitivity to
# a control a naive specification might reach for.
# =============================================================================

m_bad <- feols(defor_norm_ref0007 ~ bf_transfers_pc_brl_2024 + ppcdam_list |
                 geocode + year, data = nar, cluster = ~geocode)
cat("\nBad-control illustration (ppcdam_list included; do not interpret):\n")
print(coeftable(m_bad))

# =============================================================================
# Notes for the chapter text:
# - Report per-SD magnitudes: transfers-pc coefficient x within-SD (~8.4 BRL)
#   ~= +0.13 SD of normalized deforestation per SD of transfers.
# - The quota-ratio coefficient collapsing under winsorization means the
#   association lives in outcome tails; say so.
# - Frame all of the above as descriptive co-movement (see header).
# =============================================================================
