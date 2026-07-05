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
dir.create("_output/_figures", recursive = TRUE, showWarnings = FALSE)
dir.create("_output/_tables",  recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path)
df
glimpse(df)

count(df$ppcdam_list)

df %>% 
  filter(at_risk_narrow == TRUE) %>% 
  summarise(n_municipalities = n_distinct(geocode))

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
ggsave("output/figures/fig00_missingness_map.png", p_miss,
       width = 9, height = 4.5, dpi = 300)

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
ggsave("output/figures/fig01_policy_timeline.png", p_a1,
       width = 10, height = 6, dpi = 300)

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

ggsave("output/figures/fig02_series_trends.png", p_a2,
       width = 10, height = 6.5, dpi = 300)

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

write.csv(wb_table, "output/tables/tab_d1_variance_decomposition.csv",
          row.names = FALSE)
print(wb_table, n = Inf)

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
# - emp_share_low_workers ~80% within: bracket shares are volatile over
#   time, consistent with their mechanical dependence on the wage/MW ratio;
#   treat as descriptive, not as an MW-policy outcome.

# =============================================================================
# End of Block A + D1. Next: Block B (cross-sectional structure), Block C
# (heterogeneity among listed municipalities), Blocks D2–D3, Block E.
# =============================================================================





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
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

df <- read.csv(data_path)

theme_eda <- theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0),
        legend.position = "bottom")

# =============================================================================
# Fig 03 — Formal employment by wage bracket (requests 1.1, 1.2)
# -----------------------------------------------------------------------------
# Brackets are fixed REAL thresholds (2024 MW, IPCA-deflated wages), so
# migration across brackets reflects real wage growth of formal workers.
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
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p3 <- p3a / p3b +
    plot_annotation(
      title = "Formal employment and the real wage distribution, 2000–2020",
      caption = paste0("Unweighted municipal means. Wage brackets are fixed real ",
                       "thresholds: multiples of the 2024 minimum wage, wages ",
                       "IPCA-deflated to 2024 BRL. Source: RAIS / IBGE Census (PIA)."))
  ggsave("output/figures/fig03_wage_brackets.png", p3,
         width = 9, height = 7.5, dpi = 300)
} else {
  ggsave("output/figures/fig03a_pia_rates.png",  p3a, width = 9, height = 4, dpi = 300)
  ggsave("output/figures/fig03b_emp_shares.png", p3b, width = 9, height = 4, dpi = 300)
}

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
ggsave("output/figures/fig04_formal_agric.png", p4,
       width = 9, height = 6.5, dpi = 300)

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

ggsave("output/figures/fig05_bolsa_familia.png", p5,
       width = 9, height = 7, dpi = 300)

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

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p6 <- p6a / p6b +
    plot_annotation(
      title = "Informality and economic activity, 2000–2020",
      caption = paste0("Informality: census measurements (2000, 2010) shown as ",
                       "points; dashed line is linear interpolation (2010–2020 ",
                       "segment anchored to the 2022 Census) and carries no ",
                       "annual information. GDP: IBGE municipal series, ",
                       "IPCA-deflated to 2024 BRL, available from 2002."))
  ggsave("output/figures/fig06_informal_gdp.png", p6,
         width = 9, height = 7, dpi = 300)
} else {
  ggsave("output/figures/fig06a_informal.png", p6a, width = 9, height = 3.5, dpi = 300)
  ggsave("output/figures/fig06b_gdp.png",      p6b, width = 9, height = 3.5, dpi = 300)
}

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

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p7 <- p7a / p7b / p7c +
    plot_annotation(
      title = "Agricultural expansion and price exposure, 2000–2020",
      caption = paste0("PAM (crops), PPM (cattle), World Bank Pink Sheet (prices). ",
                       "Totals describe the 502-municipality sample; major ",
                       "Cerrado-MT soy municipalities lie outside it. Pre-2005 ",
                       "cattle growth partly reflects early-period PPM ",
                       "measurement noise."))
  ggsave("output/figures/fig07_agriculture_prices.png", p7,
         width = 9, height = 10, dpi = 300)
} else {
  ggsave("output/figures/fig07a_crops.png",  p7a, width = 9, height = 3.5, dpi = 300)
  ggsave("output/figures/fig07b_cattle.png", p7b, width = 9, height = 3.5, dpi = 300)
  ggsave("output/figures/fig07c_prices.png", p7c, width = 9, height = 3.5, dpi = 300)
}

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

ggsave("output/figures/fig08_combined_trends.png", p8,
       width = 10.5, height = 6.5, dpi = 300)

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
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

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

ggsave("output/figures/fig09_defor_vs_groups.png", p9,
       width = 10, height = 7.5, dpi = 300)
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

# df <- read.csv("final_dataset_6.csv")   # uncomment if running standalone
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

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

ggsave("output/figures/fig09_defor_vs_groups.png", p9,
       width = 10, height = 7.5, dpi = 300)


p9
glimpse(df)
