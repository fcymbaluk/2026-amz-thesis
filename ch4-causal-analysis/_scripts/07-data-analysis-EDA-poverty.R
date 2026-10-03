# =============================================================================
# 09-atlas-poverty.R — Atlas Brasil poverty data: merge and analyses
#   1. Name-based merge of Atlas indicators to IBGE geocodes (with aliases)
#   2. fig26: social geography of the frontier (poverty/income vs baseline
#      deforestation)
#   3. fig27: the tracking loop (transfers follow poverty; coverage does not)
#   4. Stratum balance additions and the poverty-based rival stratification
#      assignment (consumed later by the stratified C&S script)
#
# Inputs : data__1_.xlsx (Atlas Brasil), ibge_geocode.xlsx, final_dataset_6.csv
# Outputs: data/atlas_poverty_geocoded.csv, output/figures/fig26-27,
#          output/tables/tab_poverty_balance.csv, output/tables/strata_rival.csv
# Requires: readxl, stringi
# =============================================================================

rm(list = ls(all = TRUE))
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)
library(stringi)

data_path <- "_data/final_dataset_6.csv"
# dir.create("_data", showWarnings = FALSE)
# dir.create("_output/figures", recursive = TRUE, showWarnings = FALSE)
# dir.create("_output/tables",  recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. Merge Atlas -> geocodes
# -----------------------------------------------------------------------------
# Both files carry "Name (UF)". Normalization: lowercase, strip accents,
# unify apostrophes/hyphens, collapse spaces. Five known spelling variants
# are aliased. Validation asserts full coverage of the 502-municipality
# sample; if it fails, inspect the printed unmatched names.
# =============================================================================

norm_key <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    tolower() |>
    gsub("[\u2019'`\u00b4-]", " ", x = _) |>
    gsub("\\s+", " ", x = _) |>
    trimws()
}

aliases <- c(
  "eldorado dos carajas (pa)"      = "eldorado do carajas (pa)",
  "santa isabel do para (pa)"      = "santa izabel do para (pa)",
  "fortaleza do tabocao (to)"      = "tabocao (to)",
  "poxoreo (mt)"                   = "poxoreu (mt)",
  "santo antonio do leverger (mt)" = "santo antonio de leverger (mt)"
)

atlas <- read_excel("_data/raw-atlas-brasil-pobreza.xlsx") |>
  rename(name = Territorialidades) |>
  mutate(key = norm_key(name),
         key = ifelse(key %in% names(aliases), aliases[key], key))

glimpse(atlas)

ibge <- read_excel("_data/raw_censo_labor_population/ibge_geocode.xlsx") |>
  mutate(
    geocode = as.numeric(geocode),
    key = norm_key(munic)
    )

tail(ibge)
sum(is.na(ibge$key))

atlas_geo <- atlas |>
  left_join(ibge |> select(key, geocode), by = "key") |>
  filter(!is.na(geocode)) |>
  distinct(geocode, .keep_all = TRUE) |>
  rename_with(\(x) x |>
                gsub("% de extremamente pobres", "ext", x = _) |>
                gsub("% de pobres", "pov", x = _) |>
                gsub("% de vulner\u00e1veis \u00e0 pobreza", "vul", x = _) |>
                gsub("\u00cdndice de Gini", "gini", x = _) |>
                gsub("Renda per capita", "rpc", x = _) |>
                gsub("Raz\u00e3o 10% mais ricos / 40% mais pobres", "r1040", x = _) |>
                gsub(" ", "_", x = _))


glimpse(atlas_geo)

df <- read.csv(data_path) |> mutate(geocode = as.numeric(geocode))

sample_geo <- unique(df$geocode)

matched <- sum(sample_geo %in% atlas_geo$geocode)

cat("Sample coverage:", matched, "of", length(sample_geo), "\n")
if (matched < length(sample_geo)) {
  print(df |> filter(!geocode %in% atlas_geo$geocode) |>
          distinct(municipality, state_abbreviation))
}
stopifnot(matched == length(sample_geo))

write.csv(atlas_geo, "_data/outcome_atlas_poverty_geocoded.csv", row.names = FALSE)

# =============================================================================
# 2. Municipality cross-section for the analyses
# =============================================================================

first_listed <- df |>
  filter(ppcdam_list == 1) |>
  group_by(geocode) |>
  summarise(cohort = min(year), .groups = "drop")

glimpse(first_listed)
glimpse(atlas_geo)
glimpse(df)

cs <- df |>
  group_by(geocode) |>
  summarise(
    base_defor = mean(deforestation_area_km2[year %in% 2000:2007], na.rm = TRUE),
    bfq  = mean(bf_families_quota_ratio[year %in% 2006:2007], na.rm = TRUE),
    bfpc = mean(bf_transfers_pc_brl_2024[year %in% 2006:2007], na.rm = TRUE),
    at_risk = any(at_risk_narrow %in% c(TRUE, "True")),
    .groups = "drop") |>
  left_join(atlas_geo |> select(geocode, pov_2000, ext_2000, gini_2000, rpc_2000),
            by = "geocode") |>
  left_join(first_listed, by = "geocode")

nar <- cs |> filter(at_risk)

# =============================================================================
# fig26. Social geography: the frontier is richer, not poorer
# -----------------------------------------------------------------------------
# rho(baseline defor, % poor 2000) ~ -0.39; rho(defor, income pc) ~ +0.43.
# Deforestation concentrates in relatively prosperous municipalities:
# capital-driven clearing, not survival clearing.
# =============================================================================

f26 <- nar |>
  filter(base_defor > 0) |>
  pivot_longer(c(pov_2000, rpc_2000), names_to = "var", values_to = "v") |>
  filter(!is.na(v)) |>
  mutate(var = factor(var, levels = c("pov_2000", "rpc_2000"),
                      labels = c("% poor, 2000", "Income per capita, 2000 (BRL-Aug/2010)")))

r26 <- f26 |> group_by(var) |>
  summarise(r = cor(base_defor, v, method = "spearman"), .groups = "drop") |>
  mutate(lab = paste0("Spearman \u03c1 = ", sprintf("%+.2f", r)))

p26 <- ggplot(f26, aes(base_defor, v)) +
  geom_point(alpha = 0.35, size = 1, color = "grey25") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#d95f02") +
  geom_text(data = r26, aes(label = lab), x = Inf, y = Inf,
            hjust = 1.1, vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ var, scales = "free_y") +
  scale_x_log10(labels = label_comma()) +
  labs(title = "The social geography of the deforestation frontier",
       subtitle = "Census-2000 poverty and income against baseline deforestation. At-risk narrow sample.",
       x = expression("Mean annual deforestation 2000\u20132007 (km"^2*", log scale)"),
       y = NULL,
       caption = paste0("Deforestation concentrates in less poor, higher-income ",
                        "municipalities: consistent with capital-intensive ",
                        "clearing rather than poverty-driven clearing. Atlas ",
                        "Brasil (PNUD/Ipea/FJP); poverty line R$140/month, ",
                        "Aug-2010 reais.")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig26_social_geography.png", p26,
#        width = 10, height = 5, dpi = 300)

p26
# =============================================================================
# fig27. The tracking loop, closed
# -----------------------------------------------------------------------------
# rho(transfers pc, % poor 2000) ~ +0.75: transfer volume is the poverty map.
# rho(quota ratio, % poor 2000) ~ -0.02: provision relative to need is
# orthogonal to poverty. This is the empirical basis for the moderator
# choice and for interpreting the positive H2 transfers coefficient as
# tracking, not effect.
# =============================================================================

f27 <- nar |>
  pivot_longer(c(bfpc, bfq), names_to = "var", values_to = "v") |>
  filter(!is.na(v), !is.na(pov_2000)) |>
  mutate(var = factor(var, levels = c("bfpc", "bfq"),
                      labels = c("Transfers per capita, 2006\u201307 (BRL-2024)",
                                 "Families/quota ratio, 2006\u201307")))

r27 <- f27 |> group_by(var) |>
  summarise(r = cor(pov_2000, v, method = "spearman"), .groups = "drop") |>
  mutate(lab = paste0("Spearman \u03c1 = ", sprintf("%+.2f", r)))

p27 <- ggplot(f27, aes(pov_2000, v)) +
  geom_point(alpha = 0.35, size = 1, color = "grey25") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#762a83") +
  geom_text(data = r27, aes(label = lab), x = -Inf, y = Inf,
            hjust = -0.1, vjust = 1.5, size = 3.2, inherit.aes = FALSE) +
  facet_wrap(~ var, scales = "free_y") +
  labs(title = "Transfers follow poverty; coverage relative to need does not",
       subtitle = "Bolsa Fam\u00edlia measures against the census-2000 poverty rate. At-risk narrow sample.",
       x = "% poor, 2000 (Atlas Brasil)", y = NULL,
       caption = paste0("Quota-ratio panel: values above 3 (MT outliers) retained; ",
                        "the near-zero rank correlation is robust to them.")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))

# ggsave("output/figures/fig27_tracking_loop.png", p27,
#        width = 10, height = 5, dpi = 300)

p27
# =============================================================================
# 3. Stratum balance additions and the rival stratification
# =============================================================================

treated <- nar |> filter(cohort %in% c(2008, 2009), !is.na(bfq))
med_cov <- median(treated$bfq)
med_pov <- median(treated$pov_2000, na.rm = TRUE)

smd <- function(a, b) (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE)) /
  sqrt((var(a, na.rm = TRUE) + var(b, na.rm = TRUE)) / 2)

hi <- treated$bfq > med_cov
bal <- tibble(
  variable = c("% poor 2000", "% extreme poor 2000", "Gini 2000",
               "Income pc 2000"),
  smd_high_vs_low_coverage = c(
    smd(treated$pov_2000[hi],  treated$pov_2000[!hi]),
    smd(treated$ext_2000[hi],  treated$ext_2000[!hi]),
    smd(treated$gini_2000[hi], treated$gini_2000[!hi]),
    smd(treated$rpc_2000[hi],  treated$rpc_2000[!hi])))
# write.csv(bal, "output/tables/tab_poverty_balance.csv", row.names = FALSE)
print(bal)

# rival stratification assignment: consumed by the stratified C&S script.
# Poverty split and coverage split agree on ~25 of 43 municipalities:
# near-independent partitions, so the rival run is a genuine falsification
# test of "provision, not poverty".
strata <- treated |>
  transmute(geocode,
            stratum_coverage = ifelse(bfq > med_cov, "high", "low"),
            stratum_poverty  = ifelse(pov_2000 > med_pov, "high", "low"))
# write.csv(strata, "output/tables/strata_rival.csv", row.names = FALSE)
cat("\nCoverage x poverty split cross-tab:\n")
print(table(strata$stratum_coverage, strata$stratum_poverty))


# =============================================================================
# 10-frontier-timing.R — Poverty dynamics and frontier timing (DESCRIPTIVE)
#   A. fig28: three-census poverty trajectories by 2000s clearing intensity
#   B. fig29: convergence-adjusted poverty change vs clearing intensity (null)
#   C. Frontier timing from PRIMARY suppression only (MapBiomas raw):
#      t50 = median clearing year; left-censoring flag from 1987 forest share
#   D. fig30: relative poverty-rank trajectories by frontier-timing group
#      (tests boom-and-bust; finds persistent divergence instead)
#   E. exports data/frontier_timing.csv for later use
#
# Inputs : raw-environ-mapbiomas-deforestation-municipalities.xlsx (raw),
#          outcome_deforestation.xlsx (tidy panel, for 1987 forest share),
#          final_dataset_6.csv, data/atlas_poverty_geocoded.csv
# Requires: readxl
#
# HEADLINE (verified externally): no boom-bust visible through 2010.
# Early frontiers (t50<=1995) improve relative poverty rank monotonically
# (0.50 -> 0.47 -> 0.40; robust to excluding pre-1987-consolidated places);
# late frontiers (t50>=2006) fall behind (0.59 -> 0.62 -> 0.67);
# rho(t50, rank change 2000->2010) = +0.29. Marginal clearing buys no
# contemporaneous dividend (fig29); consolidation, not clearing, carries
# the prosperity. All descriptive.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)

data_path <- "_data/final_dataset_6.csv"
# dir.create("data", showWarnings = FALSE)
# dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

df    <- read.csv(data_path) |> mutate(geocode = as.numeric(geocode))
atlas <- read.csv("_data/outcome_atlas_poverty_geocoded.csv") |>
  mutate(geocode = as.numeric(geocode))
nar_ids <- df |> group_by(geocode) |>
  summarise(a = any(at_risk_narrow %in% c(TRUE, "True"))) |>
  filter(a) |> pull(geocode)

# =============================================================================
# A/B. Decade intensity analyses (uses the panel's combined measure: this is
# the outcome concept, primary+secondary, as in the causal chapter)
# =============================================================================

dec <- df |>
  group_by(geocode) |>
  summarise(cum_0110 = sum(deforestation_area_km2[year %in% 2001:2010], na.rm = TRUE),
            area = first(munic_area_km2), .groups = "drop") |>
  mutate(intensity = 100 * cum_0110 / area) |>
  left_join(atlas |> select(geocode, pov_1991, pov_2000, pov_2010,
                            rpc_2000, rpc_2010), by = "geocode") |>
  filter(geocode %in% nar_ids, !is.na(pov_2000), !is.na(pov_2010)) |>
  mutate(q = ntile(intensity, 4),
         q = factor(q, labels = paste0("Q", 1:4, c(" (lowest)", "", "", " (highest)"))),
         dpov = pov_2010 - pov_2000)

# fig28 — parallel falling lines ARE the result
f28 <- dec |>
  group_by(q) |>
  summarise(across(c(pov_1991, pov_2000, pov_2010), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(-q, names_to = "census", values_to = "pov") |>
  mutate(year = as.integer(sub("pov_", "", census)))

p28 <- ggplot(f28, aes(year, pov, color = q)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_viridis_d(option = "rocket", end = 0.8, name = "2001\u201310 clearing\nintensity quartile") +
  scale_x_continuous(breaks = c(1991, 2000, 2010)) +
  labs(title = "Poverty fell everywhere, regardless of clearing intensity",
       subtitle = "Mean poverty rate at three censuses, by quartile of 2001\u20132010 deforestation intensity (% of municipal area).",
       x = NULL, y = "% poor",
       caption = paste0("At-risk narrow sample. Near-parallel declines: the 2000s ",
                        "poverty reduction was a national tide, not a frontier ",
                        "dividend. Descriptive.")) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))
# ggsave("output/figures/fig28_pov_by_intensity.png", p28, width = 8.5, height = 5.5, dpi = 300)

p28


# fig29 — convergence-adjusted change vs intensity: the null made visible
conv <- lm(dpov ~ pov_2000, data = dec)
dec$dpov_resid <- resid(conv)[match(rownames(dec), names(resid(conv)))]
dec$dpov_resid <- dec$dpov - predict(conv, dec)
rho29 <- cor(dec$intensity, dec$dpov_resid, method = "spearman", use = "complete.obs")

p29 <- ggplot(dec, aes(intensity, dpov_resid)) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
  geom_point(alpha = 0.35, size = 1, color = "grey25") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#d95f02") +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3.2,
           label = paste0("Spearman \u03c1 = ", sprintf("%+.2f", rho29))) +
  labs(title = "Clearing more bought no extra poverty reduction",
       subtitle = "Change in poverty 2000\u20132010, residualized on the 2000 level (convergence), against decade clearing intensity.",
       x = "Cumulative deforestation 2001\u20132010 (% of municipal area)",
       y = "Poverty change 2000\u20132010, convergence-adjusted (pp)",
       caption = "Negative values: fell more than convergence predicts. Descriptive.") +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(size = 7.5, hjust = 0))
# ggsave("output/figures/fig29_convergence_null.png", p29, width = 8.5, height = 5.5, dpi = 300)

p29

# =============================================================================
# C. Frontier timing from PRIMARY suppression only
# -----------------------------------------------------------------------------
# t50 = first year by which half of a municipality's 1987-2023 primary-
# vegetation suppression had occurred. Secondary suppression is excluded:
# reclearing of regrowth is a post-frontier signature and would blur timing.
# Left-censoring: places largely cleared before 1987 get a flag from the
# 1987 forest share (tidy panel area_forest, ha; munic_area_km2 * 100).
# =============================================================================

raw <- read_excel("_data/raw-environ-mapbiomas-deforestation-municipalities.xlsx",
                  sheet = "DEF_SECVEG")
yr_cols <- names(raw)[grepl("^(19|20)\\d\\d$", names(raw))]
prim <- raw |>
  mutate(geocode = as.numeric(geocode)) |>
  filter(geocode %in% unique(df$geocode),
         transition_name == "Supress\u00e3o Veg. Prim\u00e1ria") |>
  group_by(geocode) |>
  summarise(across(all_of(yr_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  pivot_longer(-geocode, names_to = "year", values_to = "supp") |>
  mutate(year = as.integer(year)) |>
  filter(year >= 1987)

prim

t50_tab <- prim |>
  arrange(geocode, year) |>
  group_by(geocode) |>
  mutate(cum = cumsum(supp), half = sum(supp) / 2) |>
  summarise(t50 = year[which(cum >= half)[1]], .groups = "drop")

tidy <- read_excel("_data/outcome_deforestation.xlsx")
fs87 <- tidy |>
  filter(year == 1987) |>
  transmute(geocode = as.numeric(geocode), forest_ha_87 = area_forest) |>
  left_join(df |> group_by(geocode) |>
              summarise(area_ha = first(munic_area_km2) * 100), by = "geocode") |>
  mutate(forest_share_87 = forest_ha_87 / area_ha)

timing <- t50_tab |>
  left_join(fs87 |> select(geocode, forest_share_87), by = "geocode") |>
  mutate(grp = cut(t50, c(1986, 1995, 2005, 2023),
                   labels = c("Early frontier (t50 \u2264 1995)",
                              "Mid frontier (1996\u20132005)",
                              "Late frontier (2006+)")),
         left_censored = forest_share_87 < 0.5)
# write.csv(timing, "data/frontier_timing.csv", row.names = FALSE)
cat("Timing groups:\n"); print(table(timing$grp))
cat("Left-censored (forest share 1987 < 0.5):", sum(timing$left_censored, na.rm = TRUE), "\n")

# =============================================================================
# D. fig30 — relative poverty-rank trajectories by frontier-timing group
# -----------------------------------------------------------------------------
# Percentile ranks strip the national tide; the boom-bust prediction is a
# rise-then-fade arc for early frontiers. Observed: monotone divergence.
# =============================================================================

w <- timing |>
  left_join(atlas |> select(geocode, pov_1991, pov_2000, pov_2010), by = "geocode") |>
  filter(geocode %in% nar_ids, !is.na(grp),
         !is.na(pov_1991), !is.na(pov_2000), !is.na(pov_2010)) |>
  mutate(across(c(pov_1991, pov_2000, pov_2010), \(x) percent_rank(x),
                .names = "rk_{.col}"))

f30 <- bind_rows(
  w |> mutate(set = "All municipalities"),
  w |> filter(!left_censored | grp != "Early frontier (t50 \u2264 1995)") |>
    mutate(set = "Excluding left-censored early group")
) |>
  group_by(set, grp) |>
  summarise(across(starts_with("rk_"), mean), n = n(), .groups = "drop") |>
  pivot_longer(starts_with("rk_"), names_to = "census", values_to = "rank") |>
  mutate(year = as.integer(sub("rk_pov_", "", census)))

p30 <- ggplot(f30, aes(year, rank, color = grp)) +
  geom_hline(yintercept = 0.5, linewidth = 0.3, color = "grey60") +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~ set) +
  scale_color_manual(values = c("#1b7837", "#762a83", "#d95f02"), name = NULL) +
  scale_x_continuous(breaks = c(1991, 2000, 2010)) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "No boom-and-bust through 2010: frontiers diverge and stay diverged",
       subtitle = paste0("Mean poverty percentile rank (higher = poorer) at three ",
                         "censuses, by frontier-timing group (t50 of primary-",
                         "vegetation suppression, 1987\u20132023)."),
       x = NULL, y = "Poverty rank within sample",
       caption = paste0("At-risk narrow sample. Early frontiers improve their ",
                        "relative position monotonically; late frontiers fall ",
                        "behind even as clearing arrives. Right panel drops early-",
                        "group municipalities already < 50% forested in 1987 ",
                        "(timing left-censored). The bust for mid/late frontiers, ",
                        "if any, lies beyond the 2010 census. Descriptive; the ",
                        "late group's decline is confounded with remoteness.")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 9),
        plot.caption = element_text(size = 7.5, hjust = 0))
# ggsave("output/figures/fig30_frontier_divergence.png", p30,
#        width = 10.5, height = 6, dpi = 300)

p30
