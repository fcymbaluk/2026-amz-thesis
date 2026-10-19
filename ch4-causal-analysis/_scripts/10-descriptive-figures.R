# 10-descriptive-figures.R
# Purpose  Descriptive sprint, displays D1-D6 (rebuild/06-descriptive-sprint.md,
#          section 6): co-movement 2004-2020, municipalities versus states,
#          grouped trajectories, structure (land-based dependence, rural and
#          urban), long differences, summary tables. Analysis layer; pipeline
#          01-06 untouched.
# Inputs   _data/final/panel_amz_2000_2020_descriptive.rds        script 09
#          _data/interim/siconfi_expenditure_munic_2000_2024.rds   script 09
#          _data/interim/siconfi_expenditure_uf_2013_2024.rds      script 09
#          geobr municipality shapes, 2010 (downloaded by geobr)
#          _scripts/utils/theme_ch4.R                              task 4.4
#          _scripts/utils/screen_siconfi.R                         D-2026-10-16-b
# Outputs  output/figures/fig-comovement-natural-units.png (D1)
#          output/figures/fig-comovement-indexed.png (D1)
#          output/figures/fig-municipal-revenue-sources.png (D1)
#          output/figures/fig-states-vs-municipalities.png (D2)
#          output/tables/tab-municipal-share-social-spending.csv (D2)
#          output/figures/fig-trajectories-listing.png (D3)
#          output/figures/fig-trajectories-terciles.png (D3)
#          output/figures/fig-structure-distributions.png (D4)
#          output/figures/fig-structure-maps.png (D4)
#          output/tables/tab-structure-crosstab.csv (D4)
#          output/figures/fig-long-differences.png (D5)
#          output/tables/tab-long-differences.csv (D5)
#          output/tables/tab-summary-by-period.csv, -long.csv (D6)
#          output/tables/tab-summary-by-group.csv (D6)
#          output/tables/tab-within-between.csv (D6)
# Status   2026-10-16, sprint day 4. Group definitions D-2026-10-16-a and
#          the SICONFI filing screen D-2026-10-16-b in DECISIONS.md.

library(tidyverse)   # magrittr %>% throughout, never the native pipe
library(scales)
library(sf)
library(geobr)
library(cowplot)

source("_scripts/utils/theme_ch4.R")
source("_scripts/utils/screen_siconfi.R")

# ---- Constants --------------------------------------------------------------
# Aggregation rules, as in script 07: totals (area, money) are sums across
# municipalities; rates, ratios and per-capita values are unweighted means
# across municipalities (the average municipality, the unit of the design).
# SICONFI reporting gaps are shown as the count of reporting municipalities,
# never imputed (D-2026-10-15-c).
N_MUNIC_EXPECTED  <- 502L
PANEL_YEARS       <- 2000:2020
COMOVE_YEARS      <- 2004:2020
EXP_YEARS         <- 2002:2024
BASELINE_YEARS    <- 2004:2007      # the long-difference and tercile baseline
END_YEARS         <- 2017:2020      # the long-difference end window
DECADE_START      <- 2000:2002      # windows around the 2000 and 2010 censuses
DECADE_END        <- 2010:2012
MIN_BASELINE_REPORTS <- 2L          # SICONFI years needed for the spending tercile
BF_INDEX_BASE     <- 2006L          # coverage indexed after the rollout (script 07)
URBAN_BREAKS      <- c(0, 0.5, 0.75, 1)                     # D-2026-10-16-a
URBAN_CLASSES     <- c("Rural (urban share below 0.5)",
                       "Intermediate (0.5 to 0.75)",
                       "Urban (0.75 and above)")
TERCILE_LABELS    <- c("Low", "Middle", "High")
SHAPE_YEAR        <- 2010L

SERIES_LABELS <- c(
  defor      = "Deforestation (km², sum)",
  bf_cov     = "BF coverage (families / quota)",
  bf_pc      = "BF transfers per capita (R$ 2024)",
  emp        = "Formal employment (jobs / working-age pop.)",
  gdp_pc     = "GDP per capita (R$ 2024)",
  health_pc  = "Health spending per capita (R$ 2024)",
  educ_pc    = "Education spending per capita (R$ 2024)",
  assist_pc  = "Social assistance spending per capita (R$ 2024)",
  social_pc  = "Social spending per capita (R$ 2024)"
)

# Filing screen (D-2026-10-16-b): the counts below are the logged ones.
N_FLAGGED_EXP_EXPECTED <- 16L     # municipality-years, 808 municipalities
N_FLAGGED_REV_EXPECTED <- 26L
N_FLAGGED_SAMPLE_EXPECTED <- 16L  # distinct municipality-years in the sample

# ---- Read -------------------------------------------------------------------
panel     <- read_rds("_data/final/panel_amz_2000_2020_descriptive.rds")
exp_munic <- read_rds("_data/interim/siconfi_expenditure_munic_2000_2024.rds")
exp_uf    <- read_rds("_data/interim/siconfi_expenditure_uf_2013_2024.rds")
rev_munic <- read_rds("_data/interim/siconfi_revenue_munic_2000_2024.rds")
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# ---- Assert: input state ----------------------------------------------------
stopifnot(
  n_distinct(panel$geocode) == N_MUNIC_EXPECTED,
  setequal(panel$year, PANEL_YEARS),
  !anyDuplicated(paste(panel$geocode, panel$year)),
  all(c("deforestation_area_km2", "bf_families_quota_ratio",
        "bf_transfers_pc_brl_2024", "emp_pia_rate_total",
        "gdp_per_capita_2024", "exp_social_pc_brl_2024",
        "exp_health_pc_brl_2024", "exp_education_pc_brl_2024",
        "exp_social_assistance_pc_brl_2024", "va_agro_share",
        "poverty_share_c2000", "poverty_share_c2010", "urban_share_c2000",
        "urban_share_c2010", "ppcdam_list", "rev_own_pc_brl_2024",
        "rev_fpm_pc_brl_2024", "rev_sus_pc_brl_2024",
        "rev_fundeb_pc_brl_2024", "rev_fnde_pc_brl_2024",
        "rev_fnas_pc_brl_2024") %in% names(panel)),
  all(!is.na(panel$deforestation_area_km2)),
  all(panel$urban_share_c2010 >= 0 & panel$urban_share_c2010 <= 1),
  setequal(exp_uf$year, 2013:2024),
  n_distinct(exp_uf$state_abbreviation) == 9L
)

# ---- Transform: SICONFI filing screen (D-2026-10-16-b) ---------------------
# Flags computed on the 808-municipality interim files, applied to the
# expenditure columns of those files and to the exp_* and rev_* columns of
# the panel. Flagged cells become reporting gaps; nothing is corrected.
flags_exp <- siconfi_flags(exp_munic, "exp_total_pc_brl_2024")
flags_rev <- siconfi_flags(rev_munic, "rev_current_pc_brl_2024")
exp_munic <- siconfi_screen(exp_munic, flags_exp,
                            names(exp_munic)[str_starts(names(exp_munic), "exp_")])
panel <- panel %>%
  siconfi_screen(flags_exp, names(panel)[str_starts(names(panel), "exp_")]) %>%
  siconfi_screen(flags_rev, names(panel)[str_starts(names(panel), "rev_")])
flags_sample <- bind_rows(flags_exp, flags_rev) %>%
  filter(geocode %in% panel$geocode) %>%
  distinct(geocode, year)
message("SICONFI screen: ", nrow(flags_exp), " expenditure and ",
        nrow(flags_rev), " revenue municipality-years flagged (808 ",
        "municipalities); ", nrow(flags_sample), " in the sample.")
stopifnot(
  nrow(flags_exp) == N_FLAGGED_EXP_EXPECTED,
  nrow(flags_rev) == N_FLAGGED_REV_EXPECTED,
  nrow(flags_sample) == N_FLAGGED_SAMPLE_EXPECTED
)

# ---- Transform: municipal groups (D3, D4, D5, D6) ---------------------------
# D-2026-10-16-a: baseline windows and class boundaries.
tercile <- function(x) {
  cut(x, quantile(x, c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = TERCILE_LABELS, include.lowest = TRUE)
}
window_mean <- function(x, year, years, min_n = 1L) {
  v <- x[year %in% years]
  if (sum(!is.na(v)) < min_n) NA_real_ else mean(v, na.rm = TRUE)
}
munic <- panel %>%
  group_by(geocode, municipality, state_abbreviation) %>%
  summarise(
    ever_listed = any(ppcdam_list == 1L),
    spend_base = window_mean(exp_social_pc_brl_2024, year, BASELINE_YEARS,
                             MIN_BASELINE_REPORTS),
    spend_end  = window_mean(exp_social_pc_brl_2024, year, END_YEARS,
                             MIN_BASELINE_REPORTS),
    land_base  = window_mean(va_agro_share, year, BASELINE_YEARS),
    land_end   = window_mean(va_agro_share, year, END_YEARS),
    defor_base = window_mean(deforestation_area_km2, year, BASELINE_YEARS),
    defor_end  = window_mean(deforestation_area_km2, year, END_YEARS),
    defor_decade_start = window_mean(deforestation_area_km2, year, DECADE_START),
    defor_decade_end   = window_mean(deforestation_area_km2, year, DECADE_END),
    poverty_c2000 = first(poverty_share_c2000),
    poverty_c2010 = first(poverty_share_c2010),
    urban_c2000 = first(urban_share_c2000),
    urban_c2010 = first(urban_share_c2010),
    population_2010 = first(population[year == 2010L]),
    .groups = "drop"
  ) %>%
  mutate(
    listing = factor(if_else(ever_listed, "Ever listed", "Never listed"),
                     levels = c("Ever listed", "Never listed")),
    spend_tercile   = tercile(spend_base),
    poverty_tercile = tercile(poverty_c2000),
    land_tercile    = tercile(land_base),
    urban_class = cut(urban_c2010, URBAN_BREAKS, labels = URBAN_CLASSES,
                      include.lowest = TRUE, right = FALSE)
  )
panel_g <- panel %>%
  left_join(munic %>% select(geocode, listing, spend_tercile, poverty_tercile,
                             land_tercile, urban_class),
            by = "geocode")

# ---- D1: co-movement 2004-2020 ----------------------------------------------
agg <- panel %>%
  filter(year %in% COMOVE_YEARS) %>%
  group_by(year) %>%
  summarise(
    defor     = sum(deforestation_area_km2),
    bf_cov    = mean(bf_families_quota_ratio, na.rm = TRUE),
    bf_pc     = mean(bf_transfers_pc_brl_2024, na.rm = TRUE),
    emp       = mean(emp_pia_rate_total, na.rm = TRUE),
    gdp_pc    = mean(gdp_per_capita_2024, na.rm = TRUE),
    health_pc = mean(exp_health_pc_brl_2024, na.rm = TRUE),
    educ_pc   = mean(exp_education_pc_brl_2024, na.rm = TRUE),
    assist_pc = mean(exp_social_assistance_pc_brl_2024, na.rm = TRUE),
    social_pc = mean(exp_social_pc_brl_2024, na.rm = TRUE),
    n_reporting = sum(!is.na(exp_social_pc_brl_2024)),
    .groups = "drop"
  )
agg_long <- agg %>%
  select(-n_reporting) %>%
  pivot_longer(-year, names_to = "key", values_to = "value") %>%
  filter(is.finite(value)) %>%
  mutate(series = factor(SERIES_LABELS[key], levels = SERIES_LABELS))
reporting_note <- paste0(
  "Municipalities reporting to SICONFI: ", min(agg$n_reporting), " to ",
  max(agg$n_reporting), " of 502 a year (", agg$n_reporting[agg$year == 2008L],
  " in 2008)."
)
caption_rules <- paste0(
  "502 municipalities of the Amazon biome. Deforestation: sample sum. ",
  "Rates, ratios and per-capita values: unweighted municipal means. ",
  "Shaded band: the 2004-2012 decline. ", reporting_note
)

p_d1_natural <- ggplot(agg_long %>% filter(key != "social_pc"),
                       aes(year, value)) +
  layer_decline_band() +
  geom_line(colour = PAL_CH4_GROUPS[1], linewidth = 0.9) +
  facet_wrap(~ series, scales = "free_y", ncol = 4,
             labeller = label_wrap_gen(28)) +
  scale_x_years(2004L, 2020L) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = NULL, y = NULL,
       title = "Deforestation, social policy and municipal social spending, 2004-2020",
       subtitle = "Sample aggregates in natural units",
       caption = str_wrap(paste0(caption_rules, " Bolsa Família ends in 2019."),
                          150)) +
  theme_ch4()
save_fig(p_d1_natural, "comovement-natural-units", width = 11, height = 6)

# Indexed overlay: deforestation repeated as the reference in every panel.
idx <- agg_long %>%
  mutate(base = if_else(key == "bf_cov", BF_INDEX_BASE, CH4_INDEX_BASE)) %>%
  group_by(key) %>%
  mutate(index = index_to_base(value, year, base[1])) %>%
  ungroup() %>%
  filter(is.finite(index))
idx_panels <- tribble(
  ~key,        ~panel,
  "bf_cov",    "A. Social protection",
  "bf_pc",     "A. Social protection",
  "gdp_pc",    "B. Income and employment",
  "emp",       "B. Income and employment",
  "health_pc", "C. Municipal social spending by function",
  "educ_pc",   "C. Municipal social spending by function",
  "assist_pc", "C. Municipal social spending by function"
)
idx_series <- idx %>% inner_join(idx_panels, by = "key")
idx_defor <- idx %>% filter(key == "defor") %>% select(year, index) %>%
  crossing(panel = unique(idx_panels$panel))
p_d1_index <- ggplot() +
  layer_decline_band() +
  layer_defor_reference(idx_defor, aes(year, index)) +
  geom_line(data = idx_series, aes(year, index, colour = series),
            linewidth = 0.85) +
  geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
  facet_wrap(~ panel, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = PAL_CH4_GROUPS,
                      labels = function(x) str_wrap(x, 30)) +
  scale_x_years(2004L, 2020L) +
  labs(x = NULL, y = "Index (2004 = 100)",
       title = "Each family of social series against the deforestation curve",
       subtitle = paste0("Thick grey line: deforestation (sample sum). Series ",
                         "indexed to 2004 = 100; BF coverage to 2006 = 100, ",
                         "after the rollout."),
       caption = str_wrap(caption_rules, 150)) +
  theme_ch4() +
  theme(panel.spacing.x = unit(2, "lines")) +
  guides(colour = guide_legend(nrow = 3))
save_fig(p_d1_index, "comovement-indexed", width = 11, height = 5.5)

# Municipal revenue by source, per capita: own revenue, FPM and the
# conditioned transfers that fund the social functions (Alcaraz 2024).
rev_agg <- panel %>%
  filter(year %in% 2002:2020, !is.na(rev_fpm_pc_brl_2024)) %>%
  group_by(year) %>%
  summarise(
    `Own revenue` = mean(rev_own_pc_brl_2024, na.rm = TRUE),
    FPM = mean(rev_fpm_pc_brl_2024, na.rm = TRUE),
    SUS = mean(rev_sus_pc_brl_2024, na.rm = TRUE),
    `FUNDEB and FNDE` = mean(rev_fundeb_pc_brl_2024 + rev_fnde_pc_brl_2024,
                             na.rm = TRUE),
    FNAS = mean(rev_fnas_pc_brl_2024, na.rm = TRUE),
    social_pc = mean(exp_social_pc_brl_2024, na.rm = TRUE),
    n_reporting = n(),
    .groups = "drop"
  )
rev_long <- rev_agg %>%
  select(year, all_of(names(PAL_CH4_REVENUE))) %>%
  pivot_longer(-year, names_to = "source", values_to = "value") %>%
  mutate(source = factor(source, levels = rev(names(PAL_CH4_REVENUE))))
rev_funding <- rev_agg %>%
  transmute(year,
            `Social spending (health, education, assistance)` = social_pc,
            `Conditioned transfers (SUS, FUNDEB and FNDE, FNAS)` =
              SUS + `FUNDEB and FNDE` + FNAS,
            `FPM` = FPM) %>%
  pivot_longer(-year, names_to = "series", values_to = "value")
p_rev_a <- ggplot(rev_long, aes(year, value, fill = source)) +
  layer_decline_band() +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = PAL_CH4_REVENUE) +
  scale_x_years(2002L, 2020L) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = NULL, y = "R$ 2024 per capita",
       title = "A. Municipal revenue by source") +
  theme_ch4() + guides(fill = guide_legend(nrow = 2, reverse = TRUE))
p_rev_b <- ggplot(rev_funding, aes(year, value, colour = series)) +
  layer_decline_band() +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = PAL_CH4_GROUPS[c(2, 3, 1)],
                      labels = function(x) str_wrap(x, 28)) +
  scale_x_years(2002L, 2020L) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
  labs(x = NULL, y = "R$ 2024 per capita",
       title = "B. Social spending and the transfers that fund it") +
  theme_ch4() + guides(colour = guide_legend(nrow = 3))
p_rev <- plot_grid(p_rev_a, p_rev_b, ncol = 2, align = "h", axis = "tb")
p_rev <- plot_grid(
  p_rev,
  ggdraw() + draw_label(str_wrap(paste0(
    "Unweighted means over municipalities reporting revenue to SICONFI (",
    min(rev_agg$n_reporting), " to ", max(rev_agg$n_reporting),
    " of 502 a year). Own revenue: current revenue net of current ",
    "transfers. Transfers gross of FUNDEB deductions (D-2026-10-15-d). ",
    "Shaded band: the 2004-2012 decline."), 170),
    size = 8.5, colour = "grey40", x = 0, hjust = 0),
  ncol = 1, rel_heights = c(1, 0.08))
save_fig(p_rev, "municipal-revenue-sources", width = 11, height = 5.5)

# ---- D2: municipalities versus states -----------------------------------------
# State-government spending per capita on the state's population (all its
# municipalities, script 09); the municipal aggregate per capita on the
# population of the municipalities that report in the year (all
# municipalities of the state, not only the biome sample). Deforestation is
# the sum over the sample municipalities of each state, which is the biome
# part of MA, MT and TO.
# Population (IBGE PIB series) ends in 2022, so per-capita values stop
# there; the money aggregates and the municipal share run to 2024.
munic_state <- exp_munic %>%
  filter(year %in% EXP_YEARS, !is.na(exp_social_brl_2024)) %>%
  mutate(has_pop = !is.na(population) & population > 0) %>%
  group_by(state_abbreviation, year) %>%
  summarise(social_brl = sum(exp_social_brl_2024),
            value = if (any(has_pop)) sum(exp_social_brl_2024[has_pop]) /
              sum(population[has_pop]) else NA_real_,
            n_reporting = n(), .groups = "drop") %>%
  mutate(series = "Municipalities (aggregate)")
uf_state <- exp_uf %>%
  transmute(state_abbreviation, year, social_brl = exp_social_brl_2024,
            value = exp_social_pc_brl_2024, series = "State government")
state_defor <- panel %>%
  filter(year >= min(EXP_YEARS)) %>%
  group_by(state_abbreviation, year) %>%
  summarise(value = sum(deforestation_area_km2), n_sample = n(),
            .groups = "drop") %>%
  mutate(series = "Deforestation")
d2 <- bind_rows(
  bind_rows(munic_state, uf_state) %>%
    mutate(panel = "Social spending per capita (R$ 2024)"),
  state_defor %>% mutate(panel = "Deforestation (km², sample municipalities)")
) %>%
  mutate(panel = factor(panel, levels = c("Social spending per capita (R$ 2024)",
                                          "Deforestation (km², sample municipalities)")),
         series = factor(series, levels = c("State government",
                                            "Municipalities (aggregate)",
                                            "Deforestation")))
state_n <- state_defor %>% filter(year == 2010L) %>%
  transmute(state_abbreviation, label = paste0(state_abbreviation, " (",
                                               n_sample, ")"))
d2 <- d2 %>% left_join(state_n, by = "state_abbreviation")
p_d2 <- ggplot(d2 %>% filter(!is.na(value)), aes(year, value, colour = series)) +
  layer_decline_band() +
  geom_line(linewidth = 0.8) +
  facet_grid(panel ~ label, scales = "free_y", switch = "y",
             labeller = labeller(panel = label_wrap_gen(22))) +
  scale_colour_manual(values = c(PAL_CH4_PAIR, PAL_CH4_DEFOR),
                      breaks = c("State government", "Municipalities (aggregate)")) +
  scale_x_continuous(breaks = c(2004, 2012, 2020), limits = c(2002, 2024),
                     oob = oob_keep) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
  labs(x = NULL, y = NULL,
       title = "Social spending per capita, state government versus municipalities, and deforestation, by state",
       subtitle = "Health, education and social assistance (functions 10, 12 and 08), stage empenhadas. State series from 2013; per-capita values to 2022.",
       caption = str_wrap(paste0(
         "Municipal aggregate: all municipalities of the state reporting to ",
         "SICONFI in the year, per capita on their population. State ",
         "government: per capita on the state population. Deforestation: sum ",
         "over the sample municipalities of the state (count in the panel ",
         "label), 2002-2020. Shaded band: the 2004-2012 decline."), 170)) +
  theme_ch4() +
  theme(strip.placement = "outside", axis.text.x = element_text(size = 8))
save_fig(p_d2, "states-vs-municipalities", width = 12, height = 5.5)

munic_share <- bind_rows(munic_state, uf_state) %>%
  filter(year >= 2013L) %>%
  select(state_abbreviation, year, series, social_brl, value) %>%
  pivot_wider(names_from = series, values_from = c(social_brl, value)) %>%
  mutate(munic_share = `social_brl_Municipalities (aggregate)` /
           (`social_brl_Municipalities (aggregate)` +
              `social_brl_State government`))
tab_munic_share <- munic_share %>%
  group_by(state = state_abbreviation) %>%
  summarise(
    munic_share_2013 = munic_share[year == 2013L],
    munic_share_2024 = munic_share[year == 2024L],
    munic_share_mean_2013_2024 = mean(munic_share),
    state_social_pc_mean_2013_2022 = mean(`value_State government`, na.rm = TRUE),
    munic_social_pc_mean_2013_2022 = mean(`value_Municipalities (aggregate)`,
                                          na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("munic_share"), ~ round(.x, 3)),
         across(contains("pc_mean"), ~ round(.x)))
save_tab(tab_munic_share, "municipal-share-social-spending")

# ---- D3: grouped trajectories ----------------------------------------------
group_series <- function(data, group_var) {
  data %>%
    filter(year %in% COMOVE_YEARS, !is.na(.data[[group_var]])) %>%
    group_by(group = .data[[group_var]], year) %>%
    summarise(
      defor     = sum(deforestation_area_km2),
      bf_cov    = mean(bf_families_quota_ratio, na.rm = TRUE),
      bf_pc     = mean(bf_transfers_pc_brl_2024, na.rm = TRUE),
      emp       = mean(emp_pia_rate_total, na.rm = TRUE),
      gdp_pc    = mean(gdp_per_capita_2024, na.rm = TRUE),
      social_pc = mean(exp_social_pc_brl_2024, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-c(group, year), names_to = "key", values_to = "value") %>%
    filter(is.finite(value)) %>%
    mutate(base = if_else(key == "bf_cov", BF_INDEX_BASE, CH4_INDEX_BASE)) %>%
    group_by(group, key) %>%
    mutate(index = index_to_base(value, year, base[1])) %>%
    ungroup() %>%
    filter(is.finite(index)) %>%
    mutate(series = factor(SERIES_LABELS[key], levels = SERIES_LABELS))
}
group_n <- function(var) {
  munic %>% filter(!is.na(.data[[var]])) %>% count(.data[[var]]) %>%
    mutate(txt = paste0(.data[[var]], " ", n)) %>% pull(txt) %>%
    paste(collapse = ", ")
}
index_caption <- paste0(
  "Series indexed to 2004 = 100 (BF coverage to 2006 = 100). Deforestation: ",
  "group sum; other series: unweighted municipal means within the group. ",
  "Shaded band: the 2004-2012 decline."
)
d3_listing <- group_series(panel_g, "listing")
p_d3_listing <- ggplot(d3_listing, aes(year, index, colour = group)) +
  layer_decline_band() +
  geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ series, scales = "free_y", ncol = 3,
             labeller = label_wrap_gen(30)) +
  scale_colour_manual(values = PAL_CH4_PAIR) +
  scale_x_years(2004L, 2020L) +
  labs(x = NULL, y = "Index (2004 = 100)",
       title = "Trajectories of listed and never-listed municipalities, 2004-2020",
       subtitle = paste0("Municipalities: ", group_n("listing"), "."),
       caption = str_wrap(index_caption, 150)) +
  theme_ch4()
save_fig(p_d3_listing, "trajectories-listing", width = 10, height = 6)

schemes <- c(spend_tercile = "Baseline social spending per capita (2004-07)",
             poverty_tercile = "Poverty rate, 2000 census",
             land_tercile = "Land-based share of value added (2004-07)")
d3_terciles <- imap_dfr(schemes, function(label, var) {
  group_series(panel_g, var) %>% mutate(scheme = label)
}) %>%
  mutate(scheme = factor(scheme, levels = schemes),
         group = factor(group, levels = TERCILE_LABELS))
p_d3_terciles <- ggplot(d3_terciles, aes(year, index, colour = group)) +
  layer_decline_band() +
  geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  facet_grid(series ~ scheme, scales = "free_y", switch = "y",
             labeller = labeller(series = label_wrap_gen(24),
                                 scheme = label_wrap_gen(28))) +
  scale_colour_manual(values = PAL_CH4_TERCILES,
                      labels = paste(TERCILE_LABELS, "tercile")) +
  scale_x_years(2004L, 2020L) +
  labs(x = NULL, y = NULL,
       title = "Trajectories by baseline terciles, 2004-2020",
       subtitle = str_wrap(paste0(
         "Terciles of the 502 municipalities on each baseline characteristic; ",
         "spending terciles cover ", sum(!is.na(munic$spend_tercile)),
         " municipalities with at least two SICONFI years in 2004-07."), 110),
       caption = str_wrap(index_caption, 150)) +
  theme_ch4() +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0),
        panel.spacing.x = unit(2, "lines"))
save_fig(p_d3_terciles, "trajectories-terciles", width = 10, height = 11)

# ---- D4: structure (tasks 4.1, 4.2) ----------------------------------------
land_dist <- munic %>%
  select(geocode, `2004-07` = land_base, `2017-20` = land_end) %>%
  pivot_longer(-geocode, names_to = "window", values_to = "share") %>%
  filter(!is.na(share))
urban_dist <- munic %>%
  select(geocode, `2000` = urban_c2000, `2010` = urban_c2010) %>%
  pivot_longer(-geocode, names_to = "census", values_to = "share")
p_d4_a <- ggplot(land_dist, aes(share, colour = window)) +
  geom_density(linewidth = 0.9, adjust = 1.2) +
  geom_vline(xintercept = quantile(munic$land_base, c(1/3, 2/3), na.rm = TRUE),
             linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = PAL_CH4_PAIR) +
  scale_x_continuous(labels = label_percent()) +
  labs(x = "Agropecuária share of value added",
       y = "Density", title = "A. Land-based dependence") +
  theme_ch4()
p_d4_b <- ggplot(urban_dist, aes(share, colour = census)) +
  geom_density(linewidth = 0.9, adjust = 1.2) +
  geom_vline(xintercept = URBAN_BREAKS[2:3], linetype = "dashed",
             colour = "grey50") +
  scale_colour_manual(values = PAL_CH4_PAIR) +
  scale_x_continuous(labels = label_percent()) +
  labs(x = "Urban population share (census definition)", y = "Density",
       title = "B. Urbanization") +
  theme_ch4()
p_d4_c <- ggplot(munic, aes(land_base, urban_c2010, colour = listing)) +
  geom_point(alpha = 0.6, size = 1.4) +
  geom_hline(yintercept = URBAN_BREAKS[2:3], linetype = "dashed",
             colour = "grey50") +
  geom_vline(xintercept = quantile(munic$land_base, c(1/3, 2/3), na.rm = TRUE),
             linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = PAL_CH4_PAIR) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Land-based share of value added, 2004-07",
       y = "Urban population share, 2010",
       title = "C. The two dimensions together") +
  theme_ch4()
p_d4 <- plot_grid(p_d4_a, p_d4_b, p_d4_c, ncol = 3, align = "h", axis = "tb")
p_d4 <- plot_grid(
  p_d4,
  ggdraw() + draw_label(str_wrap(paste0(
    "502 municipalities. Dashed lines: tercile boundaries of the 2004-07 ",
    "land-based share and the urban-share class boundaries at 0.5 and 0.75 ",
    "(D-2026-10-16-a). The census urban definition is administrative (task 4.9)."),
    170), size = 8.5, colour = "grey40", x = 0, hjust = 0),
  ncol = 1, rel_heights = c(1, 0.08))
save_fig(p_d4, "structure-distributions", width = 12, height = 4.8)

tab_crosstab <- munic %>%
  filter(!is.na(land_tercile)) %>%
  group_by(land_tercile, urban_class) %>%
  summarise(n = n(), ever_listed = sum(ever_listed),
            poverty_2010 = round(mean(poverty_c2010), 3),
            social_spending_pc_2004_07 = round(mean(spend_base, na.rm = TRUE)),
            .groups = "drop") %>%
  arrange(land_tercile, urban_class)
save_tab(tab_crosstab, "structure-crosstab")

# Maps. geobr fetches the 2010 municipality polygons; the biome sample only.
shapes <- read_municipality(year = SHAPE_YEAR, showProgress = FALSE) %>%
  mutate(geocode = as.character(code_muni)) %>%
  select(geocode, geom) %>%
  inner_join(munic %>% select(geocode, land_base, urban_c2010, listing),
             by = "geocode")
stopifnot(nrow(shapes) == N_MUNIC_EXPECTED)
p_map_a <- ggplot(shapes) +
  geom_sf(aes(fill = land_base), colour = "white", linewidth = 0.05) +
  scale_fill_gradientn(colours = pal_ch4_seq(), labels = label_percent(),
                       name = "Share of\nvalue added") +
  labs(title = "A. Land-based dependence, 2004-07") +
  theme_ch4_map()
p_map_b <- ggplot(shapes) +
  geom_sf(aes(fill = urban_c2010), colour = "white", linewidth = 0.05) +
  scale_fill_gradientn(colours = pal_ch4_seq(), labels = label_percent(),
                       name = "Urban\nshare") +
  labs(title = "B. Urban population share, 2010") +
  theme_ch4_map()
p_map <- plot_grid(p_map_a, p_map_b, ncol = 2)
save_fig(p_map, "structure-maps", width = 12, height = 5.5)

# ---- D5: long differences --------------------------------------------------
ld <- munic %>%
  mutate(
    d_spend = spend_end - spend_base,
    d_log_defor = log1p(defor_end) - log1p(defor_base),
    d_poverty_pp = (poverty_c2010 - poverty_c2000) * 100,
    d_log_defor_decade = log1p(defor_decade_end) - log1p(defor_decade_start)
  )
ld_fit <- function(y, x, data, label) {
  d <- data %>% filter(is.finite(.data[[y]]), is.finite(.data[[x]]))
  m <- lm(reformulate(x, y), data = d)
  tibble(relation = label, n = nrow(d),
         slope = unname(coef(m)[2]),
         se = unname(sqrt(diag(vcov(m)))[2]),
         correlation = cor(d[[x]], d[[y]]))
}
tab_ld <- bind_rows(
  ld_fit("d_log_defor", "d_spend", ld %>% mutate(d_spend = d_spend / 1000),
         "Change in log(1 + km2) 2004-07 to 2017-20 on change in social spending per capita (thousand R$ 2024)"),
  ld_fit("d_log_defor_decade", "d_poverty_pp", ld,
         "Change in log(1 + km2) 2000-02 to 2010-12 on change in poverty rate 2000-2010 (points)")
) %>%
  mutate(across(c(slope, se, correlation), ~ round(.x, 3)))
save_tab(tab_ld, "long-differences")
p_d5_a <- ggplot(ld %>% filter(is.finite(d_spend)),
                 aes(d_spend, d_log_defor)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(aes(colour = listing), alpha = 0.6, size = 1.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = PAL_CH4_DEFOR,
              linewidth = 0.8, se = TRUE) +
  scale_colour_manual(values = PAL_CH4_PAIR) +
  scale_x_continuous(labels = label_comma()) +
  labs(x = "Change in social spending per capita, 2004-07 to 2017-20 (R$ 2024)",
       y = "Change in log(1 + annual km²), 2004-07 to 2017-20",
       title = "A. Social spending and deforestation") +
  theme_ch4()
p_d5_b <- ggplot(ld, aes(d_poverty_pp, d_log_defor_decade)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(aes(colour = listing), alpha = 0.6, size = 1.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = PAL_CH4_DEFOR,
              linewidth = 0.8, se = TRUE) +
  scale_colour_manual(values = PAL_CH4_PAIR) +
  labs(x = "Change in the poverty rate, 2000 to 2010 census (points)",
       y = "Change in log(1 + annual km²), 2000-02 to 2010-12",
       title = "B. Poverty reduction and deforestation") +
  theme_ch4()
p_d5 <- plot_grid(p_d5_a, p_d5_b, ncol = 2, align = "h", axis = "tb")
p_d5 <- plot_grid(
  p_d5,
  ggdraw() + draw_label(str_wrap(paste0(
    "Each point is a municipality; window means of annual clearing. Line: ",
    "OLS fit with its 95 percent band (slopes in tab-long-differences). ",
    "Panel A covers the ", tab_ld$n[1], " municipalities with at least two ",
    "SICONFI years in both windows."), 170),
    size = 8.5, colour = "grey40", x = 0, hjust = 0),
  ncol = 1, rel_heights = c(1, 0.08))
save_fig(p_d5, "long-differences", width = 11, height = 5.5)

# ---- D6: tables ------------------------------------------------------------
TABLE_VARS <- c(
  deforestation_area_km2            = "Deforestation (km2)",
  deforestation_forest_rate         = "Deforestation (share of remaining forest)",
  bf_families_quota_ratio           = "BF coverage (families / quota)",
  bf_transfers_pc_brl_2024          = "BF transfers per capita (R$ 2024)",
  emp_pia_rate_total                = "Formal employment (jobs / working-age pop.)",
  emp_share_low_workers             = "Share of formal workers at or below 1 MW",
  gdp_per_capita_2024               = "GDP per capita (R$ 2024)",
  va_agro_share                     = "Land-based share of value added",
  exp_total_pc_brl_2024             = "Municipal spending per capita, total (R$ 2024)",
  exp_social_pc_brl_2024            = "Municipal social spending per capita (R$ 2024)",
  exp_health_pc_brl_2024            = "  Health",
  exp_education_pc_brl_2024         = "  Education",
  exp_social_assistance_pc_brl_2024 = "  Social assistance",
  rev_fpm_pc_brl_2024               = "FPM per capita (R$ 2024)",
  rev_sus_pc_brl_2024               = "SUS transfers per capita (R$ 2024)",
  rev_fundeb_pc_brl_2024            = "FUNDEB per capita (R$ 2024)",
  cattle_per_km2                    = "Cattle per km2",
  price_index_crop                  = "Crop price exposure index"
)
summary_long <- panel %>%
  mutate(period = ch4_period(year)) %>%
  select(geocode, period, all_of(names(TABLE_VARS))) %>%
  pivot_longer(-c(geocode, period), names_to = "key", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(key, period) %>%
  summarise(mean = mean(value), sd = sd(value), n_obs = n(),
            n_munic = n_distinct(geocode), .groups = "drop") %>%
  mutate(variable = factor(TABLE_VARS[key], levels = TABLE_VARS)) %>%
  arrange(variable, period) %>%
  select(variable, period, mean, sd, n_obs, n_munic)
save_tab(summary_long %>% mutate(across(c(mean, sd), ~ signif(.x, 4))),
         "summary-by-period-long")
# Cells: integers for variables whose largest mean is 100 or more, three
# decimals otherwise, the rule applied per variable so rows read evenly.
format_cells <- function(df) {
  df %>%
    group_by(variable) %>%
    mutate(cell = if (max(abs(mean)) >= 100) format(round(mean), big.mark = ",")
           else format(round(mean, 3), nsmall = 3),
           cell = str_trim(cell)) %>%
    ungroup()
}
tab_period <- summary_long %>%
  format_cells() %>%
  select(variable, period, cell) %>%
  pivot_wider(names_from = period, values_from = cell) %>%
  arrange(variable)
save_tab(tab_period, "summary-by-period")

tab_group <- panel_g %>%
  filter(year %in% BASELINE_YEARS) %>%
  select(geocode, listing, all_of(names(TABLE_VARS))) %>%
  pivot_longer(-c(geocode, listing), names_to = "key", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(key, listing) %>%
  summarise(mean = mean(value), n_munic = n_distinct(geocode),
            .groups = "drop") %>%
  bind_rows(
    panel %>%
      filter(year %in% BASELINE_YEARS) %>%
      select(geocode, all_of(names(TABLE_VARS))) %>%
      pivot_longer(-geocode, names_to = "key", values_to = "value") %>%
      filter(!is.na(value)) %>%
      group_by(key) %>%
      summarise(mean = mean(value), n_munic = n_distinct(geocode),
                .groups = "drop") %>%
      mutate(listing = "All")
  ) %>%
  mutate(variable = factor(TABLE_VARS[key], levels = TABLE_VARS),
         listing = factor(listing, levels = c("Ever listed", "Never listed", "All"))) %>%
  format_cells() %>%
  select(variable, listing, cell) %>%
  pivot_wider(names_from = listing, values_from = cell) %>%
  arrange(variable)
save_tab(tab_group, "summary-by-group")

# Within/between decomposition, as in script 07 block D1: total SS = between
# SS (municipal means around the grand mean) + within SS (deviations around
# the own municipal mean), on the years each variable exists, 2002-2020.
decompose <- function(x, geocode) {
  d <- tibble(x, geocode) %>% filter(!is.na(x))
  mu <- d %>% group_by(geocode) %>% mutate(mu = mean(x)) %>% pull(mu)
  gm <- mean(d$x)
  between <- sum((mu - gm)^2)
  within  <- sum((d$x - mu)^2)
  tibble(between_pct = 100 * between / (between + within),
         within_pct  = 100 * within  / (between + within),
         n_obs = nrow(d), n_munic = n_distinct(d$geocode))
}
tab_wb <- imap_dfr(TABLE_VARS, function(label, key) {
  d <- panel %>% filter(year >= 2002L)
  decompose(d[[key]], d$geocode) %>% mutate(variable = label)
}) %>%
  select(variable, between_pct, within_pct, n_obs, n_munic) %>%
  mutate(across(ends_with("_pct"), ~ round(.x, 1)))
save_tab(tab_wb, "within-between")

# ---- Assert: output state --------------------------------------------------
outputs <- c(
  "output/figures/fig-comovement-natural-units.png",
  "output/figures/fig-comovement-indexed.png",
  "output/figures/fig-municipal-revenue-sources.png",
  "output/figures/fig-states-vs-municipalities.png",
  "output/figures/fig-trajectories-listing.png",
  "output/figures/fig-trajectories-terciles.png",
  "output/figures/fig-structure-distributions.png",
  "output/figures/fig-structure-maps.png",
  "output/figures/fig-long-differences.png",
  "output/tables/tab-municipal-share-social-spending.csv",
  "output/tables/tab-structure-crosstab.csv",
  "output/tables/tab-long-differences.csv",
  "output/tables/tab-summary-by-period.csv",
  "output/tables/tab-summary-by-period-long.csv",
  "output/tables/tab-summary-by-group.csv",
  "output/tables/tab-within-between.csv"
)
stopifnot(
  all(file.exists(outputs)),
  nrow(munic) == N_MUNIC_EXPECTED,
  sum(munic$ever_listed) == 61L,
  all(table(munic$land_tercile) >= 160),
  all(table(munic$poverty_tercile) >= 160),
  sum(tab_crosstab$n) == sum(!is.na(munic$land_tercile)),
  nrow(tab_munic_share) == 9L,
  all(tab_munic_share$munic_share_mean_2013_2024 > 0 &
        tab_munic_share$munic_share_mean_2013_2024 < 1),
  nrow(tab_period) == length(TABLE_VARS),
  nrow(tab_wb) == length(TABLE_VARS),
  all(abs(tab_wb$between_pct + tab_wb$within_pct - 100) < 0.2)
)
