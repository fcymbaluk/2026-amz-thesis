# theme_ch4.R
# Purpose  Task 4.4: the chapter's shared figure convention, so that every
#          fig-*.png reads as one system. Definitions only; sourced after the
#          libraries by scripts 07 (at its restyle), 10 and 11.
# Inputs   none
# Outputs  theme_ch4(), the palettes PAL_CH4_*, period constants and two
#          layer helpers (decline band, deforestation reference line)
# Status   2026-10-16, sprint day 4; recorded in the chapter README at
#          phase 3 (issue #15)

# ---- Periods ---------------------------------------------------------------
# The chapter's periodization (GUIDANCE.md, "Chapter's Purpose and Argument"):
# decline 2004-2012, rise 2013-2022. The shaded band marks the decline.
CH4_DECLINE_YEARS <- c(2004L, 2012L)
CH4_INDEX_BASE    <- 2004L      # first year with Bolsa Família data
CH4_PERIODS <- tibble::tribble(
  ~period,     ~from, ~to,
  "2000-2003", 2000L, 2003L,
  "2004-2007", 2004L, 2007L,
  "2008-2012", 2008L, 2012L,
  "2013-2016", 2013L, 2016L,
  "2017-2020", 2017L, 2020L
)
ch4_period <- function(year) {
  idx <- findInterval(year, CH4_PERIODS$from)
  factor(CH4_PERIODS$period[idx], levels = CH4_PERIODS$period)
}

# ---- Palettes --------------------------------------------------------------
# Categorical: Okabe-Ito, colour-blind safe, the reference series in grey.
PAL_CH4_GROUPS <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                    "#56B4E9", "#F0E442", "#999999")
PAL_CH4_DEFOR  <- "grey25"                      # deforestation reference line
PAL_CH4_BAND   <- "grey85"                      # 2004-2012 decline band
# Two-level contrasts (listed versus never, state versus municipalities).
PAL_CH4_PAIR   <- c("#D55E00", "#0072B2")
# Terciles, low to high: one hue, three steps.
PAL_CH4_TERCILES <- c("#9ecae1", "#3182bd", "#08519c")
# Sequential (maps, heatmaps): viridis "mako", reversed so dark is high.
pal_ch4_seq <- function(n = 7) rev(viridisLite::mako(n, begin = 0.15,
                                                       end = 0.95))
# Expenditure functions, fixed so the same function has the same colour in
# every figure.
PAL_CH4_FUNCTIONS <- c(Health = "#0072B2", Education = "#009E73",
                       `Social assistance` = "#D55E00",
                       `Social security` = "#CC79A7",
                       `Environmental management` = "#E69F00",
                       Total = "grey40")
PAL_CH4_REVENUE <- c(`Own revenue` = "grey55", FPM = "#0072B2",
                     SUS = "#009E73", `FUNDEB and FNDE` = "#D55E00",
                     FNAS = "#CC79A7")

# ---- Theme -----------------------------------------------------------------
theme_ch4 <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0,
                                         size = base_size),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0,
                                           size = base_size * 0.8),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = "grey30")
    )
}
theme_ch4_map <- function(base_size = 11) {
  theme_ch4(base_size) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(size = base_size * 0.9))
}

# ---- Layer helpers ---------------------------------------------------------
# The 2004-2012 decline band, drawn first so series sit on top of it.
layer_decline_band <- function() {
  ggplot2::annotate("rect", xmin = CH4_DECLINE_YEARS[1] - 0.5,
                    xmax = CH4_DECLINE_YEARS[2] + 0.5,
                    ymin = -Inf, ymax = Inf, fill = PAL_CH4_BAND, alpha = 0.5)
}
# Deforestation as the thick grey reference line behind the coloured series.
layer_defor_reference <- function(data, mapping) {
  ggplot2::geom_line(data = data, mapping = mapping, colour = PAL_CH4_DEFOR,
                     linewidth = 1.2)
}
# oob_keep: the decline band starts half a year before 2004 and must be
# clipped at the panel edge, not dropped, when the axis starts in 2004.
scale_x_years <- function(from = 2000L, to = 2020L, by = 4L) {
  ggplot2::scale_x_continuous(breaks = seq(from, to, by), limits = c(from, to),
                              oob = scales::oob_keep,
                              expand = ggplot2::expansion(mult = 0.02))
}
# Index a series to its base year (base = 100); NA where the base is absent.
index_to_base <- function(value, year, base = CH4_INDEX_BASE) {
  b <- value[year == base]
  if (length(b) != 1 || is.na(b) || b == 0) return(rep(NA_real_, length(value)))
  value / b * 100
}
# Save with the chapter's defaults.
save_fig <- function(plot, slug, width = 9, height = 5.5, dpi = 300) {
  ggplot2::ggsave(file.path("output/figures", paste0("fig-", slug, ".png")),
                  plot, width = width, height = height, dpi = dpi, bg = "white")
}
save_tab <- function(df, slug) {
  readr::write_csv(df, file.path("output/tables", paste0("tab-", slug, ".csv")))
}
