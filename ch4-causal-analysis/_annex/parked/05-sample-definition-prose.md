# Parked prose from the legacy 05-sample-definition.R (phase 1, 2026-10-12)

Raw material for the annex (phase 3) and the chapter methods section. Taken verbatim from the script retired at the phase 1 script 05 refactor (git history: commit a5101e2 and earlier). Counts refer to the legacy run on the recovered panel; the rebuilt counts are in `_audit/panel-sample.qmd`.

## Header design notes

SAMPLE IS MODULAR BY ESTIMAND. Two flags are produced and should be used for
different designs, never collapsed into one global filter:
  avail_ok                  -> forest-availability floor only. This is the
                               sample for the DIRECT effect of cash transfers
                               or employment on deforestation, where you want
                               the full range of social-policy exposure and
                               must NOT condition on a deforestation-correlated
                               gate.
  at_risk_broad / _narrow   -> forest floor PLUS a baseline-deforestation
                               frontier gate. This is the sample for the
                               PPCDAm contrast and the PPCDAm-by-social-policy
                               triple difference, where positivity requires
                               controls that are plausibly listable.

Design notes
 - Availability floor: a municipality needs forest to lose. Barely binds here
   (almost every unit is forested) but kept for documentation.
 - Pressure scope: baseline deforestation AREA (not rate). Absolute area is what
   PPCDAm priority status is assigned on, so it preserves overlap with the
   treatment; the rate misleads because listed units are large, heavily
   forested municipalities with big absolute clearings but modest rates.
 - Agricultural pressure (cattle, crop) is a CONTINUOUS covariate for matching
   and heterogeneity, not a gate: listed units have lower baseline cattle
   density, so an OR-gate on it would pull in off-support units.
 - Treatment for the DIAGNOSTICS is defined to match the analysis WINDOW, not
   the full panel. A unit first listed in 2017-2020 is a not-yet-treated
   control in a 2004-2010 design, and the positivity check must treat it as
   such. Two windows are reported: the main absorbing window (2004-2010, zero
   exits) and the robustness window (2004-2012).

## For the methodological appendix (main window, 2004-2010)

Sample-definition diagnostics. The at-risk sample is defined on pre-treatment characteristics averaged over 2000–2003, before the priority-list policy began. Three diagnostics describe how the restriction behaves, each reported for the full sample of 502 municipalities and for two nested restrictions: the broad sample (forest available and baseline annual clearing of at least 1 km²) and the narrow sample (clearing of at least 5 km²). Throughout, a municipality counts as treated if it entered the priority list at any point within the analysis window of 2004–2010; municipalities listed only after the window serve as controls. A parallel set of tables is produced for the 2004–2012 robustness window.

Retention. This table confirms that the restriction removes only control units and never discards a treated one, since dropping a listed municipality would change the estimand. The column n is the number of municipalities in the sample, treated the number listed within the window, and control the remainder. All 43 listed municipalities survive both restrictions; only never-listed units are removed, from 459 controls in the full sample to 425 in the broad and 365 in the narrow.

Standardized mean differences. This table compares the baseline characteristics of listed and non-listed municipalities. For each covariate it reports the treated-group mean (mean_treated), the control-group mean (mean_control), and the standardized mean difference (smd), defined as the difference in means divided by the pooled within-group standard deviation. Values near zero indicate similar groups, and absolute values above about 0.1 indicate imbalance that a comparison must address. Listed municipalities have much higher baseline deforestation area (smd near 1.5), are more forested (smd 0.7 to 0.9), and have somewhat lower deforestation rate and cattle density (smd around −0.4 to −0.6). The imbalance does not shrink as the sample narrows, and on some covariates it grows. This is expected and is not the task of the restriction. The restriction secures comparable treatment probability (overlap); the remaining covariate imbalance is removed at the estimation stage by matching or weighting within the at-risk sample. The table therefore identifies the covariates the estimator must condition on: forest share, deforestation area, deforestation rate, and cattle density.

Propensity overlap. This table assesses common support, the requirement that listed and non-listed municipalities share a comparable range of treatment probability. A municipality's propensity is its predicted probability of being listed, from a logistic regression of listing status on the baseline covariates (forest share, log baseline deforestation area, log cattle density, log crop share). The columns are: n, the municipalities with complete covariates entering the model, three being dropped for missing baseline values; treated, the number listed; ps_support_lo and ps_support_hi, the bounds of the region where treated and control propensities overlap; off_support_treated and off_support_control, the counts of each group falling outside that shared region; and treated_med_ps and control_med_ps, the median propensity in each group. The groups separate sharply: the median listed municipality has a propensity of 0.69 while the median non-listed municipality is near zero. Most controls in the full sample, 410 of the 456 in the model, lie below the treated range, meaning they have almost no probability of being listed and add little to a credible comparison. The restriction reduces this off-support control mass to 377 in the broad sample and 317 in the narrow, while keeping all 43 listed municipalities. A residual 14 listed municipalities lie above the control range; these are extreme-frontier units for which no control offers a close match, and estimates that depend on them rest on extrapolation and are reported with that caveat.

## For the methodological section of the chapter

The enforcement analysis is restricted to municipalities at risk of frontier deforestation, identified from pre-treatment characteristics measured over 2000–2003: a municipality enters the sample if it retained forest and recorded baseline annual clearing above a minimum area. The restriction serves the positivity requirement of the design. Priority-list municipalities are large, heavily forested frontier units with high absolute baseline deforestation, and most Amazonian municipalities have almost no probability of ever being listed; comparing listed units against these would set them against municipalities that were never at risk of the policy. The restriction removes such off-support controls, cutting their number by roughly a fifth, while retaining every municipality listed within the analysis window, so it improves comparability without altering the treated group or selecting on the outcome. Differences that remain between listed and non-listed municipalities on baseline forest cover, deforestation, and cattle density are handled at the estimation stage by matching within the at-risk sample. A small group of extreme-frontier listed municipalities has no close control match, and estimates are reported with and without them. The area threshold defining the sample is reported in a broad and a narrow version to show that the results do not turn on where the line is drawn.

## For the methodological appendix (robustness window, 2004-2012)

Robustness window (2004–2012). These tables repeat the sample-definition diagnostics under the wider analysis window, in which a municipality counts as treated if it entered the priority list at any point between 2004 and 2012. This window adds the cohorts listed in 2011 and 2012, raising the treated group from 43 to 51, at the cost of a few municipalities that later left the list, which is why it serves as a robustness check rather than the main specification. The columns carry the same meanings as in the MAIN-window tables: in the retention table, n, treated, and control are the municipality counts; in the balance table, mean_treated and mean_control are the group means and smd the difference divided by the pooled within-group standard deviation; in the overlap table, n is the municipalities entering the propensity model, ps_support_lo and ps_support_hi bound the shared propensity region, off_support_treated and off_support_control count units outside it, and treated_med_ps and control_med_ps are the group median propensities.

Retention. All 51 listed municipalities survive both restrictions. Only never-listed controls are removed, from 451 in the full sample to 417 in the broad and 357 in the narrow. As in the main window, the restriction touches controls only.

Standardized mean differences. The pattern matches the main window. Listed municipalities have far higher baseline deforestation area (smd near 1.4), are more forested (smd 0.74 to 0.92), and have lower deforestation rate and cattle density (smd around −0.4 to −0.6), and the imbalance does not shrink as the sample narrows. One difference from the main window is worth noting: baseline crop share now sits higher in the treated group (smd 0.14 to 0.19, rather than near zero), because the 2011 and 2012 cohorts include more crop-oriented municipalities. Crop share therefore joins forest share, deforestation area, deforestation rate, and cattle density as a covariate the estimator must condition on when this window is used.

Propensity overlap. The separation between groups is again sharp, with the median listed municipality at a propensity of 0.67 against near zero for controls. Adding the later cohorts widens the lower bound of common support slightly (from 0.088 to 0.035) and lowers the off-support control mass relative to the main window, since the larger and marginally less extreme treated group brings more controls within range. Off-support controls fall from 357 in the full sample to 324 in the broad and 264 in the narrow, while all 51 listed municipalities are retained. The residual block of off-support treated units is 12 here, slightly smaller than the 14 in the main window; these remain extreme-frontier units with no close control, and estimates relying on them are reported with that caveat.

## For the methodological section of the chapter (robustness window)

As a robustness check, the analysis window is widened to 2004–2012, which adds the priority-list cohorts of 2011 and 2012 and raises the treated group from 43 to 51 municipalities. The sample diagnostics behave as in the main window: the at-risk restriction removes only never-listed controls, cutting the off-support control mass while retaining every listed municipality, and the listed group remains distinct on baseline forest cover, deforestation, and cattle density, with these differences handled by matching at the estimation stage. The wider window differs in one respect, a higher baseline crop share among listed municipalities, reflecting the more crop-oriented profile of the later cohorts, which is accordingly included among the covariates conditioned on. The consistency of the diagnostics across the two windows indicates that the sample definition and the comparability of the treated and control groups do not depend on the precise treatment window or on the area threshold used to draw the sample.

## Closing notes of the legacy script

1. Sample by estimand: use at_risk_broad/_narrow for the PPCDAm and PPCDAm-by-
   social-policy designs; use avail_ok (or the full sample) for the direct
   effect of transfers/employment. Do not impose the frontier gate on the
   latter.
2. Baseline moderators for the triple difference (pre-listing means of BF per
   capita and employment, e.g. 2004-2007) are NOT built here. They belong in a
   companion baseline-characteristics step so the moderator is fixed and
   pre-treatment, never contemporaneous.
3. Matching inside the at-risk set: MatchIt + cobalt on ppcdam_main, e.g.
     m <- matchit(ppcdam_main ~ forest_share_2000 + log_defor_area_b +
                  log_cattle_dens_b + log_crop_share_b,
                  data = subset(muni, at_risk_broad), method = "nearest")
     bal.tab(m)