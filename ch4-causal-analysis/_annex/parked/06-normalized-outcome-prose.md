# Parked prose: script 06 (normalized outcome), phase 1

Raw material for the annex (phase 3) and the chapter methods section. Taken verbatim from the script retired at the phase 1 script 06 refactor (git history: commit 26bc5e7 and earlier, `_scripts/06-normalized-outcome.R`, itself the legacy `08-normalized-outcome.R`). Counts refer to the legacy run on the recovered panel; the rebuilt counts are in `_audit/panel-outcome.qmd`. The normalization logic cites Assunção, Gandour and Rocha (2015); the citation key is to be confirmed against `manuscript/references.bib` before any of this enters the text.

## Legacy header (design notes)

Build the within-municipality normalized deforestation increment from a
PRE-TREATMENT reference, for the linear staggered-DiD specifications, and the
exposure offset that PPML needs. "Pre-treatment reference" means each
municipality's mean and standard deviation are estimated only on years before
the priority-list treatment, so the outcome scale is not contaminated by the
policy effect it is later used to estimate.

Two references are produced; pick one for the main result, report the other as
robustness:
  defor_norm_ref0003 : 2000-2003, fully pre-PPCDAm, but only 4 years (noisier sd)
  defor_norm_ref0007 : 2000-2007, pre-priority-list (treatment begins 2008),
                       8 years (steadier sd). Recommended default.

Outcome by estimator:
  Linear staggered DiD (Callaway-Sant'Anna, de Chaisemartin-D'Haultfoeuille):
      use a defor_norm_ref* column. Negative values are expected and fine.
  PPML (Poisson pseudo-maximum-likelihood): use deforestation_area_km2 in
      LEVELS as the outcome (non-negative) with log_forest_exposure as the
      offset. Do NOT feed the normalized column to PPML; a centred z-score is
      negative for about half the observations and is not a valid Poisson
      outcome.

## Comments in the body

---- Normalized increment (centre and scale on the pre-treatment reference) --
Under municipality fixed effects, only the SD scaling matters: the subtracted
mean is absorbed by the unit effect, so the choice of pre-treatment vs
full-sample mean does not change the DiD estimate. Fixing the SD on pre-policy
years is the substantive decision, which is what this does. A municipality
with no pre-treatment variation (sd = 0) cannot be normalized and is set NA;
such units sit outside the at-risk sample anyway.
Exposure is the forest standing at the start of the year (the forest at risk of
being cleared), i.e. lagged remaining forest. The PPML model is then
  fixest::fepois(deforestation_area_km2 ~ X | geocode + year,
                 offset = ~ log_forest_exposure, data = subset(df, at_risk_broad))
so coefficients read as effects on the clearing rate. The first year has no
lag and is dropped from PPML (offset NA).
"Note: heavy tails in the FULL sample come from a few municipalities with
near-zero pre-treatment variance; they fall outside the at-risk sample,
where the outcome is well-behaved. Estimate on the at-risk sample. If a
tail remains, winsorise at the modelling stage and report it, e.g.:
  q <- quantile(d$defor_norm_ref0007, c(.01,.99), na.rm = TRUE)
  d$defor_norm_ref0007_w <- pmin(pmax(d$defor_norm_ref0007, q[1]), q[2])

## Method section (brief)

Outcome construction. The dependent variable for the difference-in-differences specifications is a within-municipality normalized deforestation increment. For each municipality the annual area of forest cleared is centered and scaled by that municipality's own mean and standard deviation, computed over a pre-treatment reference period (2000–2007, before the priority-list policy began in 2008). Normalizing on each municipality's own scale makes the increment comparable across units of very different size, and fixing the scale on pre-treatment years keeps it free of any influence from the policy whose effect is being estimated. A shorter reference (2000–2003) is used as a robustness check. Because municipality fixed effects absorb the subtracted mean, the substantive choice embedded in this variable is the period that sets each municipality's standard deviation, not the period that sets its center. For the Poisson pseudo-maximum-likelihood specifications the outcome is instead the cleared area in levels, with lagged remaining forest entered as an exposure offset, so the coefficients read as effects on the clearing rate.

## Appendix (more detailed)

Within-municipality normalized deforestation increment. The annual deforestation increment, the area of forest cleared in a municipality in a given year, varies across municipalities by orders of magnitude because municipalities differ greatly in size and in forest stock. A common level model would then be dominated by the largest units. Following the normalization logic in Assunção, Gandour and Rocha (2015), the increment is rescaled to each municipality's own distribution. For municipality i in year t, the normalized increment is the cleared area in (i, t) minus the municipality's reference mean, divided by the municipality's reference standard deviation, so that the variable expresses how far a given year's clearing departs from that municipality's typical clearing, in its own standard-deviation units.
The reference mean and standard deviation are estimated on a pre-treatment window rather than on the full panel. This is deliberate. If the scale were estimated over years that include the post-2008 period, the standard deviation in the denominator would itself be affected by the policy effect, which would contaminate the outcome with the very treatment under study. Two reference windows are produced. The main version uses 2000–2007, the eight years preceding the priority-list policy that begins in 2008; the longer window yields a steadier per-municipality standard deviation. The robustness version uses 2000–2003, which is fully prior to the launch of PPCDAm in 2004 but rests on only four years and therefore a noisier scale. Results are reported on the main version and checked against the shorter one.
One property of this construction is worth stating, because it determines what the pre-treatment choice actually does. The estimating equations include municipality fixed effects, which absorb any constant that is subtracted from each municipality's outcome. The subtracted reference mean is such a constant, so it does not affect the estimated coefficients; the pre-treatment-versus-full-sample distinction for the center is immaterial. What is not absorbed, and what the pre-treatment window genuinely fixes, is the standard deviation in the denominator. The substantive decision embedded in the variable is therefore the period over which each municipality's scale is set, and that period is held to pre-policy years.
For the Poisson pseudo-maximum-likelihood specifications the normalized increment is not used, because a centered variable is negative for roughly half the observations and is not a valid non-negative outcome for a Poisson model. The outcome there is the cleared area in levels, and differences in municipality size are handled through an exposure offset equal to the log of lagged remaining forest, the stock of forest standing at the start of the year and therefore available to be cleared. With this offset the coefficients are interpreted as effects on the rate of clearing relative to the forest at risk.

## Notes and flags for the method text

These belong in the method section, since they bear on which observations enter the estimates and how the outcome behaves.
A municipality with no variation in deforestation over the reference window has a reference standard deviation of zero and cannot be normalized. These units receive a missing value on the normalized outcome rather than an undefined one: two municipalities under the 2000–2007 reference and one under the 2000–2003 reference. All of them fall outside the at-risk estimation sample, so no analysis observation is lost to this rule.
The normalized outcome has heavy tails in the full panel, driven by a small number of municipalities whose pre-treatment clearing was almost invariant, so that a small denominator inflates the normalized value. These municipalities have negligible baseline deforestation and lie outside the at-risk sample by construction. Within the at-risk sample the outcome is well behaved, with the bulk of values between roughly minus three and plus six standard deviations under the 2000–2007 reference. The estimations are run on the at-risk sample for this reason among others. Where a thin tail remains in a given specification, the outcome is winsorized at the first and ninety-ninth percentiles and the unwinsorized result is reported alongside.
The Poisson specifications drop each municipality's first sample year, because the exposure offset uses lagged remaining forest and the lag is undefined in the initial year. This removes one year per municipality and does not affect the treated or control composition.
One point worth carrying into the writing: under municipality fixed effects only the standard-deviation scaling actually enters the estimate, since the subtracted mean is absorbed by the unit effect. So the substantive pre-treatment choice you are making here is which years set each municipality's scale, not which years set its center. That is the sentence to use when you justify the construction in the methods text.
