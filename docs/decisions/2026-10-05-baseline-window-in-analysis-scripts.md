# 2026-10-05 — Baseline covariates are built in the analysis scripts, with the window left open

**Context.** The naming convention (version 1) proposed panel names for the
baseline covariates `gdp0`, `informal0` and `agri0` with a fixed 2006-2007
window (`_bl0607`), and task 5.1 refers to a "predetermined 2006-07
moderator". The surviving analysis script
(`08-data-analysis-2WFE-CS.R`) averages these covariates over 2002-2007
(`BASE_YEARS <- 2002:2007`). The two records disagreed, and the question
of which window to store had to be settled at the stage 3 gate before the
crosswalk could be written.

**Decision.** The baseline covariates are not stored in the final panel.
They are built in the analysis scripts that use them, named under the
convention's grammar with the window digits set by each script
(`eco_gdp_pc_brl24_log_bl<window>`, `lab_informal_share_bl<window>`,
`lab_emp_formal_agri_share_bl<window>`). No single window is fixed now:
the author will explore alternative windows at the causal stage.

**Rationale.** The governing rule for derived variables (Chapter 4
guidance, Methodological standards) places any variable that is a
parameter varied in robustness checks in the analysis scripts, and the
baseline window is exactly such a parameter. Fixing a window in the panel
would pre-commit the identification design, which the chapter guidance
keeps open. The alternative, storing one window now and overriding it
later, was rejected because it would leave a stored variable whose
meaning the analysis no longer respects.

**Consequences.** The naming convention (rule 7) now records this choice.
The crosswalk for phase 2R carries no `_bl` variables. Task 5.1's
reference to a "predetermined 2006-07 moderator" describes the current
script's design and stays as the starting point, not a commitment; the
window is restated in each candidate design script in `_scripts/models/`
when the causal stage opens.
