# Audit notebook template

One Quarto notebook per data source under `_audit/`, rendered to HTML, with the rendered output committed. The notebook is the lab record: it holds the exploration and validation code that used to sit inside the pipeline scripts, and it ends each section with a finding and a decision. The pipeline never depends on a notebook.

## Purpose

The notebook answers the question "how do I know?" for every assertion in the pipeline and every decision in `DECISIONS.md`. A committee member or a replicator who doubts an assertion opens the notebook. Future-you who forgets why June is the reference month opens the notebook.

## Rules

1. One notebook per source: `_audit/mapbiomas.qmd`, `_audit/ibge-biome.qmd`, `_audit/ppcdam.qmd`, `_audit/rais.qmd`, `_audit/bf.qmd`, `_audit/ibge-pib.qmd`, `_audit/tse.qmd`, `_audit/pam-ppm.qmd`, `_audit/prices.qmd`, `_audit/census-population.qmd`, `_audit/atlas.qmd` (added when the Atlas urban/rural and poverty variables enter).
2. Every section is a question. Every section ends with a **Finding** and a **Decision** line. The decision line carries a decision ID or the word `open`.
3. The notebook reads from `_data/raw/` or `_data/interim/`, never from `_data/reference/`, and writes nothing to `_data/`.
4. Code is shown and evaluated. Output tables and figures are the evidence, so they stay in the rendered file.
5. Interpretation is one or two sentences per question. Argument belongs in the annex or the chapter.
6. When a pipeline assertion is added, the notebook section that justifies it is cited in the assertion's comment, and the notebook section names the assertion.
7. The notebook is re-rendered whenever its source file changes or the sample changes (Task 1.1). The rendered date is the evidence date.

## Skeleton

```markdown
---
title: "Audit: Bolsa Família (MDS)"
date: 2026-03-10
format:
  html:
    toc: true
    code-fold: true
execute:
  echo: true
---

Source: MDS Dados Abertos, release March 2026, accessed 2026-03-10.
Raw files: `_data/raw/mds_bolsa_familia/bf-YYYY.txt`, 2004 to 2026.
Pipeline script: `_scripts/02-social-bolsa-familia.R`.

## Q1. Are the yearly raw files complete and internally consistent?

What is checked: missingness, zeros, negatives, positive families with
zero transfers, duplicate municipality-month keys, valid month codes.

[code and table]

**Finding.** No duplicate keys and no negative values in any year.
2020 and 2021 show positive beneficiary counts with zero transfers in a
large share of municipality-months.
**Decision.** D-2025-11-03-b: panel truncated at 2019.
**Assertion.** `02-social-bolsa-familia.R`, raw-state block.

## Q2. Which month represents the year?

Candidates: February, June, July, September, November. Criteria:
stability relative to adjacent months, distance from the yearly median,
completeness.

[code and decision table]

**Finding.** June has the lowest median instability, the smallest share
of month-to-month jumps above 20 percent, and the highest completeness.
**Decision.** D-2025-11-03-a: `BF_REF_MONTH = 6`, February retained as
`BF_ALT_MONTH`.

## Q3. Is the implied transfer per family plausible across years?

[distribution by year, p1, median, p99, max]

**Finding.** ...
**Decision.** none required.

## Q4. Do the municipal quota values look plausible?

[distribution of bf_families_quota_ratio in 2006-07; the 14 Mato Grosso
cases above 3; comparison with an alternative quota source if available]

**Finding.** Fourteen Mato Grosso municipalities exceed 3 (max 11.9).
Four of them account for 91 percent of moderator variance among treated
units.
**Decision.** open (Task 1.4).
```

## Standard questions by source

These are the questions the current scripts already answer implicitly. The agent moves the corresponding code here in phase 1.

**MapBiomas.** Are 1985 and 1986 all zero for primary suppression? Does the aggregation of primary and secondary classes match the online platform for sampled municipalities? What does the distribution of `area_deforestation` look like and are the tails real? Does Mojuí dos Campos carry data for years before its 2013 emancipation?

**IBGE biome list.** Does the list contain 503 Amazônia municipalities? Which municipalities share a name across states? Does every geocode have seven digits?

**PPCDAm.** Does the manual compilation match the ordinances year by year? Which municipalities enter and exit, and in which year?

**RAIS.** How are CBO codes distributed across the agricultural cutoff? What share of establishments falls in public administration?

**Bolsa Família.** Q1 to Q4 above.

**IBGE PIB.** Do Santarém plus Mojuí dos Campos recover the combined pre-2013 total? Which municipalities lack GDP before 2010 (Colniza)?

**TSE.** Is the election year mapped to the governing period correctly? (Task 1.5.) Are uncontested elections coded as intended?

**Prices.** Is the index anchored to 1 in 2000 for both crop and cattle? (Task 1.3.) Are the scales comparable?

**Census population.** Do the 2000, 2010 and 2022 anchors interpolate without negative or implausible values? Are the nine post-2000 MT municipalities NA, never zero, before their installation year?

**Atlas.** Are poverty, extreme poverty and vulnerability on 0-1 after rescaling? Do all three vintages join completely on geocode?
