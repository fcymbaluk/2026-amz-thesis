# Annex "Data description": structure and per-variable template

This single annex supersedes the earlier plan of separate appendices (data description and codebook): the codebook is its section 6, rendered from `_data/aux/codebook.csv`, and provenance is section 7, rendered from the chapter README. The annex is the reader-facing description of the analysis panel. It contains no code. It is organized by the role each variable plays in the design, not by the processing steps that produced it. It is written in phase 3, from the final state of the pipeline, using the script headers, the audit notebooks and `DECISIONS.md` as sources.

## Section order

1. Unit of analysis and sample. Biome filter, Legal Amazon extension, panel window, balancing, the at-risk restriction, Mojuí dos Campos, Colniza.
2. Outcome. Deforestation area, forest area, rate, and the normalized outcomes.
3. Treatment. PPCDAm listing, cohorts, entries and exits.
4. Moderators. Bolsa Família coverage ratio and its components, minimum-wage exposure.
5. Covariates. GDP per capita, informality, agricultural structure, electoral competition, commodity price index.
6. Codebook. Rendered from `_data/aux/codebook.csv`.
7. Data provenance. Rendered from the provenance table in `README.md`.

Sections 6 and 7 are rendered from files, never typed by hand.

Amendment pending (phase 3): the H3 political-institutional variables do not fit this order cleanly. Under H3 several of them are second-order moderators, not covariates. When the H3 variable set is settled, either section 4 becomes "Moderators (H2 and H3)" or a section is inserted between 4 and 5, renumbering the rendered sections. Decide then; the annex describes the dataset that exists.

## Per-variable template

Every variable follows the same subsection order, so the reader learns the pattern once. A subsection that does not apply is dropped, not filled with "not applicable". Each subsection is one paragraph unless noted.

```
### [Variable label] (`variable_name`)

Concept.        What the variable operationalizes and its role in the
                design (outcome, treatment, moderator, covariate,
                sample criterion). Cross-reference the chapter section
                that motivates it.
Source.         Provider, dataset name, version or release date, access
                date, URL.
Coverage.       Units, years available, years used, known gaps.
Raw measure.    What the provider records, at what frequency and
                granularity.
Construction.   The rule that maps the raw measure to the variable, in
                words or a formula.
Decisions.      Each non-obvious choice, one paragraph each, with its
                justification and decision ID.
Validation.     What was checked and the result, with a pointer to the
                audit notebook section. No code, no tables.
Limitations.    What the variable cannot capture, and known measurement
                issues.
```

## Prose rules for the annex

The annex follows the dissertation's prose rules (`docs/writing-rules.md`, at the repository root). Periods and commas as the primary punctuation. No em dashes as connectors. No semicolons. No negative parallelism. Direct constructions. Same term for the same concept throughout. Citations in author-year style.

## Worked example

### Bolsa Família coverage ratio (`bf_families_quota_ratio`)

**Concept.** The primary moderator. It measures how far the Bolsa Família rollout had reached its administrative target in each municipality when enforcement began, and it operationalizes the cushioning capacity of social protection developed in Chapter 2. The two mechanisms proposed there, cushioning as compensation and cushioning as forbearance, predict opposite signs for the moderation difference.

**Source.** Beneficiary families and total transfers come from the Bolsa Família administrative records published by the Ministério do Desenvolvimento e Assistência Social (MDS) on the federal Dados Abertos portal (release of March 2026, accessed 10 March 2026, https://dados.gov.br/dados/conjuntos-dados/bolsa-familia). Municipal quotas come from the MDS estimate of eligible families (accessed 10 March 2026).

**Coverage.** All Brazilian municipalities, monthly from January 2004. The panel uses 2004 to 2019.

**Raw measure.** Monthly counts of beneficiary families and the total value transferred, by municipality.

**Construction.** For each municipality and year, the number of beneficiary families in the reference month divided by the municipal quota in force that year. The moderator enters the analysis as the mean of this ratio over 2006 and 2007.

**Decisions.** *Reference month (D-2025-11-03-a).* Monthly records must be reduced to one value per year. I compared five candidate months on three criteria: stability relative to adjacent months, proximity to the yearly median, and completeness. June performs best on all three and is the reference month. February is retained as an alternative for robustness checks.

*Truncation at 2019 (D-2025-11-03-b).* From 2020 the series records positive beneficiary counts with zero transfers for many municipality-months. This reflects the replacement of Bolsa Família payments by emergency programs during the pandemic, and the series is not comparable across the break.

*Baseline window (D-2026-02-20).* The moderator is fixed at 2006 to 2007 because coverage responds to listing itself. Wider windows produce a negative effect of listing on coverage, between 0.13 and 0.18, and would introduce post-treatment variation into the moderator.

**Validation.** Integrity checks on the raw files found no duplicate municipality-month keys and no negative values (audit notebook `bf`, question 1). Fourteen Mato Grosso municipalities show ratios above 3 in the baseline window. [Phase 3: replace with the outcome of Task 1.4.]

**Limitations.** Coverage relative to quota measures administrative reach, not benefit adequacy. The quota is an MDS estimate of eligible families and is revised irregularly, so the ratio can exceed one for reasons unrelated to program expansion.

## Second worked example, shorter

### Normalized deforestation, 2000 to 2007 baseline (`defor_norm_ref0007`)

**Concept.** The primary outcome. A within-municipality z-score of annual deforestation, scaled on the municipality's own 2000 to 2007 distribution, so that effects are read as deviations from each unit's pre-listing behavior in standard-deviation units.

**Source.** MapBiomas, Deforestation and Secondary Vegetation statistics, collection 9 (released August 2024, accessed [date], https://brasil.mapbiomas.org/estatisticas/).

**Coverage.** All municipalities, annual, 1987 to 2023. The panel uses 2000 to 2020.

**Raw measure.** Area in hectares of primary and secondary vegetation suppression per municipality and year, Level 1 classes.

**Construction.** Annual deforestation is the sum of primary and secondary suppression, converted to km². The normalized outcome subtracts the municipality's 2000 to 2007 mean and divides by its 2000 to 2007 standard deviation.

**Decisions.** *Baseline window (D-[id]).* The window ends in 2007, the last full year before the first listing cohort. *At-risk sample (D-[id]).* Municipalities with near-zero baseline deforestation produce explosive z-scores from small absolute changes, so the primary analysis uses the at-risk narrow sample (n = 408). This is a functional-form restriction, not a substantive sample choice, and the full-sample estimate on the rate outcome is reported alongside it.

**Validation.** Summed primary and secondary suppression matches the values displayed on the MapBiomas platform for sampled municipalities (audit notebook `mapbiomas`, question 2).

**Limitations.** The z-score is undefined for municipalities with zero baseline variance and unstable for those with very low variance, which is why the at-risk restriction exists.
