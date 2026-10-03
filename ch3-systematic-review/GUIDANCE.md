---
chapter: 3
title: Systematic review — causes of Amazon deforestation
stage: conception # conception | drafting | full draft | under review | revising | final
updated: 2026-09-23
current_focus: "Protocol design: research question, eligibility criteria, search strategy"
next_milestone: "Protocol finalized and searches executed"
---

# Chapter 3 — Systematic review of the causes of Amazon deforestation: working guidance

> **About this document.** Repo-wide behavior (git, permissions, the R/Python policy, renv, manuscript conventions) is governed by `CLAUDE.md`; this file adds chapter-specific guidance only. The cross-chapter argument chain is canonical in `docs/dissertation-map.md`. This chapter was created after the Aug 2026 supervisor review, so it carries no review checklist; requirements affecting it may arrive in a future review round. Sections marked TODO are protocol decisions the author has not yet made: do not fill them by assumption. This file's instructions are living and evolve with the work (see CLAUDE.md, "Guidance documents are living records"); the frozen protocol, once written, is the exception — it changes only by dated amendment.

## Work plan — PRISMA stages (in order)

Stage conventions live in "Method: PRISMA workflow" below; this list tracks completion only. Check items off with a pointer to the record that proves them (log file, export, script). The frontmatter `current_focus` names the live stage; do not reopen checked stages without being asked.

- [ ] Settle the protocol decisions marked TODO in "Protocol": final research-question wording, spatial scope, time window, study types, languages, publication types, databases.
- [ ] Decide whether to preregister the protocol (e.g., OSF; PROSPERO generally does not accept reviews outside health). Optional, but decide explicitly and record the decision.
- [ ] Write and freeze the protocol in `1-protocol/`: eligibility criteria, the fixed list of exclusion reasons, extraction field list, and search strings per database.
- [ ] Pilot the search strings; log retrieval counts; adjust and version the strings.
- [ ] Execute the searches; log each (database, date, exact string, filters, N); raw exports to `2-search/raw/`.
- [ ] Script the deduplication; record the deduplicated corpus count.
- [ ] Title/abstract screening, logged per the screening rules (decision + reason; AI-proposed vs author-made marked; samples validated).
- [ ] Full-text retrieval and screening; exclusions logged with reasons; missing full texts flagged and listed.
- [ ] Enter included studies in Zotero (keys pinned → `manuscript/references.bib`) before any citation.
- [ ] Extraction: one row per included study, fields per protocol.
- [ ] Synthesis: cause typology, classification by Chapter 1 lens, evidence map (cause × design × finding).
- [ ] DAGs: one per cause family plus integrated where supported; conflicting implied structures drawn and documented.
- [ ] Generate the PRISMA flow diagram by script; reconcile every count with the screening logs.
- [ ] Write the chapter (split into per-section checkboxes when writing starts; `docs/writing-rules.md` applies).

## Chapter's Purpose and Argument

Chapter 3 establishes the state of the art of causal research on Amazon deforestation through a systematic review conducted under the PRISMA framework. It identifies, screens, and synthesizes studies from approximately the last ten years — the window may narrow or widen depending on the initial results — that make causal claims, quantitative or qualitative, about the causes of deforestation and forest degradation in the Amazon. Descriptive, purely correlational, and projection studies are outside the scope: the object is the literature's causal claims and the assumptions behind them.

The chapter does three things with the included studies. First, it maps the causes the literature identifies, organized into a typology and read through the analytical lenses derived in Chapter 1 (economic, political economy, institutional). Second, it examines how each family of studies establishes its causal claims: designs, identification strategies, data, and the assumptions these require. Third, it formalizes the theoretical assumptions of the reviewed literature as directed acyclic graphs (DAGs), making explicit the causal structures the studies presuppose, where those structures agree, and where they conflict.

Within the dissertation, the chapter closes the gap between the interpretive history and the empirical analysis. Chapter 2 deliberately does not review this literature; it justifies the period of interest and supplies the historical context against which the reviewed studies' assumptions can be read. Chapter 3 delivers to Chapter 4 the state of the art on causal inference about Amazon deforestation: the cause inventory, the DAGs, the identification strategies already tried, and the gaps — including whether and how social policy appears in this literature as a studied cause or condition.

## Method: PRISMA workflow

The review follows PRISMA 2020. Every stage below leaves a written record in this chapter's folders; the PRISMA flow diagram's numbers must reconcile exactly with those records.

### Protocol (`1-protocol/`)

Written and frozen before searches are executed; amendments after that point are logged in the protocol with date and reason, never silently.

- **Research question.** TODO (author). Working formulation: what causes of deforestation and forest degradation in the Amazon does the recent literature identify through causal claims, and on what theoretical and methodological assumptions do those claims rest?
- **Eligibility criteria.** To be fixed in the protocol. Dimensions to decide (TODO, author):
 - Spatial scope: Brazilian Amazon only, or Pan-Amazonian.
 - Time window: publications from the last ~10 years; adjustable after initial results, with the adjustment logged as a protocol amendment.
 - Study types: empirical studies making explicit causal claims, quantitative (experimental, quasi-experimental, observational with identification strategy) or qualitative (process tracing, comparative case designs, QCA, mechanism-based accounts). Reviews and meta-analyses tracked separately for reference mining, not included as primary studies.
 - Languages: TODO (candidate: English, Portuguese, Spanish).
 - Publication types: peer-reviewed articles; whether to include working papers and book chapters is TODO.
- **Search strategy.** Databases (TODO, author; candidates: Web of Science, Scopus, SciELO for the Portuguese-language literature) and the full search strings per database, versioned in `1-protocol/`.

### Search (`2-search/`)

- Every executed search is logged: database, date, exact string, filters applied, number of records returned.
- Raw exports go to `2-search/raw/` exactly as downloaded and are never edited — the folder is immutable, like `ch4-causal-analysis/_data/raw/`.
- Deduplication is scripted (in `5-analysis/`), never manual, so the deduplicated corpus is regenerable from the raw exports.

### Screening (`3-screening/`)

- Two stages: title/abstract, then full text. Each stage produces a log with one row per record and the decision plus reason (exclusion reasons from a fixed list defined in the protocol).
- Full-text exclusions are reported with reasons, as PRISMA requires.

### Extraction (`4-extraction/`)

One structured sheet, one row per included study. Field list is fixed in the protocol; the working set:

- bibliographic record (citation key, matching `manuscript/references.bib`)
- causal factor(s) studied and claimed direction of effect
- mechanism as stated by the authors
- design and identification strategy; data and period; spatial scope and unit of analysis
- main findings, including nulls
- the study's implied causal structure (nodes and arrows), as input to the DAG work
- notes on how the study relates to the three Chapter 1 lenses

### Synthesis and DAGs (`5-analysis/`, `6-output/`)

- Narrative synthesis organized by the cause typology; an evidence map (cause × design × finding) as a table.
- DAGs are built in R with `dagitty`/`ggdag`; DAG source files live in `5-analysis/`, rendered figures in `6-output/`. One DAG per cause family, plus an integrated DAG where the literature supports it. Where studies presuppose incompatible structures, both are drawn and the conflict is discussed — conflicts are findings.
- The PRISMA flow diagram is generated from the screening logs by script, not drawn by hand.

## Reproducibility rules for the review

- `2-search/raw/` is immutable input, under the same protection logic as Chapter 4's raw data.
- Everything downstream — deduplicated corpus, screening tallies, PRISMA counts, evidence map, DAG figures — must be regenerable from `2-search/raw/` plus the logs and scripts.
- Protocol amendments, window adjustments included, are dated and logged; the review's history must be reconstructible.

## AI assistance

### Working tasks

- Draft and refine search strings per database; translate strings across database syntaxes; pilot and report retrieval counts.
- Script deduplication and the PRISMA flow diagram.
- Assist screening under the rules below.
- Assist extraction: locate in the full text the passages supporting each extracted field; quote locations, not paraphrase, when the author will verify.
- Classify included studies by cause typology and Chapter 1 lens; flag ambiguous cases for the author.
- Code DAGs in `dagitty`/`ggdag` from the extraction sheet's implied structures; keep DAG code and the extraction sheet consistent.
- For the chapter text: follow `docs/writing-rules.md`, always.

### Screening and extraction rules (strict)

- Apply the protocol's eligibility criteria as written; do not loosen, tighten, or reinterpret them. Ambiguity in the criteria is reported to the author, not resolved by judgment.
- Borderline inclusion decisions belong to the author. AI-proposed decisions are proposals: every screening decision is recorded as AI-proposed or author-made, and the author validates samples of AI-proposed decisions at both stages.
- Never fabricate, guess, or "recall" a study. Every record entering the corpus comes from a logged search export; every included study's bibliographic data is verified against the actual record and entered in Zotero (key in `manuscript/references.bib`) before it is cited.
- Missing full text: flag and list for the author; never extract from an abstract as if it were the full text.

### Boundaries

- The review reports what the literature claims, including claims the dissertation's framework disagrees with; disagreement is analyzed in the synthesis, not filtered at screening.
- Do not treat the DAGs as the dissertation's own causal model: they formalize the reviewed literature's assumptions. Chapter 4 decides what to adopt.
- Report the review's gaps and biases (language, region, publication bias) rather than smoothing them over.

### The connection to Chapters 1, 2 and 4

The full chain is canonical in `docs/dissertation-map.md`. In brief: Chapter 1 supplies the analytical lenses used to classify the reviewed studies; Chapter 2 supplies the justification for the period of interest and the historical context for reading the studies' assumptions; Chapter 3 hands Chapter 4 the state of the art — cause inventory, DAGs, identification strategies already tried, and the gaps the dissertation's empirical contribution addresses.
