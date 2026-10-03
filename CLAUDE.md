# CLAUDE.md — Dissertation project

## Context

PhD dissertation on eco-social policy and environmental governance in the
Brazilian Amazon. Author: Fernando, political science / climate policy
researcher. Four chapters: (1) narrative literature review; (2)
institutional-historical analysis; (3) PRISMA systematic review of causal
research on the causes of Amazon deforestation, with DAG construction;
(4) descriptive analysis and causal inference on the interaction of social
policy, environmental enforcement, and deforestation. A major supervisor
review (Aug 2026) triggered a dataset rebuild and a re-evaluation of the
causal design; its required changes are tracked in the guidance files of
Chapters 1, 2 and 4 under "Supervisor review — required changes (Aug
2026)" as checkbox lists (Chapter 3 postdates the review and has none):
consult the relevant chapter's list before substantive work on that
chapter, and update checkboxes (with a pointer to where the item was
addressed) as work progresses.

## Repository layout

Each chapter has its own top-level folder holding its guidance and its
working materials; shared documents live at the root and in `docs/`.

- `ch1-theoretical/`, `ch2-historical/` — guidance plus notes.
- `ch3-systematic-review/` — the PRISMA pipeline, numbered in stage
  order: `1-protocol/`, `2-search/raw/` (immutable exports),
  `3-screening/`, `4-extraction/`, `5-analysis/`, `6-output/`.
- `ch4-causal-analysis/` — the empirical pipeline: `_data/` (`raw/` and
  `reference/` immutable; `interim/`, `final/`, `aux/`), `_audit/`
  (one validation notebook per source), `_scripts/` (01-06 pipeline,
  07+ analysis, `utils/`, `models/`, `robustness/`), `rebuild/` (the
  rebuild specification: phases, templates, naming convention, RAIS
  brief, task board), `DECISIONS.md`, `output/`.
- `manuscript/` — the Quarto book (all chapters' text).
- `docs/` — `dissertation-map.md`, `writing-rules.md`, `decisions/`,
  `submitted/`.

Folder and file naming across the repository follows
`docs/repo-conventions.md` (numbered workflow folders, source-first
interim datasets, content-first final datasets); consult it before
creating or renaming anything.

The overall stage sequence (machine setup through the causal stage) is
tracked in `docs/execution-path.md`; its `current_stage` field says where
the project stands.

Orientation documents, in reading order:

- `docs/dissertation-map.md` — the cross-chapter argument chain
  (canonical as current record, not frozen commitment: across documents
  the map wins; across time the chapters lead and the map is updated in
  the same session — it is never a reason to withhold an argument
  improvement).
- Chapter guidance, read before working on a chapter: each chapter
  folder's `GUIDANCE.md`.
- `docs/writing-rules.md` — anti-AI writing rules; ALWAYS applied when
  producing or revising final text.

## Guidance documents are living records

What the dissertation-map rule says about the map holds for every
guidance document, chapter GUIDANCE files included: across documents,
within a session, the guidance as written governs; across time, the work
leads and the guidance follows. The author revises instructions as the
chapters develop, and an instruction the work has outgrown is never a
reason to suppress an improvement. When work reveals that a guidance
instruction is outdated or wrong, propose the guidance edit alongside the
work that motivated it, as an explicit flagged pair for the author's
approval — never silently rewrite standing instructions, your own least
of all. This liveliness covers argument, scope, method and workflow
content; the safety layer (immutable folders, git conduct, BigQuery
rules, citation discipline) changes only by the author's explicit
decision, and proposals to relax it are treated with suspicion, not
initiative.

## Status metadata and trust

Each `GUIDANCE.md` carries YAML frontmatter with `stage`, `updated`,
`current_focus`, and `next_milestone`. Trust it when recent; if `updated`
is more than ~2 weeks old, ask the author whether it still holds before
relying on it. Analysis scripts carry a four-line header (Purpose /
Inputs / Outputs / Status) under the same rule; pipeline scripts 01-06
follow the fuller header in
`ch4-causal-analysis/rebuild/01-script-template.md`.

## Hard rules

- **Never write to, modify, or delete anything in
  `ch4-causal-analysis/_data/raw/`, `ch4-causal-analysis/_data/reference/`
  (the frozen oracle for equivalence checks) or
  `ch3-systematic-review/2-search/raw/`.** These are the immutable layers.
  If a raw file looks wrong, report it; do not fix it.
- **Never install R or Python packages without asking.** After any approved
  install, run `renv::snapshot()` (R) so `renv.lock` stays current.
- **Git: propose, then act.** Committing and pushing are welcome, but
  always propose first: state what will be included and the commit
  message, and wait for approval before running it. Propose each commit
  (or an explicit batch); never chain commits silently after a single
  approval. Never commit unrelated changes together, and never run
  destructive git commands (`push --force`, `reset --hard`, `clean -f`,
  history rewriting).
- **Respect the reproducibility contract** (see README):
  ``ch4-causal-analysis/_data/interim/` and `_data/final/`, 
  `ch4-causal-analysis/output/` and `ch3-systematic-review/6-output/` must be 
  regenerable from the immutable inputs plus scripts and logs. 
  Never edit files in those folders directly, by any tool: to
  change generated data or output, change the generating script and
  rerun it.
- **Never silently drop observations, recode variables, or change a
  definition.** Flag every such change and log it with a decision ID in
  `ch4-causal-analysis/DECISIONS.md` (append-only; format in
  `rebuild/04-codebook-and-decisions.md`). The same logic governs the
  systematic review: screening and extraction decisions are always logged,
  and protocol amendments are dated, never silent.
- Before structural changes (refactors, file moves, pipeline redesign),
  present a plan and wait for approval.

## Languages and tools

- **R is the language of the pipeline and of all deliverable code.**
  Tidyverse style; the pipe operator is magrittr's `%>%` (loaded via
  tidyverse), never the native `|>`. Interactive work happens in
  RStudio/Positron; you can execute scripts headlessly with `Rscript` to
  verify they run. DAG work in Chapter 3 uses `dagitty`/`ggdag`.
- **Python is allowed in two places:** (a)
  `ch4-causal-analysis/_scripts/robustness/`, for independent
  reimplementation of key results as cross-checks (robustness scripts
  must never become dependencies of the main pipeline); and (b) freely,
  as throwaway tooling for routine tasks — file inspection, format
  conversion, quick verification. Two conditions on (b): no artifact the
  pipeline depends on may be produced by Python outside `robustness/`,
  and throwaway scripts are not committed (use the gitignored `scratch/`).
- Reusable functions go in `ch4-causal-analysis/_scripts/utils/` and are
  sourced by scripts; pipeline scripts 01-06 and analysis scripts 07+
  live in `ch4-causal-analysis/_scripts/`, structure and templates per
  `ch4-causal-analysis/rebuild/`.
- Chapter 2's qualitative evidence is coded in Zotero (highlights as
  quotations, tags as codes) and exported as plain text into
  `ch2-historical/evidence/`; agents never write into Zotero. Rules in
  `ch2-historical/GUIDANCE.md` (Zotero-highlights workflow).
- Dependency management: `renv`. The lockfile is authoritative.

## BigQuery

Rules in `docs/bigquery.md`; reading it is mandatory before any query,
from any session. Non-negotiable anchors: `social_employment_1` is
read-only; every query is dry-run first and executed with the 100 GB
`maximum_bytes_billed` cap; never ask for or store credentials.

## Chapter-specific critical rules

Each chapter's binding rules live in its `GUIDANCE.md`; reading it before
chapter work is mandatory, not advisory. Non-negotiable anchors: no study
enters Chapter 3's corpus except through logged search exports, and
inclusion decisions belong to the author; Chapter 4's identification
strategy is open — no design is treated as committed, and candidate
designs are separate scripts in `ch4-causal-analysis/_scripts/models/`.

## Working memory architecture

Each kind of record has one authoritative home. Route by type first:

- **A standing rule** → `CLAUDE.md` or the guidance files.
- **A past choice, including significant abandonments** ("decided not to
  pursue X because Y") → `docs/decisions/`, one dated memo each (see
  `docs/decisions/TEMPLATE.md`). Negative decisions are decisions; trivial
  dead ends need no record.
- **A granular data-pipeline change** →
  `ch4-causal-analysis/DECISIONS.md`, append-only, one D-ID entry each;
  feeds the annex. Review-side equivalents: screening/extraction
  logs and dated protocol amendments in `ch3-systematic-review/`.
- **Current state of a chapter** → its `GUIDANCE.md` frontmatter.
- **What changed in the files** → git history. Commit messages are the
  session record; write them informatively.

On a genuine contradiction between homes, precedence is: (1) `CLAUDE.md`
and guidance files, with `docs/dissertation-map.md` winning cross-chapter
claims; (2) decision memos and the data-decisions log; (3) guidance
frontmatter. Git history is the factual record of what changed and is not
overridden by any document. Never silently pick a winner: flag the
contradiction to the author — the contradiction itself is information.

**End-of-session ritual** for substantive sessions, done unprompted:
(1) update the touched chapter's frontmatter (`stage`, `current_focus`,
`next_milestone`, `updated`); (2) promote anything durable — design
choices and significant abandonments to `docs/decisions/`, data-pipeline
changes to `ch4-causal-analysis/DECISIONS.md`.

## Manuscript

- The dissertation text is a Quarto book in `manuscript/` (one `.qmd` per
  chapter). Edit `.qmd` files directly; the author reviews in the
  RStudio/Positron visual editor and via `quarto preview`.
- Citations: `@key` syntax against `manuscript/references.bib`, which is
  auto-exported from Zotero via Better BibTeX (pinned keys). Never invent
  citation keys — if a needed reference is not in the `.bib`, list it for
  the author to add via Zotero.
- **Final text always follows `docs/writing-rules.md`.**
- Renders: HTML for day-to-day preview; `docx` for supervisor comment
  rounds; LaTeX/PDF for formal versions. Do not commit rendered output
  except deliberate copies in `docs/submitted/`.
- Writing register: precise academic prose, EN or PT-BR as the document
  dictates. Direct critical feedback on the author's text is wanted.

## Review workflow

All work happens on short-lived branches off `main` (GitHub flow): branch,
work, review via diff, merge, delete the branch. Never work directly on
`main`. Work is reviewed through git diffs, not chat transcripts. Keep
changes scoped so a diff is reviewable: one concern per branch, small
commits with informative messages. Do not close GitHub issues without the
author's approval.

`README.md` is the human-facing companion to this file: it holds the full
repository map, the tag conventions for milestone versions, and the
deferred-upgrades list. Consult it when those are relevant; where the two
files overlap, this file governs agent behavior.
