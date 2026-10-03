# Eco-Social Policy and Environmental Governance in the Brazilian Amazon
# PhD dissertation in Political Science, University of São Paulo (USP).

PhD dissertation research compendium. Four main chapters:

1. **Chapter 1 — Theoretical review.** Narrative review of environmental
   politics and policy traditions, the Eco-Social State literature and its
   gaps, deriving three operationalizable dimensions for frontier settings.
2. **Chapter 2 — Historical trajectory.** Interpretive history of the
   Brazilian Amazon's environmental, economic, and social trajectory; the
   two-arenas contrast and the eco-social arena with mechanisms A/B.
3. **Chapter 3 — Systematic review.** PRISMA review of the last ~10 years
   of causal research on the causes of Amazon deforestation (quantitative
   and qualitative causal claims), with the literature's theoretical
   assumptions formalized as DAGs.
4. **Chapter 4 — Descriptive analysis and causal inference.** Whether and
   under what conditions social policy shapes deforestation, directly or by
   conditioning enforcement effectiveness (H1–H3; see
   `ch4-causal-analysis/GUIDANCE.md`).

## Repository map

```
CLAUDE.md                  Global instructions for AI-assisted work
.claude/                   Claude Code project settings (permissions)
docs/
  repo-conventions.md      Folder and file naming rules for the whole repo
  dissertation-map.md      Canonical cross-chapter argument chain
  writing-rules.md         Anti-AI writing rules (always applied to final text)
  decisions/               Dated decision memos, incl. significant abandonments
  submitted/               Frozen copies of rendered versions actually sent
ch1-theoretical/           GUIDANCE.md + notes/
ch2-historical/            GUIDANCE.md + notes/ + evidence/
ch3-systematic-review/     GUIDANCE.md + PRISMA pipeline (numbered stages):
  1-protocol/              Protocol, eligibility criteria, search strings
  2-search/raw/            Database exports as downloaded. IMMUTABLE.
  3-screening/             Screening logs (title/abstract, full text)
  4-extraction/            Extraction sheet(s)
  5-analysis/              Dedup, PRISMA flow, synthesis, DAG scripts (R)
  6-output/                PRISMA diagram, DAG figures, evidence-map tables
ch4-causal-analysis/       GUIDANCE.md + empirical pipeline:
  _data/raw/               Immutable inputs. NEVER modified by scripts or AI.
  _data/reference/         Frozen oracle for equivalence checks. Immutable;
                           gitignored, backed up outside git.
  _data/interim|final|aux/ Regenerable pipeline outputs; deflators, codebook
  _audit/                  One validation notebook per source (Quarto)
  _scripts/                01-06 pipeline, 07+ analysis; utils/, models/,
                           robustness/ (Python cross-checks only)
  rebuild/                 Rebuild specification: phases, templates, naming
                           convention, RAIS brief, task board
  DECISIONS.md             Append-only decision log, D-IDs (→ annex)
  output/                  Generated figures and tables. Regenerable.
manuscript/                Quarto book project (the dissertation text)
scratch/                   Gitignored throwaway scripts (any language)
renv.lock                  Exact R package versions (renv)
```

Supervisor-review (Aug 2026) items are tracked as checklists inside the
guidance files of Chapters 1, 2 and 4 (Chapter 3 postdates the review).

## Reproducibility contract

`ch4-causal-analysis/_data/interim/` and `_data/final/`, 
`ch4-causal-analysis/output/` and `ch3-systematic-review/6-output/` must be 
regenerable from the immutable inputs.
(`ch4-causal-analysis/_data/raw/`, `ch3-systematic-review/2-search/raw/`)
plus the scripts and logs; `compare_outputs.R` checks builds against the
frozen `_data/reference/`. If that
contract ever breaks, fixing it takes priority over new analysis. PRISMA
flow numbers must reconcile exactly with the screening logs.

To rebuild the environment on a new machine: open R in the project root and
run `renv::restore()`.

## Git conventions

- **GitHub flow.** `main` is always the best current state. Each unit of work
  happens on a short-lived branch, is reviewed via diff, merged, and deleted.
- **Tags mark milestones** ("final" versions): e.g. `pre-reorg`,
  `v1-ch4-submitted`, `qualifying-draft`. Rendered artifacts actually sent
  out also get a dated copy in `docs/submitted/`.
- Commit small and often, with informative messages — the commit log doubles
  as the session record. Push often.

## Deferred upgrades (revisit when the need arises)

- `{targets}` pipeline to formalize the dependency graph once the analysis
  pipeline stabilizes.
- Git worktrees for parallel Claude Code sessions on separate branches.
- Quarto thesis-format extension wrapping the university's LaTeX template
  (title page, front matter, institutional formatting) once template files
  are gathered.
- Session logs (`logs/` folder) if, during the rebuild, git history proves
  insufficient as a record of what happened between commits.
- Reference-manager integration for the systematic review corpus (e.g. a
  dedicated Zotero collection per screening stage) once searches run.
- In Chapter 2, if mid-coding, the manual application of accepted tags proves genuinely burdensome at corpus scale, there's a controlled relaxation available — an agent applying approved tags from an approved proposals file to your existing highlights, never creating highlights or notes.
