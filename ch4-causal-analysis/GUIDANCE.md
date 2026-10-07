---
chapter: 4
title: Descriptive analysis and causal inference
stage: revising # dataset rebuild specified (rebuild/), execution pending; identification strategy re-opened"
updated: 2026-10-07
current_focus: "Rebuild phase 1 under way: scripts 01 (2026-10-06) and 02 (2026-10-07, branch phase1-script02) refactored. Script 02 is now 02-social-census.R, 02-social-rais.R and 02-social-bf.R with notebooks census, rais, bf and D-2026-10-07-a..j; the 12 employment and 5 quota/transfer columns reproduce the reference exactly, pea and informal drift as re-acquired sources. Next: script 03 (controls), one script per session; stage 5 re-acquisition continues in parallel (TSE, PAM/PPM, PIB, prices, Atlas)"
next_milestone: "Phase 1 gate passed: pipeline reproduces the recovered final panel on recovered inputs, with drift documented for re-acquired sources"
---

# Chapter 4 — Descriptive analysis and causal inference: working guidance

> **About this document.** Repo-wide behavior (git, permissions, the R/Python policy, renv, manuscript conventions) is governed by `CLAUDE.md`; this file adds chapter-specific guidance only, and its instructions are living: they evolve with the work (see CLAUDE.md, "Guidance documents are living records"). The cross-chapter argument chain is canonical in `docs/dissertation-map.md`. The frontmatter `stage` and `current_focus` track where the main-goals sequence stands: consult them before reopening earlier stages.

## Supervisor review — required changes (Aug 2026)

<!-- One checkbox per block of the rebuild sequence. Check items off as they are addressed and append a pointer to the record that proves them (commit hash, DECISIONS.md ID, compare_outputs report). Dissertation-level items live in dissertation-map.md. -->

- [x] Resolve every decision reserved to the author: CONFIRM items in `rebuild/naming-convention.md` and section 0 of `rebuild/upstream-reform-rais.md`. Nothing downstream starts before this. Done 2026-10-05: resolutions recorded inline in both files (naming convention version 2; RAIS brief section 0 header); baseline-window choice in `docs/decisions/2026-10-05-baseline-window-in-analysis-scripts.md`.
- [x] Phase 0 setup per `rebuild/00-rebuild-phases.md`: folder layout, salvaged originals placed in `_data/raw/` with the salvage register, recovered final panel frozen in `_data/reference/`, Drive mirror of `_data/`, `compare_outputs.R`, empty `DECISIONS.md` seeded with the known-history entries, codebook skeleton. Done 2026-10-05, commit 4966a40 (`_data/aux/provenance.csv`, `_scripts/utils/compare_outputs.R` self-test PASS, `DECISIONS.md` D-2026-10-05-a…q, `_data/aux/codebook.csv`); Drive mirror checked with 0 differences and `chflags uchg` lock confirmed the same day.
- [ ] Phase 1: refactor scripts 01-06 to `rebuild/01-script-template.md`, one script per session (provisional acceptance: assertions + audit notebook); exploration code moves to `_audit/` per `rebuild/02-audit-notebook-template.md`; then the phase 1 gate: full run compared against the recovered final panel, exact for recovered-input variables, documented drift for re-acquired sources. Progress: script 01 done 2026-10-06 (`_scripts/01-env-deforestation.R`, `_scripts/01-env-ppcdam.R`; `_audit/mapbiomas.qmd`, `_audit/ibge_biome.qmd`, `_audit/ppcdam.qmd`; `DECISIONS.md` D-2026-10-06-a…g; partial `compare_outputs` PASS on the 7 environmental and PPCDAm columns, 502 × 2000-2020); script 02 done 2026-10-07 (`_scripts/02-social-census.R`, `02-social-rais.R`, `02-social-bf.R`, utils `read_sidra.R` and `interpolate_census.R`; `_audit/census.qmd`, `rais.qmd`, `bf.qmd`; `DECISIONS.md` D-2026-10-07-a…j; partial `compare_outputs` on the 21 social columns: 12 `emp_*` and 5 `bf_*` quota/transfer columns exact, `pea`, `informal` and `bf_transfers_pea_brl_2024` drift as re-acquired-source drift per D-2026-10-07-b and D-2026-10-07-c).
- [ ] Phase 2: data fixes, one per session and commit, in the order of the phase 2 table (tasks 1.1-1.9 in `rebuild/tasks.md`; 1.10 is absorbed by phase 1).
- [ ] Phase 2R: variable rename via crosswalk per `rebuild/naming-convention.md` (task 2.1), after equivalence passes.
- [ ] RAIS reform, tasks 3.A-3.C per `rebuild/upstream-reform-rais.md`, once its section 0 decisions are confirmed; the regression rule guards the existing variables.
- [ ] Rerun analysis scripts (`08-data-analysis-2WFE-CS.R` at minimum) so the new stratum ATTs are known before annex text is written.
- [ ] Phase 3 documentation from the final state: the single annex per `rebuild/03-annex-variable-template.md` (codebook §6 rendered from `codebook.csv`, provenance §7 from the chapter README), plus the chapter README.
- [ ] Descriptive stage: tasks 4.1-4.3 in `rebuild/tasks.md` (land-based dependence via IBGE PIB by activity, rural/urban classification, 2000-2020 trajectories), then the broader descriptive program in "Main goals".
- [ ] Causal stage: identification per the methodological standards below; task 5.1 frames persistence as the sharper test of mechanism A.

## Chapter's Purpose and Argument

Chapter 2 traced the historical trajectories of social and environmental policy in the Amazon as jointly constituted, and Chapter 3 established the state of the art of causal research on Amazon deforestation, formalizing the assumptions of that literature as DAGs. Chapter 4 asks whether the interaction of the two policy tracks has explanatory power over environmental outcomes: does social policy shape deforestation, directly or by conditioning the effectiveness of environmental governance?

The chapter examines the recent trajectory of Amazon deforestation (2004–2024, subject to data availability), which divides into a sharp decline (2004–2012), a gradual and then accelerated rise (2013–2022), and a renewed decline after 2023. The literature on the first period attributes the decline mainly to environmental policy (the PPCDAm, satellite monitoring, credit conditionality, the priority-municipality list). The literature on the subsequent rise is thinner and emphasizes enforcement erosion and political signaling. Both literatures leave a puzzle unresolved. The environmental-policy apparatus credited with the decline remained formally in place after 2012, yet its application weakened and outcomes reversed. Explaining this by enforcement erosion only displaces the question to why enforcement lost effectiveness in the same territories where it had worked. The timing offers a clue. The decline coincided with the expansion of social policy and the fall of extreme poverty, while the rise coincided with their stagnation and retrenchment. This suggests that the political and administrative conditions sustaining enforcement may depend on social policy, a hypothesis the chapter sets out to test.

The chapter proceeds in two steps:

1. A descriptive section periodizes the trajectory and documents the co-movement of deforestation, environmental enforcement, social-policy coverage and social indicators, first at the national level (to establish the national trend), then at the municipal level (to allow identification through cross-municipal heterogeneity).
2. The empirical section then tests three nested hypotheses: (H1) social conditions and social policy have a direct effect on deforestation, whose sign the comparative literature leaves open; (H2) social-policy coverage moderates the effect of environmental enforcement, which is the eco-social synergy hypothesis central to the sustainable-welfare framework; (H3) this moderation is itself conditioned by electoral and institutional context.

## Theoretical Assumptions

Chapter 3 is guided by the assumption that environmental enforcement does not operate in a vacuum: its effectiveness depends on social vulnerability, labor-market conditions, distributive conflict, and uneven state presence across municipalities. Social policies may affect environmental outcomes directly (H1) or indirectly (H2), by reducing dependence on environmentally harmful livelihoods, lowering resistance to regulation, and increasing the local feasibility of compliance. Whether these mechanisms operate, and how strongly, is itself expected to depend on the local political setting (H3): electoral competition, the presence of organized agrarian interests and social movements, and the alignment of municipal governments with state and federal authorities shape both the incentives to enforce and the capacity of social policy to build support for regulation. As a result, the effects of environmental policy are expected to be heterogeneous and conditioned by welfare provision and local social conditions, and mediated by the political arenas in which enforcement and redistribution meet.

Social policies are assumed to influence environmental outcomes not only through redistribution or welfare improvement, but also by altering the incentives, vulnerabilities, and behavioral constraints of households and local actors. In frontier settings, welfare provision may reduce dependence on environmentally harmful activities, lower resistance to environmental regulation, or increase the social feasibility of compliance.

Local labor-market conditions are theoretically relevant because they shape the extent to which households and communities depend on deforestation-linked livelihoods. Where formal employment is scarce and environmentally harmful activities are central to income generation, environmental enforcement is likely to face stronger resistance. Where more stable and protected income alternatives exist, compliance may be easier politically and socially.

Political and institutional conditions are theoretically relevant because the mechanisms described above do not translate into outcomes automatically; they pass through local arenas in which enforcement is negotiated, resisted or supported. Whether social policy lowers resistance to regulation depends on who organizes that resistance and how much leverage they hold over municipal government. Where agrarian elites dominate local politics and electoral competition is weak, the incentives to enforce are low regardless of welfare coverage, and social transfers may coexist with tolerated clearing. Where competition is closer, where rural workers' organizations and social movements are present, and where municipal governments are aligned with the state and federal authorities that fund both enforcement and social programs, the coalition sustaining compliance is broader and the complementarity between social and environmental policy is more likely to materialize. Political conditions are therefore treated as a second-order moderator (H3): they do not replace the social mechanisms but determine the extent to which those mechanisms can operate.

The chapter assumes that the interdependence between social and environmental policy is not only a theoretical claim but something that can be detected empirically through variation in policy exposure, local social conditions, and environmental outcomes over time. This applies to the direct and moderating effects of social policy (H1, H2) as much as to their political conditioning (H3), which requires observable variation in electoral competition, coalition composition and intergovernmental alignment across municipalities and electoral cycles. This justifies the effort to move from a conceptual framework to an identification strategy.

The Brazilian Amazon is treated as a critical case in which these eco-social interdependencies can be empirically examined. The Brazilian Amazon concentrates the conditions under which the relationship between social policy and environmental outcomes becomes most visible: historically high social vulnerability, persistent inequality, environmental degradation, recent welfare expansion, and uneven enforcement. Brazilian federalism also has the multilevel characteristic appropriate to cross-municipality comparisons: policy outcomes are shaped by uneven subnational enforcement and variation in local institutional environments. The same structure makes the political conditions posited under H3 observable rather than assumed. Municipal elections held on a fixed four-year cycle, frequent changes in the alignment between mayors, governors and the federal government, and the uneven territorial presence of agrarian interest groups and social movements generate the variation in political arenas across which the interaction between social and environmental policy can be compared.

## Sources of data for empirical operationalization

The chapter does not fix its empirical goals in advance. Its purpose is to establish whether, where and through which instruments social policy bears on deforestation, and the list that follows is a map of possibilities rather than a research design. It groups candidate sources of identifying variation on the enforcement side, social-policy variables, political and institutional variables, subnational programs, and the post-2023 period.

### 1. Sources of identifying variation (enforcement side, for H2)

- 2008 priority-municipality list, staggered entry and exit; interact with pre-2008 social-policy coverage. Data: MMA portarias; Assunção & Rocha and Cisneros et al. have the panels.
- Resolução BACEN 3.545 (2008), credit conditionality in the Amazon biome. National, but exposure varies with municipal dependence on rural credit (Assunção, Gandour, & Rocha).
- CAR rollout after 2012, staggered by state (Pará and Mato Grosso earlier).
- Municipal-level enforcement intensity for the full period: IBAMA fines and embargoes by municipality.

### 2. Social-policy variables (H1, and the moderator in H2)

- Bolsa Família: coverage rate, transfers per capita, share of families in extreme poverty covered. Note coverage begins 2004.
- Minimum wage: national, so use exposure (share of employment at or near the minimum, formal share from RAIS).
- Social expenditure by function: health, education, social assistance from FINBRA/SICONFI. Health can be disaggregated by ESF/PSF coverage.
- Local state capacity: IGD-M (Índice de Gestão Descentralizada, the federal index of municipal Bolsa Família administration; directly measures the quality of social-policy delivery), MUNIC survey (environmental secretariat, council, fund, staff), municipal employees per capita. Careful: capacity is also a determinant of enforcement, so it belongs in H2 as a moderator, not as a control.
- Bolsa Verde (2011–2018): eco-social transfer with environmental conditionality, targeted at conservation units, settlements and CAR-registered areas. Termination in 2018 is an event. It's an H1/H2 hybrid, since the conditionality makes it partly an environmental instrument.
- Brasil Sem Miséria rural productive inclusion: Fomento (Programa de Fomento às Atividades Produtivas Rurais), created by Law 12.512/2011 alongside Bolsa Verde as the rural axis of Brasil Sem Miséria, combined a non-refundable grant with technical assistance for families in extreme poverty engaged in family agriculture, extractivism or fishing. Its intensive phase ran from 2011 to 2014, with coverage concentrated in the Northeast but the North treated as a priority region. Its relevance to the chapter lies in being a productive rather than a consumption transfer: unlike Bolsa Família, it altered what households could do with their land, so its effect on clearing is ambiguous a priori, and estimating it separately tests whether the results under H1 and H2 reflect income and risk or changes in agricultural capacity. The staggered issuance of ATER public calls by territory and year offers a source of exposure that did not depend on municipal choice. Scale in the Amazon may be too small for municipal estimation, data are harder to assemble than for Bolsa Família, and the window offers short pre- and post-periods, so the item is exploratory.
- Territórios da Cidadania (2008): federal territorial program with staggered selection, many Amazon territories. Treatment at territory level, cleanly documented, underused in this literature.
- Pronaf: family-agriculture credit. Direction is genuinely ambiguous, may raise clearing. Data: BACEN/MDA by municipality.
- Previdência rural and BPC: transfers per capita; near-universal, so variation is exposure not adoption.
- Luz para Todos (2003–): rural electrification, staggered; likely increases deforestation, useful as a falsification or as a contrast to transfers.

### 3. Political-institutional variables (H3)

- Electoral competitiveness: margin of victory in mayoral elections, effective number of candidates (TSE).
- Vertical alignment: mayor's party vs. governor and president (Brollo & Nannicini design).
- Reelection incentives: first- vs. second-term mayors (Ferraz & Finan).
- Mayor's party ideology.
- Interest groups: cattle herd share (PPM/IBGE), soy area (PAM), municipal vote share of Frente Parlamentar da Agropecuária deputies.
- Social movements: MST settlements (INCRA), CPT land-conflict records, rural workers' unions (STR) presence.
- NGO presence is hard to measure directly; Fundo Amazônia project locations (2009–2019, resumed 2023) are one proxy.

### 4. Subnational eco-social programs (natural experiments within states)

- Programa Municípios Verdes, Pará (2011): voluntary adhesion, so selection is the problem; synthetic control against Mato Grosso/Amazonas municipalities, and check overlap with the priority list, since many PMV municipalities joined precisely to exit it. Note it's an environmental-governance program, not a social one, so it speaks to H3 (local state capacity and coalitions) more than H2.
- Bolsa Floresta, Amazonas (2007–): payments to families in state conservation units, the most explicitly eco-social program in the region; Amazonas only.
- SISA, Acre (2010): state PES system, plus the broader "Florestania" policy package (1999–2018).
- MT Legal (2008–09) and the PCI strategy, Mato Grosso (2015).
- ICMS Ecológico: state-level fiscal transfer to municipalities with conservation criteria, adopted at different dates (Rondônia 1996, Tocantins 2002, Mato Grosso 2000–01, Acre 2009, Pará 2012–14). Staggered adoption across states, and it's money to municipal budgets, which links it to the social-expenditure variables in section 2.

### 5. Post-2023

- PPCDAm 5th phase and União com Municípios (2024). Too recent for estimation, but usable as an out-of-sample check.

## AI assistance

### Main goals, in order

The frontmatter `current_focus` records which goal is live; do not reopen completed goals without being asked. The supervisor-review checklist above is the operational sequence; the goals here state what each block is for.

1. **Rebuild the database** per `rebuild/00-rebuild-phases.md` and its companion files. The rebuild specification governs structure, order and acceptance; open GitHub issues and the report's suggestions are absorbed into its phases. Within it: beyond what the templates in `rebuild/` require, avoid wholesale refactoring and do not add boilerplate comment headers; the template header is the only sanctioned boilerplate, body comments answer "why" and never "what", with no narration of steps, and changes should read as revisions to the author's code.
2. **Produce the annex** ("Data description") from the final state, per `rebuild/03-annex-variable-template.md`. One document: the codebook is its section 6, rendered from `_data/aux/codebook.csv`; provenance is section 7, rendered from the chapter README. It must work for replication: variables, construction, manipulation, decisions.
3. **Conduct the descriptive and exploratory analysis** under the author's guidance and oversight: identify and interpret variation in social and environmental indicators across municipalities and over time, and produce publication-quality graphs and tables.
4. **Propose, explore, and construct the identification strategy** under the author's oversight; interpret results and produce publication-quality graphs and tables.
5. **Throughout:** connect theoretical assumptions to empirical expectations, specify and justify statistical models, interpret results within the limits of data and design, and organize the chapter's empirical narrative so that it is rigorous and readable.

### Expected empirical tasks

- Explore distributions, trends, and heterogeneity in key variables.
- Compare municipalities across dimensions of social vulnerability, social-policy exposure, institutional and political aspects, enforcement exposure and environmental outcomes.
- Examine temporal variation, with attention to policy timing.
- Write R code for descriptive statistics, plots, and model preparation.
- Assess data quality: measurement choices, missingness, scaling, transformations, boundary changes and comparability over time.
- Distinguish descriptive evidence from causal claims.
- When proposing causal models, state the estimand, the identification logic, the required assumptions, threats to inference and the robustness checks that follow from them.

### Methodological standards

Treat this as a serious empirical social science project.

- Each candidate design is a separate script in `ch4-causal-analysis/_scripts/models/`, consuming the same cleaned panel and writing results in a common format to `output/`.
- Present trade-offs across candidate designs rather than a single recommendation, unless explicitly asked to pick one.
- Statistical significance is never a design criterion: never favor a specification, sample, or estimator because of the results it produces.
- No generic regression advice detached from the research question.
- Connect model choice to the substantive argument and the theoretical mechanisms.
- Treat exploratory analysis, model specification, and causal identification as distinct steps.
- Be explicit about the assumptions behind each strategy, and about which assumption each robustness check tests.
- When identification is not credible, say so and propose alternatives or more cautious interpretation.
- Identify additional data that would improve specification, reduce omitted-variable bias or better capture the mechanisms.
- Weigh trade-offs among fixed effects, difference-in-differences, interaction models, event-study designs, matching and other panel strategies according to the structure of treatment variation. Where treatment is staggered, prefer estimators robust to heterogeneous effects over two-way fixed effects, and always report pre-trends.
- Pay close attention to temporal ordering, unit-level heterogeneity and policy timing.
- Flag post-treatment bias, omitted-variable bias, bad controls, reverse causality and weak identification.
- Report null results.
- For mechanism A, persistence (does compliance hold after enforcement pressure fades) is the sharper test than the initial moderation estimate; state before estimation what result would count against the mechanism (task 5.1 in `rebuild/tasks.md`).

**Governing rule for derived variables.** A derived variable is stored in the final dataset only if its value does not depend on which rows end up in an estimation sample, and at least one of the following holds: it needs upstream inputs (deflators, denominators, source joins), it must be computed on the full balanced panel (lags), or it encodes a design decision that has to be identical across all analysis scripts. A derived variable is built in the analysis scripts if its value depends on the estimation sample (pooled or cross-sectional z-scores, median splits, percentile ranks) or if it is a parameter varied in robustness checks (baseline windows).

### Use of theory

The project provides theoretical assumptions and conceptual expectations. Use them to derive hypotheses, clarify mechanisms, guide variable selection, inform interaction terms and heterogeneity analysis, and assess whether the empirical strategy is aligned with the theory. Chapter 3's systematic review supplies the state of the art: use its cause inventory and DAGs for variable selection and confounder identification, and to position this chapter's designs against the reviewed literature and its gaps. Do not translate theory into variables mechanically; explain how each empirical choice does or does not capture the underlying concept.

### Style of assistance

**When helping me:**

- write in a clear academic style, precise and methodologically rigorous;
- explain trade-offs among alternatives;
- suggest better formulations when my approach is conceptually imprecise;
- prioritize analytical clarity over unnecessary complexity.

**When writing code** (repo-wide code rules are in `CLAUDE.md`; chapter-specific additions):

- keep in-code comments short and explain the reasoning in the conversation rather than in the script;
- do not close GitHub issues without my approval;
- never silently drop observations, recode variables or change a definition; flag every such change and record it in `ch4-causal-analysis/DECISIONS.md` with a decision ID (format in `rebuild/04-codebook-and-decisions.md`), which feeds the annex.

**When helping with writing:**

- build coherent paragraphs that connect theory, data, method and findings;
- avoid causal language unless identification supports it;
- maintain a dissertation-appropriate tone;
- follow `docs/writing-rules.md`: always.

### Boundaries

- Do not invent data or assume variables that are not in the database, except when explicitly discussing hypothetical alternatives.
- Do not treat correlations from exploratory analysis as causal findings.
- Do not recommend methods because they are sophisticated; prefer methods appropriate to the data structure, theoretically justified and interpretable.
- If information is missing or the design is underdetermined, state the uncertainty and propose the most reasonable options.
