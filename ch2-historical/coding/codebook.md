---
role: codebook
chapter: 2
status: draft for the author's approval
created: 2026-10-18
updated: 2026-10-20
source: "ch2-historical/GUIDANCE.md (dimensions as revised 2026-10-18); the author's draft codebook of 2026-10-18; revised 2026-10-20 for coding in ATLAS.ti (docs/decisions/2026-10-20-ch2-highlights-in-zotero-coding-in-atlasti.md)"
---

# Chapter 2 codebook: code vocabulary and color map

The evidence for Chapter 2 is highlighted in Zotero and coded in ATLAS.ti
(decision of 2026-10-20). In Zotero a highlight is a quotation, its color
is the dimension, its comment is a one-line paraphrase of the claim in
English. The importer brings each source's PDF into ATLAS.ti, named by
citation key, and places every highlight on it as a quotation pre-coded
with its dimension code; every other code is applied in ATLAS.ti. This file is the only
authority on which codes exist and what they mean. A code not listed here
is not applied; a new code is added here, dated, before its first use
(see "Adding codes"). Coding is the author's act.

## Rules

1. **One string per code, exact.** Lowercase ASCII, no spaces, no
   diacritics, hyphen as the only separator, form `family-value`. Two
   levels at most: nothing like `gov-local-resistance` (that is two codes,
   `gov-municipal` and `dir-exploit`). No bare family codes (`gov` alone
   is not a code).
2. **Flat codes, no parent-child hierarchy.** The family prefix groups
   the codes for sorting and for the scripts, nothing more. ATLAS.ti code
   folders and code groups may mirror the families as organization, but
   no code is a child of another. Hierarchy is replaced by combination: a
   quotation carries one code from each family that applies.
3. **Single-valued and multi-valued families.** Dimension, period and
   voice take exactly one value per quotation. Asset, field, level,
   direction, activity, actor and policy take as many values as the
   passage contains, and may be empty.
4. **Refinement codes match the dimension.** Asset codes go on `d1`
   quotations only, field codes on `d2` only, level codes on `d3` only.
   Direction codes go on `d2` or `d3`. A validation script enforces this
   on the exported quotation table.
5. **The quotation is the evidence; the comment is the claim.** The
   comment restates, in one English sentence, what the passage says that
   matters for the chapter. The matrix cell shows the comment and the
   quotation together, with page and citation key.
6. **What is not coded.** Phases 1 and 2, the two-arenas contrast and
   mechanisms A and B are interpretations made at matrix time, not codes
   (author's decision, 2026-10-18); "Retrieval of the post-coding
   constructs" below says how each is found from the codes. Source tier
   and the author's discipline are item-level attributes and live in
   `corpus-inventory.csv`, not in codes.

## Dimension (single-valued, required): `d0` to `d3`, set by the Zotero color

Colors are Zotero 7's fixed palette. Yellow is Zotero's default highlight
color, so a highlight the coder leaves yellow is context by default. The
importer turns the color into the dimension code; in ATLAS.ti the code
carries the matching palette color.

| Zotero color | Hex | Code (ATLAS.ti color) | Question the passage answers |
|---|---|---|---|
| Red | `#ff6666` | `d1-conflict` (Red) | **Distributive conflict:** who captures the rents of degradation, who bears the costs of conservation, how the conflict is organized |
| Blue | `#2ea8e5` | `d2-institutions` (Light Blue) | **Institutional configuration:** what rules and provisions the state has in place: a law, a program, a credit line, a decree, a protection regime |
| Green | `#5fb236` | `d3-governance` (Light Green) | **Multilevel governance:** how rules are made and applied across levels of government: an operation, a fine, a refusal to enforce, a conflict between levels, an actor's political behavior |
| Yellow | `#ffd400` | `d0-context` (Yellow) | **Context:** events, dates, figures and background with no dimension (the chronology the narrative needs) |

**The d2/d3 line** is the line between a rule and its enforcement. A
protected-area decree is blue; the operation that evicts invaders from it
is green. The Forest Code is blue; the decision not to fine smallholders
under it is green. **The d1/d2 line** is the line between the
distribution of gains and losses and the institutions that produce it:
"SUDAM incentives went to large ranchers" is red if the point is who
gained, blue if the point is what the incentive was. When a passage does
both, choose the point the writer is making and put the other in the
comment.

Purple, magenta, orange, gray and black are not in the map. A highlight in
those colors arrives in ATLAS.ti without a dimension code and the importer
reports it; the author assigns the dimension code in ATLAS.ti or recolors
the highlight in Zotero before the import. Highlights made before this
codebook (2026-10-18) are the expected case.

## Code families

### Period (single-valued, required): `p1` to `p6`

The six periods of the guidance. A highlight that spans periods takes the
period of the event it reports; a passage comparing periods takes the
later one, and the comment names the comparison.

| Code | Years | Content |
|---|---|---|
| `p1-rubber` | c. 1870–1952 | Rubber cycles and extractivism |
| `p2-integration` | 1953–1985 | State-led integration and colonization: SPVEA, SUDAM, Transamazônica, directed settlement, intensified 1964–1985 |
| `p3-founding` | 1985–2003 | Democratization, the 1988 Constitution, the founding of both tracks |
| `p4-ppcdam` | 2004–2015 | The PPCDAm era: enforcement, monitoring, protected-area expansion, concurrent welfare expansion |
| `p5-drift` | 2016–2022 | Asymmetric drift: environmental dismantling versus social-track survival |
| `p6-present` | 2023– | Present conjuncture |

Two deviations from the author's draft, for the author's approval: the
draft's `p3_pressure` is `p3-founding` here, since the guidance names the
period by the founding of both tracks; and the years 1946–1963, which
neither the guidance nor the draft assigned, are given to `p2-integration`
(SPVEA, 1953, and the Belém–Brasília highway, 1960, are integration
policies by the chapter's own criterion). Period boundaries remain
provisional in the guidance; a change there changes this table.

### Asset (multi-valued; red highlights only): `as-`

Chapter 1's asset typology, the reading key of dimension 1. The code names
the asset position of the actor the passage is about: what the actor
gains from (deforestation or conservation) and in what form (rent or
livelihood). The first segment names the Amazon instance of Chapter 1's
axis: `deforest` is the degradation-forcing side, `conserv` the
conservation-aligned side. "Dependent" is not used in these names because
in the dissertation dependence names the livelihood relation, which is
the second axis.

| Code | Holder |
|---|---|
| `as-deforest-rent` | Gains rent from deforestation; loss is marginal: ranchers, land speculators, soy and timber capital, meatpackers |
| `as-deforest-livelihood` | Lives from deforestation, directly or indirectly; loss is existential: workers in deforestation-linked activities and their households, local commerce and services that depend on them, smallholders who clear, garimpeiros |
| `as-conserv-rent` | Gains rent from conservation: bioeconomy, carbon-credit and tourism capital |
| `as-conserv-livelihood` | Lives from the standing forest; loss is existential: indigenous peoples, extractivists, ribeirinhos, fishers |

Gaining or losing is not tagged: it follows from the asset position and
the direction of the action in the passage (a forcing holder loses under
enforcement and gains under subsidy). The draft's `win_`/`loose_` subcodes
are therefore dropped, as are its three asset names, which predate the
revised typology. A red highlight about the organization of conflict
itself (violence, land conflict counts, coalitions) carries the actors
involved and no asset code if the passage does not say what they hold.

### Field (multi-valued; blue highlights only): `inst-`

Which field of the institutional configuration the passage describes.
The four fields are the guidance's three capture areas, with land tenure
separated from market regulation because in the Amazon it is a policy
field of its own (colonization, titling, regularization).

| Code | Covers |
|---|---|
| `inst-market` | Credit, fiscal incentives, subsidies, prices and trade rules, supply-chain agreements, credit conditionality |
| `inst-land` | Land tenure, colonization and settlement, titling and regularization, land-grabbing rules |
| `inst-social` | Labor regime and social protection: contributory and non-contributory benefits, cash transfers, health, education, labor rights, and the fiscal base they rest on |
| `inst-conserv` | Conservation rules: protected areas and extractive reserves, the Forest Code, licensing, monitoring systems as rules, embargo and sanction rules |

The draft's `security_` is not kept: if it meant the national-security
doctrine of the military period, that is a motive for integration policy
and belongs in the comment or in a `dir-exploit` green highlight on the
federal role; if it meant public security and rural violence, that is
dimension 1. The author decides whether either reading needs a code.

### Level (multi-valued; green highlights only): `gov-`

| Code | Covers |
|---|---|
| `gov-federal` | Federal executive and agencies (IBAMA, INCRA, SUDAM, ministries, Federal Police), Congress, federal courts |
| `gov-state` | State governments and agencies (state environmental agencies, state land institutes), state assemblies |
| `gov-municipal` | Municipal governments, mayors, councils |
| `gov-intergov` | Relations between levels: alignment and conflict, decentralization of licensing, transfers and conditionality, federal pressure on subnational governments |
| `gov-international` | International pressure on Brazilian governance: foreign governments, multilateral organizations and donors, international agreements, foreign markets and consumer campaigns, when the passage is about their action on rule-making or enforcement (added 2026-10-20) |
| `gov-accountability` | Relations between governments or agencies and the accountability system: courts, the Ministério Público (the parquet), audit courts, the Federal Police as investigator, the press and NGOs as watchdogs, when the passage is about their action on government conduct; a relation, not a level of government (added 2026-10-20) |

The draft's `local_supportive_` and `local_resistance_` are split into
level and direction (next family), because the stance toward protection
belongs to every level, including the federal one.

`gov-international` and `gov-accountability` (author's addition,
2026-10-20) describe who acts on government, from outside the federation
or from its accountability bodies; they go on `d3` quotations only, like
the rest of the family. They are kept apart from the actor codes
`who-international`, `who-ngo` and `who-movement`, which name an actor
present in the passage whatever its dimension: a donor's grant to a
state program is `who-international` on a `d2` quotation; the same
donor suspending funds until enforcement resumes is `gov-international`
(with `who-international`) on a `d3` quotation. The press has no actor
code; it appears only through `gov-accountability`.

### Direction (single-valued when present; blue or green highlights): `dir-`

The direction in which the rule or the action pushes land use.

| Code | Meaning |
|---|---|
| `dir-exploit` | Promotes occupation, clearing, extraction or impunity for them (incentives, colonization, amnesty, non-enforcement) |
| `dir-protect` | Restrains clearing or protects the forest and its peoples (protected areas, enforcement, conditionality, demarcation) |

This family carries the federal role reversal (dimension 3, point a) and
the state-manufactured rents of phase 1 (dimension 2). A passage about a
rule with no direction (a procedural change, a budget figure) takes no
direction code.

### Activity (multi-valued): `act-`

| Code | Covers |
|---|---|
| `act-cattle` | Ranching, pasture, the beef chain |
| `act-crops` | Soy, grains, other agriculture |
| `act-logging` | Timber extraction, legal and illegal |
| `act-mining` | Garimpo and industrial mining |
| `act-extractive` | Rubber, nuts, fishing, other forest products |
| `act-infra` | Roads, dams, hydroelectric plants, ports, colonization infrastructure |

The draft's `agro_` is split into cattle and crops because the two have
different rent structures, actors and policy histories, and Chapter 4
measures them separately. Extractive and infrastructure activities are
added because period 1 and the frontier's opening cannot be coded without
them.

### Actor (multi-valued): `who-`

Social actors. State actors are not in this family: they are coded by
level (`gov-`) because that is what the chapter asks about them.

| Code | Covers |
|---|---|
| `who-landowner` | Large landowners, ranchers, agribusiness and allied capital (meatpackers, traders, banks) |
| `who-smallholder` | Small and medium farmers, settlers (colonos), posseiros |
| `who-worker` | Rural wage and informal labor, including garimpeiros as workers, and forced labor |
| `who-local-economy` | The population of frontier municipalities whose income depends indirectly on a deforestation-linked activity: commerce, services, urban residents, the municipal economy as a whole |
| `who-indigenous` | Indigenous peoples and their organizations |
| `who-extractivist` | Rubber tappers, nut gatherers, ribeirinhos, artisanal fishers, quilombolas |
| `who-movement` | Social, environmental and socio-environmental movements, unions, church organizations (CPT, CIMI) |
| `who-ngo` | Domestic and international NGOs and foundations |
| `who-international` | Foreign governments, multilateral organizations, donors, foreign markets and consumers |

`who-local-economy` is the actor counterpart of the indirect dependence
in `as-deforest-livelihood`: use it when the source speaks of the town,
the local economy or the population at large, and the producer or worker
codes when it names a producing group. The two can co-occur on a passage
that names both.

The draft's split of movements into environmental and social is folded
into `who-movement`: the socio-environmental fusion is an interpretive
claim of the chapter, and it is retrieved by the co-occurrence of
`who-movement` with `who-extractivist` or `who-indigenous` on blue or
green highlights, not by a code that presupposes it. The draft's
Portuguese labels are in English for consistency with every other code
and for the scripts.

### Policy (multi-valued, open family): `pol-`

Named policies, programs and institutions, as retrieval keys. One slug per
policy, short, lowercase ASCII; the registry below is seeded from the
guidance and grows inductively (see "Adding codes"). Policy codes say
*which* policy; the color and the field codes say what kind of thing it
is in the passage.

| Code | Policy |
|---|---|
| `pol-spvea` | SPVEA (1953) |
| `pol-sudam` | SUDAM and its fiscal incentives (1966–) |
| `pol-pin` | Programa de Integração Nacional and the Transamazônica (1970) |
| `pol-polamazonia` | Polamazônia (1974) |
| `pol-funrural` | FUNRURAL (1971) |
| `pol-forest-code` | Forest Code, 1965 and 2012 |
| `pol-ibama` | IBAMA (1989) |
| `pol-snuc` | SNUC, the protected-area system (2000) |
| `pol-resex` | Extractive reserves |
| `pol-sus` | SUS |
| `pol-bpc` | BPC |
| `pol-bolsa-familia` | Bolsa Família (2003) and its successors |
| `pol-ppcdam` | PPCDAm (2004) |
| `pol-deter` | DETER and PRODES monitoring |
| `pol-credit-3545` | Rural credit conditionality, Resolução 3.545 (2008) |
| `pol-soy-moratorium` | Soy moratorium (2006) |
| `pol-beef-tac` | Cattle agreements (TAC da carne, 2009) |
| `pol-amazon-fund` | Fundo Amazônia (2008) |
| `pol-bolsa-verde` | Bolsa Verde (2011) |
| `pol-seguro-defeso` | Seguro-defeso |
| `pol-terra-legal` | Terra Legal (2009) |
| `pol-car` | CAR, the rural environmental registry (2012) |

The draft's `social_inclusion`/`social_exclusion` subcodes are not kept:
whether a social policy included or excluded frontier populations is the
phase-1/phase-2 reading, which the author placed outside the codebook; it
is retrieved from `inst-social` by period, and the comment states the
inclusion or exclusion the passage reports.

### Voice (single-valued, required): `v-`

Whose claim the passage carries. This family enforces the guidance's
register discipline (narrative, interpretive and hypothesized claims kept
apart) and the source-tier rule at matrix time.

| Code | Meaning |
|---|---|
| `v-fact` | The writer reports an event, date, figure or decision (a narrative claim) |
| `v-science` | The writer reports a research finding (deforestation measurements, remote sensing, econometric or social-science results), cited or paraphrased; the comment names the study and the discipline when the passage gives them |
| `v-interp` | The writer interprets, explains or judges (an analytical claim; journalistic-tier sources cannot be the sole support) |
| `v-actor` | A claim attributed in the text to a named actor (a politician, a rancher, a leader, a document) |

The draft's `Claim_` and `Conclusion_` are `v-actor` and `v-interp`;
`v-fact` is added so that chronology is not forced into either, and
`v-science` replaces the draft's `Science_` family (author's
clarification, 2026-10-18: it codes scientific findings). The draft's
split into natural and social science, and of social science into
economics, sociology and political science, is not kept as codes: the
comment names the discipline, and a split can be added if the matrix
turns out to need it.

### Dropped from the draft

- `Science_` as a family: replaced by the single voice value `v-science`
  (see Voice). The discipline of a source as such is an attribute of the
  item and goes to a `discipline` column in `corpus-inventory.csv`.
- Dimension tags as Zotero tags: the color carries the dimension in
  Zotero. Since 2026-10-20 the importer turns the color into the
  dimension code `d0`–`d3` in ATLAS.ti, so the family exists as codes
  there and is never typed by hand.

## Per-highlight procedure

In Zotero, while reading:

1. Highlight the passage; set the color to its dimension (leave yellow
   for context).
2. Write the comment: one English sentence stating the claim.

After the source is fully highlighted, import it (the guidance's
"Zotero and ATLAS.ti workflow"). In ATLAS.ti, per quotation:

3. Check the dimension code the importer set; add it by hand on
   off-palette highlights.
4. Code the period (`p1`–`p6`) and the voice (`v-`).
5. Code the refinement of the dimension: `as-` on `d1`, `inst-` on `d2`,
   `gov-` on `d3`; `dir-` on `d2` or `d3` when the passage has a
   direction.
6. Code `act-`, `who-` and `pol-` for every activity, actor and policy
   the passage names.

Minimum codes on a `d1`–`d3` quotation: dimension, period, voice, one
refinement. Minimum on a `d0` quotation: dimension, period, voice.

## Retrieval of the post-coding constructs

How the constructs the author placed outside the codebook are found from
the codes at matrix time, which is the test that the vocabulary is
sufficient:

- **Phase 1 / phase 2:** by period (`p1`–`p2` versus `p3`–`p6`), read
  across `inst-market` and `inst-land` with `dir-exploit` (state-made
  rents) and `inst-social` (exclusion, then incorporation).
- **Two-arenas contrast:** green highlights with `gov-intergov` or
  `gov-municipal`, compared between `inst-social` policies (`pol-sus`,
  `pol-bolsa-familia`) and `inst-conserv` enforcement with `dir-protect`.
- **Mechanism A:** `inst-social` or `pol-resex`, `pol-bolsa-verde`,
  `pol-seguro-defeso` co-occurring with `dir-protect` and
  `as-deforest-livelihood` or `as-conserv-livelihood`.
- **Mechanism B:** green highlights with `dir-exploit` co-occurring with
  `as-deforest-livelihood`, `who-local-economy` or `who-smallholder`
  (non-enforcement justified
  by vulnerability).
- **Asymmetric drift:** `p5-drift`, `inst-conserv` and `gov-federal` with
  `dir-exploit`, against `inst-social` in the same period.
- **Federal role reversal:** `gov-federal` with `dir-exploit` in `p2`
  against `gov-federal` with `dir-protect` from `p3`.

## Adding codes

An inductive code is added to the table of its family in this file, with
the date and a one-line definition, before it is created in ATLAS.ti. New
`pol-` slugs are the expected case. A new family, or a change to a
single-valued family, is the author's decision and is recorded in
`ch2-historical/GUIDANCE.md` as well. Renaming a code in ATLAS.ti
keeps its codings, but the name in this file changes in the same session.

## Import and export mechanics

**Import.** `scratch/zotero_to_atlasti.py` (run from the repo root with
`python3 -I`, options `--collection <key>`, `--item <key>`, `--dry-run`)
reads a source's highlight and underline annotations from the Zotero
local API (text, comment, color, page, sort order), resolves the
citation key through Better BibTeX, imports the PDF attachment from
Zotero's storage folder into the ATLAS.ti project `2026-amz-ch2`,
folder `Corpus`, as a document named by the citation key (a work split
into several PDFs gives one document per file, `citekey · file`, in a
subfolder), and places each highlight on the PDF as a quotation, located
in the text ATLAS.ti extracts (exact match after whitespace and
hyphenation normalization, then an anchored fuzzy match). Each
quotation is coded with its dimension code and carries the Zotero
comment plus a provenance line (`zotero <key>, p. <page>, <color>`) as
its ATLAS.ti comment. The comment written on a highlight (step 2 of
the per-highlight procedure) is therefore kept; what is skipped, and
counted, are the annotation types that have no highlighted passage:
Zotero's standalone sticky notes ("note"), free-text boxes ("text"),
ink drawings and image areas. Highlights that cannot be located are listed in
`scratch/atlasti-import/` for placement by hand. The importer writes
nothing into Zotero, skips a document that already exists, and rebuilds
one only under `--replace --yes`, which deletes its codings: the
author's command only. Verified 2026-10-20 on `london2007` (40
highlights, all located).

**Export.** The quotation table (document, quotation text, codes,
comment) is exported from ATLAS.ti into `ch2-historical/evidence/
quotations/`, dated; the page and the Zotero key are parsed from the
comment's provenance line and the citation key is the document name.
The validation script (rule 4) and the matrix builder read this table.
