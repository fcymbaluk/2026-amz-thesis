---
chapter: 2
title: Historical trajectory
stage: drafting # conception | drafting | full draft | under review | revising | final
updated: 2026-10-01
current_focus: "Work plan step 1: codebook creation (tag vocabulary and color map from this guidance)"
next_milestone: "Evidence matrix exported to ch2-historical/evidence/"
---

# Chapter 2 — Historical trajectory: working guidance

> **About this document.** The "Purpose," hypothesis, and conceptual sections summarize the chapter's argument: they describe the text, they are not the text, and like every instruction in this file they evolve with the work (see CLAUDE.md, "Guidance documents are living records"). Repo-wide behavior is governed by `CLAUDE.md`; this file adds chapter-specific guidance only. The cross-chapter argument chain is canonical in `docs/dissertation-map.md`.

## Supervisor review — required changes (Aug 2026)

```{=html}
<!-- One checkbox per required change. Check items off as they are addressed and append a pointer to where ("→ draft §2.3", "→ docs/decisions/2026-10-02-…"). An item lives in the chapter that must act on it; if two chapters must act, it becomes two items. Dataset-wide items live in Chapter 4's list; dissertation-level items live in dissertation-map.md. -->
```

- [ ] Create the codebook as a tag vocabulary. Derive the deductive codes from this guidance rather than from scratch: the three operationalizable dimensions, the six periods, the two-arenas contrast, mechanisms A and B. Define the exact Zotero tag string for each code (prefixed, e.g. `d1-`, `p1`-`p6`, `mA`/`mB`) and the highlight-color map (colors carry the three dimensions; tags carry the rest). Leave room for inductive codes; a tag not in the codebook is added there, dated, before it is applied. Deliverable: `ch2-historical/coding/codebook.md`.
- [ ] Assemble the corpus in Zotero (dedicated collection; both tiers from "Sources and epistemic rules"; EPUBs admissible, since Zotero annotates them natively). Before a work enters the corpus, its record is in Zotero with a pinned Better BibTeX key. Keep the corpus inventory (citation key, work, edition, tier) at `ch2-historical/coding/corpus-inventory.csv`; tier lives here, per item, not as a per-highlight tag. Zotero account sync is ON: Zotero is the source of truth for annotations and sits outside git, so sync is its backup.
- [ ] Code the corpus in Zotero: highlight and tag per the codebook, colors per the dimension map. Coding is the author's act; AI code proposals (e.g., for untagged highlights) are a reviewed first pass, accepted or rejected in Zotero by the author, never applied by an agent.
- [ ] Export the evidence: per source, "Add Note from Annotations", then Markdown export into `ch2-historical/evidence/notes/` (dated). Exports are regenerable snapshots of the Zotero annotation layer; refresh them at milestones and whenever a source's coding changes.
- [ ] Build the evidence matrix from the exported notes (Claude Code): period × dimension, cells holding claims with supporting quotations, page numbers, citation keys, and tier from the inventory. Output to `ch2-historical/evidence/` as Markdown or CSV; flag empty and thin cells. The matrix is the base of the writing process.
- [ ] Write the chapter outline: periods as narrative units, the three threads applied within each, the two-phase arc as the interpretive frame. This guidance already drafts the skeleton; the outline converts it into section headers with the matrix's evidence mapped to each.
- [ ] Write the sections, from the outline and the evidence matrix. Split this item into one checkbox per period plus the framing sections when writing starts; apply the register discipline and the source-tier rule throughout.

## Chapter's Purpose and Argument

Chapter 2 provides the historical foundation of the dissertation. It reconstructs the environmental, economic, and social trajectory of the Brazilian Amazon and interprets that trajectory through the conceptual apparatus of the Eco-Social State literature, with Latin American scholarship on welfare expansion and on institutional weakness supplying the tools for explaining institutional change, persistence, and exclusion.

The chapter is not a chronicle. It is an interpretive history organized around a working hypothesis and three analytical threads, and its purpose within the dissertation is threefold: (1) to demonstrate historically that the three operationalizable dimensions derived in Chapter 1 (distributive conflict, social conditions, multilevel governance) are constitutive features of Amazonian environmental governance, not analytical impositions; (2) to develop the chapter's concluding conceptual contribution, the eco-social arena, with its two mechanisms; and (3) to produce the periodization, causal mechanisms, and hand-off concepts that Chapters 3 and 4 take up.

## Working Hypothesis

The hypothesis inverts the Global North sequence, but the inversion concerns the relationship between welfare and environmental statehood, not merely their order. In developed countries, the environmental state was layered onto an already-consolidated welfare state: sequencing with layering, which gave environmental policy social embedding. In the Brazilian Amazon, the trajectory followed a two-phase macro-arc with a different structure:

**Phase 1 — State-led environmental exploitation with regulated exclusion (c. 1870–1985, intensified 1964–1985).** The commodification and exploitation of nature were not a market outcome but national policy: SUDAM fiscal incentives, subsidized credit, directed colonization, the Transamazônica. The state actively manufactured land as a source of rent. Frontier labor, meanwhile, was unprotected not through liberal neglect but through the design of the citizenship regime itself: under cidadania regulada [@santos1979], social protection was tied to state-recognized occupational categories, and rural and informal frontier workers sat outside them (FUNRURAL being the partial, late exception). Phase 1 is therefore not laissez-faire; it is a specific institutional configuration combining state-driven commodification of nature with the structured exclusion of frontier labor.

**Phase 2 — Environmental protection and social inclusion without integration (1988–2022).** The 1988 Constitution is the founding moment of both tracks at once. Track one, environmental statehood: the constitutional environmental chapter, IBAMA (1989), the institutionalization of socioambientalismo, built through a combination of international pressure and domestic socio-environmentalism, culminating in market regulation to protect nature and the partial decommodification of nature (protected areas, extractive reserves). Track two, incorporation of outsiders [@arretche2018; @garay2016]: universal and non-contributory social policies (SUS, basic education, BPC) and later targeted transfers (Bolsa Família, 2003), driven by democratic electoral competition. The pattern repeats in the 2000s: Bolsa Família (2003) and PPCDAm (2004) launched by the same government, one year apart. The two tracks were built simultaneously, from the same critical juncture, on top of the phase-1 legacy — but in separate institutional arenas that never integrated. Social and labor policies (cash transfers, minimum-wage valorization) were not treated as connected to environmental state goals.

**Partial fusions as exceptions that prove the rule.** The socio-environmental movement (rubber tappers, extractive reserves, the alliance behind socioambientalismo) produced a historically distinctive fusion of the social and environmental agendas. Later, policies such as Bolsa Verde and seguro-defeso, and the social components of PPCDAm, attempted integration. These fusions existed at the margins of both policy systems, chronically underfunded, institutionalized only partially, and vulnerable to drift. They demonstrate that integration was possible but never institutionalized.

**Observable implication — asymmetric resilience.** If the environmental state has regulatory capacity without embedded social legitimacy, while the social track built mass constituencies, the two tracks should behave differently under attack. In 2016–2022 they did: the environmental apparatus drifted dramatically (enforcement collapse, budget hollowing, personnel purges) while Bolsa Família, attacked and rebranded, was structurally preserved because dismantling it was politically prohibitive. This asymmetric resilience is a direct test of the hypothesis and is what Mahoney and Thelen's drift predicts for institutions without mobilized defenders.

The chapter tests and refines this hypothesis against the historical record. Where the record contradicts it, the hypothesis is revised, not defended.

## The Two-Arenas Contrast

A core analytical claim, drawn from [@arretche2012], is that both arenas are federally regulated — IBAMA is federal, the Forest Code is national law, the federal government has a decision-making role in health, education, and cash-transfer policies — so the contrast is not centralization versus decentralization. The mechanism is the territorial distribution of costs and benefits:

- **Social policy arena:** central regulation plus aligned incentives. Municipalities comply with SUS and Bolsa Família rules because compliance brings federal money, and local implementation creates local winners (beneficiaries, health workers, mayors who deliver). Result: territorial convergence [@arretche2012] — social policy reached and embedded in the frontier.
- **Environmental enforcement arena:** central regulation whose territorial implementation imposes concentrated local costs without local benefits. Enforcement shuts down activities that local economies and elites depend on, while the beneficiaries (national and global publics) are elsewhere. Result: territorial unevenness, subnational contestation, and dependence on federal willingness to impose losses.

Same federation, two arenas, opposite territorial logics: one distributes benefits, the other distributes costs. This is why cash transfers reached the frontier while environmental protection never embedded there, and it is the same logic behind asymmetric resilience: constituencies defend Bolsa Família; nobody local defends IBAMA. The contrast is where Thread 1 (distributive conflict) and Thread 3 (multilevel dynamics) meet in a single mechanism.

## The Eco-Social Arena (concluding conceptual contribution)

The chapter closes by defining the eco-social arena: the space where social and environmental policy interact on the frontier. Two mechanisms with opposite signs operate there, and both are empirically real in the Amazon:

- **Mechanism A — cushioning as compensation.** Social protection absorbs the costs environmental regulation imposes on frontier populations, creating local stakes in conservation and making enforcement politically sustainable. Social policy strengthens the environmental state. Extractive reserves, Bolsa Verde, and seguro-defeso are embryonic versions.
- **Mechanism B — cushioning as forbearance.** Social vulnerability becomes the justification for non-enforcement: sanctioning smallholders who deforest to survive is politically and morally costly, so poverty licenses forbearance [@holland2016; @brinks2020]. Social conditions weaken the environmental state.

The chapter's historical claim: because the two arenas were never integrated, mechanism A remained marginal while mechanism B operated by default.

**Register discipline.** The chapter documents the separation of arenas and the marginality of mechanism A; it hypothesizes that integration would stabilize enforcement; Chapter 4 tests under what conditions social policy produces A rather than B.

## Chapter guidances

### Periodization (provisional, to be refined during drafting)

The two-phase macro-arc of the hypothesis is layered over a six-period chronology; the periods are the narrative units, the phases the interpretive arc.

1. Rubber cycles and extractivism (c. 1870–1945)
2. Military-era integration and colonization (1964–1985): SUDAM incentives, Transamazônica, directed settlement
3. Democratization, the 1988 Constitution, and the founding of both tracks (1985–2003): environmental institutionalization and socioambientalismo; universal and non-contributory social policy architecture
4. The PPCDAm era (2004–2015): enforcement, monitoring, protected-area expansion, concurrent welfare expansion — the two tracks at maximum strength, still unintegrated
5. Asymmetric drift (2016–2022): environmental dismantling versus social-track survival — the hypothesis test
6. Present conjuncture (2023– )

Period boundaries are analytical claims and may be revised. Each period is examined through the three threads below.

### Operationalizable dimensions (questions applied to every period)

1. **Distributive conflict around environment:** In each period, who captures the rents from land and natural resource use, who bears the costs of regulation or of its absence, and how is that conflict politically organized? Pay attention to concentrated local costs of enforcement.
2. **Social conditions of state action:** In each period, what is the labor regime and the state of social protection on the frontier, and how does it connect to, or remain severed from, the environmental agenda? Pay attention to labor regimes outside the citizenship regime in phase 1, and universal and non-contributory welfare expansion reaching the frontier in phase 2.
3. **Multilevel governance:** In each period, which level of government holds effective authority over land use, and in which direction does it push? Attend to (a) the reversal of the federal role, from promoter of deforestation (military-era incentives and colonization) to regulator (1988–2004 onward); (b) the growth of subnational resistance to environmental protection in later periods; and (c) the two-arenas contrast: centrally regulated social policy converging territorially while environmental enforcement, equally federal, remains territorially uneven and contested.

### Conceptual apparatus

#### Eco-social concepts

- **Commodification and decommodification of labor and nature.** Grounded directly in Polanyi's fictitious commodities [@polanyi1944 (1944)], not only in the secondary eco-social literature. The chapter's claim is about a shared political logic and a shared historical process: in the Amazon, the commodification of land (as a source of rent) and of labor (frontier work outside the protection regime) advanced through the same state-led expansion, and their partial decommodification (protected areas, extractive reserves; non-contributory welfare) was pursued through separate and sometimes conflicting state interventions. The chapter does not claim a general causal interdependence between the two processes; whether and how they causally interact is an empirical question reserved for Chapter 4. The Amazon case is analytically interesting precisely because the two can move in opposite directions: extractive reserves decommodify nature while leaving labor commodified. Key references: @polanyi1944, @zimmermann2020, @cucca2023.
- **Interdependence between nature, economy, and work.** How the arenas of social protection and environmental protection were constructed in the Amazon, by which levels of government, and with what degree of integration or separation. Key references: @zimmermann2020, @cucca2023, @gough2016.
- **Trajectory of the environmental and social agendas.** [@meadowcroft2006] identifies four phases of the environmental state in developed countries: expansion, deepening, increased centrality, politicization. @gough2016 argues welfare and environmental states share drivers but face divergent effects. @zimmermann2020 note that environmental and social concerns may conflict. The chapter uses these as a comparative template against which the Amazonian macro-arc is measured. Divergences from the Global North sequence are findings, not anomalies.

#### Social inclusion and institutional weakness scholarship

- **Cidadania regulada.** Building upon [@santos1979], social protection tied to state-recognized occupational categories; the analytical key to phase 1, reframing frontier labor from "unprotected by neglect" to "excluded by design of the citizenship regime."
- **Incorporation of outsiders.** Social policy expansion in Brazil and Latin America [@arretche2018; @garay2016] explains phase 2: electoral competition drives non-contributory expansion to outsiders. Note the double role: the same author theorizes the social track [@garay2016] and the enforcement track [@milmanda2019; @milmanda2020a] — the apparatus coheres.
- **Federalism and territorial convergence.** Brazilian social policy is centrally regulated and produces territorial convergence [@arretche2012]; this underpins the two-arenas contrast and operationalizable dimension 3.
- **Forbearance.** Politically motivated non-enforcement against the poor [@holland2016]; the theoretical core of mechanism B.

#### Historical institutionalism

Used in the background, with four named concepts:

- **Policy legacies:** how earlier state interventions (colonization programs, fiscal incentives, land tenure regimes) structure later policy options and coalitions.
- **Gradual institutional change, especially drift:** environmental rules formally intact while enforcement is hollowed. Gradual institutional change [@mahoney2009], especially drift, is the primary tool for interpreting the post-2016 period and the asymmetric-resilience test.
- **State capacity:** enforcement as the operative margin of environmental statehood, variable across territory and over time.
- **Weak institutions:** distinguishing weakness by design from weakness by capacity [@brinks2020]. Forbearance-style non-enforcement is the relevant subtype for subnational environmental politics.

### The connection to Chapters 1, 3 and 4

The full cross-chapter chain, including the dimension–concept–manifestation correspondence table, is canonical in `docs/dissertation-map.md`. Chapter 2's own hand-offs, in brief:

**To Chapter 3 (systematic review):** Chapter 2 does not review the causal literature on deforestation; it deliberately leaves that to the systematic review, and supplies its warrant: the justification for the period of interest (the policy-regime trajectory that makes the recent window the relevant one) and the historical context against which the reviewed studies' assumptions can be read.

**To Chapter 4 (causal analysis):**

1. **The periodization**, defining the temporal scope and the policy-regime breaks Chapter 4 uses for identification.
2. **Candidate causal mechanisms** linking social conditions to environmental outcomes, stated precisely enough to be operationalized.
3. **Historical grounding for subnational enforcement variation** as a real margin, justifying multilevel analysis.
4. **The eco-social arena concept with mechanisms A and B**, generating Chapter 4's causal question: under what conditions does social policy produce compensation (A) rather than forbearance (B)?

## AI assistance

### Scope of assistance

**Analytical and conceptual (related to the argument):**

- The most important: make it a clear and logical narrative, but innovative, with a new understanding of an already well-known story: the one of the Amazon rainforest. The text must become clear to the reader. The theoretical approach brings its novelty; but if theory and concepts start to make it hard to understand, tone down the theory.
- Clarify and refine the eco-social, social inclusion, and institutionalist concepts as applied to the Amazonian case.
- Test the working hypothesis against the historical record period by period; flag where it needs revision.
- Keep the three operationalizable dimensions applied consistently across all periods; flag periods where a dimension is underdeveloped.
- Guard the distinction between narrative claims (what happened), interpretive claims (what it means for the framework), and hypothesized claims (what Chapter 4 tests).

**Writing and revision (on the actual text):**

- Strengthen argumentation, logical flow, and internal coherence: only when asked.
- Improve clarity, phrasing, and adherence to academic register: only when asked.
- Review grammar, syntax, and typographical accuracy: only when asked.
- Ensure consistency and correctness in citations and formatting: only when asked.
- Follow `docs/writing-rules.md`: always.

### Zotero-highlights workflow (evidence rules)

Qualitative evidence is coded directly in Zotero: highlights are the quotations, tags are the codes, highlight colors mark the dimensions. There is no separate QDA tool; the evidence layer lives as exported plain text in the repo, versioned like everything else.

- **Source of truth and backup.** The Zotero annotation layer is authoritative and sits outside git; Zotero account sync stays on as its backup. Exported notes in `evidence/notes/` are dated, regenerable snapshots, never hand-edited.
- **Codebook-first.** Tags are applied only as the codebook specifies; a new code enters `coding/codebook.md`, dated, before first use. Tag strings are exact: no variants, no synonyms.
- **The author codes; the AI organizes.** Agents never write into Zotero. Claude Code reads the exported notes and builds the derived layer: the matrix, gap reports, and section drafts that pull quotations with citations in place. AI-proposed codes are a reviewed first pass the author accepts in Zotero.
- **The citation key is the corpus identifier.** Every corpus item has a pinned Better BibTeX key in `manuscript/references.bib` before coding starts; notes, inventory, and matrix cells all reference works by key, so matrix evidence converts mechanically to `@key` citations at writing time. The never-invent-keys rule applies here as everywhere. Source metadata (tier, edition) lives in the corpus inventory, keyed by citekey, not in per-highlight tags.
- **Discipline carries over.** The source-tier rule (journalistic tier never sole support for analytical claims) and the register discipline bind coding and matrix construction as they bind writing.

### Sources and epistemic rules

**Priority hierarchy on Amazon trajectory reconstitution:**

1. Academic historiography and peer-reviewed literature.
2. Journalistic and essayistic works (see below).
3. Authoritative institutional sources (INPE, IBGE, IPEA, international organizations) for factual and quantitative claims.

**Permitted use by tier:** journalistic and essayistic works may support events, dates, atmosphere, and actor-level detail; they must never be the sole support for an analytical or interpretive claim — those require the academic tier. When tiers conflict on a factual point, the academic tier prevails and the conflict is noted.

**Academic historiography and theory:**

- Hecht and Cockburn, *The Fate of the Forest*
- London and Kelly, *The last forest: the Amazon in the age of globalization* 
- Schmink and Wood, *Contested Frontiers in Amazonia*
- Bunker, *Underdeveloping the Amazon*
- Bertha Becker (frontier geography and state territorial strategy)
- Hochstetler and Keck, *Greening Brazil*; later work by Hochstetler (links to the Chapter 1 matrix, meso level)

**Journalistic and essayistic works:**

- *O silêncio da motosserra* — Claudio Angelo and Tasso Azevedo
- *Arrabalde* — João Moreira Salles
- *Amazônia, uma década de esperança* — João Paulo Capobianco
- *Fronteira Amazônica* — John Hemming
- *Amazônia na encruzilhada* — Míriam Leitão
- *Histórias da Amazônia* — Márcio Souza
- *Uma história das florestas brasileiras* — Zé Pedro de Oliveira Costa
- *Amazônia* — Francisco Benedito da Costa Barbosa

**Citation discipline:**

- Do not invent citations or attribute arguments incorrectly.
- Clearly identify which authors, theories, or debates claims are based on.
- Provide full bibliographic references (author, year, title, journal/publisher).
- If uncertain about a source, say so explicitly rather than guessing.
- Every citation key used in drafts (and in this file) must exist in `manuscript/references.bib`. If a needed reference is missing, list it for the author to add via Zotero; never fabricate a key.

**Epistemic discipline:**

- No speculative claims, popularized summaries, or non-academic sources outside the rules above unless explicitly requested.
- If a claim cannot be grounded in the source hierarchy, flag it rather than proceeding.
- Historical claims about events, dates, and actors must be verifiable in the listed sources; when sources conflict, note the conflict.
