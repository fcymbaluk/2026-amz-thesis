# Chapter 4 rebuild: task board

Detailed task descriptions for the phases defined in `00-rebuild-phases.md`. Task IDs are stable: scripts, `DECISIONS.md` entries and commit messages cite them. Phase mapping: tasks 1.x are the phase 2 fixes (order in the phase 2 table), 2.1 is phase 2R, 3.x is the RAIS track, 4.x the descriptive stage, 5.x the causal stage.

## 1. Data fixes (phase 2)

### 1.1 Include all Legal Amazon municipalities and create biome flags

Rather than permanently excluding non-biome Legal Amazon municipalities, extend the panel to full Legal Amazon coverage, using data on the 9 states that encompass the Legal Amazon (all MA included). Add a constant `biome_amazon` dummy (1 = the current 502, 0 = the rest) and a `legal_amazon` flag (same rationale). Purpose: allow a Legal-Amazon-versus-biome robustness check and any future generalizability check. Row-level change, so it runs last in phase 2 (step 2.9); all `N_MUNIC_EXPECTED` assertions update once.

### 1.2 Extend Bolsa Família data to 2020-2024

All BF variables are 100% missing in 2020-2024 (PRODES, RAIS and prices are present). BF operated until late 2021, was substituted by Auxílio Brasil, then returned as Bolsa Família. Bring those data into the dataset. Fix or document; until fixed, 2019 is the endpoint for BF-involved analyses.

### 1.3 Fix price index normalization

`price_index_crop` (~0.014-0.03) and `price_index_cattle` (~26-51) are on incompatible scales and neither equals 1 in 2000, contradicting the documented Pink Sheet spec (MUV-deflated, 2000 = 1). Cause: normalization ran before the final panel join. The normalization step moves to a script devoted to normalized variables (script 06).

### 1.4 Check MT quota outliers

14 municipalities with 2006-07 `bf_families_quota_ratio` > 3 (max 11.9), all in Mato Grosso: Alta Floresta, Apiacás, Barra do Bugres, Confresa, Guarantã do Norte, Juara, Nova Canaã do Norte, Nova Olímpia, Pontes e Lacerda, São José dos Quatro Marcos, Santa Carmem, Salto do Céu, São Félix do Araguaia, Nova Monte Verde. MT's median is normal (0.96), so this looks like a quota-source problem for specific municipal codes in script 02, not statewide. Phase 1 evidence (2026-10-07, `_audit/bf.qmd` Q4): the values sit in the LAI workbook itself, so the problem is source-borne; it is also wider than the fourteen: 69 municipality-years exceed 3 in 2006-07, 61 of them in Mato Grosso (Barra do Garças among the others) plus a few in MS and PR. Still open for investigation; an external quota series would be needed for any fix.

### 1.5 Fix electoral off-by-one

Stated rule: election values apply during governing years (2004 election → 2005-2008). As stored, values change IN election years (2004 election → 2004-2007), so 2008 (a listing year) carries the October-2008 election. Fix in script 03. The governing rule then applies to all derived electoral variables.

### 1.6 Fix data for municipalities created after the 2000 census

Nine MT municipalities absent from the 2000 census: Bom Jesus do Araguaia (5101852), Colniza (5103254), Conquista D'Oeste (5103361), Curvelândia (5103437), Ipiranga do Norte (5104526), Itanhangá (5104542), Nova Santa Helena (5106190), Rondolândia (5107578), Santa Cruz do Xingu (5107743). All nine are in the at-risk narrow sample, and Colniza is a treated unit (2008 cohort, low-coverage stratum). Problems: population is NA until 2009 although gdp_brl exists from 2002 for most, so gdp_per_capita_* is NA and the unit drops from every covariate-adjusted specification (why Colniza disappears from adjusted rows); pea and informal are interpolated from 0 in 2000 to the 2010 census value, and that zero is a missing value coded as zero (Colniza's informal reads 46.9 in 2006 against 78.1 in 2010, a pure artifact). Fix: treat absence from the 2000 census as NA, never zero. Population: IBGE's official annual municipal population estimates from the installation year onward (verify the right SIDRA table), spliced to the 2010 census, with `pop_source` recording the origin. pea, pia, informal: NA before 2010, no backcasting. Recompute every derived ratio. Add `muni_new_post2000`, `muni_parent_geocode`, `muni_install_year`. Report the parent municipalities: their 2000 census values include territory they later lost, so their interpolated 2000-2010 series are biased downward in slope. Flag them (`muni_parent_split = 1`); do not build minimum comparable areas. The same situation one census later: Mojuí dos Campos (1504752, installed 2013) and its parent Santarém (1506807). Phase 1 keeps the inherited handling (D-2026-10-07-d): PEA split between the two by the 2022 labour-force share, informality copied, PIA and population not split (Mojuí NA before 2022, Santarém's series include Mojuí's territory until the 2022 anchor); Mojuí itself is dropped in script 05 (D-2026-10-05-l). Treat Santarém under the parent rule here. The four other municipalities created after 2010 are outside the Amazon.

### 1.7 Fix census-interpolated series, keep the anchors

population, pea, pia and informal are linear interpolations between the 2000, 2010 and 2022 censuses. Any 2004-2007 "baseline" value of these is a weighted blend of 2000 and 2010, and 2010 is post-treatment for the 2008/09 cohorts. Store the raw census values in `muni_baseline` so the analysis can use a clean informal_2000. Mark the interpolated panel columns in the codebook as `interpolated = TRUE` with their anchors. Do not change the interpolation itself.

### 1.8 Audit NA versus zero

Audit every ratio for 0/0 and missing-as-zero cases and document the rule in the codebook. emp_share_agri_low/mid/high_workers are NA in 16-21% of rows, mostly municipalities with zero formal agricultural workers (0/0): keep NA, add `emp_agri_workers_n` (the denominator count) so the analysis can tell "no agricultural formal employment" from "missing". Found at phase 1 (2026-10-07, `_audit/rais.qmd` Q3): the lost script's `replace_na()` on the agricultural wage-group table named the all-worker columns, so `workers_agri_low/mid/high` stay NA wherever the municipality-year has no bond at all in that bracket while the all-worker counts read 0; `02-social-rais.R` reproduces this on purpose (KNOWN DEFECTS) and the fix lands with this task. `bf_families_quota_ratio == 0` in 14 rows, all 2004 (bf_families_n = 0 with a positive quota): check against the source whether this is rollout or a missing record; do not alter without evidence. Phase 1 evidence (2026-10-07, `_audit/bf.qmd` Q1): nationally the 2004-2006 files hold 930, 213 and 41 municipality-months with both families and transfers at zero (98 municipalities in June 2004, 23 in June 2005, 4 in June 2006), never positive families with zero transfers, which is consistent with rollout; positive families with zero transfers occur only in 2020. Also to settle here: the municipality-years absent from the RAIS export (`_audit/rais.qmd` Q2: 51 in the biome sample 2000-2020, early years and small municipalities), which the panel carries as NA in every `emp_*` column; decide zero (no active bond) versus NA (municipality not yet installed, using `muni_install_year` from task 1.6). Crop areas and cattle heads: confirm that a municipality absent from PAM/PPM in a year is coded 0 only when the survey covered it.

### 1.9 Unit pattern

Units are mixed: deforestation_forest_rate and informal are on 0-100, all *_share and emp_pia_rate_* are on 0-1. Per the naming convention: rescale everything to 0-1, record the unit of every column in the codebook, no percent storage. Also record whether deforestation_forest_rate divides by contemporaneous or lagged forest area (script 01, ~line 253).

### 1.10 Character to numeric (handled in phase 1)

Roughly 60 of the 85 panel columns are stored as character even though they hold numeric values (bf_families_n, population, gdp_brl and others), usually NA strings or decimal commas upstream. Coerce to numeric at the source in scripts 01-03 during the phase 1 refactor (the one permitted behavior change), record the cause in DECISIONS.md, confirm no non-NA value is lost.

## 2. Rename and storage audit (phase 2R)

### 2.1 Variable rename via crosswalk

Per `naming-convention.md`: resolve CONFIRM items, build `codebook/crosswalk.csv`, rename through the crosswalk after equivalence passes, guard assertions in script 06, update downstream scripts and appendices, generate the codebook from the crosswalk.

### 2.2 Storage audit of existing derived variables

While building the crosswalk, classify every stored derived variable against the governing rule for derived variables (`GUIDANCE.md`, Methodological standards): does its value depend on which rows end up in an estimation sample, and does it meet at least one storage condition (upstream inputs, full-balanced-panel computation, design decision that must be identical across analysis scripts)? Record the verdict in the codebook. Expected borderline cases: the pooled standardizations (`price_index_*_z` and lagged variants; state the reference population and confirm it is sample-independent) and any variable a CONFIRM item marks ambiguous. A variable that fails is removed from the panel and rebuilt in the analysis scripts that use it, with its own decision entry; the resulting schema change is reported by `compare_outputs.R` and documented in the commit like the rename. One closing decision entry summarizes the classification of all stored variables.

## 3. RAIS reform (parallel track)

Per `upstream-reform-rais.md`, after its section 0 decisions are confirmed. 3.A municipal wage distribution (Query 3). 3.B sector and legal-nature codes, public/private disaggregation (`_priv` variants, `workers_public`). 3.C sectoral employment counts and shares by CNAE section, with national and Legal-Amazon-excluded reference series. Regression rule throughout: the rebuilt extraction reproduces the current `_data/data_employment_rais` exactly before any new variable is used. Access, cost and isolation rules are in the brief and are not relaxed.

## 4. Descriptive stage

### 4.1 Dependence on land-based activities

Rank and map municipalities by the weight of agriculture, livestock, mining and logging in the local economy. The panel has no GDP by economic activity, so this needs the IBGE PIB dos Municípios series (value added by agropecuária, indústria, serviços, administração pública; 2002 onward, SIDRA), joined by geocode.

### 4.2 Rural versus urban municipalities

Classify municipalities by urban population share from the 2000, 2010 and 2022 Censuses (Atlas Brasil carries the urban/rural split). Show the distribution, the change between censuses, and the cross-tab of rurality against land-based dependence, since the two are correlated but not identical (mining towns can be urban; cattle municipalities can be sparsely populated).

### 4.3 Trajectories 2000-2020

Small-multiple time series, indexed to a base year and split by groups: population, municipal GDP and GDP per capita (2024 R$), poverty and extreme poverty (Atlas 1991/2000/2010 points), and the share of land-based activities in value added. Purpose: show whether the municipalities most exposed to enforcement were growing, urbanizing and leaving poverty on the same or different paths from the rest.

## 5. Causal stage

### 5.1 Persistence as the sharper test of mechanism A

Consider whether the persistence question (does compliance hold after enforcement pressure fades) is the sharper test of mechanism A, since sustainability of enforcement is what mechanism A actually claims. Mechanism A (cushioning as compensation) claims that social protection makes enforcement politically sustainable, not only that it makes the initial enforcement shock bite harder. The stratified moderation estimate tests the second; the persistence question tests the first. Present the persistence results as the natural extension of the moderation result rather than as a separate exploration. Keep the existing designs and their labels (system-level preferred, municipality-level descriptive at best), the predetermined 2006-07 moderator, and the price-exposure control. State before estimation what result would count against mechanism A here: a rebound after 2016/2019 that is as steep or steeper in high-coverage municipalities.
