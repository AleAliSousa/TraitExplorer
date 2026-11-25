# TraitExplorer

Shiny application for exploring the [Evo-M1-Trait-Data](https://github.com/AleAliSousa/Evo-M1-Trait-Data) comparative trait database, featuring first-class specimen tracking and multi-domain comparative trait search.

## Features

### 1. Specimen Search & Crosswalk Explorer
- **Search specimens** by canonical ID (e.g. `PONGO-YN85-38`, `Cro-Magnon 1`), specimen name / house name (e.g. `Harry`, `Briggs`, `Disco`), primary or alternate identifiers (e.g. `OY 1148`, `YN85-38`, `GPZ-5542`), holding collection, species, publication, or notes.
- **Filter** by specimen kind (`historical_biological_specimen`, `fossil_specimen`, `in_vivo_subject`), resolved taxon, collection, publication study, match status, and taxon conflict.
- **Specimen Inspector**:
  - **Identity & Taxonomy**: Displays canonical ID, house name, identifiers, collection, sex, specimen kind, published vs resolved taxon, taxon concept, decomposability status, and taxon conflict alerts.
  - **Cross-Study Tracking**: View all citations and occurrences of the same physical individual across different studies (e.g. how `YN85-38` is tracked across MacLeod 2000, Smaers 2011, Smaers 2017, and de Sousa 2010).
  - **Evidence Sources**: Backing evidence sources linked from `specimen_source_registry.csv` with access classes and publication statuses.
  - **Physical Measurements**: Auto-matched measurements from published tables (brain weight, fixed volume, V1 volume, neocortex volume, cerebellum volume, etc.).
  - **Specimen Dossiers**: Interactive reader for documented markdown notes (Gibbon *Disco*, *Pongo*, Early *Homo sapiens*, and *Kaas/Young/Collins* overlap).
- **Fossil Comparisons**: Side-by-side comparison of Kochiyama et al. 2018 vs Weaver 2001 reconstructions and method offsets.
- **Published Specimen Tables**: Direct browsing and export of curated specimen tables (MacLeod 2000, Smaers 2010, de Sousa 2010, Kochiyama 2018, Weaver 2001, Barger 2007, Collins 2016, Armstrong 1979).
- **Taxon Concepts & Registries**: Reference tables for taxon concepts (`taxon_concept_registry.csv`), collections (`collection_registry.csv`), and evidence sources.

### 0. Start Here
- Landing tab with live counts (trait values, species, domains, specimens tracked, brain-region concepts, files) and three one-click worked examples (gibbon *Disco*; chimpanzee cerebellum volume; mouse CMRgl vs neuron number) that jump to the right tab with the inputs filled in.

### 1b. Specimen identity: species-level accepted name
- Every crosswalk row carries `accepted_name` (species-level, from `_keys/specimen_crosswalk/specimen_taxon_resolution.csv`) next to the verbatim `resolved_taxon`. The Species filter and the inspector use it, so *Rattus norwegicus* / *Rattus norvegicus* or *Felis domestica* / *Felis catus* are one entry. Under-review labels are flagged `needs_review`.
- Dossier routing now covers every note present upstream (including the green monkey and Sherwood/Karl notes) and falls back to scanning note text for the specimen's id or house name.

### 2. Comparative Trait Search
- Instant multi-term AND searching across the datasets of the compiled table (26 on 2026-11-23: Brain-structure volumes, Cell counts, Brain mass, Body, Ecology, Life history, Behaviour, Sensory performance, ...), with each value's category (`Category`, `Measure_class`).
- Domain selector to search across all domains or focus on a specific merge.
- Export results as CSV.

### 2b. Region Comparison (new)
- Pairs two different kinds of measurement on the **same brain regions in the same species** -- e.g. mouse regional glucose metabolic rate (CMR domain) against regional neuron numbers (cell-count domain) -- and fits an OLS line (log-log by default) with Pearson/Spearman statistics. Points are brain regions. A second mode makes points species for one region.
- Regions from different domains are matched through two upstream keys: `_keys/variable_domain.csv` (variable label -> canonical structure, measure, unit) and `_keys/anatomy_crosswalk.csv` (domain term -> concept, `direct` / `proxy`). Structures with no crosswalk row pair only on an identical name and are labelled `own name`. Every pairing shows its link class and sources; unpaired regions are listed so a missing crosswalk row is visible, not silent.
- Covers: cell counts, volumes, brain-part weights, cerebral metabolic rate, GLI, cortical areas, cell morphology, M1 cortical layers. Download the paired table as CSV.
- Species names in Trait Search / Species Coverage / Region Comparison are harmonised through `_keys/species_display_aliases.csv`; pickers show common names from `_keys/species_cohorts.csv` where known.

### 2c. Structure Profile (within a species)
- Pick a species and a kind of measurement (volume, mass, neuron number or density, CMRgl/CMRO2, blood flow, surface area, ...) to see every structure-level value side by side, on one unit scale (volumes in mm3, masses in g; cellular units kept as published).
- Optionally express each structure as a % of a reference (e.g. a weighed whole-brain mass; its basis is named) and add up to three comparison species. Two variables for the same structure stay side by side, never averaged.

### 2d. Trait Reconciliation
- Candidate pairs of variables that look like the same trait from different sources (same structure + measurement + unit + life-stage/sex scope, or the same name once the author-year tag is removed), with shared species, median ratio, % within 1% / 10% and a verdict.
- Pair detail (species-by-species values and a DIFFERS flag at a tolerance you set) and a Reconciled view (every source per species, a precedence order you choose, first available wins, source of each value named). Decisions in `_keys/variable_canonical.csv` are shown; nothing is pooled or edited.
- Labels match the Evo-M1 app (`load_harmonized_values()` in `data_layer.R` ports its `load_compiled()`), except that raw trait-table columns are not superseded here, so they can be compared with the merges that replace them.
- Every trait value in Trait Search, Species Coverage, Structure Profile and Trait Reconciliation comes from one file, `__compiled/compiled_long.csv.gz` in the data repo (built there by `__compiled/build_compiled.R`); the Evo-M1 app reads the same file, so the two apps show the same values, labels and datasets. These public compiled views have no independent feed list. If the file cannot be fetched the app stops with an error saying so. Region Comparison is the exception: it still reads eight per-domain merge tables, because it needs their region, layer and sample-size columns, which the compiled table does not carry.

### 2e. Neuron methods (optional local view)

- Compare source-level primate whole-cortex neurons per hemisphere, other-source/HH ratios, and **exact V1** neurons in three core interactive Plotly views. Filter by source paper, method group, taxon, source-reported sex and age group, observation role, and quality/shared-specimen status. In the ratio view the demographic filters apply to the numerator; both sides remain visible in the table and details. The table, hover, and selected-row details preserve the published taxon, specimen, hemisphere basis, anatomical scope, source-reported age and sex with their source locus, condition, caveat, duplicate group, and primary-source link. Download the filtered rows as CSV.
- This tab appears only when `TRAIT_NEURON_COMPARISON_DIR` points to a valid local `method_comparison` directory containing `whole_cortex_observations.csv`, `paired_relative_to_hh.csv`, and `v1_observations.csv`. It also loads `whole_cortex_individual_observations.csv` and `v1_individual_observations.csv` when present. If `charvet_fig7_reassessment.csv` is valid, a fourth focused Charvet view appears; `charvet_whole_cortex_history.csv` supplies a separate overlap table when present. These files are read directly; they do not enter the public compiled feed, GitHub cache, refresh flow, or deployment bundle. With the variable unset or invalid, the public app runs as before and the tab is absent.
- The default shows **primary normal** observations. Context, duplicate, individual-component, alternative, and epilepsy rows require an explicit role selection. Individual components overlap their published cohort summaries and are not additional independent cohorts. Hollow circles mark caveats or shared specimens, hollow squares mark individual components, and hollow diamonds mark other context rows. When `region_proxy_excluded` is selected, non-V1 context rows such as Christensen 2007's occipital-cortex proxy appear in the V1 table and export labelled "Excluded non-V1 context"; they never enter the V1 plot.
- HH2007, Gabi2010, and Azevedo2009 published bilateral cortical means were divided by two in these input files; Collins/Young/Turner values were not doubled. HH2008 and HH2015 reuse earlier cases. Young 09-27 duplicates Collins 2010, two Turner bonnet-macaque hemispheres are one animal, and Miller/Collins chimp V1 rows reuse one chimp. *Papio* ratios are visibly genus-level proxies; original *Cebus*/*Sapajus* labels remain in details. These ratios compare different cohorts and anatomical scopes, so they are descriptive, not a universal method correction. Mass/area/volume columns are row-specific context and blank when the matching measured structure was not reported; Collins 2016's 344 g is whole-brain mass, not neocortex. Suner 1996 used a 3D optical disector density × volume method, not an optical fractionator.
- For rhesus, [HH2007](https://pmc.ncbi.nlm.nih.gov/articles/PMC1805542/)'s cerebral cortex includes hippocampus and piriform cortex. [Collins 2010](https://pmc.ncbi.nlm.nih.gov/articles/PMC2936588/) reports a dissected cortical sheet, but its exact gray/white and allocortical border is not documented. The app exposes each row's anatomical scope and does not assume these two preparations are equivalent; the boundary difference alone does not explain the count gap or provide a correction factor. For baboons, [HH2008](https://pmc.ncbi.nlm.nih.gov/articles/PMC2527956/) says hippocampus was excluded, whereas [Gabi2010](https://pmc.ncbi.nlm.nih.gov/articles/PMC2980814/) gives a broader general definition for its reused cohort; this border remains uncertain. Source-reported sex, age or age group are shown where available; blank source fields display as “Not reported,” never as an inferred specimen value. A cohort demographic label does not automatically apply to each animal; the demographic source locus indicates its reporting level.
- Charvet 2013 capuchin and tamarin cases are reused in Charvet 2017; its owl case contributes to a later mean with another unresolved owl. Giannaris 2012 individual V1 counts have paired measured V1 volumes, while Suner 1996's individual counts have no corresponding case-level V1 volumes; Suner's reported per-case CI half-widths are distinct from the sample SD. O'Kusky 1982's historical V1 estimate pools two macaque species and has no paired measured V1 volume.
- The focused Charvet Figure 7 view separates paired measured Giannaris volumes, Collins volumes inferred from V1 mass / 1.036 g per cm³, count-only Suner and O'Kusky rows, and Christensen's broader occipital proxy. The Collins 2010 seven-piece V1 map (IDs 6, 7, 9–13; 220.431M neurons) is visually inferred and awaits human signoff. A nine-piece candidate containing non-V1 pieces is excluded by default and can be enabled only as a diagnostic. The 2013/2017 whole-cortex history table stays separate from the V1 plots; cohort means and their individual components are not independent samples.

### 3. Repository Explorer
- Browse and search repository files by study, author, file type, and year.
- Direct links to open files on GitHub.
- Download selected repository files.

## Files

- `app.R` - Shiny application (UI + server)
- `data_layer.R` - public data access: fetches Evo-M1-Trait-Data from GitHub, caches it locally, builds the public tables the app displays. Sourced by both `app.R` and `refresh_cache.R`.
- `neuron_methods.R` - optional, local-only loader and filters for the restricted Neuron methods comparison. It never uses `data_layer.R` fetch or cache helpers.
- `structure_reconcile.R` - Structure Profile and Trait Reconciliation functions, shared with the Evo-M1 app (`__ShinyApp/structure_reconcile.R`); keep the two copies identical
- `refresh_cache.R` - standalone script (`Rscript refresh_cache.R`) that updates the local GitHub cache without launching Shiny. Run this whenever the data repo has changed and you want the next launch to be instant.
- `config.R` - optional GitHub repo/branch override
- `check_setup.R` - checks R packages and GitHub reachability
- `run_app.R` - launches the app

## Data location

The public trait and specimen views have **no local-repository dependency**. Their tables are fetched over HTTPS from `github.com/AleAliSousa/Evo-M1-Trait-Data` and cached under `TraitExplorer/.gh_cache/` (mirroring the repo's folder layout) purely for speed on repeat launches. Deleting `.gh_cache/` just means the next load re-downloads. The optional Neuron methods view reads a separate local restricted directory as described below.

To point at a fork, a different branch, or a private mirror:

```r
Sys.setenv(TRAIT_DATA_OWNER = "your-org")        # default: AleAliSousa
Sys.setenv(TRAIT_DATA_REPO_NAME = "your-repo")   # default: Evo-M1-Trait-Data
Sys.setenv(TRAIT_DATA_BRANCH = "your-branch")    # default: main
shiny::runApp("/path/to/TraitExplorer")
```

(or set the same via `GITHUB_OWNER`/`GITHUB_REPO`/`GITHUB_BRANCH` in `config.R`.)

## Previewing un-pushed data (optional)

Set `TRAIT_DATA_LOCAL_DIR` to a local checkout of Evo-M1-Trait-Data to run the app against files that are not yet on GitHub (e.g. a crosswalk edit you want to see before committing):

```r
Sys.setenv(TRAIT_DATA_LOCAL_DIR = "/path/to/Evo-M1-Trait-Data")
shiny::runApp("/path/to/TraitExplorer")
```

The Start Here tab shows a "PREVIEWING LOCAL CHECKOUT" banner in this mode. The app still writes nothing into that folder. Unset the variable (or start a fresh R session) to go back to GitHub.

## Local neuron-method comparison (optional)

Set the separate restricted comparison directory before launching the app:

```r
Sys.setenv(TRAIT_NEURON_COMPARISON_DIR = "/path/to/restricted_checks/_cross_table/Collins_Young_vs_HerculanoHouzel_primate_regions/method_comparison")
shiny::runApp("/path/to/TraitExplorer")
```

The directory must contain the three required CSVs named above, with their required headers and linked observation IDs. The individual and Charvet audit files are optional and are validated when present; an invalid focused audit file leaves the three core views available and prints a warning. The optional `sex_as_published`, `age_as_published`, `age_group`, `demographic_source_locus`, and `demographic_source_url` columns load when present; older directories display “Not reported” for missing demographics and use the neuron source URL as a demographic link only when a demographic value is reported. The age/sex link may point to a different paper than the neuron count source. Extra observation columns are retained for CSV export. An invalid required directory disables the tab and prints the validation reason at startup. Unset `TRAIT_NEURON_COMPARISON_DIR` and restart to restore the public-only interface. This variable is independent of `TRAIT_DATA_LOCAL_DIR`, which previews the public data checkout.

## Updating

- **In-app**: click "Refresh data from GitHub" at the top of any tab. This re-lists the repo and re-downloads every file the app reads (crosswalks, the per-domain trait tables, specimen notes) -- a few dozen small requests, fast enough to run interactively.
- **From the command line**: `Rscript refresh_cache.R` does the same thing without starting Shiny -- useful before a demo, or as a scheduled job to keep the cache warm.

## First test

From the TraitExplorer folder, run:

```r
source("check_setup.R")
source("run_app.R")
```

`check_setup.R` verifies required packages are installed and that GitHub is reachable (falling back to the last-known cache if not). The app is read-only over the source data either way.

## Deploying to the web

TraitExplorer has its own shinyapps.io app, separate from `evo-m1-brain-traits` (the Evo-M1-Trait-Data repo's app). Deployment uploads only app code (`app.R`, `data_layer.R`, `neuron_methods.R`, `structure_reconcile.R`, `config.R`). The restricted comparison CSVs remain outside this repository and are never uploaded. Without a local `TRAIT_NEURON_COMPARISON_DIR`, the deployed app has no Neuron methods tab. Public data still comes from GitHub at runtime, so a pure public-data change needs no redeploy.

`__compiled/compiled_long.csv.gz` must be on the data repo's branch before a `data_layer.R` that reads it is deployed; otherwise the app stops at start-up.

- **macOS** -- double-click **`Deploy_TraitExplorer.command`** in the repo root
- Or, from a terminal: `Rscript deploy_traitexplorer.R`

First-time setup (once per computer, only for the deploy step): log into shinyapps.io -> Account -> Tokens -> Show -> "Show secret" -> Copy, then paste the copied `rsconnect::setAccountInfo(...)` line into an R console once.

GitHub repository: [https://github.com/AleAliSousa/TraitExplorer](https://github.com/AleAliSousa/TraitExplorer)
