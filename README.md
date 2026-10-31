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
- Instant multi-term AND searching across 16 compiled domains (Volumes, Cell Counts, Brain Mass, Body & Ecology, Behaviour, Cortical Layers, Endocranial Volume, Sensory, Sleep, etc.).
- Domain selector to search across all domains or focus on a specific merge.
- Export results as CSV.

### 2b. Region Comparison (new)
- Pairs two different kinds of measurement on the **same brain regions in the same species** -- e.g. mouse regional glucose metabolic rate (CMR domain) against regional neuron numbers (cell-count domain) -- and fits an OLS line (log-log by default) with Pearson/Spearman statistics. Points are brain regions. A second mode makes points species for one region.
- Regions from different domains are matched through two upstream keys: `_keys/variable_domain.csv` (variable label -> canonical structure, measure, unit) and `_keys/anatomy_crosswalk.csv` (domain term -> concept, `direct` / `proxy`). Structures with no crosswalk row pair only on an identical name and are labelled `own name`. Every pairing shows its link class and sources; unpaired regions are listed so a missing crosswalk row is visible, not silent.
- Covers: cell counts, volumes, brain-part weights, cerebral metabolic rate, GLI, cortical areas, cell morphology, M1 cortical layers. Download the paired table as CSV.
- Species names in Trait Search / Species Coverage / Region Comparison are harmonised through `_keys/species_display_aliases.csv`; pickers show common names from `_keys/species_cohorts.csv` where known.

### 3. Repository Explorer
- Browse and search repository files by study, author, file type, and year.
- Direct links to open files on GitHub.
- Download selected repository files.

## Files

- `app.R` - Shiny application (UI + server)
- `data_layer.R` - all data access: fetches Evo-M1-Trait-Data from GitHub, caches it locally, builds the tables the app displays. Sourced by both `app.R` and `refresh_cache.R`.
- `refresh_cache.R` - standalone script (`Rscript refresh_cache.R`) that updates the local GitHub cache without launching Shiny. Run this whenever the data repo has changed and you want the next launch to be instant.
- `config.R` - optional GitHub repo/branch override
- `check_setup.R` - checks R packages and GitHub reachability
- `run_app.R` - launches the app

## Data location

TraitExplorer has **no local-repository dependency**. It never reads a checkout of Evo-M1-Trait-Data from disk; every table is fetched over HTTPS from `github.com/AleAliSousa/Evo-M1-Trait-Data` and cached under `TraitExplorer/.gh_cache/` (mirroring the repo's folder layout) purely for speed on repeat launches. Deleting `.gh_cache/` just means the next load re-downloads.

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

TraitExplorer has its own shinyapps.io app, separate from `evo-m1-brain-traits` (the Evo-M1-Trait-Data repo's app). Because TraitExplorer reads its data straight from GitHub at runtime, deploying only ever uploads the app code (`app.R`, `data_layer.R`, `config.R`) -- a live TraitExplorer always sees current GitHub data with no redeploy needed for a pure data change.

- **macOS** -- double-click **`Deploy_TraitExplorer.command`** in the repo root
- Or, from a terminal: `Rscript deploy_traitexplorer.R`

First-time setup (once per computer, only for the deploy step): log into shinyapps.io -> Account -> Tokens -> Show -> "Show secret" -> Copy, then paste the copied `rsconnect::setAccountInfo(...)` line into an R console once.

GitHub repository: [https://github.com/AleAliSousa/TraitExplorer](https://github.com/AleAliSousa/TraitExplorer)
