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

### 2. Comparative Trait Search
- Instant multi-term AND searching across 16 compiled domains (Volumes, Cell Counts, Brain Mass, Body & Ecology, Behaviour, Cortical Layers, Endocranial Volume, Sensory, Sleep, etc.).
- Domain selector to search across all domains or focus on a specific merge.
- Export results as CSV.

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

GitHub repository: [https://github.com/AleAliSousa/TraitExplorer](https://github.com/AleAliSousa/TraitExplorer)
