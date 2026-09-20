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

- `app.R` - Shiny application
- `config.R` - optional data-repository configuration
- `check_setup.R` - checks R packages and finds the data repository
- `run_app.R` - launches the app

## Data location

The app does **not** require the Evo-M1-Trait-Data repository to be inside TraitExplorer.

It first uses `TRAIT_DATA_REPO` when set, then searches common local locations including:

`~/Library/CloudStorage/*/Species/Evo-M1-Trait-Data`

To force a location:

```r
Sys.setenv(TRAIT_DATA_REPO = "/path/to/Evo-M1-Trait-Data")
shiny::runApp("/path/to/TraitExplorer")
```

## First test

From the TraitExplorer folder, run:

```r
source("check_setup.R")
source("run_app.R")
```

The app reads the local data repository in read-only mode and does not modify source files.

GitHub repository: [https://github.com/AleAliSousa/TraitExplorer](https://github.com/AleAliSousa/TraitExplorer)
