# TraitExplorer

TraitExplorer is a Shiny application for exploring the [Evo-M1-Trait-Data](https://github.com/AleAliSousa/Evo-M1-Trait-Data) comparative trait database.

## What it does

- Browse and search repository files by filename, folder, year and file type.
- Restrict the file browser to paper folders or `__Public/comparative-data`.
- Open the corresponding GitHub file directly from the table.
- Select and download a local source file.
- Search merged trait records using one or more terms. Multiple terms are treated as AND terms.
- Search all fields or restrict the search to a specific column.
- Preserve `source_dataset` and `source_folder` in merged records for provenance.
- Download the current trait-search results as CSV.

## Requirements

R packages:

```r
install.packages(c("shiny", "DT", "dplyr", "stringr", "readr", "readxl", "janitor"))
```

## Configure the data repository

The app first checks `TRAIT_DATA_REPO`, then searches common local locations (including `~/Library/CloudStorage/*/Species/Evo-M1-Trait-Data`, the current working directory, and its parent directory).

For a different machine, the cleanest option is:

```r
Sys.setenv(TRAIT_DATA_REPO = "/path/to/Evo-M1-Trait-Data")
```

Then run:

```r
shiny::runApp()
```

The app reads the repository but does not modify its source files.
