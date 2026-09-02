# TraitExplorer

Shiny application for exploring the Evo-M1-Trait-Data repository.

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
shiny::runApp()
```

The app reads the local data repository and does not modify it.

GitHub repository: [https://github.com/AleAliSousa/TraitExplorer](https://github.com/AleAliSousa/TraitExplorer)
