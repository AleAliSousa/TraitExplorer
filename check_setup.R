# TraitExplorer startup check

cat("Checking TraitExplorer setup...\n\n")

required_packages <- c("shiny", "DT", "dplyr", "stringr", "readr", "readxl", "janitor")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  cat("Missing package(s):", paste(missing, collapse = ", "), "\n")
  quit(status = 1)
}

cat("All required R packages are installed.\n")

repo <- Sys.getenv("TRAIT_DATA_REPO", unset = "")
candidates <- unique(c(
  repo,
  Sys.glob(path.expand("~/Library/CloudStorage/*/Species/Evo-M1-Trait-Data")),
  path.expand("~/Species/Evo-M1-Trait-Data"),
  file.path(getwd(), "Evo-M1-Trait-Data"),
  file.path(dirname(getwd()), "Evo-M1-Trait-Data")
))
candidates <- candidates[nzchar(candidates)]
existing <- candidates[dir.exists(candidates)]

if (!length(existing)) {
  cat("\nEvo-M1-Trait-Data was not found.\n")
  cat("Set TRAIT_DATA_REPO to the local repository path, e.g.:\n")
  cat('Sys.setenv(TRAIT_DATA_REPO = ".../Evo-M1-Trait-Data")\n')
  quit(status = 1)
}

found_repo <- normalizePath(existing[[1]], winslash = "/")
cat("Data repository found:\n", found_repo, "\n")

# Check specimen crosswalk
spec_file <- file.path(found_repo, "_keys", "specimen_crosswalk", "specimen_crosswalk.csv")
if (file.exists(spec_file)) {
  cat("Specimen crosswalk found: OK\n")
} else {
  cat("Note: Specimen crosswalk file not found at expected path.\n")
}

# Check merging directory
merging_dirs <- list.dirs(found_repo, recursive = FALSE, full.names = TRUE)
merging_dirs <- merging_dirs[grepl("__merging", basename(merging_dirs), fixed = TRUE)]
cat("Comparative trait domains found:", length(merging_dirs), "\n")

cat("\nSetup looks OK.\n")
