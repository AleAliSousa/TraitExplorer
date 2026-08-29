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

cat("Data repository found:\n", normalizePath(existing[[1]], winslash = "/"), "\n")
cat("\nSetup looks OK.\n")
