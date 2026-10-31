# TraitExplorer startup check
#
# TraitExplorer has no local-repository dependency: it reads Evo-M1-Trait-Data
# only from GitHub. This script checks the two things that can actually break
# that -- missing R packages, and GitHub being reachable -- instead of
# searching for a local checkout.

cat("Checking TraitExplorer setup...\n\n")

required_packages <- c("shiny", "DT", "dplyr", "stringr", "readr", "readxl", "httr", "jsonlite")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  cat("Missing package(s):", paste(missing, collapse = ", "), "\n")
  quit(status = 1)
}

cat("All required R packages are installed.\n")

app_dir <- dirname(normalizePath(
  tryCatch(sys.frame(1)$ofile, error = function(e) "check_setup.R"),
  winslash = "/", mustWork = FALSE
))
config_path <- file.path(app_dir, "config.R")
if (file.exists(config_path)) source(config_path, local = TRUE)
GITHUB_OWNER  <- if (exists("GITHUB_OWNER"))  GITHUB_OWNER  else "AleAliSousa"
GITHUB_REPO   <- if (exists("GITHUB_REPO"))   GITHUB_REPO   else "Evo-M1-Trait-Data"
GITHUB_BRANCH <- if (exists("GITHUB_BRANCH")) GITHUB_BRANCH else "main"

api_url <- sprintf("https://api.github.com/repos/%s/%s/branches/%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH)
resp <- tryCatch(httr::GET(api_url, httr::user_agent("TraitExplorer setup check")), error = function(e) NULL)

if (is.null(resp) || httr::http_error(resp)) {
  cat("\nCould not reach ", GITHUB_OWNER, "/", GITHUB_REPO, "@", GITHUB_BRANCH, " on GitHub.\n", sep = "")
  cat("Check network access, or that the repo/branch names in config.R are correct.\n")
  cat("If TraitExplorer has a cache from a previous successful run, it will still launch\n")
  cat("and fall back to that cached data.\n")
  quit(status = 1)
}

cat("GitHub repository reachable: ", GITHUB_OWNER, "/", GITHUB_REPO, "@", GITHUB_BRANCH, "\n", sep = "")

cache_dir <- Sys.getenv("TRAITEXPLORER_CACHE_DIR", unset = file.path(app_dir, ".gh_cache"))
if (dir.exists(cache_dir)) {
  n_cached <- length(list.files(cache_dir, recursive = TRUE))
  cat("Local cache present at ", cache_dir, " (", n_cached, " file(s)).\n", sep = "")
} else {
  cat("No local cache yet at ", cache_dir, " -- first launch will populate it.\n", sep = "")
}

cat("\nSetup looks OK.\n")
