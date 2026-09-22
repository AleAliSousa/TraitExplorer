#!/usr/bin/env Rscript
# refresh_cache.R
#
# Standalone update script for TraitExplorer's GitHub cache. Run this
# whenever Evo-M1-Trait-Data has changed upstream (new paper folder, an
# updated __merging_* table, a crosswalk edit) and you want TraitExplorer's
# NEXT launch to pick it up immediately rather than fetching on first use.
#
# This does the same "Refresh data from GitHub" work as the in-app button,
# but from the command line and without starting Shiny -- useful for a cron
# job, a pre-deploy step, or just warming the cache before a demo.
#
# Usage:  Rscript refresh_cache.R
#
# Both this script and app.R source data_layer.R, so there is exactly one
# place that knows how to talk to GitHub -- keeping them in sync is not a
# separate maintenance job.

app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(app_file) || !nzchar(app_file)) app_file <- "refresh_cache.R"
app_dir <- dirname(normalizePath(app_file, winslash = "/", mustWork = FALSE))

config_path <- file.path(app_dir, "config.R")
if (file.exists(config_path)) source(config_path, local = TRUE)

source(file.path(app_dir, "data_layer.R"), local = TRUE)

message(sprintf("Refreshing TraitExplorer's cache from %s/%s@%s ...", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH))
message("Cache dir: ", CACHE_DIR)

result <- load_all_data(force_tree = TRUE, force_content = TRUE)

message(
  "Done.\n",
  "  Files indexed: ", nrow(result$index_tbl), "\n",
  "  Specimen records: ", nrow(result$specimen_data$crosswalk), "\n",
  "  Trait records: ", nrow(result$trait_data$combined), " across ",
  length(result$trait_data$domain_tables), " domains\n",
  "  Loaded at: ", format(result$loaded_at, "%Y-%m-%d %H:%M:%S")
)
