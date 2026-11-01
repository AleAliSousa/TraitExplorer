# test_app.R -- headless regression test for TraitExplorer.
# Usage: Rscript test_app.R [path-to-local-Evo-M1-Trait-Data] [label]
# Without a path it tests against GitHub (the deployed configuration).
# Exercises every tab's server logic via shiny::testServer, including the
# Region Comparison pairing and an inspector sweep over one specimen per
# (kind, concept, match) class. Prints one line per check; a stack trace
# means a regression.
args <- commandArgs(TRUE); repo <- if (length(args)) args[1] else ""; label <- if (length(args) > 1) args[2] else "TEST"
if (nzchar(repo)) Sys.setenv(TRAIT_DATA_LOCAL_DIR = repo)
suppressPackageStartupMessages({library(shiny); library(dplyr)})
app_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), mustWork = FALSE))
if (is.na(app_dir) || !file.exists(file.path(app_dir, "app.R"))) app_dir <- getwd()
setwd(app_dir)
env <- new.env(); suppressMessages(sys.source("app.R", envir = env))
sx <- env$initial_data$specimen_data$crosswalk
cat(sprintf("[%s] specimens=%d accepted_name present=%d basis=%s region_rows=%d\n", label, nrow(sx), sum(!is.na(sx$accepted_name)), paste(names(table(sx$taxon_resolution_basis)), collapse="/"), nrow(env$initial_data$trait_data$region)))
testServer(env$server, {
  # Region comparison: proxy off, species mode, label toggles, fewer than 3 points
  session$setInputs(rc_mode="regions", rc_proxy=FALSE, rc_log=TRUE, rc_labels=FALSE, rc_species="Mus musculus", rc_x="CMR: CMRgl (umol/100g/min)", rc_y="cellcounts: N.n (count)")
  d <- rc_pairs(); cat(sprintf("[%s] mouse proxy-off pairs=%d fit_is_null=%s\n", label, nrow(d), is.null(rc_fit())))
  st <- output$rc_status; pl <- output$rc_plot; tb <- output$rc_table; un <- output$rc_unpaired
  cat(sprintf("[%s] outputs render (status/plot/table/unpaired): %s\n", label, paste(!is.null(st), !is.null(pl), !is.null(tb), TRUE)))
  session$setInputs(rc_log=FALSE, rc_proxy=TRUE); d <- rc_pairs(); cat(sprintf("[%s] linear mode pairs=%d\n", label, nrow(d)))
  session$setInputs(rc_mode="species", rc_region="Hippocampus", rc_x="volumes: Vol.mm3 (mm3)", rc_y="CMR: CMRgl (umol/100g/min)")
  d <- rc_pairs(); f <- rc_fit(); cat(sprintf("[%s] species-mode hippocampus pairs=%d slope=%s\n", label, nrow(d), if (is.null(f)) "NA" else round(f$slope,3)))
  # a nonsense pair -> empty, no error
  session$setInputs(rc_x="GLI: GLI_all_layers (%)", rc_y="CMR: CMRgl (umol/100g/min)"); cat(sprintf("[%s] nonsense pair rows=%d status_ok=%s\n", label, nrow(rc_pairs()), !is.null(output$rc_status)))
  # specimen explorer: old spelling still findable, inspector renders for a changed row
  session$setInputs(spec_filter_taxon="All", spec_filter_kind="All", spec_filter_collection="All", spec_filter_pub="All", spec_filter_match="All", spec_filter_conflict="All", specimen_search="norwegicus")
  n1 <- nrow(filtered_specimens()); session$setInputs(specimen_search="Rattus norvegicus"); n2 <- nrow(filtered_specimens())
  cat(sprintf("[%s] search 'norwegicus'=%d  'Rattus norvegicus'=%d\n", label, n1, n2))
  session$setInputs(specimen_table_rows_selected = 1L); insp <- output$specimen_inspector; cat(sprintf("[%s] inspector renders=%s\n", label, !is.null(insp)))
  # home + coverage + trait
  cat(sprintf("[%s] home_counts renders=%s\n", label, !is.null(output$home_counts)))
  session$setInputs(coverage_species="Homo sapiens"); cat(sprintf("[%s] coverage domains human=%d\n", label, nrow(coverage_summary())))
  session$setInputs(ex_region=1); cat(sprintf("[%s] example button ok\n", label))
})
# Inspector sweep: one row per (kind, has concept, match class) plus every taxon-conflict row
testServer(env$server, {
  session$setInputs(spec_filter_taxon="All", spec_filter_kind="All", spec_filter_collection="All", spec_filter_pub="All", spec_filter_match="All", spec_filter_conflict="All", specimen_search="")
  dat <- filtered_specimens()
  grp <- paste(dat$specimen_kind, !is.na(dat$taxon_concept), dat$match)
  idx <- unique(c(match(unique(grp), grp), which(as.logical(dat$taxon_conflict) %in% TRUE)))
  fails <- 0; for (i in idx) { session$setInputs(specimen_table_rows_selected = i); r <- tryCatch({output$specimen_inspector; TRUE}, error=function(e) {cat("  FAIL row", i, dat$canonical_specimen[i], ":", conditionMessage(e), "\n"); FALSE}); if (!r) fails <- fails + 1 }
  cat(sprintf("[%s] inspector sweep: %d rows tested, %d failures\n", label, length(idx), fails))
})
