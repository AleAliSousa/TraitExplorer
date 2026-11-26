# test_app.R -- headless regression test for TraitExplorer.
# Usage: Rscript test_app.R [path-to-local-Evo-M1-Trait-Data] [label]
# Without a path it tests against GitHub (the deployed configuration).
# Exercises every tab's server logic via shiny::testServer, including the
# Region Comparison pairing and an inspector sweep over one specimen per
# (kind, concept, match) class. Prints one line per check; a stack trace
# means a regression.
# Set TRAIT_NEURON_COMPARISON_DIR for focused local Neuron methods checks.
args <- commandArgs(TRUE); repo <- if (length(args)) args[1] else ""; label <- if (length(args) > 1) args[2] else "TEST"
if (nzchar(repo)) Sys.setenv(TRAIT_DATA_LOCAL_DIR = repo)
suppressPackageStartupMessages({library(shiny); library(dplyr)})
app_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]), mustWork = FALSE))
if (is.na(app_dir) || !file.exists(file.path(app_dir, "app.R"))) app_dir <- getwd()
setwd(app_dir)
env <- new.env(); suppressMessages(sys.source("app.R", envir = env))

# A candidate public projection is off until its manifest is explicitly
# approved and all listed files match their hashes and row counts.
nm_off <- env$load_neuron_methods("", public_directory = tempfile("nm_absent_"))
stopifnot(!nm_off$enabled, identical(nm_off$status, "disabled"))
cat(sprintf("[%s] neuron methods default-off=%s\n", label, !nm_off$enabled))
nm_public_test <- tempfile("nm_public_manifest_test_")
dir.create(nm_public_test)
make_nm_observation <- function(id, source, region, neurons) {
  fields <- stats::setNames(as.list(rep("", length(env$NM_OBSERVATION_COLUMNS))),
                            env$NM_OBSERVATION_COLUMNS)
  fields$observation_id <- id; fields$species <- "Synthetic species"
  fields$species_as_published <- "Synthetic species"
  fields$specimen <- id; fields$source <- source; fields$year <- "2026"
  fields$lab_method <- "Synthetic method"; fields$method <- "Synthetic method"
  fields$region <- region; fields$scope <- "Synthetic tissue"
  fields$hemisphere_basis <- "one hemisphere"; fields$hemisphere <- "left"
  fields$n_individuals <- "1"; fields$neurons <- as.character(neurons)
  fields$condition <- "normal"; fields$plot_role <- "primary"
  fields$source_url <- "https://example.org/synthetic-fixture"
  fields$source_locus <- "Synthetic test fixture"
  for (column in env$NM_DEMOGRAPHIC_COLUMNS) fields[[column]] <- ""
  fields$release_basis <- "derived_from_published"
  fields$derivation_note <- "Synthetic test only"
  as.data.frame(fields, stringsAsFactors = FALSE, check.names = FALSE)
}
write.csv(rbind(make_nm_observation("synthetic_other", "Synthetic other", "cerebral cortex", 120),
                make_nm_observation("synthetic_hh", "Synthetic HH", "cerebral cortex", 100)),
          file.path(nm_public_test, "whole_cortex_observations.csv"), row.names = FALSE, na = "")
write.csv(make_nm_observation("synthetic_v1", "Synthetic V1", "V1", 25),
          file.path(nm_public_test, "v1_observations.csv"), row.names = FALSE, na = "")
ratio_fixture <- data.frame(species = "Synthetic species",
                     comparison_observation_id = "synthetic_other",
                     hh_observation_id = "synthetic_hh",
                     other_lab_method = "Synthetic method", other_neurons = 120,
                     hh_neurons = 100, other_over_hh = 1.2,
                     taxon_match = "same reported species", specimen = "",
                     quality_flag = "", release_basis = "derived_from_published",
                     derivation_note = "Calculated between synthetic cohorts",
                     other_source_url = "https://example.org/synthetic-fixture",
                     other_source_locus = "Synthetic test fixture",
                     hh_source_url = "https://example.org/synthetic-fixture",
                     hh_source_locus = "Synthetic test fixture",
                     other_release_basis = "derived_from_published",
                     hh_release_basis = "derived_from_published")
for (side in c("other", "hh"))
  for (column in env$NM_DEMOGRAPHIC_COLUMNS)
    ratio_fixture[[paste0(side, "_", column)]] <- ""
write.csv(ratio_fixture, file.path(nm_public_test, "paired_relative_to_hh.csv"),
          row.names = FALSE, na = "")
fixture_files <- env$NM_PUBLIC_REQUIRED_FILES
fixture_manifest <- list(
  schema_version = 1L, status = "NOT_APPROVED", reviewed_by = "", reviewed_at = "",
  files = lapply(fixture_files, function(filename) {
    path <- file.path(nm_public_test, filename)
    list(filename = filename, sha256 = digest::digest(path, algo = "sha256", file = TRUE),
         row_count = nrow(read.csv(path, check.names = FALSE)))
  })
)
fixture_manifest_path <- file.path(nm_public_test, "manifest.json")
jsonlite::write_json(fixture_manifest, fixture_manifest_path, auto_unbox = TRUE, pretty = TRUE)
candidate <- env$load_neuron_methods("", public_directory = nm_public_test)
stopifnot(!candidate$enabled, grepl("not approved", candidate$reason, fixed = TRUE),
          !env$nm_public_manifest_check(nm_public_test)$approved)
fixture_manifest$status <- "APPROVED"
fixture_manifest$reviewed_by <- "synthetic test reviewer"
fixture_manifest$reviewed_at <- "2026-01-01"
jsonlite::write_json(fixture_manifest, fixture_manifest_path, auto_unbox = TRUE, pretty = TRUE)
public_fixture <- env$load_neuron_methods("", public_directory = nm_public_test)
invalid_local <- env$load_neuron_methods(file.path(nm_public_test, "missing_local_audit"),
                                          public_directory = nm_public_test)
stopifnot(public_fixture$enabled,
          identical(public_fixture$data_mode, "published_source_subset"),
          nrow(public_fixture$whole) == 2L, nrow(public_fixture$v1) == 1L,
          nrow(public_fixture$pairs) == 1L,
          "release_basis" %in% names(public_fixture$whole),
          "derivation_note" %in% names(public_fixture$pairs),
          grepl("Neuron count basis", env$nm_observation_hover(public_fixture$whole)[1], fixed = TRUE),
          grepl("Calculated between synthetic cohorts",
                env$nm_ratio_hover(public_fixture$pairs)[1], fixed = TRUE),
          length(env$nm_public_bundle_files(nm_public_test)) == 4L,
          !invalid_local$enabled, identical(invalid_local$status, "invalid"),
          grepl("does not exist", invalid_local$reason, fixed = TRUE))
original_neuron_methods <- env$initial_neuron_methods
env$initial_neuron_methods <- public_fixture
testServer(env$server, {
  session$setInputs(nm_view = "whole", nm_roles = "primary", nm_quality = "all")
  stopifnot(all(c("Neuron count basis", "Count derivation") %in% names(nm_table_data())))
  session$setInputs(nm_view = "ratio")
  stopifnot(all(c("Ratio basis", "Ratio derivation") %in% names(nm_table_data())))
})
env$initial_neuron_methods <- original_neuron_methods
fixture_manifest$review_payload <- "synthetic restricted metadata"
jsonlite::write_json(fixture_manifest, fixture_manifest_path, auto_unbox = TRUE, pretty = TRUE)
bad_manifest_key <- env$nm_public_manifest_check(nm_public_test)
stopifnot(!bad_manifest_key$approved,
          grepl("unlisted fields", bad_manifest_key$reason, fixed = TRUE))
fixture_manifest$review_payload <- NULL
fixture_manifest$files[[1]]$private_note <- "synthetic restricted metadata"
jsonlite::write_json(fixture_manifest, fixture_manifest_path, auto_unbox = TRUE, pretty = TRUE)
bad_entry_key <- env$nm_public_manifest_check(nm_public_test)
stopifnot(!bad_entry_key$approved,
          grepl("unlisted fields", bad_entry_key$reason, fixed = TRUE))
fixture_manifest$files[[1]]$private_note <- NULL
v1_fixture_path <- file.path(nm_public_test, "v1_observations.csv")
v1_fixture <- read.csv(v1_fixture_path, stringsAsFactors = FALSE, check.names = FALSE)
v1_fixture$private_note <- "synthetic field that must not ship"
write.csv(v1_fixture, v1_fixture_path, row.names = FALSE, na = "")
fixture_manifest$files[[2]]$sha256 <- digest::digest(v1_fixture_path, algo = "sha256", file = TRUE)
fixture_manifest$files[[2]]$row_count <- nrow(v1_fixture)
jsonlite::write_json(fixture_manifest, fixture_manifest_path, auto_unbox = TRUE, pretty = TRUE)
bad_public_column <- env$nm_public_manifest_check(nm_public_test)
stopifnot(!bad_public_column$approved,
          grepl("unlisted public columns", bad_public_column$reason, fixed = TRUE))
v1_fixture$private_note <- NULL
write.csv(v1_fixture, v1_fixture_path, row.names = FALSE, na = "")
fixture_manifest$files[[2]]$sha256 <- digest::digest(v1_fixture_path, algo = "sha256", file = TRUE)
jsonlite::write_json(fixture_manifest, fixture_manifest_path, auto_unbox = TRUE, pretty = TRUE)
stopifnot(env$nm_public_manifest_check(nm_public_test)$approved)
writeLines("extra staging file", file.path(nm_public_test, "review_manifest.json"))
unlisted_file <- env$nm_public_manifest_check(nm_public_test)
stopifnot(!unlisted_file$approved, grepl("unlisted file", unlisted_file$reason, fixed = TRUE))
unlink(file.path(nm_public_test, "review_manifest.json"))
write("", file.path(nm_public_test, "v1_observations.csv"), append = TRUE)
bad_hash <- env$load_neuron_methods("", public_directory = nm_public_test)
local_override <- env$load_neuron_methods(nm_public_test, public_directory = nm_public_test)
stopifnot(!bad_hash$enabled, grepl("SHA-256", bad_hash$reason, fixed = TRUE),
          local_override$enabled, identical(local_override$data_mode, "full_local_audit"),
          !length(env$nm_public_bundle_files(nm_public_test)))
unlink(nm_public_test, recursive = TRUE)
cat(sprintf("[%s] public candidate blocked; approved fixture loaded; UI provenance columns rendered; invalid local path, extra manifest fields, private CSV column, and extra file failed closed; hash mismatch blocked; local override loaded\n", label))
if (isTRUE(env$initial_neuron_methods$enabled) &&
    identical(env$initial_neuron_methods$data_mode, "published_source_subset")) {
  stopifnot(nrow(env$initial_neuron_methods$whole) > 0,
            nrow(env$initial_neuron_methods$v1) > 0,
            nrow(env$initial_neuron_methods$pairs) > 0)
}
if (isTRUE(env$initial_neuron_methods$enabled) &&
    identical(env$initial_neuron_methods$data_mode, "full_local_audit")) {
  nm <- env$initial_neuron_methods
  stopifnot(nrow(nm$whole) > 0, nrow(nm$v1) > 0, nrow(nm$pairs) > 0)
  stopifnot(all(env$NM_DEMOGRAPHIC_COLUMNS %in% names(nm$whole)),
            all(env$NM_DEMOGRAPHIC_COLUMNS %in% names(nm$v1)),
            all(paste0("other_", env$NM_DEMOGRAPHIC_COLUMNS) %in% names(nm$pairs)),
            all(paste0("hh_", env$NM_DEMOGRAPHIC_COLUMNS) %in% names(nm$pairs)))
  cross_paper <- nm$whole[nm$whole$observation_id == "Collins10_09-27", , drop = FALSE]
  stopifnot(nrow(cross_paper) == 1L,
            nzchar(cross_paper$demographic_source_url),
            cross_paper$demographic_source_url != cross_paper$source_url,
            grepl("PMC3839763", cross_paper$demographic_source_url, fixed = TRUE))
  w <- env$nm_filter_view(nm, "whole")
  v <- env$nm_filter_view(nm, "v1")
  r <- env$nm_filter_view(nm, "ratio")
  stopifnot(all(w$plot_role == "primary"), all(v$plot_role == "primary"),
            all(v$region == "V1"), all(r$other_plot_role == "primary"),
            !any(tolower(w$condition) == "epilepsy"))
  all_w <- env$nm_filter_view(nm, "whole", roles = "all")
  all_v <- env$nm_filter_view(nm, "v1", roles = "all")
  stopifnot(any(all_w$plot_role == "duplicate"),
            any(all_w$plot_role == "condition_excluded"),
            any(all_v$plot_role == "condition_excluded"),
            !any(all_v$plot_role == "region_proxy_excluded"),
            all(all_v$region == "V1"))
  stopifnot(all(r$other_over_hh > 0),
            any(grepl("genus complex", r$taxon_match, fixed = TRUE)),
            any(grepl("Cebus/Sapajus", r$taxon_match, fixed = TRUE)),
            any(nm$v1$duplicate_group == "chimp_11_38"),
            any(nm$whole$duplicate_group == "Turner_12-58"))
  if (nrow(nm$whole_individual)) {
    stopifnot(all(nm$whole_individual$plot_role == "individual_component"),
              all(nm$whole_individual$point_status == "component"),
              nrow(env$nm_filter_view(nm, "whole", roles = "individual_component")) == nrow(nm$whole_individual),
              all(env$nm_observation_symbol(nm$whole_individual) == "square-open"))
  }
  if (nrow(nm$v1_individual)) {
    stopifnot(all(nm$v1_individual$plot_role == "individual_component"),
              all(nm$v1_individual$region == "V1"),
              nrow(env$nm_filter_view(nm, "v1", roles = "individual_component")) == nrow(nm$v1_individual),
              all(env$nm_observation_symbol(nm$v1_individual) == "square-open"))
  }
  if (isTRUE(nm$charvet_enabled)) {
    fig <- env$nm_charvet_rows(nm)
    with_diagnostic <- env$nm_charvet_rows(nm, show_diagnostic = TRUE)
    stopifnot(nrow(nm$fig7) > 0, nrow(nm$history) > 0,
              !any(fig$plot_status == "diagnostic_excluded"),
              nrow(with_diagnostic) == nrow(fig) + sum(nm$fig7$plot_status == "diagnostic_excluded"),
              all(is.finite(fig$volume_mm3[fig$plot_status == "measured_individual"])),
              all(is.na(fig$volume_mm3[fig$source == "Suner & Rakic 1996"])),
              all(fig$region[fig$plot_status == "region_proxy"] != "V1"),
              all(nzchar(fig$source_url)),
              all(nzchar(nm$history$charvet2013_url)),
              all(nzchar(nm$history$charvet2017_url)),
              all(nzchar(nm$history$hh2007_url)))
    stopifnot(all(c(env$NM_DEMOGRAPHIC_COLUMNS, "scope") %in% names(fig)))
    diagnostic <- with_diagnostic[with_diagnostic$plot_status == "diagnostic_excluded", , drop = FALSE]
    stopifnot(all(grepl("8 and 14", diagnostic$source_locus, fixed = TRUE)))
    mapped <- fig[fig$plot_status == "V1_map_pending_signoff", , drop = FALSE]
    stopifnot(nrow(mapped) == 1L, mapped$neurons == 220430950,
              grepl("signoff", mapped$notes, ignore.case = TRUE))
  }
  shared_chimp <- all_v[all_v$duplicate_group == "chimp_11_38" &
                          all_v$plot_role == "primary", , drop = FALSE]
  stopifnot(nrow(shared_chimp) > 1,
            all(env$nm_observation_symbol(shared_chimp) == "circle-open"),
            all(env$nm_ratio_symbol(r[r$other_duplicate_group == "Turner_12-58", , drop = FALSE]) == "circle-open"))
  bad_dir <- tempfile("neuron_methods_bad_schema_")
  dir.create(bad_dir)
  file.copy(file.path(nm$directory, c("whole_cortex_observations.csv",
                                       "v1_observations.csv", "paired_relative_to_hh.csv")),
            bad_dir)
  # An optional column in only one observation file must not disable loading.
  v1_file <- file.path(bad_dir, "v1_observations.csv")
  v1_extra <- read.csv(v1_file, stringsAsFactors = FALSE, check.names = FALSE,
                       colClasses = "character", na.strings = character(0))
  v1_extra$demographic_source_url <- NULL
  v1_extra$added_optional_field <- seq_len(nrow(v1_extra))
  write.csv(v1_extra, v1_file, row.names = FALSE, na = "")
  augmented <- env$load_neuron_methods(bad_dir)
  extra_colors <- env$nm_method_colors(c("HH IF", "New comparison method"))
  stopifnot(augmented$enabled, !augmented$charvet_enabled,
            "added_optional_field" %in% names(augmented$v1),
            identical(augmented$v1$demographic_source_url,
                      ifelse(env$nm_nonblank(augmented$v1$sex_as_published) |
                               env$nm_nonblank(augmented$v1$age_as_published) |
                               env$nm_nonblank(augmented$v1$age_group),
                             augmented$v1$source_url, "")),
            "New comparison method" %in% names(extra_colors),
            nzchar(extra_colors[["New comparison method"]]))
  if (isTRUE(nm$charvet_enabled)) {
    writeLines(c("point_id,source", "bad,bad"),
               file.path(bad_dir, "charvet_fig7_reassessment.csv"))
    audit_bad <- env$load_neuron_methods(bad_dir)
    stopifnot(audit_bad$enabled, !audit_bad$charvet_enabled,
              nzchar(audit_bad$charvet_warning))
    unlink(file.path(bad_dir, "charvet_fig7_reassessment.csv"))
  }
  bad_file <- file.path(bad_dir, "whole_cortex_observations.csv")
  bad_lines <- readLines(bad_file, warn = FALSE)
  bad_lines[1] <- sub("source_url", "source_link", bad_lines[1], fixed = TRUE)
  writeLines(bad_lines, bad_file)
  bad <- env$load_neuron_methods(bad_dir)
  unlink(bad_dir, recursive = TRUE)
  stopifnot(!bad$enabled, grepl("missing required headers: source_url", bad$reason, fixed = TRUE))
  cat(sprintf("[%s] neuron methods rows whole/V1/ratios=%d/%d/%d; default=%d/%d/%d; optional V1 column=%s; schema rejects missing source_url=%s\n",
              label, nrow(nm$whole), nrow(nm$v1), nrow(nm$pairs),
              nrow(w), nrow(v), nrow(r), augmented$enabled, !bad$enabled))
  testServer(env$server, {
    session$setInputs(nm_view = "whole", nm_roles = "primary", nm_quality = "all")
    stopifnot(nrow(nm_filtered()) == nrow(w), !is.null(output$nm_plot), !is.null(output$nm_table))
    session$setInputs(nm_sex = "Not reported")
    stopifnot(nrow(nm_filtered()) == sum(!env$nm_nonblank(w$sex_as_published)))
    session$setInputs(nm_sex = "All", nm_age_group = "Not reported")
    stopifnot(nrow(nm_filtered()) == sum(!env$nm_nonblank(w$age_group)))
    session$setInputs(nm_age_group = "All")
    if (nrow(nm$whole_individual)) {
      session$setInputs(nm_roles = "individual_component")
      stopifnot(nrow(nm_filtered()) == nrow(nm$whole_individual), !is.null(output$nm_plot))
      session$setInputs(nm_roles = "primary")
    }
    selected_sources <- head(unique(w$source), 2L)
    session$setInputs(nm_source = selected_sources)
    stopifnot(all(nm_filtered()$source %in% selected_sources))
    session$setInputs(nm_source = character())
    session$setInputs(nm_view = "v1")
    stopifnot(nrow(nm_filtered()) == nrow(v), !is.null(output$nm_plot))
    if (nrow(nm$v1_individual)) {
      session$setInputs(nm_roles = "individual_component")
      stopifnot(nrow(nm_filtered()) == nrow(nm$v1_individual), !is.null(output$nm_plot))
      session$setInputs(nm_roles = "primary")
    }
    if (any(nm$v1$plot_role == "historical_context")) {
      session$setInputs(nm_roles = "historical_context")
      stopifnot(nrow(nm_filtered()) > 0, !is.null(output$nm_plot))
      session$setInputs(nm_roles = "primary")
    }
    session$setInputs(nm_roles = c("primary", "region_proxy_excluded"))
    selected_context <- nm_non_v1_context()
    stopifnot(nrow(nm_filtered()) == nrow(v),
              nrow(nm_visible_rows()) == nrow(v) + nrow(selected_context),
              all(selected_context$plot_role == "region_proxy_excluded"),
              !any(nm_filtered()$plot_role == "region_proxy_excluded"),
              all(nm_filtered()$region == "V1"))
    session$setInputs(nm_roles = "primary")
    session$setInputs(nm_view = "ratio")
    stopifnot(nrow(nm_filtered()) == nrow(r), !is.null(output$nm_plot))
    session$setInputs(nm_sex = "Not reported")
    stopifnot(nrow(nm_filtered()) == sum(!env$nm_nonblank(r$other_sex_as_published)))
    session$setInputs(nm_sex = "All")
    session$setInputs(nm_table_rows_selected = 1L)
    stopifnot(!is.null(output$nm_detail))
    if (isTRUE(nm$charvet_enabled)) {
      session$setInputs(nm_view = "charvet", nm_charvet_diagnostic = FALSE)
      focused <- nm_charvet_data()
      stopifnot(nrow(focused) == nrow(fig),
                !is.null(output$nm_charvet_volume_plot),
                !is.null(output$nm_charvet_count_plot),
                !is.null(output$nm_charvet_table),
                !is.null(output$nm_charvet_history_table))
      session$setInputs(nm_charvet_table_rows_selected = 1L,
                        nm_charvet_history_table_rows_selected = 1L)
      stopifnot(!is.null(output$nm_charvet_detail),
                !is.null(output$nm_charvet_history_detail))
      session$setInputs(nm_charvet_diagnostic = TRUE)
      stopifnot(nrow(nm_charvet_data()) == nrow(with_diagnostic),
                any(nm_charvet_data()$plot_status == "diagnostic_excluded"))
    }
    cat(sprintf("[%s] neuron methods server views render whole/V1/ratio%s and source detail\n",
                label, if (isTRUE(nm$charvet_enabled)) "/Charvet" else ""))
  })
}
sx <- env$initial_data$specimen_data$crosswalk
cat(sprintf("[%s] specimens=%d accepted_name present=%d basis=%s region_rows=%d\n", label, nrow(sx), sum(!is.na(sx$accepted_name)), paste(names(table(sx$taxon_resolution_basis)), collapse="/"), nrow(env$initial_data$trait_data$region)))
ts <- env$initial_data$trait_data$combined
cat(sprintf("[%s] trait search rows=%d Hemispheres column=%s doubled=%d one hemisphere=%d\n", label, nrow(ts), "Hemispheres" %in% names(ts), sum(grepl("2 x one hemisphere", ts$Hemispheres)), sum(grepl("^one hemisphere", ts$Hemispheres))))
rb_err <- attr(env$initial_data$trait_data$region, "build_error")
if (!is.null(rb_err)) cat(sprintf("[%s] REGION TABLE BUILD ERROR: %s\n", label, rb_err))
rb_miss <- attr(env$initial_data$trait_data$region, "missing_columns")
if (!is.null(rb_miss)) cat(sprintf("[%s] region table missing columns (filled NA): %s\n", label, paste(rb_miss, collapse = "; ")))
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
  # trait search: a doubled value is found by its Hemispheres note and the inspector names it
  session$setInputs(trait_domain="All Domains (Combined)", trait_column="Hemispheres", trait_search="doubled", trait_limit=1000)
  tr <- trait_results(); session$setInputs(trait_table_rows_selected = 1L)
  cat(sprintf("[%s] search 'doubled' in Hemispheres=%d inspector shows the note=%s\n", label, attr(tr, "match_count"), nrow(tr) > 0 && grepl("Hemispheres: </strong>", paste(unlist(output$trait_detail_card), collapse = " "), fixed = TRUE)))
  # trait search by category: Domain, then Measure class; the source dataset is a provenance filter
  td <- rv$trait_data; doms <- sort(unique(td$combined$Domain)); ds_all <- names(td$dataset_tables)
  session$setInputs(trait_column="All fields", trait_search="", trait_limit=50000)
  n_dom <- vapply(doms, function(d) { session$setInputs(trait_domain=d, trait_class="All measure classes", trait_dataset="All source datasets"); attr(trait_results(), "match_count") }, numeric(1))
  n_ds <- vapply(ds_all, function(d) { session$setInputs(trait_domain="All Domains (Combined)", trait_dataset=d); attr(trait_results(), "match_count") }, numeric(1))
  cat(sprintf("[%s] domains=%d rows by domain sum=%d | source datasets=%d rows by dataset sum=%d | a dataset named as a domain=%d\n", label, length(doms), sum(n_dom), length(ds_all), sum(n_ds), sum(doms %in% ds_all)))
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
# Structure Profile + Trait Reconciliation (shared structure_reconcile.R)
testServer(env$server, {
  h <- rv$harmonized
  cat(sprintf("[%s] harmonized values=%d structure-profile species=%d reconciliation pairs=%d\n", label,
              nrow(h$values), length(h$species_choices), nrow(h$candidates)))
  sp <- if ("Homo sapiens" %in% h$species_choices) "Homo sapiens" else unname(h$species_choices)[1]
  keys <- env$sr_keys_for(h$structure_rows, sp)
  session$setInputs(bs_species = sp, bs_key = unname(keys)[1], bs_ref = "", bs_log = TRUE, bs_top = 40)
  p <- bs_prof()
  cat(sprintf("[%s] structure profile %s | %s: rows=%d table renders=%s matrix renders=%s\n", label, sp,
              names(keys)[1], nrow(p), !is.null(output$bs_table), !is.null(output$bs_matrix)))
  refs <- env$sr_reference_choices(h$values, h$meta, sp, unname(keys)[1])
  if (length(refs)) {
    session$setInputs(bs_ref = refs[1]); p <- bs_prof()
    cat(sprintf("[%s] %% of %s: finite=%d of %d\n", label, refs[1], sum(is.finite(p$Pct_of_reference)), nrow(p)))
  }
  session$setInputs(bs_compare = setdiff(unname(h$species_choices), sp)[1:2]); p <- bs_prof()
  cat(sprintf("[%s] with comparison species: species=%d rows=%d\n", label, length(unique(p$Species)), nrow(p)))
  session$setInputs(tr_verdict = env$SR_VERDICTS, tr_kind = "all", tr_search = "", tr_tol = 10)
  f <- tr_filtered(); cat(sprintf("[%s] reconciliation pairs listed=%d status renders=%s\n", label, nrow(f), !is.null(output$tr_status)))
  if (nrow(f)) {
    session$setInputs(tr_pairs_rows_selected = 1L)
    dd <- tr_detail(); g <- tr_group()
    cat(sprintf("[%s] pair '%s' vs '%s': detail rows=%d differs=%d reconciled rows=%d\n", label,
                f$Series_A[1], f$Series_B[1], nrow(dd), sum(dd$Status == "DIFFERS"), nrow(g)))
  }
  session$setInputs(tr_kind = "S", tr_search = "neocortex"); cat(sprintf("[%s] brain-structure pairs matching 'neocortex'=%d\n", label, nrow(tr_filtered())))
})
