# Source-level neuron-method comparisons for TraitExplorer. A local restricted
# directory takes precedence. A bundled public projection is read only after
# its release manifest passes the approval, filename, and checksum gate.
# Neither source uses the GitHub data layer, its cache, or the compiled feed.

NM_PUBLIC_CSV_FILES <- c(
  "whole_cortex_observations.csv", "v1_observations.csv",
  "paired_relative_to_hh.csv", "whole_cortex_individual_observations.csv",
  "v1_individual_observations.csv"
)
NM_PUBLIC_REQUIRED_FILES <- NM_PUBLIC_CSV_FILES[1:3]

nm_public_directory <- function() {
  root <- get0("app_dir", inherits = TRUE, ifnotfound = getwd())
  file.path(root, "data", "neuron_methods_public")
}

nm_public_expected_columns <- function(filename) {
  observation <- c(NM_OBSERVATION_COLUMNS, NM_DEMOGRAPHIC_COLUMNS,
                   "release_basis", "derivation_note")
  individual <- c(NM_OBSERVATION_COLUMNS, NM_DEMOGRAPHIC_COLUMNS,
                  "sex", "age_years", "neuron_ci95_halfwidth",
                  "release_basis", "derivation_note")
  ratio <- c(NM_RATIO_COLUMNS, paste0("other_", NM_DEMOGRAPHIC_COLUMNS),
             paste0("hh_", NM_DEMOGRAPHIC_COLUMNS),
             "other_source_url", "other_source_locus",
             "hh_source_url", "hh_source_locus",
             "other_release_basis", "hh_release_basis",
             "release_basis", "derivation_note")
  switch(filename,
         whole_cortex_observations.csv = observation,
         v1_observations.csv = observation,
         whole_cortex_individual_observations.csv = individual,
         v1_individual_observations.csv = individual,
         paired_relative_to_hh.csv = ratio)
}

nm_public_manifest_check <- function(directory = nm_public_directory()) {
  fail <- function(reason) list(approved = FALSE, reason = reason, files = character())
  if (!dir.exists(directory)) return(fail("No reviewed public neuron-method projection is installed."))
  if (nzchar(Sys.readlink(directory))) return(fail("Public projection directory must not be a symbolic link."))
  manifest_path <- file.path(directory, "manifest.json")
  if (!file.exists(manifest_path)) return(fail("Public neuron-method projection has no approval manifest."))
  if (nzchar(Sys.readlink(manifest_path))) return(fail("Approval manifest must be a regular file."))
  if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("digest", quietly = TRUE))
    return(fail("Public projection verification requires jsonlite and digest."))
  manifest <- tryCatch(jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
                       error = function(e) NULL)
  manifest_keys <- c("schema_version", "status", "reviewed_by", "reviewed_at", "files")
  if (!is.list(manifest) || !setequal(names(manifest), manifest_keys) ||
      anyDuplicated(names(manifest)))
    return(fail("Approval manifest contains missing or unlisted fields."))
  if (!is.list(manifest) || length(manifest$schema_version) != 1L ||
      !identical(as.integer(manifest$schema_version), 1L))
    return(fail("Approval manifest has an unsupported schema version."))
  if (!identical(manifest$status, "APPROVED"))
    return(fail("Public neuron-method projection is not approved for release."))
  nonblank_scalar <- function(x) is.character(x) && length(x) == 1L &&
    !is.na(x) && nzchar(trimws(x))
  if (!nonblank_scalar(manifest$reviewed_by) || !nonblank_scalar(manifest$reviewed_at))
    return(fail("Approved projection requires reviewer and review date."))
  entries <- manifest$files
  if (!is.list(entries) || !length(entries) || !all(vapply(entries, is.list, logical(1))))
    return(fail("Approval manifest has no valid file list."))
  file_keys <- c("filename", "sha256", "row_count")
  if (any(!vapply(entries, function(x) setequal(names(x), file_keys) &&
                  !anyDuplicated(names(x)), logical(1))))
    return(fail("Approval manifest file entry contains missing or unlisted fields."))
  filenames <- vapply(entries, function(x) if (nonblank_scalar(x$filename)) x$filename else "", character(1))
  if (anyDuplicated(filenames) || any(!filenames %in% NM_PUBLIC_CSV_FILES) ||
      !all(NM_PUBLIC_REQUIRED_FILES %in% filenames))
    return(fail("Approval manifest filenames are missing, repeated, or outside the allowlist."))
  present_files <- list.files(directory, all.files = TRUE, no.. = TRUE, full.names = FALSE)
  if (!setequal(present_files, c("manifest.json", filenames)))
    return(fail("Projection directory contains a missing or unlisted file."))
  for (entry in entries) {
    filename <- entry$filename
    path <- file.path(directory, filename)
    if (!file.exists(path) || nzchar(Sys.readlink(path)))
      return(fail(paste0(filename, " is missing or is not a regular file.")))
    expected <- entry$sha256
    if (!nonblank_scalar(expected) || !grepl("^[0-9a-fA-F]{64}$", expected))
      return(fail(paste0(filename, " has no valid SHA-256 in the manifest.")))
    actual <- tryCatch(digest::digest(path, algo = "sha256", file = TRUE),
                       error = function(e) "")
    if (!identical(tolower(expected), tolower(actual)))
      return(fail(paste0(filename, " differs from its approved SHA-256.")))
    row_count <- entry$row_count
    if (length(row_count) != 1L || is.na(row_count) ||
        !is.numeric(row_count) || row_count < 0 || row_count != floor(row_count))
      return(fail(paste0(filename, " has no valid row count in the manifest.")))
    actual_rows <- tryCatch(nrow(utils::read.csv(path, stringsAsFactors = FALSE,
                                                check.names = FALSE)), error = function(e) NA_integer_)
    if (is.na(actual_rows) || actual_rows != row_count)
      return(fail(paste0(filename, " differs from its approved row count.")))
    actual_columns <- tryCatch(names(utils::read.csv(path, nrows = 0L,
                                                    check.names = FALSE,
                                                    fileEncoding = "UTF-8")),
                               error = function(e) character())
    expected_columns <- nm_public_expected_columns(filename)
    if (anyDuplicated(actual_columns) || !setequal(actual_columns, expected_columns))
      return(fail(paste0(filename, " has missing or unlisted public columns.")))
  }
  parsed <- nm_load_comparison_data(directory)
  if (!isTRUE(parsed$enabled))
    return(fail(paste0("Approved projection failed schema validation: ", parsed$reason)))
  for (rows in list(parsed$whole, parsed$v1, parsed$pairs)) {
    if (!nrow(rows)) next
    if (!all(c("release_basis", "derivation_note") %in% names(rows)) ||
        any(!nm_nonblank(rows$release_basis)) || any(!nm_nonblank(rows$derivation_note)))
      return(fail("Every public row requires a neuron-count or ratio basis and derivation note."))
  }
  for (rows in list(parsed$whole, parsed$v1)) {
    if (any(!rows$release_basis %in% c("source_reported", "derived_from_published")))
      return(fail("Public observation has an unsupported neuron-count basis."))
    if (any(nm_nonblank(rows$source_file)) || any(nm_nonblank(rows$notes)))
      return(fail("Public observation contains internal source_file or notes text."))
  }
  ratio_path <- file.path(directory, "paired_relative_to_hh.csv")
  ratio_raw <- utils::read.csv(ratio_path, stringsAsFactors = FALSE,
                               check.names = FALSE, colClasses = "character",
                               na.strings = character(0))
  ratio_links <- c("other_source_url", "other_source_locus",
                   "hh_source_url", "hh_source_locus")
  if (!all(c("release_basis", "derivation_note", ratio_links) %in% names(ratio_raw)) ||
      any(ratio_raw$release_basis != "derived_from_published") ||
      any(!nm_nonblank(ratio_raw$derivation_note)) ||
      any(!grepl("^https?://", ratio_raw$other_source_url)) ||
      any(!grepl("^https?://", ratio_raw$hh_source_url)) ||
      any(!nm_nonblank(ratio_raw$other_source_locus)) ||
      any(!nm_nonblank(ratio_raw$hh_source_locus)))
    return(fail("Public ratio rows require calculated basis and both primary-source links and loci."))
  whole <- parsed$whole
  other_idx <- match(ratio_raw$comparison_observation_id, whole$observation_id)
  hh_idx <- match(ratio_raw$hh_observation_id, whole$observation_id)
  if (anyNA(other_idx) || anyNA(hh_idx) ||
      any(ratio_raw$other_source_url != whole$source_url[other_idx]) ||
      any(ratio_raw$other_source_locus != whole$source_locus[other_idx]) ||
      any(ratio_raw$hh_source_url != whole$source_url[hh_idx]) ||
      any(ratio_raw$hh_source_locus != whole$source_locus[hh_idx]))
    return(fail("Public ratio citations differ from referenced released observations."))
  list(approved = TRUE, reason = "Approved projection verified.", files = filenames,
       directory = normalizePath(directory, winslash = "/"))
}

nm_public_bundle_files <- function(directory = nm_public_directory()) {
  check <- nm_public_manifest_check(directory)
  if (!isTRUE(check$approved)) return(character())
  file.path("data", "neuron_methods_public", c("manifest.json", check$files))
}

NM_OBSERVATION_COLUMNS <- c(
  "observation_id", "species", "species_as_published", "specimen", "source",
  "year", "lab_method", "method", "region", "scope", "hemisphere_basis",
  "hemisphere", "n_individuals", "neurons", "neurons_sd", "tissue_mass_g",
  "surface_area_mm2", "volume_mm3", "condition", "plot_role", "quality_flag",
  "duplicate_group", "source_file", "source_url", "source_locus", "notes"
)

# These fields are optional for older local comparison directories. A blank
# value means no source-backed value at this row's reporting level. An absent
# demographic URL in an older schema falls back to its neuron source URL only
# when that row reports a demographic value.
NM_DEMOGRAPHIC_COLUMNS <- c(
  "sex_as_published", "age_as_published", "age_group",
  "demographic_source_locus", "demographic_source_url"
)

NM_RATIO_COLUMNS <- c(
  "species", "comparison_observation_id", "hh_observation_id",
  "other_lab_method", "other_neurons", "hh_neurons", "other_over_hh",
  "taxon_match", "specimen", "quality_flag"
)

NM_CHARVET_FIG7_COLUMNS <- c(
  "point_id", "source", "region", "species", "specimen", "neurons",
  "neurons_sd", "volume_mm3", "volume_sd_mm3", "volume_status",
  "plot_status", "source_file", "notes"
)

NM_CHARVET_HISTORY_COLUMNS <- c(
  "taxon", "species", "charvet2013_specimen", "charvet2013_neurons",
  "charvet2017_neurons", "hh2007_neurons", "charvet2013_over_hh",
  "charvet2017_over_hh", "overlap_note"
)

nm_empty_comparison <- function(status = "disabled", message = "", directory = "") {
  list(
    enabled = FALSE, status = status, reason = message, message = message,
    directory = directory, data_mode = "none", data_label = "",
    whole = data.frame(), v1 = data.frame(),
    whole_individual = data.frame(), v1_individual = data.frame(),
    pairs = data.frame(), ratios = data.frame(), fig7 = data.frame(),
    history = data.frame(), charvet_enabled = FALSE, charvet_warning = ""
  )
}

nm_read_csv <- function(path, columns, label, allow_empty = FALSE) {
  x <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character", na.strings = character(0), fileEncoding = "UTF-8"
  )
  if (anyDuplicated(names(x))) {
    stop(label, " has duplicate headers: ", paste(unique(names(x)[duplicated(names(x))]), collapse = ", "))
  }
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(label, " is missing required headers: ", paste(missing, collapse = ", "))
  if (!nrow(x) && !allow_empty) stop(label, " has no rows")
  x
}

nm_bind_observations <- function(summary, individual) {
  if (!nrow(individual)) return(summary)
  if (anyDuplicated(c(summary$observation_id, individual$observation_id))) {
    stop("Summary and individual observation files share an observation_id")
  }
  columns <- union(names(summary), names(individual))
  align <- function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- rep(NA, nrow(x))
    x[columns]
  }
  out <- rbind(align(summary), align(individual))
  rownames(out) <- NULL
  out
}

nm_nonblank <- function(x) !is.na(x) & nzchar(trimws(x))

nm_numeric_column <- function(x, column, label, positive = FALSE) {
  x <- trimws(x)
  out <- suppressWarnings(as.numeric(x))
  bad <- nm_nonblank(x) & (!is.finite(out) | is.na(out))
  if (any(bad)) stop(label, " has nonnumeric ", column, " at row(s) ", paste(which(bad), collapse = ", "))
  if (positive && any(!is.na(out) & out <= 0)) {
    stop(label, " requires positive ", column, " at row(s) ", paste(which(!is.na(out) & out <= 0), collapse = ", "))
  }
  out
}

nm_validate_observations <- function(x, label, v1 = FALSE) {
  # Earlier individual tables used `sex` before the cross-paper audit added
  # `sex_as_published`. Preserve that literal value only for old schemas.
  if (!"sex_as_published" %in% names(x) && "sex" %in% names(x))
    x$sex_as_published <- x$sex
  old_demographic_url <- !"demographic_source_url" %in% names(x)
  for (column in setdiff(NM_DEMOGRAPHIC_COLUMNS, names(x)))
    x[[column]] <- rep("", nrow(x))
  if (old_demographic_url) {
    has_demographic <- nm_nonblank(x$sex_as_published) |
      nm_nonblank(x$age_as_published) | nm_nonblank(x$age_group)
    x$demographic_source_url <- ifelse(has_demographic, x$source_url, "")
  }
  for (column in c("observation_id", "species", "species_as_published", "source",
                   "lab_method", "method", "region", "scope", "hemisphere_basis",
                   "plot_role", "source_url", "source_locus")) {
    if (any(!nm_nonblank(x[[column]]))) stop(label, " has blank ", column, " at row(s) ",
                                            paste(which(!nm_nonblank(x[[column]])), collapse = ", "))
  }
  if (anyDuplicated(x$observation_id)) stop(label, " has duplicate observation_id values")
  if (any(!grepl("^https?://", x$source_url, ignore.case = TRUE))) {
    stop(label, " contains a source_url that is not HTTP(S)")
  }
  bad_demo_url <- nm_nonblank(x$demographic_source_url) &
    !grepl("^https?://", x$demographic_source_url, ignore.case = TRUE)
  if (any(bad_demo_url))
    stop(label, " contains a demographic_source_url that is not HTTP(S)")
  for (column in c("neurons", "neurons_sd", "tissue_mass_g", "surface_area_mm2", "volume_mm3")) {
    x[[column]] <- nm_numeric_column(x[[column]], column, label, positive = column == "neurons")
  }
  if (any(is.na(x$neurons))) stop(label, " has missing neuron totals")
  x$n_individuals <- nm_numeric_column(x$n_individuals, "n_individuals", label)
  if (v1 && any(x$plot_role == "primary" & x$region != "V1")) {
    stop(label, " has a non-V1 primary row; exact V1 rows must be marked region = V1")
  }
  x$point_status <- ifelse(x$plot_role == "individual_component", "component",
                           ifelse(x$plot_role != "primary", "secondary",
                                  ifelse(nm_nonblank(x$quality_flag) | nm_nonblank(x$duplicate_group),
                                         "caveated", "primary")))
  x
}

nm_validate_ratios <- function(x, whole) {
  label <- "paired_relative_to_hh.csv"
  for (column in c("species", "comparison_observation_id", "hh_observation_id",
                   "other_lab_method", "taxon_match")) {
    if (any(!nm_nonblank(x[[column]]))) stop(label, " has blank ", column, " at row(s) ",
                                            paste(which(!nm_nonblank(x[[column]])), collapse = ", "))
  }
  for (column in c("other_neurons", "hh_neurons", "other_over_hh")) {
    x[[column]] <- nm_numeric_column(x[[column]], column, label, positive = TRUE)
    if (any(is.na(x[[column]]))) stop(label, " has missing ", column)
  }
  other_i <- match(x$comparison_observation_id, whole$observation_id)
  hh_i <- match(x$hh_observation_id, whole$observation_id)
  if (anyNA(other_i) || anyNA(hh_i)) {
    stop(label, " contains observation IDs absent from whole_cortex_observations.csv")
  }
  if (any(abs(x$other_neurons / x$hh_neurons - x$other_over_hh) > 1e-4)) {
    stop(label, " has ratios inconsistent with its neuron totals")
  }
  if (any(abs(x$other_neurons - whole$neurons[other_i]) > 0.5) ||
      any(abs(x$hh_neurons - whole$neurons[hh_i]) > 0.5)) {
    stop(label, " has neuron totals inconsistent with its referenced observation rows")
  }
  for (side in c("other", "hh")) {
    idx <- if (side == "other") other_i else hh_i
    for (column in setdiff(names(whole), c("observation_id", "neurons"))) {
      target <- paste0(side, "_", column)
      if (!target %in% names(x)) x[[target]] <- whole[[column]][idx]
    }
  }
  # Generic numerator columns let all three views share source/method pickers.
  x$source <- x$other_source
  x$lab_method <- x$other_lab_method
  x$plot_role <- x$other_plot_role
  x$condition <- x$other_condition
  x$duplicate_group <- x$other_duplicate_group
  x$point_status <- ifelse(nm_ratio_caveated(x), "caveated", "primary")
  x
}

nm_validate_charvet_fig7 <- function(x, v1) {
  label <- "charvet_fig7_reassessment.csv"
  for (column in c("point_id", "source", "region", "species", "volume_status", "plot_status")) {
    if (any(!nm_nonblank(x[[column]]))) stop(label, " has blank ", column)
  }
  if (anyDuplicated(x$point_id)) stop(label, " has duplicate point_id values")
  for (column in c("neurons", "neurons_sd", "volume_mm3", "volume_sd_mm3")) {
    x[[column]] <- nm_numeric_column(x[[column]], column, label, positive = column == "neurons")
    if (column != "neurons" && any(x[[column]] < 0, na.rm = TRUE))
      stop(label, " has negative ", column)
  }
  if (anyNA(x$neurons)) stop(label, " has missing neuron counts")
  key <- paste(v1$source, v1$species, sep = "\r")
  idx <- match(paste(x$source, x$species, sep = "\r"), key)
  missing <- is.na(idx)
  idx[missing] <- match(x$source[missing], v1$source)
  for (column in c("source_url", "source_locus", "method", "quality_flag", "duplicate_group",
                   "hemisphere_basis", "hemisphere", "scope", NM_DEMOGRAPHIC_COLUMNS)) {
    value <- v1[[column]][idx]
    value[is.na(value)] <- ""
    if (column %in% names(x)) {
      given <- nm_nonblank(x[[column]])
      value[given] <- x[[column]][given]
    }
    x[[column]] <- value
  }
  diagnostic <- x$plot_status == "diagnostic_excluded"
  x$source_locus[diagnostic] <- "Fig. 2 + SI Fig. S3 + Dataset S1, IDs 6–14 (includes non-V1 pieces 8 and 14)"
  x
}

nm_validate_charvet_history <- function(x, whole) {
  label <- "charvet_whole_cortex_history.csv"
  if (any(!nm_nonblank(x$taxon)) || any(!nm_nonblank(x$species)))
    stop(label, " has blank taxon or species")
  for (column in c("charvet2013_neurons", "charvet2017_neurons", "hh2007_neurons",
                   "charvet2013_over_hh", "charvet2017_over_hh")) {
    x[[column]] <- nm_numeric_column(x[[column]], column, label, positive = TRUE)
  }
  key <- paste(whole$source, whole$species, sep = "\r")
  for (source in c("Charvet 2013", "Charvet 2017", "HH 2007 SI2")) {
    prefix <- switch(source, "Charvet 2013" = "charvet2013",
                     "Charvet 2017" = "charvet2017", "HH 2007 SI2" = "hh2007")
    idx <- match(paste(source, x$species, sep = "\r"), key)
    url_idx <- idx
    url_idx[is.na(url_idx)] <- match(source, whole$source)
    url <- whole$source_url[url_idx]
    url[is.na(url)] <- ""
    x[[paste0(prefix, "_url")]] <- url
    for (column in NM_DEMOGRAPHIC_COLUMNS) {
      value <- whole[[column]][idx]
      value[is.na(value)] <- ""
      x[[paste0(prefix, "_", column)]] <- value
    }
  }
  x
}

# The published bilateral HH/Gabi/Azevedo means were already divided by two
# when these CSVs were generated. Never transform neuron totals here.
nm_load_comparison_data <- function(directory = Sys.getenv("TRAIT_NEURON_COMPARISON_DIR", unset = "")) {
  if (is.null(directory) || !length(directory) || !nzchar(trimws(directory[1]))) {
    return(nm_empty_comparison("disabled", "Neuron methods is off. Set TRAIT_NEURON_COMPARISON_DIR to a local method_comparison directory."))
  }
  directory <- path.expand(trimws(directory[1]))
  if (!dir.exists(directory)) {
    return(nm_empty_comparison("invalid", paste0("Neuron comparison directory does not exist: ", directory), directory))
  }
  files <- c(
    whole = "whole_cortex_observations.csv",
    v1 = "v1_observations.csv",
    ratios = "paired_relative_to_hh.csv"
  )
  paths <- stats::setNames(file.path(directory, unname(files)), names(files))
  missing <- files[!file.exists(paths)]
  if (length(missing)) {
    return(nm_empty_comparison("invalid", paste0("Neuron comparison directory is missing: ", paste(missing, collapse = ", ")), directory))
  }
  tryCatch({
    whole <- nm_validate_observations(nm_read_csv(paths[["whole"]], NM_OBSERVATION_COLUMNS, files[["whole"]]), files[["whole"]])
    v1 <- nm_validate_observations(nm_read_csv(paths[["v1"]], NM_OBSERVATION_COLUMNS, files[["v1"]]), files[["v1"]], v1 = TRUE)
    optional_observations <- function(filename, v1_only = FALSE) {
      path <- file.path(directory, filename)
      if (!file.exists(path)) return(data.frame())
      rows <- nm_read_csv(path, NM_OBSERVATION_COLUMNS, filename, allow_empty = TRUE)
      if (!nrow(rows)) return(data.frame())
      nm_validate_observations(rows, filename, v1 = v1_only)
    }
    whole_individual <- optional_observations("whole_cortex_individual_observations.csv")
    v1_individual <- optional_observations("v1_individual_observations.csv", v1_only = TRUE)
    if (nrow(whole_individual) && any(whole_individual$plot_role != "individual_component")) {
      stop("whole_cortex_individual_observations.csv must use plot_role = individual_component")
    }
    if (nrow(v1_individual) && any(v1_individual$plot_role != "individual_component")) {
      stop("v1_individual_observations.csv must use plot_role = individual_component")
    }
    whole <- nm_bind_observations(whole, whole_individual)
    v1 <- nm_bind_observations(v1, v1_individual)
    ratios <- nm_validate_ratios(nm_read_csv(paths[["ratios"]], NM_RATIO_COLUMNS, files[["ratios"]]), whole)
    optional_audit <- function(filename, columns, validate_rows) {
      path <- file.path(directory, filename)
      if (!file.exists(path)) return(list(rows = data.frame(), warning = ""))
      tryCatch(
        list(rows = validate_rows(nm_read_csv(path, columns, filename)), warning = ""),
        error = function(e) list(rows = data.frame(), warning = paste0(filename, ": ", conditionMessage(e)))
      )
    }
    fig7 <- optional_audit("charvet_fig7_reassessment.csv", NM_CHARVET_FIG7_COLUMNS,
                           function(x) nm_validate_charvet_fig7(x, v1))
    history <- optional_audit("charvet_whole_cortex_history.csv", NM_CHARVET_HISTORY_COLUMNS,
                             function(x) nm_validate_charvet_history(x, whole))
    charvet_warning <- paste(Filter(nzchar, c(fig7$warning, history$warning)), collapse = "; ")
    list(
      enabled = TRUE, status = "ready",
      message = paste0(sprintf("Loaded %d whole-cortex rows (%d individual components), %d V1/related rows (%d individual components), and %d relative-to-HH rows from local files.",
                               nrow(whole), nrow(whole_individual), nrow(v1), nrow(v1_individual), nrow(ratios)),
                       if (nzchar(charvet_warning)) paste0(" Focused Charvet audit warning: ", charvet_warning) else ""),
      reason = "", directory = normalizePath(directory, winslash = "/"),
      whole = whole, v1 = v1, whole_individual = whole_individual,
      v1_individual = v1_individual, pairs = ratios, ratios = ratios,
      fig7 = fig7$rows, history = history$rows,
      charvet_enabled = nrow(fig7$rows) > 0, charvet_warning = charvet_warning
    )
  }, error = function(e) nm_empty_comparison("invalid", paste0("Neuron methods could not load: ", conditionMessage(e)), directory))
}

# Stable integration API for app.R. An explicit local directory always wins;
# a rejected public candidate never falls back to the full restricted audit.
load_neuron_methods <- function(path = Sys.getenv("TRAIT_NEURON_COMPARISON_DIR", unset = ""),
                                public_directory = nm_public_directory()) {
  if (length(path) && !is.na(path[1]) && nzchar(trimws(path[1]))) {
    out <- nm_load_comparison_data(path)
    out$data_mode <- "full_local_audit"
    out$data_label <- "Full local audit"
    return(out)
  }
  if (!dir.exists(public_directory))
    return(nm_empty_comparison("disabled", "No approved public neuron-method projection is installed."))
  check <- nm_public_manifest_check(public_directory)
  if (!isTRUE(check$approved))
    return(nm_empty_comparison("invalid", check$reason, public_directory))
  out <- nm_load_comparison_data(public_directory)
  if (!isTRUE(out$enabled)) return(out)
  out$data_mode <- "published_source_subset"
  out$data_label <- "Published-source subset"
  out$message <- paste(out$message, "Approved public projection verified against manifest.")
  out
}

nm_match_filter <- function(x, selected) {
  if (is.null(selected) || !length(selected) || any(tolower(trimws(selected)) %in% c("all", ""))) {
    return(rep(TRUE, length(x)))
  }
  x %in% selected
}

nm_condition_filter <- function(x, selected = "normal") {
  if (is.null(selected) || !length(selected) || any(tolower(selected) == "all")) {
    return(rep(TRUE, length(x)))
  }
  if (identical(selected, "normal")) return(!nm_nonblank(x) | tolower(trimws(x)) == "normal")
  x %in% selected
}

nm_quality_filter <- function(caveated, selected = "all") {
  if (is.null(selected) || !length(selected) || identical(selected, "all")) return(rep(TRUE, length(caveated)))
  if (identical(selected, "caveated")) return(caveated)
  if (identical(selected, "uncaveated")) return(!caveated)
  stop("quality must be all, caveated, or uncaveated")
}

nm_observation_caveated <- function(x) {
  if (!nrow(x)) return(logical(0))
  x$plot_role != "primary" | nm_nonblank(x$quality_flag) |
    nm_nonblank(x$duplicate_group) | !(tolower(trimws(x$condition)) %in% c("", "normal"))
}

# Filters accept NULL or "All" for unrestricted choices. Default roles and
# conditions show normal primary observations only. Pass roles = "all" and
# conditions = "all" to inspect duplicate, context, or epileptic rows.
nm_filter_observations <- function(x, sources = NULL, methods = NULL, species = NULL,
                                   roles = "primary", quality = "all", conditions = "normal",
                                   v1_only = FALSE) {
  if (!nrow(x)) return(x)
  keep <- nm_match_filter(x$source, sources) &
    nm_match_filter(x$lab_method, methods) &
    nm_match_filter(x$species, species) &
    nm_match_filter(x$plot_role, roles) &
    nm_condition_filter(x$condition, conditions) &
    nm_quality_filter(nm_observation_caveated(x), quality)
  if (v1_only) keep <- keep & x$region == "V1"
  x[which(keep), , drop = FALSE]
}

nm_filter_v1 <- function(x, sources = NULL, methods = NULL, species = NULL,
                         roles = "primary", quality = "all", conditions = "normal") {
  out <- nm_filter_observations(x, sources, methods, species, roles, quality,
                                conditions, v1_only = TRUE)
  out[out$plot_role != "region_proxy_excluded", , drop = FALSE]
}

nm_ratio_caveated <- function(x) {
  if (!nrow(x)) return(logical(0))
  nm_nonblank(x$quality_flag) | nm_nonblank(x$other_quality_flag) |
    nm_nonblank(x$hh_quality_flag) | x$taxon_match != "same reported species" |
    x$other_plot_role != "primary" | x$hh_plot_role != "primary" |
    nm_nonblank(x$other_duplicate_group)
}

# Source and method select the compared observation; HH is the reference.
nm_filter_ratios <- function(x, sources = NULL, methods = NULL, species = NULL,
                             roles = "primary", quality = "all", conditions = "normal") {
  if (!nrow(x)) return(x)
  species_keep <- nm_match_filter(x$species, species)
  if (!is.null(species) && length(species) && !any(tolower(trimws(species)) %in% c("all", ""))) {
    species_keep <- species_keep | x$other_species %in% species | x$other_species_as_published %in% species
  }
  keep <- nm_match_filter(x$other_source, sources) &
    nm_match_filter(x$other_lab_method, methods) &
    species_keep &
    nm_match_filter(x$other_plot_role, roles) &
    nm_condition_filter(x$other_condition, conditions) &
    nm_quality_filter(nm_ratio_caveated(x), quality)
  x[which(keep), , drop = FALSE]
}

nm_filter_view <- function(bundle, view, sources = character(), methods = character(),
                           species = character(), roles = "primary", quality = "all",
                           conditions = NULL) {
  if (!isTRUE(bundle$enabled)) return(data.frame())
  if (is.null(conditions)) {
    conditions <- if (any(roles %in% c("all", "condition_excluded"))) "all" else "normal"
  }
  if (view %in% c("whole", "whole_cortex", "Whole cortex")) {
    return(nm_filter_observations(bundle$whole, sources, methods, species, roles, quality, conditions))
  }
  if (view %in% c("v1", "V1")) {
    return(nm_filter_v1(bundle$v1, sources, methods, species, roles, quality, conditions))
  }
  if (view %in% c("ratio", "ratios", "relative_to_hh", "Relative to HH")) {
    return(nm_filter_ratios(bundle$pairs, sources, methods, species, roles, quality, conditions))
  }
  stop("Unknown neuron-method view: ", view)
}

# Plotly supports open symbols for visibly caveated observations. A secondary
# role gets a diamond to distinguish it from a flagged primary point.
nm_observation_symbol <- function(x) {
  if (!nrow(x)) return(character(0))
  ifelse(x$plot_role == "individual_component", "square-open",
         ifelse(x$plot_role != "primary", "diamond-open",
                ifelse(nm_observation_caveated(x), "circle-open", "circle")))
}

nm_ratio_symbol <- function(x) {
  if (!nrow(x)) return(character(0))
  ifelse(nm_ratio_caveated(x), "circle-open", "circle")
}

nm_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

nm_hover_value <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) return("not stated")
  nm_html_escape(x)
}

nm_hover_line <- function(label, value) paste0("<b>", label, ":</b> ", nm_hover_value(value))

nm_demographic_display <- function(x) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(trimws(out))] <- "Not reported"
  out
}

nm_filter_demographics <- function(x, sex = "All", age_group = "All", prefix = "") {
  if (!nrow(x)) return(x)
  sex_field <- paste0(prefix, "sex_as_published")
  age_field <- paste0(prefix, "age_group")
  if (!sex_field %in% names(x) || !age_field %in% names(x))
    stop("Demographic columns are missing from the comparison view")
  if (length(sex) && !identical(sex, "All"))
    x <- x[nm_demographic_display(x[[sex_field]]) %in% sex, , drop = FALSE]
  if (length(age_group) && !identical(age_group, "All"))
    x <- x[nm_demographic_display(x[[age_field]]) %in% age_group, , drop = FALSE]
  x
}

nm_observation_hover <- function(x) {
  if (!nrow(x)) return(character(0))
  vapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    paste(c(
      paste0("<b>", nm_hover_value(r$species_as_published), "</b> — ",
             format(r$neurons, big.mark = ",", scientific = FALSE, trim = TRUE), " neurons/hemi"),
      nm_hover_line("Source", paste(r$source, r$source_locus, sep = ", ")),
      nm_hover_line("Specimen", r$specimen),
      nm_hover_line("Canonical taxon", r$species),
      nm_hover_line("Hemisphere", paste(r$hemisphere, r$hemisphere_basis, sep = "; ")),
      nm_hover_line("Anatomy", paste(r$region, r$scope, sep = "; ")),
      nm_hover_line("Method", paste(r$lab_method, r$method, sep = "; ")),
      nm_hover_line("Sex as published", nm_demographic_display(r$sex_as_published)),
      nm_hover_line("Age as published", nm_demographic_display(r$age_as_published)),
      nm_hover_line("Age group", nm_demographic_display(r$age_group)),
      nm_hover_line("Demographic source locus", nm_demographic_display(r$demographic_source_locus)),
      nm_hover_line("Demographic source URL", nm_demographic_display(r$demographic_source_url)),
      if ("release_basis" %in% names(r)) nm_hover_line("Neuron count basis", r$release_basis),
      if ("derivation_note" %in% names(r)) nm_hover_line("Count derivation", r$derivation_note),
      if ("age_years" %in% names(r) && nm_nonblank(r$age_years)) nm_hover_line("Age (years)", r$age_years),
      if ("neuron_ci95_halfwidth" %in% names(r) && nm_nonblank(r$neuron_ci95_halfwidth))
        nm_hover_line("Neuron 95% CI half-width (neurons)", r$neuron_ci95_halfwidth),
      nm_hover_line("Condition", r$condition),
      nm_hover_line("Plot role", r$plot_role),
      nm_hover_line("Caveat", r$quality_flag),
      nm_hover_line("Duplicate group", r$duplicate_group),
      nm_hover_line("Notes", r$notes),
      nm_hover_line("Source URL", r$source_url)
    ), collapse = "<br>")
  }, character(1))
}

nm_ratio_hover <- function(x) {
  if (!nrow(x)) return(character(0))
  vapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    paste(c(
      paste0("<b>", nm_hover_value(r$species), "</b> — other / HH = ",
             format(round(r$other_over_hh, 3), nsmall = 3), " (between cohorts)"),
      nm_hover_line("Taxon match", r$taxon_match),
      if ("release_basis" %in% names(r)) nm_hover_line("Ratio basis", r$release_basis),
      if ("derivation_note" %in% names(r)) nm_hover_line("Ratio derivation", r$derivation_note),
      nm_hover_line("Other source", paste(r$other_source, r$other_source_locus, sep = ", ")),
      nm_hover_line("Other published taxon", r$other_species_as_published),
      nm_hover_line("Other specimen", r$other_specimen),
      nm_hover_line("Other hemisphere", paste(r$other_hemisphere, r$other_hemisphere_basis, sep = "; ")),
      nm_hover_line("Other anatomy", paste(r$other_region, r$other_scope, sep = "; ")),
      nm_hover_line("Other sex as published", nm_demographic_display(r$other_sex_as_published)),
      nm_hover_line("Other age as published", nm_demographic_display(r$other_age_as_published)),
      nm_hover_line("Other age group", nm_demographic_display(r$other_age_group)),
      nm_hover_line("Other demographic locus", nm_demographic_display(r$other_demographic_source_locus)),
      nm_hover_line("Other demographic source URL", nm_demographic_display(r$other_demographic_source_url)),
      nm_hover_line("Other method", paste(r$other_lab_method, r$other_method, sep = "; ")),
      nm_hover_line("Other condition", r$other_condition),
      nm_hover_line("Other caveat", paste(r$quality_flag, r$other_quality_flag, sep = "; ")),
      nm_hover_line("Duplicate group", r$other_duplicate_group),
      nm_hover_line("HH source", paste(r$hh_source, r$hh_source_locus, sep = ", ")),
      nm_hover_line("HH published taxon", r$hh_species_as_published),
      nm_hover_line("HH specimen", r$hh_specimen),
      nm_hover_line("HH hemisphere", paste(r$hh_hemisphere, r$hh_hemisphere_basis, sep = "; ")),
      nm_hover_line("HH anatomy", paste(r$hh_region, r$hh_scope, sep = "; ")),
      nm_hover_line("HH sex as published", nm_demographic_display(r$hh_sex_as_published)),
      nm_hover_line("HH age as published", nm_demographic_display(r$hh_age_as_published)),
      nm_hover_line("HH age group", nm_demographic_display(r$hh_age_group)),
      nm_hover_line("HH demographic locus", nm_demographic_display(r$hh_demographic_source_locus)),
      nm_hover_line("HH demographic source URL", nm_demographic_display(r$hh_demographic_source_url)),
      nm_hover_line("HH method", paste(r$hh_lab_method, r$hh_method, sep = "; ")),
      nm_hover_line("HH condition", r$hh_condition),
      nm_hover_line("HH caveat", r$hh_quality_flag),
      nm_hover_line("HH duplicate group", r$hh_duplicate_group),
      nm_hover_line("Other source URL", r$other_source_url),
      nm_hover_line("HH source URL", r$hh_source_url)
    ), collapse = "<br>")
  }, character(1))
}

nm_source_anchor <- function(url, label = "Primary source") {
  if (length(url) != 1 || is.na(url) || !grepl("^https?://", url, ignore.case = TRUE)) return("")
  paste0('<a href="', nm_html_escape(url), '" target="_blank" rel="noopener noreferrer">',
         nm_html_escape(label), '</a>')
}

nm_charvet_status_label <- function(status) {
  labels <- c(
    exact_V1_measured = "Giannaris measured cohort mean",
    measured_individual = "Giannaris measured individual",
    exact_V1_inferred = "Collins mass-inferred V1",
    V1_map_pending_signoff = "Collins V1 map pending signoff",
    count_only = "Suner count-only cohort mean",
    count_only_individual = "Suner count-only hemisphere",
    legacy_count_only = "Historical mixed-species count",
    region_proxy = "Occipital proxy (not exact V1)",
    diagnostic_excluded = "Excluded nine-piece diagnostic"
  )
  out <- unname(labels[status])
  out[is.na(out)] <- status[is.na(out)]
  out
}

# A common chart/table schema keeps the Fig. 7 source audit and its measured
# individual components inspectable without implying the summaries are extra
# specimens. Count-only rows retain missing volume; no proxy x is fabricated.
nm_charvet_rows <- function(bundle, show_diagnostic = FALSE) {
  if (!isTRUE(bundle$charvet_enabled)) return(data.frame())
  f <- bundle$fig7
  if (!isTRUE(show_diagnostic)) f <- f[f$plot_status != "diagnostic_excluded", , drop = FALSE]
  summary <- data.frame(
    point_id = f$point_id, source = f$source, species = f$species,
    specimen = f$specimen, region = f$region, scope = f$scope, neurons = f$neurons,
    neurons_sd = f$neurons_sd, volume_mm3 = f$volume_mm3,
    volume_sd_mm3 = f$volume_sd_mm3, volume_status = f$volume_status,
    plot_status = f$plot_status, record_type = rep("audit_summary", nrow(f)),
    source_file = f$source_file, source_url = f$source_url,
    source_locus = f$source_locus, method = f$method,
    hemisphere_basis = f$hemisphere_basis, hemisphere = f$hemisphere,
    quality_flag = f$quality_flag, duplicate_group = f$duplicate_group,
    sex = rep("", nrow(f)), age_years = rep("", nrow(f)),
    sex_as_published = f$sex_as_published,
    age_as_published = f$age_as_published,
    age_group = f$age_group,
    demographic_source_locus = f$demographic_source_locus,
    demographic_source_url = f$demographic_source_url,
    neuron_ci95_halfwidth = rep("", nrow(f)), notes = f$notes,
    stringsAsFactors = FALSE
  )
  i <- bundle$v1_individual
  if (nrow(i)) i <- i[i$source %in% c("Giannaris & Rosene 2012", "Suner & Rakic 1996"), , drop = FALSE]
  if (!nrow(i)) {
    summary$status_label <- nm_charvet_status_label(summary$plot_status)
    return(summary)
  }
  field <- function(name) if (name %in% names(i)) i[[name]] else rep("", nrow(i))
  individual <- data.frame(
    point_id = i$observation_id, source = i$source, species = i$species,
    specimen = i$specimen, region = i$region, scope = i$scope, neurons = i$neurons,
    neurons_sd = i$neurons_sd, volume_mm3 = i$volume_mm3,
    volume_sd_mm3 = NA_real_,
    volume_status = ifelse(is.finite(i$volume_mm3),
                           "measured paired Cavalieri volume", "not reported for this case"),
    plot_status = ifelse(i$source == "Giannaris & Rosene 2012",
                         "measured_individual", "count_only_individual"),
    record_type = "individual_component",
    source_file = i$source_file, source_url = i$source_url,
    source_locus = i$source_locus, method = i$method,
    hemisphere_basis = i$hemisphere_basis, hemisphere = i$hemisphere,
    quality_flag = i$quality_flag, duplicate_group = i$duplicate_group,
    sex = field("sex"), age_years = field("age_years"),
    sex_as_published = field("sex_as_published"),
    age_as_published = field("age_as_published"),
    age_group = field("age_group"),
    demographic_source_locus = field("demographic_source_locus"),
    demographic_source_url = field("demographic_source_url"),
    neuron_ci95_halfwidth = field("neuron_ci95_halfwidth"), notes = i$notes,
    stringsAsFactors = FALSE
  )
  out <- rbind(summary, individual)
  out$status_label <- nm_charvet_status_label(out$plot_status)
  rownames(out) <- NULL
  out
}

nm_charvet_hover <- function(x) {
  if (!nrow(x)) return(character(0))
  vapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    paste(c(
      paste0("<b>", nm_hover_value(r$source), " — ", nm_hover_value(r$specimen), "</b>"),
      nm_hover_line("Evidence", r$status_label),
      nm_hover_line("Anatomy", paste(r$region, r$scope, sep = "; ")),
      nm_hover_line("Sex as published", nm_demographic_display(r$sex_as_published)),
      nm_hover_line("Age as published", nm_demographic_display(r$age_as_published)),
      nm_hover_line("Age group", nm_demographic_display(r$age_group)),
      nm_hover_line("Demographic source locus", nm_demographic_display(r$demographic_source_locus)),
      nm_hover_line("Demographic source URL", nm_demographic_display(r$demographic_source_url)),
      nm_hover_line("Neurons", format(r$neurons, big.mark = ",", scientific = FALSE, trim = TRUE)),
      nm_hover_line("Volume (mm³)", if (is.finite(r$volume_mm3)) signif(r$volume_mm3, 5) else "not reported"),
      nm_hover_line("Volume basis", r$volume_status),
      nm_hover_line("Method", r$method),
      nm_hover_line("Hemisphere", paste(r$hemisphere, r$hemisphere_basis, sep = "; ")),
      nm_hover_line("Quality caveat", r$quality_flag),
      nm_hover_line("Shared group", r$duplicate_group),
      nm_hover_line("Notes", r$notes),
      nm_hover_line("Primary source", r$source_url)
    ), collapse = "<br>")
  }, character(1))
}
