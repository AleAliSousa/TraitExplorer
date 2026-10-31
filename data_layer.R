# data_layer.R
# All data access for TraitExplorer: fetching Evo-M1-Trait-Data from GitHub,
# caching it locally, and building the tables the app displays. No function
# in this file ever reads the user's local Evo-M1-Trait-Data checkout --
# TraitExplorer's only source of truth is the GitHub repository named below.
#
# Sourced by both app.R (interactively, inside Shiny) and refresh_cache.R
# (a plain Rscript that warms/updates the cache without launching the app).
# Keeping this logic out of app.R is what makes that second use possible.

required_data_packages <- c("readr", "readxl", "httr", "jsonlite", "stringr", "dplyr")
missing_data_packages <- required_data_packages[
  !vapply(required_data_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_data_packages)) {
  stop(
    "Please install the missing R package(s): ",
    paste(missing_data_packages, collapse = ", "),
    call. = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x

# Some Evo-M1-Trait-Data paths carry non-ASCII characters (e.g.
# "Barbeito-Andr\u00e9s_etal_2019/"). Under a C/POSIX locale (the R default on many
# servers and CI runners), base functions like dirname()/basename() abort
# with "unable to translate ... to native encoding" the moment they see one.
# Switching LC_CTYPE to a UTF-8 locale, if one is available on this machine,
# fixes it without changing any path-handling logic; on a system that is
# already UTF-8 (most desktop R installs) this is a harmless no-op.
if (l10n_info()$`UTF-8` != TRUE) {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
    if (isTRUE(tryCatch(nzchar(Sys.setlocale("LC_CTYPE", loc)), error = function(e) FALSE))) break
  }
}

# --------------------------------------------------
# CONFIG
# --------------------------------------------------
# config.R (if present, sourced by app.R before this file) may set
# GITHUB_OWNER / GITHUB_REPO / GITHUB_BRANCH. Fall back to the known repo.
if (!exists("GITHUB_OWNER"))  GITHUB_OWNER  <- Sys.getenv("TRAIT_DATA_OWNER",  unset = "AleAliSousa")
if (!exists("GITHUB_REPO"))   GITHUB_REPO   <- Sys.getenv("TRAIT_DATA_REPO_NAME", unset = "Evo-M1-Trait-Data")
if (!exists("GITHUB_BRANCH")) GITHUB_BRANCH <- Sys.getenv("TRAIT_DATA_BRANCH", unset = "main")

GITHUB_BASE_URL <- sprintf("https://github.com/%s/%s/blob/%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH)
RAW_BASE_URL    <- sprintf("https://raw.githubusercontent.com/%s/%s/%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH)
API_TREE_URL    <- sprintf(
  "https://api.github.com/repos/%s/%s/git/trees/%s?recursive=1",
  GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH
)

# Every file TraitExplorer reads is fetched from GitHub and cached here,
# mirroring the repo's folder layout. This is NOT the user's local
# Evo-M1-Trait-Data checkout -- it is a private, disposable cache the app
# manages itself; deleting it just means the next load re-downloads.
CACHE_DIR <- Sys.getenv("TRAITEXPLORER_CACHE_DIR", unset = file.path(app_dir, ".gh_cache"))
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

gh_ua <- httr::user_agent("TraitExplorer (https://github.com/AleAliSousa/TraitExplorer)")

# OPT-IN preview of a local Evo-M1-Trait-Data checkout. Off by default: the
# app's source of truth stays GitHub. Set TRAIT_DATA_LOCAL_DIR to the folder
# of a local checkout (e.g. the OneDrive copy) to preview data edits BEFORE
# they are pushed -- the tree listing is built from the folder and every
# fetch reads the local file. Nothing is written into that folder.
LOCAL_REPO_DIR <- Sys.getenv("TRAIT_DATA_LOCAL_DIR", unset = "")
USING_LOCAL_REPO <- nzchar(LOCAL_REPO_DIR) && dir.exists(LOCAL_REPO_DIR)
if (nzchar(LOCAL_REPO_DIR) && !USING_LOCAL_REPO) {
  message("TRAIT_DATA_LOCAL_DIR is set but does not exist (", LOCAL_REPO_DIR, "); falling back to GitHub.")
}

local_list_tree <- function() {
  files <- list.files(LOCAL_REPO_DIR, recursive = TRUE, all.files = FALSE, full.names = FALSE)
  files <- files[!grepl("(^|/)[.]git(/|$)", files)]
  sizes <- file.info(file.path(LOCAL_REPO_DIR, files))$size
  list(
    sha = "local",
    tree = Map(function(p, s) list(path = p, type = "blob", size = s), files, sizes),
    truncated = FALSE
  )
}

# --------------------------------------------------
# GITHUB ACCESS
# --------------------------------------------------

# One recursive listing of every file in the repo (path, type, size), via a
# single GitHub API call. Cached to _tree.json; a stale cache is used as a
# fallback if GitHub is briefly unreachable, so a network hiccup degrades to
# "last known state" rather than a crash.
gh_list_tree <- function(force = FALSE) {
  if (USING_LOCAL_REPO) return(local_list_tree())
  cache_file <- file.path(CACHE_DIR, "_tree.json")

  if (!force && file.exists(cache_file)) {
    cached <- tryCatch(jsonlite::fromJSON(cache_file, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(cached)) return(cached)
  }

  resp <- tryCatch(httr::GET(API_TREE_URL, gh_ua), error = function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) {
    if (file.exists(cache_file)) {
      message("GitHub tree listing request failed; using the last cached listing.")
      cached <- tryCatch(jsonlite::fromJSON(cache_file, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(cached)) return(cached)
    }
    stop(
      "Could not list ", GITHUB_OWNER, "/", GITHUB_REPO, "@", GITHUB_BRANCH,
      " from the GitHub API, and no cached listing exists at ", cache_file, ".",
      call. = FALSE
    )
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  writeLines(txt, cache_file)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# Blob paths only (files, not tree/directory entries), as a plain character
# vector -- what build_index()/load_specimen_system()/load_trait_data() walk
# to discover which files exist, without ever touching local disk outside
# CACHE_DIR.
gh_blob_paths <- function(tree) {
  entries <- tree$tree
  is_blob <- vapply(entries, function(e) identical(e$type, "blob"), logical(1))
  vapply(entries[is_blob], function(e) e$path, character(1))
}

gh_blob_sizes <- function(tree) {
  entries <- tree$tree
  is_blob <- vapply(entries, function(e) identical(e$type, "blob"), logical(1))
  vapply(entries[is_blob], function(e) if (is.null(e$size)) NA_real_ else as.numeric(e$size), numeric(1))
}

# Fetches one file's raw bytes from GitHub and writes them to CACHE_DIR,
# mirroring the repo path. Returns the local cache path (so every downstream
# reader -- read_csv/read_excel/readLines -- keeps taking a normal file
# path) or NULL on a 404/network failure with nothing cached yet. A stale
# cache entry is served if a forced re-fetch fails, same fallback logic as
# gh_list_tree().
gh_fetch <- function(rel_path, force = FALSE) {
  if (USING_LOCAL_REPO) {
    p <- file.path(LOCAL_REPO_DIR, rel_path)
    return(if (file.exists(p)) p else NULL)
  }
  local_path <- file.path(CACHE_DIR, rel_path)
  if (!force && file.exists(local_path)) return(local_path)

  url <- paste0(RAW_BASE_URL, "/", utils::URLencode(rel_path))
  resp <- tryCatch(httr::GET(url, gh_ua), error = function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) {
    return(if (file.exists(local_path)) local_path else NULL)
  }

  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  writeBin(httr::content(resp, "raw"), local_path)
  local_path
}

read_gh_csv <- function(rel_path, force = FALSE) {
  p <- gh_fetch(rel_path, force = force)
  if (is.null(p)) return(data.frame())
  tryCatch(
    read.csv(p, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE),
    error = function(e) data.frame()
  )
}

# Generic tabular reader kept for parity with the original local-disk
# version; not currently called from app.R (no call site reads an arbitrary
# file into a data.frame outside the fixed lookups below), but any future
# feature that wants to preview an arbitrary repo file can call
# read_data_file(gh_fetch(rel_path)) rather than reinventing this switch.
read_data_file <- function(f) {
  if (is.null(f) || !file.exists(f)) return(NULL)
  ext <- tolower(tools::file_ext(f))
  tryCatch(
    switch(
      ext,
      csv = readr::read_csv(f, show_col_types = FALSE),
      tsv = readr::read_tsv(f, show_col_types = FALSE),
      xls = readxl::read_excel(f),
      xlsx = readxl::read_excel(f),
      NULL
    ),
    error = function(e) NULL
  )
}

match_terms <- function(values, query) {
  if (!length(values)) return(logical())
  terms <- strsplit(trimws(tolower(query)), "[[:space:]]+")[[1]]
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(rep(TRUE, length(values)))
  Reduce(`&`, lapply(terms, function(term) grepl(term, values, fixed = TRUE)))
}

make_fast_search_blob <- function(df) {
  if (!nrow(df) || !ncol(df)) return(character())
  clean_cols <- lapply(df, function(col) {
    col <- as.character(col)
    col[is.na(col) | col == "NA"] <- ""
    col
  })
  tolower(do.call(paste, c(clean_cols, sep = " ")))
}

# --------------------------------------------------
# REPOSITORY INDEX
# --------------------------------------------------

build_index <- function(tree) {
  paths <- gh_blob_paths(tree)
  sizes <- gh_blob_sizes(tree)

  # Hide git internals, editor/session state, and OS/filesystem junk from the
  # browsable index -- none of it is comparative trait data. Dotfiles are
  # excluded by basename generally (matches normal file-browser convention:
  # .gitignore, .gitattributes, .DS_Store, .Rhistory, .fuse_hidden* -- the
  # last are FUSE-mounted-filesystem temp files left behind when a program
  # deletes an open file on a network/cloud-sync mount, arbitrary leftover
  # bytes never meant to be read); *.RData session dumps are excluded by
  # extension since they aren't dotfiles.
  basenames <- basename(paths)
  # Root-level planning/audit notes for maintaining the repo itself -- not
  # comparative trait data, so they don't belong in a data browser either.
  # (The audit note is moving from the root to _keys/: done locally on 2026-10-08,
  # on GitHub once pushed. Both paths are listed so it stays hidden either way.)
  internal_docs <- c("APP_PLAN.md", "AUDIT_brain_size_compatibility.md",
                     "_keys/AUDIT_brain_size_compatibility.md")
  keep <- !grepl("(^|/)([.]git|[.]Rproj[.]user)(/|$)", paths) &
    !startsWith(basenames, ".") &
    !grepl("[.]RData$", basenames) &
    !(paths %in% internal_docs)
  paths <- paths[keep]; sizes <- sizes[keep]

  dplyr::tibble(
    relative_path = paths,
    size_kb = round(sizes / 1024, 1)
  ) |>
    dplyr::mutate(
      folder = dirname(relative_path),
      filename = basename(relative_path),
      extension = tolower(tools::file_ext(filename)),
      study_folder = vapply(
        strsplit(relative_path, "/", fixed = TRUE),
        function(x) if (length(x) >= 2) x[[1]] else "(root)",
        character(1)
      ),
      year = stringr::str_extract(study_folder, "(?:19|20)[0-9]{2}"),
      author = stringr::str_replace(study_folder, "_etal.*|__.*", ""),
      is_paper_folder = !startsWith(study_folder, "__"),
      github_url = paste0(GITHUB_BASE_URL, "/", relative_path)
    )
}

# --------------------------------------------------
# SPECIMEN SYSTEM LOADER
# --------------------------------------------------

load_specimen_system <- function(tree, force = FALSE) {
  key_dir <- "_keys/specimen_crosswalk"

  spec_crosswalk   <- read_gh_csv(file.path(key_dir, "specimen_crosswalk.csv"), force)
  fossil_crosswalk <- read_gh_csv(file.path(key_dir, "fossil_specimen_crosswalk.csv"), force)
  spec_sources     <- read_gh_csv(file.path(key_dir, "specimen_source_registry.csv"), force)
  taxon_concepts   <- read_gh_csv(file.path(key_dir, "taxon_concept_registry.csv"), force)
  external_links   <- read_gh_csv(file.path(key_dir, "specimen_external_links.csv"), force)
  collections      <- read_gh_csv("_keys/collection_registry.csv", force)
  fossil_comp      <- read_gh_csv(file.path(key_dir, "fossil_specimen_cerebellum_comparison.csv"), force)

  # Clean collection_group / collection_qualifier -- built by
  # _keys/build_collection_resolution.R from collection_registry.csv, so the
  # app never re-derives (or re-drifts) the raw `collection` string's meaning
  # itself. Joined onto the crosswalk here rather than shipping the raw
  # string alone: collection_group separates the actual holding collection
  # from the institute that hosts it (Hirnforschung hosts Stephan/Zilles, it
  # is not itself a fourth collection) and from the animal's source colony
  # (Yerkes is where the animal lived, not a holder -- it gets its own YERKES
  # group and never gets conflated with the Duesseldorf holding collection a
  # Yerkes-sourced animal's brain actually lives in).
  collection_resolution <- read_gh_csv("_keys/collection_resolution.csv", force)
  if (nrow(spec_crosswalk) && nrow(collection_resolution)) {
    res_idx <- match(spec_crosswalk$collection, collection_resolution$collection)
    spec_crosswalk$collection_group <- collection_resolution$collection_group[res_idx]
    spec_crosswalk$collection_qualifier <- collection_resolution$collection_qualifier[res_idx]
  } else {
    spec_crosswalk$collection_group <- NA_character_
    spec_crosswalk$collection_qualifier <- NA_character_
  }

  # Species-level accepted name for every resolved_taxon string, from
  # _keys/specimen_crosswalk/specimen_taxon_resolution.csv (built 2026-10-31).
  # resolved_taxon is kept verbatim (it may legitimately be a trinomial such
  # as Gorilla beringei graueri, or a genus-only label); accepted_name is the
  # species_reference name the rest of the database joins on, so the Species
  # filter and the cross-domain tabs group one animal under one name. Rows
  # whose resolution is still under review (status = review) keep their
  # resolved_taxon string as accepted_name -- so they stay selectable -- and
  # are tagged basis = "needs_review" so the inspector says so.
  taxon_resolution <- read_gh_csv(file.path(key_dir, "specimen_taxon_resolution.csv"), force)
  if (nrow(spec_crosswalk)) {
    if (nrow(taxon_resolution)) {
      res_idx <- match(spec_crosswalk$resolved_taxon, taxon_resolution$resolved_taxon)
      in_key <- !is.na(res_idx)
      resolved_ok <- in_key & taxon_resolution$status[res_idx] %in% "resolved"
      spec_crosswalk$accepted_name <- NA_character_
      spec_crosswalk$taxon_resolution_basis <- NA_character_
      spec_crosswalk$accepted_name[resolved_ok] <- taxon_resolution$accepted_name[res_idx[resolved_ok]]
      spec_crosswalk$taxon_resolution_basis[resolved_ok] <- taxon_resolution$basis[res_idx[resolved_ok]]
      review <- in_key & !resolved_ok
      spec_crosswalk$accepted_name[review] <- spec_crosswalk$resolved_taxon[review]
      spec_crosswalk$taxon_resolution_basis[review] <- "needs_review"
      # Anything the key does not cover (a new upstream row) falls back to
      # resolved_taxon itself so it is never silently dropped from the filter.
      miss <- !in_key & !is.na(spec_crosswalk$resolved_taxon)
      spec_crosswalk$accepted_name[miss] <- spec_crosswalk$resolved_taxon[miss]
      spec_crosswalk$taxon_resolution_basis[miss] <- "not in resolution key"
    } else {
      spec_crosswalk$accepted_name <- spec_crosswalk$resolved_taxon
      spec_crosswalk$taxon_resolution_basis <- NA_character_
    }
  }

  # Markdown notes: discovered from the GitHub tree listing (folder prefix +
  # .md extension) rather than list.files() on a local directory.
  note_dirs <- c("____Collections and Specimen notes", key_dir)
  blob_paths <- gh_blob_paths(tree)
  specimen_notes <- list()
  for (nd in note_dirs) {
    md_paths <- blob_paths[startsWith(blob_paths, paste0(nd, "/")) & grepl("[.]md$", blob_paths)]
    for (mp in md_paths) {
      nm <- basename(mp)
      if (nm %in% names(specimen_notes)) next
      p <- gh_fetch(mp, force = force)
      if (!is.null(p)) specimen_notes[[nm]] <- paste(readLines(p, warn = FALSE), collapse = "\n")
    }
  }

  # Published specimen-level measurement tables, at their fixed repo paths.
  pt_defs <- list(
    "MacLeod 2000 (Appendix I - 47 primates)" = "MacLeod__2000/MacLeod__2000_APPENDIXI.csv",
    "Smaers 2010 (Table 1 - Stephan specimens via Frahm)" =
      "Smaers_etal_2010/Smaers_etal_2010_Table1_Stephan_specimen_data_via_Frahm.csv",
    "de Sousa et al. 2010 (Table 1 - Hominoids)" = "deSousa_etal_2010/deSousa_etal_2010_Table1.csv",
    "Kochiyama et al. 2018 (Fossil Specimens Text)" =
      "Kochiyama_etal_2018/Kochiyama_etal_2018_FossilSpecimensText.csv",
    "Weaver 2001 (Table A-15 Fossils & Extant)" = "Weaver__2001/Weaver__2001_TableA-15.csv",
    "Barger et al. 2007 (Table 1 - Ape Amygdala)" = "Barger_etal_2007/Barger_etal_2007_Table1.csv",
    "Collins et al. 2016 (Table 1 - Chimpanzee Cortex)" = "Collins_etal_2016/Collins_etal_2016_Table1.csv",
    "Armstrong 1979 (Specimen Crosswalk)" = "Armstrong__1979/Armstrong__1979_specimen_crosswalk.csv",
    # Fobbs & Johnson 2011 catalogs the NMHM/AFIP-held collections; these are
    # the primary evidence behind several _keys/collection_registry.csv groups
    # (JOHNSON_MSU, MEYER_PHIPPS_JHU, CROSBY_UMICH) -- browsing them alongside
    # the Specimen Explorer's collection_group filter shows exactly what each
    # resolved group is grounded in.
    "Fobbs & Johnson 2011 (Table S1a - developmental specimens, diagnostic categories)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS1a.csv",
    "Fobbs & Johnson 2011 (Table S1b - species/study catalog)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS1b.csv",
    "Fobbs & Johnson 2011 (Table S2 - stains and sectioning catalog)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS2.csv",
    "Fobbs & Johnson 2011 (Table S3 - Johnson Michigan State collection)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS3.csv",
    "Fobbs & Johnson 2011 (Table S4 - Meyer-Phipps collection)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS4.csv",
    "Fobbs & Johnson 2011 (Table S5a - Huber-Crosby collection)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS5a.csv",
    "Fobbs & Johnson 2011 (Table S5b - Crosby-Lauer collection)" =
      "Fobbs_etal_2011/Fobbs_etal_2011_TableS5b.csv",
    # Zilles et al. 2011 catalogs the Duesseldorf Hirnforschung institute's
    # three brain collections -- the evidence behind the DUSSELDORF_HIRNFORSCHUNG
    # group (Stephan/Zilles/Zilles-Amunts share this one building).
    "Zilles et al. 2011 (Table S1 - Stephan/Zilles collection)" =
      "Zilles_etal_2011/Zilles_etal_2011_TableS1.csv",
    "Zilles et al. 2011 (Table S2 - Hirnforschung shared collection)" =
      "Zilles_etal_2011/Zilles_etal_2011_TableS2.csv",
    "Zilles et al. 2011 (Table S3 - Zilles/Amunts developmental sub-collection)" =
      "Zilles_etal_2011/Zilles_etal_2011_TableS3.csv",
    # Case tables kept next to the crosswalk: which studies used the gibbon
    # Disco, and the Glendenning-vs-Nudo green monkey (FSU) value comparison.
    "Disco (gibbon GPZ-5542) - public study list" =
      file.path(key_dir, "Disco_study_list_public.csv"),
    "Green monkey FSU - Glendenning vs Nudo value comparison" =
      file.path(key_dir, "Green_monkey_FSU_glendenning_vs_nudo.csv"),
    "Specimen taxon resolution key (resolved_taxon -> accepted_name)" =
      file.path(key_dir, "specimen_taxon_resolution.csv")
  )
  paper_tables <- list()
  for (lbl in names(pt_defs)) {
    d <- read_gh_csv(pt_defs[[lbl]], force)
    if (nrow(d) > 0) paper_tables[[lbl]] <- d
  }

  spec_search_blob <- make_fast_search_blob(spec_crosswalk)

  list(
    crosswalk = spec_crosswalk,
    fossil_crosswalk = fossil_crosswalk,
    sources = spec_sources,
    taxon_concepts = taxon_concepts,
    taxon_resolution = taxon_resolution,
    external_links = external_links,
    collections = collections,
    fossil_comp = fossil_comp,
    notes = specimen_notes,
    paper_tables = paper_tables,
    search_blob = spec_search_blob
  )
}

# --------------------------------------------------
# TRAIT DATA LOADER (HARMONIZED MERGES)
# --------------------------------------------------

load_trait_data <- function(tree, force = FALSE) {
  blob_paths <- gh_blob_paths(tree)
  top_level <- vapply(strsplit(blob_paths, "/", fixed = TRUE), `[`, character(1), 1)
  merge_dirs <- sort(unique(top_level[!is.na(top_level) & startsWith(top_level, "__merging")]))

  domain_tables <- list()
  combined_list <- list()

  for (dir in merge_dirs) {
    domain <- sub("^__merging_", "", dir)
    dir_files <- blob_paths[startsWith(blob_paths, paste0(dir, "/"))]

    preferred <- file.path(dir, paste0(domain, "_long.csv"))
    long_candidates <- dir_files[grepl("_long[.]csv$", basename(dir_files))]
    long_candidates <- long_candidates[!grepl("qa|comparison|dedupe|observations", basename(long_candidates))]

    long_file <- if (preferred %in% dir_files) {
      preferred
    } else if (length(long_candidates)) {
      long_candidates[[1]]
    } else {
      NA_character_
    }
    if (is.na(long_file)) next

    d <- read_gh_csv(long_file, force)
    if (nrow(d) == 0) next
    domain_tables[[domain]] <- d

    sp_col  <- grep("^species$", names(d), ignore.case = TRUE, value = TRUE)
    var_col <- grep("^(variable|measure|standardized_term|canonical_structure)$", names(d), ignore.case = TRUE, value = TRUE)
    val_col <- grep("^(value|gi|mass_g|gli_pct)$", names(d), ignore.case = TRUE, value = TRUE)
    unit_col <- grep("^units?$", names(d), ignore.case = TRUE, value = TRUE)
    src_col <- grep("^(source|teams?|sources)$", names(d), ignore.case = TRUE, value = TRUE)

    comb_df <- data.frame(
      Domain = domain,
      Species = if (length(sp_col)) as.character(d[[sp_col[[1]]]]) else "",
      Variable = if (length(var_col)) as.character(d[[var_col[[1]]]]) else "",
      Value = if (length(val_col)) as.character(d[[val_col[[1]]]]) else "",
      Units = if (length(unit_col)) as.character(d[[unit_col[[1]]]]) else "",
      Source = if (length(src_col)) as.character(d[[src_col[[1]]]]) else "",
      stringsAsFactors = FALSE
    )
    combined_list[[length(combined_list) + 1L]] <- comb_df
  }

  combined_df <- if (length(combined_list)) do.call(rbind, combined_list) else data.frame()

  # Harmonised species name next to the printed one, so the Species filter,
  # Species Coverage and the Region Comparison agree on what one species is.
  species_keys <- load_species_keys(force)
  if (nrow(combined_df)) {
    combined_df$Species_printed <- combined_df$Species
    combined_df$Species <- harmonize_species(combined_df$Species, species_keys)
    combined_df <- combined_df[, c("Domain", "Species", "Variable", "Value", "Units", "Source", "Species_printed")]
  }
  combined_search_blob <- make_fast_search_blob(combined_df)

  anatomy_keys <- load_anatomy_keys(force)
  region_tbl <- tryCatch(
    build_region_table(domain_tables, anatomy_keys, species_keys),
    error = function(e) {
      message("Region table could not be built: ", conditionMessage(e))
      data.frame()
    }
  )

  list(
    domain_tables = domain_tables,
    combined = combined_df,
    search_blob = combined_search_blob,
    species_keys = species_keys,
    anatomy_keys = anatomy_keys,
    region = region_tbl
  )
}

# --------------------------------------------------
# SPECIMEN LOOKUP HELPERS (pure functions over already-loaded data)
# --------------------------------------------------

lookup_specimen_measurements <- function(canonical, p_id = NA, alt_ids = NA, name = NA, paper_tables = list()) {
  res <- list()
  split_alts <- if (!is.na(alt_ids) && is.character(alt_ids) && nzchar(alt_ids)) {
    strsplit(alt_ids, "[; ]+")[[1]]
  } else character()
  all_ids <- unique(c(canonical, p_id, split_alts, name))
  all_ids <- all_ids[!is.na(all_ids) & nzchar(all_ids) & all_ids != "NA"]
  if (!length(all_ids)) return(res)

  if ("MacLeod 2000 (Appendix I - 47 primates)" %in% names(paper_tables)) {
    d <- paper_tables[["MacLeod 2000 (Appendix I - 47 primates)"]]
    mask <- Reduce(`|`, lapply(all_ids, function(id) {
      grepl(id, paste(d$primary_identifier, d$specimen_name, d$alternate_identifiers, d$specimen), fixed = TRUE)
    }))
    if (any(mask)) {
      cols <- intersect(c("primary_identifier", "specimen_name", "species", "brain_weight_g", "fixed_volume_cm3", "body_weight_kg", "collection_source", "cause_of_death"), names(d))
      res[["MacLeod 2000 (Appendix I)"]] <- d[mask, cols, drop = FALSE]
    }
  }

  if ("de Sousa et al. 2010 (Table 1 - Hominoids)" %in% names(paper_tables)) {
    d <- paper_tables[["de Sousa et al. 2010 (Table 1 - Hominoids)"]]
    mask <- Reduce(`|`, lapply(all_ids, function(id) grepl(id, d$code, fixed = TRUE)))
    if (any(mask)) {
      cols <- intersect(c("code", "species", "collection", "brain_mass_g", "brain_volume_cm3", "left_V1_volume_cm3", "left_LGN_volume_cm3", "neocortex_volume_cm3"), names(d))
      res[["de Sousa et al. 2010 (Table 1)"]] <- d[mask, cols, drop = FALSE]
    }
  }

  if ("Smaers 2010 (Table 1 - Stephan specimens via Frahm)" %in% names(paper_tables)) {
    d <- paper_tables[["Smaers 2010 (Table 1 - Stephan specimens via Frahm)"]]
    mask <- Reduce(`|`, lapply(all_ids, function(id) grepl(id, paste(d$specimen_key, d$catalogue_number), fixed = TRUE)))
    if (any(mask)) {
      cols <- intersect(c("specimen_key", "species", "catalogue_number", "total_brain_volume_mm3", "neopallium_volume_mm3", "basal_ganglia_volume_mm3"), names(d))
      res[["Smaers et al. 2010 (Table 1)"]] <- d[mask, cols, drop = FALSE]
    }
  }

  if ("Kochiyama et al. 2018 (Fossil Specimens Text)" %in% names(paper_tables)) {
    d <- paper_tables[["Kochiyama et al. 2018 (Fossil Specimens Text)"]]
    mask <- Reduce(`|`, lapply(all_ids, function(id) grepl(id, d$Specimen, fixed = TRUE)))
    if (any(mask)) {
      cols <- intersect(c("Specimen", "Species", "date_mean_yBP", "Cerebrum_Vol.cc", "Cerebellum_Vol.cc", "Cerebellum_Cerebrum_ratio"), names(d))
      res[["Kochiyama et al. 2018"]] <- d[mask, cols, drop = FALSE]
    }
  }

  if ("Weaver 2001 (Table A-15 Fossils & Extant)" %in% names(paper_tables)) {
    d <- paper_tables[["Weaver 2001 (Table A-15 Fossils & Extant)"]]
    clean_ids <- unique(c(all_ids, sub(" [0-9]+$", "", all_ids)))
    mask <- Reduce(`|`, lapply(clean_ids, function(id) grepl(id, d$Specimen, fixed = TRUE)))
    if (any(mask)) {
      cols <- intersect(c("Specimen", "Group_label", "CBLM_cc", "BoMass_kg", "BrMass_g"), names(d))
      res[["Weaver 2001 (Table A-15)"]] <- d[mask, cols, drop = FALSE]
    }
  }

  if ("Barger et al. 2007 (Table 1 - Ape Amygdala)" %in% names(paper_tables)) {
    d <- paper_tables[["Barger et al. 2007 (Table 1 - Ape Amygdala)"]]
    mask <- Reduce(`|`, lapply(all_ids, function(id) grepl(id, paste(d$specimen_name, d$specimen_id), fixed = TRUE)))
    if (any(mask)) {
      cols <- intersect(c("specimen_name", "specimen_id", "species", "sex", "age", "amygdala_volume_mm3"), names(d))
      res[["Barger et al. 2007 (Table 1)"]] <- d[mask, cols, drop = FALSE]
    }
  }

  res
}

# Which specimen note (dossier) is relevant to a crosswalk row. Two passes:
# (1) a small set of hand-written routing rules for the well-known cases;
# (2) a generic fallback that scans every note's text for the row's canonical
# id, primary identifier, or house name -- so a note added upstream (e.g. the
# green monkey or Sherwood/Karl notes) is surfaced without an app change.
# `notes` is the named list from load_specimen_system(); only notes that
# actually exist are ever returned.
lookup_specimen_dossier <- function(canonical, note_text = "", published_taxon = "", resolved_taxon = "",
                                    primary_id = "", specimen_name = "", notes = list()) {
  available <- names(notes)
  pick <- function(nm) if (!length(available) || nm %in% available) nm else NULL
  blob <- paste(canonical, note_text, published_taxon, resolved_taxon)
  hit <- NULL
  if (grepl("PONGO", canonical, ignore.case = TRUE) || grepl("Pongo", blob, ignore.case = TRUE)) {
    hit <- pick("Pongo_specimen_note.md")
  } else if (grepl("DISCO|5542", canonical, ignore.case = TRUE) || grepl("Disco", blob, ignore.case = TRUE)) {
    hit <- pick("Disco_gibbon_specimen_note.md")
  } else if (canonical %in% c("Cro-Magnon 1", "Qafzeh 9", "Skhul 5", "Mlade\u010d 1", "Amud 1", "La Chapelle-aux-Saints 1", "La Ferrassie 1") ||
      grepl("early Homo sapiens|Neanderthal|fossil hominin", blob, ignore.case = TRUE)) {
    hit <- pick("EarlyHomoSapiens_fossil_vs_extant_specimen_note.md")
  } else if (grepl("Kaas|Young|Collins|11_38|Turner", blob, ignore.case = TRUE)) {
    hit <- pick("Kaas_Young_Collins_specimen_overlap_note.md")
  } else if (grepl("Chlorocebus|green monkey|Glendenning|Nudo", blob, ignore.case = TRUE)) {
    hit <- pick("Green_monkey_FSU_specimen_note.md")
  } else if (grepl("Sherwood|Karl", blob, ignore.case = TRUE)) {
    hit <- pick("Sherwood2020_Karl2024_specimen_note.md")
  }
  if (!is.null(hit)) return(hit)

  # Generic pass: any note that mentions this animal's id or name.
  keys <- unique(c(canonical, primary_id, specimen_name))
  keys <- keys[!is.na(keys) & nzchar(keys) & keys != "NA" & nchar(keys) >= 4]
  if (length(keys) && length(notes)) {
    for (nm in available) {
      txt <- notes[[nm]]
      if (any(vapply(keys, function(k) grepl(k, txt, fixed = TRUE), logical(1)))) return(nm)
    }
  }
  NULL
}

# --------------------------------------------------
# SPECIES NAME HARMONIZATION (shared keys)
# --------------------------------------------------

# Builds one lookup: printed/variant species string -> accepted_name, from
# _keys/species_display_aliases.csv (variant -> canonical) plus the
# specimen_taxon_resolution key. Everything not covered maps to itself.
# Also returns the common-name table from _keys/species_cohorts.csv, used
# only for display ("Mus musculus (House mouse)") -- never for joining.
load_species_keys <- function(force = FALSE) {
  aliases   <- read_gh_csv("_keys/species_display_aliases.csv", force)
  cohorts   <- read_gh_csv("_keys/species_cohorts.csv", force)
  alias_map <- if (nrow(aliases) && all(c("variant", "canonical") %in% names(aliases))) {
    stats::setNames(as.character(aliases$canonical), as.character(aliases$variant))
  } else character()
  common <- if (nrow(cohorts) && all(c("species_sci", "common_name") %in% names(cohorts))) {
    cohorts <- cohorts[!is.na(cohorts$species_sci) & !is.na(cohorts$common_name), ]
    cohorts <- cohorts[!duplicated(cohorts$species_sci), ]
    stats::setNames(as.character(cohorts$common_name), as.character(cohorts$species_sci))
  } else character()
  list(alias_map = alias_map, common_names = common)
}

harmonize_species <- function(x, species_keys) {
  x <- as.character(x)
  out <- x
  if (length(species_keys$alias_map)) {
    hit <- match(x, names(species_keys$alias_map))
    out[!is.na(hit)] <- unname(species_keys$alias_map[hit[!is.na(hit)]])
  }
  out
}

# "Mus musculus (House mouse)" when a common name is known, else the binomial.
species_display_label <- function(x, species_keys) {
  cn <- species_keys$common_names[x]
  ifelse(is.na(cn), x, paste0(x, " (", cn, ")"))
}

# --------------------------------------------------
# REGION-LEVEL TABLE (cross-domain, within-species comparisons)
# --------------------------------------------------

# One long table with one row per (species, domain, brain structure, measure)
# so that two different kinds of measurement of the SAME structures in the
# SAME species can be paired -- e.g. mouse regional glucose metabolism (CMR)
# against mouse regional neuron numbers (cell counts). Structure names are
# harmonised through two upstream keys:
#   _keys/variable_domain.csv   label -> canonical_structure / Measure / Unit
#   _keys/anatomy_crosswalk.csv (domain, term) -> concept, link_class
# A structure with no crosswalk row keeps its own name as the concept, so
# domains that already spell a structure identically still pair up; the
# `link_class` column tells the user whether a pairing is 'direct',
# 'proxy' (overlapping / containing definitions) or 'own name' (unreviewed).
load_anatomy_keys <- function(force = FALSE) {
  list(
    variable_domain   = read_gh_csv("_keys/variable_domain.csv", force),
    anatomy_crosswalk = read_gh_csv("_keys/anatomy_crosswalk.csv", force),
    variable_defs     = read_gh_csv("_keys/variable_definitions.csv", force)
  )
}

build_region_table <- function(domain_tables, anatomy_keys, species_keys) {
  vd <- anatomy_keys$variable_domain
  ax <- anatomy_keys$anatomy_crosswalk
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))

  # label -> (canonical_structure, Measure, Unit) within one variable_domain dataset
  vd_lookup <- function(dataset_name, labels) {
    if (!nrow(vd)) return(data.frame(structure = NA_character_, measure = NA_character_, unit = NA_character_)[rep(1, length(labels)), ])
    sub <- vd[vd$dataset %in% dataset_name, ]
    sub <- sub[!duplicated(sub$label), ]
    i <- match(labels, sub$label)
    data.frame(structure = sub$canonical_structure[i], measure = sub$Measure[i], unit = sub$Unit[i], stringsAsFactors = FALSE)
  }

  # Species name: the merge's own accepted_name column where it exists (the
  # upstream resolver has already applied the species key), else Species.
  sp <- function(d) {
    s <- as.character(d$Species)
    if ("accepted_name" %in% names(d)) {
      a <- as.character(d$accepted_name)
      s <- ifelse(!is.na(a) & nzchar(a), a, s)
    }
    s
  }

  pieces <- list()
  add_piece <- function(domain, species, structure, term, measure, unit, value, source, n = NA) {
    if (!length(value)) return(invisible(NULL))
    pieces[[length(pieces) + 1L]] <<- data.frame(
      Domain = domain,
      Species_printed = as.character(species),
      Structure = as.character(structure),
      Term = as.character(term),
      Measure = as.character(measure),
      Unit = as.character(unit),
      Value = num(value),
      Source = as.character(source),
      n = as.character(n),
      stringsAsFactors = FALSE
    )
  }

  d <- domain_tables[["cellcounts"]]
  if (!is.null(d) && nrow(d)) {
    lk <- vd_lookup("Cell counts", d$Variable)
    add_piece("cellcounts", sp(d), lk$structure, d$Variable, lk$measure, lk$unit, d$Value, d$Source)
  }
  d <- domain_tables[["volumes"]]
  if (!is.null(d) && nrow(d)) {
    lk <- vd_lookup("Brain-structure volumes", d$Variable)
    # Structure comes from the label itself (everything before "_Vol.mm3" /
    # "_Mass.mg"), not from variable_domain's canonical_structure: the key
    # folds Cerebellum_grey_matter and Cerebellum_white_matter into
    # "Cerebellum", which would put three different volumes under one
    # (structure, measure) cell. The label keeps them apart.
    structure <- sub("_(Vol|Mass)[.][A-Za-z0-9]+$", "", d$Variable)
    measure <- ifelse(is.na(lk$measure), sub("^.*_((Vol|Mass)[.][A-Za-z0-9]+)$", "\\1", d$Variable), lk$measure)
    unit <- ifelse(is.na(lk$unit), sub("^.*[.]", "", d$Variable), lk$unit)
    add_piece("volumes", sp(d), structure, d$Variable, measure, unit, d$Value, d$Sources, d$n_sources)
  }
  d <- domain_tables[["cerebral_metabolic_rate"]]
  if (!is.null(d) && nrow(d)) {
    src <- if ("Primary_references" %in% names(d)) d$Primary_references else d$Compilations
    add_piece("CMR", sp(d), d$Region, d$Region, d$Measure, d$Units, d$Value, src, d$n_studies)
  }
  d <- domain_tables[["weights"]]
  if (!is.null(d) && nrow(d) && "canonical_structure" %in% names(d)) {
    add_piece("weights", sp(d), d$canonical_structure, d$canonical_structure, "Mass.g", "g", d$mass_g, d$source, d$n)
  }
  d <- domain_tables[["GLI"]]
  if (!is.null(d) && nrow(d) && all(c("area_as_published", "stratum", "GLI_pct") %in% names(d))) {
    add_piece("GLI", sp(d), d$area_as_published, d$area_as_published, paste0("GLI_", d$stratum), "%", d$GLI_pct, d$source, d$n_specimens)
  }
  d <- domain_tables[["cortical_areas"]]
  if (!is.null(d) && nrow(d)) {
    term <- d$Standardized_Term
    structure <- sub("_(Surface_Area|Area|Thickness).*$", "", term)
    measure <- ifelse(grepl("_", term), sub("^[^_]+_", "", term), term)
    unit <- ifelse(grepl("[.]mm2$", term), "mm2", ifelse(grepl("[.]mm$", term), "mm", ifelse(grepl("pct", term), "%", "count")))
    add_piece("cortical_areas", sp(d), structure, term, measure, unit, d$value, d$source, d$n)
  }
  d <- domain_tables[["cell_morphology"]]
  if (!is.null(d) && nrow(d) && all(c("region", "measure", "value") %in% names(d))) {
    meas <- paste(d$measure, ifelse(is.na(d$cell_type), "", d$cell_type), ifelse(is.na(d$layer), "", paste0("L", d$layer)))
    meas <- trimws(gsub("[[:space:]]+", " ", meas))
    add_piece("cell_morphology", sp(d), d$region, d$region, meas, d$unit, d$value, d$source, d$n)
  }
  d <- domain_tables[["cortical_layers"]]
  if (!is.null(d) && nrow(d) && all(c("region", "layer", "measure", "value") %in% names(d))) {
    add_piece("cortical_layers", sp(d), d$region, d$region, paste0(d$measure, " L", d$layer), d$unit, d$value, d$source, d$n_specimens)
  }

  if (!length(pieces)) return(data.frame())
  out <- do.call(rbind, pieces)
  out <- out[!is.na(out$Value) & !is.na(out$Structure) & nzchar(out$Structure), ]
  out$Species <- harmonize_species(out$Species_printed, species_keys)

  # Concept + link class from the anatomy crosswalk. Domain names in the
  # crosswalk: volumes, cellcounts, CMR, weights, cortical_areas, GLI,
  # cortical_layers. A term may link to several concepts (Striatum ->
  # Striatum / Caudate / Putamen / Basal_ganglia); keep every link as its own
  # row so each concept sees it, and tag the link class.
  if (nrow(ax)) {
    ax_dom <- ax$domain
    ax_term <- as.character(ax$term)
    ax_key <- paste(ax_dom, ax_term, sep = "|")
    # cellcounts / weights / CMR link by structure name; volumes / cortical_areas by variable label
    row_key_term <- paste(out$Domain, out$Term, sep = "|")
    row_key_struct <- paste(out$Domain, out$Structure, sep = "|")
    linked <- list()
    for (k in seq_len(nrow(ax))) {
      m <- which(row_key_term == ax_key[k] | row_key_struct == ax_key[k])
      if (!length(m)) next
      sub <- out[m, ]
      sub$Concept <- ax$concept[k]
      sub$link_class <- ax$link_class[k]
      sub$link_relation <- ax$relation[k]
      linked[[length(linked) + 1L]] <- sub
    }
    linked_df <- if (length(linked)) do.call(rbind, linked) else out[0, ]
    has_link <- row_key_term %in% ax_key | row_key_struct %in% ax_key
    own <- out[!has_link, ]
    if (nrow(own)) {
      own$Concept <- own$Structure
      own$link_class <- "own name"
      own$link_relation <- "same (name not yet reviewed in anatomy_crosswalk)"
    }
    out <- rbind(linked_df, own)
  } else {
    out$Concept <- out$Structure
    out$link_class <- "own name"
    out$link_relation <- NA_character_
  }
  rownames(out) <- NULL
  # A readable variable label used in the pickers: "CMR: CMRgl (umol/100g/min)"
  out$VariableKey <- paste0(out$Domain, ": ", out$Measure, ifelse(is.na(out$Unit) | !nzchar(out$Unit), "", paste0(" (", out$Unit, ")")))
  out
}

# --------------------------------------------------
# TOP-LEVEL LOAD
# --------------------------------------------------

# Runs the whole GitHub-only pipeline. force_tree re-lists the repo (picks up
# added/removed/renamed files); force_content also re-downloads every fixed
# file load_specimen_system()/load_trait_data() read (crosswalks, paper
# tables, the ~17 per-domain _long.csv merges, markdown notes) -- a few dozen
# small requests, fast enough to run on every refresh. It does NOT bulk
# re-fetch the Repository Explorer's file contents; those are pulled lazily,
# one at a time, only when a user clicks Download.
load_all_data <- function(force_tree = FALSE, force_content = FALSE) {
  tree <- gh_list_tree(force = force_tree)
  list(
    tree = tree,
    index_tbl = build_index(tree),
    specimen_data = load_specimen_system(tree, force = force_content),
    trait_data = load_trait_data(tree, force = force_content),
    loaded_at = Sys.time()
  )
}
