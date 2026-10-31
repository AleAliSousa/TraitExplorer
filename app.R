# TraitExplorer
# Shiny interface for exploring the Evo-M1-Trait-Data repository,
# with first-class specimen tracking and comparative trait search.
#
# Data access: TraitExplorer never reads a local Evo-M1-Trait-Data checkout.
# Every table is fetched from GitHub (see data_layer.R) and cached under
# .gh_cache/ next to this file. Click "Refresh data from GitHub" in the app,
# or run `Rscript refresh_cache.R`, to pick up dataset changes.

required_packages <- c("shiny", "DT", "dplyr", "stringr", "plotly")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(
    "Please install the missing R package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(plotly)

# Safe fallback for null coalescence
`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x

# A small number of rows in body_ecology / brain_mass carry a bare specimen
# ID (e.g. "1", "204") in the Species column instead of a taxon name --
# a data-layer issue upstream, not something to guess-fix here. Drop those
# tokens from the species picker so they don't clutter the top of the list
# (numbers sort before letters); the underlying rows are untouched.
species_choices_from <- function(species_vec) {
  sp <- unique(species_vec)
  sp <- sp[!is.na(sp) & nzchar(sp) & !grepl("^[0-9]+$", sp)]
  sort(sp)
}

app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(app_file) || !nzchar(app_file)) app_file <- "app.R"
app_dir <- dirname(normalizePath(app_file, winslash = "/", mustWork = FALSE))
config_path <- file.path(app_dir, "config.R")
if (file.exists(config_path)) source(config_path, local = TRUE)

# local = TRUE is load-bearing: Shiny sources app.R into a private per-app
# environment (not globalenv), so data_layer.R must be sourced into THAT
# same environment -- otherwise its assignments (CACHE_DIR, GITHUB_OWNER,
# ...) land in globalenv where app_dir (defined above, in the private env)
# isn't visible, and CACHE_DIR's `file.path(app_dir, ...)` default errors
# with "object 'app_dir' not found".
source(file.path(app_dir, "data_layer.R"), local = TRUE)

# --------------------------------------------------
# INITIALIZE DATA
# --------------------------------------------------

initial_data <- load_all_data()

message(
  "TraitExplorer ready (GitHub-only: ", GITHUB_OWNER, "/", GITHUB_REPO, "@", GITHUB_BRANCH, "):\n",
  "  Cache dir: ", CACHE_DIR, "\n",
  "  Files indexed: ", nrow(initial_data$index_tbl), "\n",
  "  Specimen records: ", nrow(initial_data$specimen_data$crosswalk), "\n",
  "  Trait records: ", nrow(initial_data$trait_data$combined), " across ",
  length(initial_data$trait_data$domain_tables), " domains"
)

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(
  titlePanel(
    div(
      span("TraitExplorer", style = "font-weight: 700; color: #1a365d;"),
      span(" | Specimen & Comparative Trait Browser", style = "font-size: 0.6em; color: #718096; font-weight: 400;")
    ),
    windowTitle = "TraitExplorer"
  ),

  tags$head(
    tags$style(HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; background-color: #f8fafc; }
      .specimen-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 18px; margin-top: 18px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); }
      .status-box { padding: 10px 16px; border: 1px solid #cbd5e1; border-radius: 6px; background: #ffffff; margin-bottom: 12px; }
      .badge-fossil { background-color: #b91c1c; color: white; padding: 3px 8px; border-radius: 4px; font-size: 0.82em; font-weight: 600; }
      .badge-bio { background-color: #1d4ed8; color: white; padding: 3px 8px; border-radius: 4px; font-size: 0.82em; font-weight: 600; }
      .badge-invivo { background-color: #047857; color: white; padding: 3px 8px; border-radius: 4px; font-size: 0.82em; font-weight: 600; }
      .badge-matched { background-color: #10b981; color: white; padding: 2px 7px; border-radius: 4px; font-size: 0.8em; }
      .badge-probable { background-color: #f59e0b; color: white; padding: 2px 7px; border-radius: 4px; font-size: 0.8em; }
      .conflict-alert { background-color: #fef3c7; color: #92400e; border: 1px solid #fde68a; border-radius: 6px; padding: 12px 16px; margin-bottom: 14px; }
      .rule-box { background-color: #eff6ff; color: #1e40af; border: 1px solid #bfdbfe; border-radius: 6px; padding: 12px 16px; margin-bottom: 14px; }
      .detail-header { font-size: 1.2em; font-weight: 700; color: #1e293b; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; margin-bottom: 12px; }
      .prop-label { font-weight: 600; color: #64748b; min-width: 150px; display: inline-block; }
      .small-note { color: #64748b; font-size: 0.88em; }
      .dossier-box { max-height: 500px; overflow-y: auto; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 6px; padding: 18px; line-height: 1.5; }
      .nav-tabs > li > a { font-weight: 600; }
      .btn-download { margin-top: 6px; margin-bottom: 6px; }
      .home-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px; min-height: 190px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
      .stat-pill { display: inline-block; background: #eff6ff; color: #1e3a8a; border-radius: 999px; padding: 4px 12px; margin: 3px 6px 3px 0; font-weight: 600; }
    "))
  ),

  fluidRow(
    column(
      12,
      div(
        style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
        actionButton("refresh_data", "Refresh data from GitHub", class = "btn btn-sm btn-outline-primary"),
        uiOutput("data_status", inline = TRUE)
      )
    )
  ),

  tabsetPanel(
    id = "main_tabs",

    # ========================================================
    # TAB 0: START HERE (orientation + guided examples)
    # ========================================================
    tabPanel(
      "Start Here",
      br(),
      div(style = "max-width: 1000px;",
        div(class = "status-box",
          h4("What is in this database right now?", style = "margin-top: 0; font-weight: 700;"),
          uiOutput("home_counts")
        ),
        fluidRow(
          column(4, div(class = "home-card",
            h5("Find a specimen", style = "font-weight: 700;"),
            p(class = "small-note", "One physical brain across several papers, under several names. Search by nickname, catalogue number, collection, or species."),
            actionButton("ex_specimen", "Example: gibbon 'Disco'", class = "btn btn-sm btn-outline-primary")
          )),
          column(4, div(class = "home-card",
            h5("Search a trait value", style = "font-weight: 700;"),
            p(class = "small-note", "Every compiled value, every domain, one search box. Type several words to narrow down (all must match)."),
            actionButton("ex_trait", "Example: chimpanzee cerebellum volume", class = "btn btn-sm btn-outline-primary")
          )),
          column(4, div(class = "home-card",
            h5("Compare brain regions within a species", style = "font-weight: 700;"),
            p(class = "small-note", "Pair two kinds of measurement on the same brain regions -- e.g. mouse regional glucose metabolism against neuron numbers -- with a regression."),
            actionButton("ex_region", "Example: mouse CMRgl vs neurons", class = "btn btn-sm btn-outline-primary")
          ))
        ),
        br(),
        h5("The tabs, in one line each", style = "font-weight: 700;"),
        tags$ul(
          tags$li(strong("Specimen Search"), " -- identity of individual brains: who holds them, what each paper called them, which species they resolve to today."),
          tags$li(strong("Trait Search"), " -- look up compiled species-level values (volumes, cell counts, metabolic rates, behaviour, sleep ...). Click a row to see every field."),
          tags$li(strong("Species Coverage"), " -- for one species, how much of each domain is filled in."),
          tags$li(strong("Region Comparison"), " -- within one species, plot one regional measurement against another across brain regions; or for one region, across species."),
          tags$li(strong("Repository Explorer"), " -- browse the source files behind the numbers and open them on GitHub.")
        ),
        div(class = "rule-box",
          strong("Reading the numbers: "),
          "Values are compiled species means from the repository's harmonised merges; they may not include every source-level measurement in the repository. ",
          "Species names are harmonised through the repository's species key; specimen taxa show both the name as resolved for that individual and the species-level accepted name."
        )
      )
    ),

    # ========================================================
    # TAB 1: SPECIMEN SEARCH (PRIMARY & NEW)
    # ========================================================
    tabPanel(
      "Specimen Search",
      br(),
      tabsetPanel(
        id = "specimen_subtabs",
        type = "pills",

        # Sub-tab 1: Specimen Explorer & Crosswalk
        tabPanel(
          "Specimen Explorer",
          br(),
          sidebarLayout(
            sidebarPanel(
              textInput(
                "specimen_search",
                "Search specimens",
                placeholder = "Harry, Disco, YN85-38, Cro-Magnon, Zilles, Pongo..."
              ),
              selectInput("spec_filter_kind", "Specimen Kind", choices = "All"),
              selectInput("spec_filter_taxon", "Resolved Taxon / Species", choices = "All"),
              selectInput("spec_filter_collection", "Collection group", choices = "All"),
              selectInput("spec_filter_pub", "Source Publication", choices = "All"),
              selectInput("spec_filter_match", "Match Status", choices = "All"),
              selectInput("spec_filter_conflict", "Taxon Conflict", choices = c("All", "Has taxon conflict", "No conflict")),
              actionButton("reset_spec_filters", "Reset filters", class = "btn btn-sm btn-outline-secondary"),
              hr(),
              downloadButton("download_specimens", "Download filtered specimens (CSV)", class = "btn btn-sm btn-primary btn-download"),
              width = 3
            ),
            mainPanel(
              div(class = "status-box", uiOutput("specimen_status")),
              DTOutput("specimen_table"),
              uiOutput("specimen_inspector"),
              width = 9
            )
          )
        ),

        # Sub-tab 2: Fossil Comparisons
        tabPanel(
          "Fossil Comparisons",
          br(),
          div(class = "status-box",
            h4("Fossil Specimen Reconstructions & Cross-Method Comparison", style = "margin-top: 0; font-weight: 700;"),
            p("Comparison between Kochiyama et al. 2018 (GM+WM deformed onto endocast) and Weaver 2001 (virtual endocast)."),
            div(class = "conflict-alert",
              strong("Consumer Rule: "), "Fossil specimens carry ", code("specimen_kind = fossil_specimen"),
              " and must NEVER be pooled automatically into the extant ", em("Homo sapiens"), " species mean."
            )
          ),
          DTOutput("fossil_comparison_table"),
          br(),
          h5("All Published Fossil Specimen Crosswalk Records", style = "font-weight: 700;"),
          DTOutput("fossil_crosswalk_table")
        ),

        # Sub-tab 3: Published Specimen Tables
        tabPanel(
          "Published Specimen Tables",
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "paper_spec_table_select",
                "Select Specimen Table:",
                choices = names(initial_data$specimen_data$paper_tables)
              ),
              textInput("paper_spec_search", "Search within table:", placeholder = "species, code, measurement..."),
              downloadButton("download_paper_spec_table", "Download table (CSV)", class = "btn btn-sm btn-primary btn-download"),
              width = 3
            ),
            mainPanel(
              div(class = "status-box", uiOutput("paper_spec_status")),
              DTOutput("paper_spec_table"),
              width = 9
            )
          )
        ),

        # Sub-tab 4: Taxon Concepts & Collections
        tabPanel(
          "Taxon Concepts & Registries",
          br(),
          tabsetPanel(
            tabPanel(
              "Taxon Concepts",
              br(),
              div(class = "rule-box",
                strong("Taxon Concepts vs Specimens: "),
                "One individual with many labels is resolved at specimen level. One broad label pooling many individuals (e.g. pre-2001 ",
                em("Pongo pygmaeus s.l."), ") cannot be un-averaged and is pinned to its broad concept."
              ),
              DTOutput("taxon_concepts_table")
            ),
            tabPanel(
              "Collections Registry",
              br(),
              p("Documented specimen collections and institutions contributing physical material."),
              DTOutput("collections_table")
            ),
            tabPanel(
              "Evidence Sources",
              br(),
              p("Evidence sources registered for specimen identity claims, access classes, and repository locations."),
              DTOutput("sources_table")
            )
          )
        ),

        # Sub-tab 5: Specimen Dossiers & Notes
        tabPanel(
          "Specimen Dossiers",
          br(),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "dossier_select",
                "Select Dossier / Note:",
                choices = names(initial_data$specimen_data$notes)
              ),
              p(class = "small-note", "Detailed notes documenting specimen identities, overlaps, taxonomic histories, and data integrity boundaries."),
              width = 3
            ),
            mainPanel(
              div(class = "dossier-box", uiOutput("dossier_content")),
              width = 9
            )
          )
        )
      )
    ),

    # ========================================================
    # TAB 2: TRAIT SEARCH (HARMONIZED MERGES)
    # ========================================================
    tabPanel(
      "Trait Search",
      br(),
      sidebarLayout(
        sidebarPanel(
          textInput(
            "trait_search",
            "Search traits / records",
            placeholder = "Pan troglodytes sleep M1 cerebellum..."
          ),
          selectInput(
            "trait_domain",
            "Domain / Merge",
            choices = c("All Domains (Combined)", sort(names(initial_data$trait_data$domain_tables)))
          ),
          selectInput("trait_column", "Search within", choices = "All fields"),
          numericInput("trait_limit", "Maximum rows", value = 1000, min = 50, max = 50000, step = 50),
          actionButton("reset_trait_filters", "Reset filters", class = "btn btn-sm btn-outline-secondary"),
          hr(),
          downloadButton("download_traits", "Download search results (CSV)", class = "btn btn-sm btn-primary btn-download"),
          width = 3
        ),
        mainPanel(
          div(class = "status-box", uiOutput("trait_status")),
          DTOutput("trait_table"),
          br(),
          uiOutput("trait_detail_card"),
          width = 9
        )
      )
    ),

    # ========================================================
    # TAB 3: SPECIES DATA COVERAGE
    # ========================================================
    tabPanel(
      "Species Coverage",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            "coverage_species",
            "Species",
            choices = NULL,
            options = list(placeholder = "Start typing a species or common name", maxOptions = 50)
          ),
          p(class = "small-note",
            "Counts distinct variables (not records) recorded for this species in each harmonized domain. Species names are harmonised through the repository's species alias key (_keys/species_display_aliases.csv); a spelling that key does not yet cover still appears under its printed form."),
          width = 3
        ),
        mainPanel(
          div(class = "status-box", uiOutput("coverage_status")),
          plotly::plotlyOutput("coverage_pie", height = "420px"),
          br(),
          DTOutput("coverage_table"),
          width = 9
        )
      )
    ),


    # ========================================================
    # TAB 3b: REGION COMPARISON (cross-domain, within species)
    # ========================================================
    tabPanel(
      "Region Comparison",
      br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons("rc_mode", "Points are",
            choices = c("Brain regions within one species" = "regions", "Species, for one brain region" = "species"),
            selected = "regions"),
          conditionalPanel("input.rc_mode == 'regions'",
            selectizeInput("rc_species", "Species", choices = NULL,
              options = list(placeholder = "Start typing a species or common name"))
          ),
          conditionalPanel("input.rc_mode == 'species'",
            selectizeInput("rc_region", "Brain region (concept)", choices = NULL,
              options = list(placeholder = "Start typing a region"))
          ),
          selectizeInput("rc_x", "X variable", choices = NULL, options = list(placeholder = "domain: measure (unit)")),
          selectizeInput("rc_y", "Y variable", choices = NULL, options = list(placeholder = "domain: measure (unit)")),
          checkboxInput("rc_log", "log10 axes (and log-log regression)", TRUE),
          checkboxInput("rc_proxy", "Allow proxy links (overlapping / containing structure definitions)", TRUE),
          checkboxInput("rc_labels", "Label points", TRUE),
          p(class = "small-note",
            "Variables are listed only if the selected species (or region) has them in at least two brain regions (or species). ",
            "Regions from different domains are paired through _keys/anatomy_crosswalk.csv; 'direct' = same structure, 'proxy' = overlapping definition, 'own name' = identical name, not yet reviewed."),
          hr(),
          downloadButton("rc_download", "Download paired data (CSV)", class = "btn btn-sm btn-primary btn-download"),
          width = 3
        ),
        mainPanel(
          div(class = "status-box", uiOutput("rc_status")),
          plotly::plotlyOutput("rc_plot", height = "520px"),
          br(),
          h5("Paired observations", style = "font-weight: 700;"),
          DTOutput("rc_table"),
          br(),
          uiOutput("rc_unpaired"),
          width = 9
        )
      )
    ),
    # ========================================================
    # TAB 4: REPOSITORY EXPLORER
    # ========================================================
    tabPanel(
      "Repository Explorer",
      br(),
      sidebarLayout(
        sidebarPanel(
          textInput("file_search", "Search files", placeholder = "species, Stephan, TableS1, brain..."),
          selectInput("file_extension", "File type", choices = "All"),
          selectInput("file_year", "Year", choices = "All"),
          checkboxInput("file_papers_only", "Paper folders only", FALSE),
          checkboxInput("file_public_only", "Public/comparative data only", FALSE),
          actionButton("reset_file_filters", "Reset filters", class = "btn btn-sm btn-outline-secondary"),
          hr(),
          downloadButton("download_selected_file", "Download selected file", class = "btn btn-sm btn-primary btn-download"),
          width = 3
        ),
        mainPanel(
          div(class = "status-box", uiOutput("repo_status")),
          DTOutput("file_table"),
          br(),
          verbatimTextOutput("file_details"),
          width = 9
        )
      )
    ),

    # ========================================================
    # TAB 5: ABOUT
    # ========================================================
    tabPanel(
      "About",
      br(),
      div(style = "max-width: 850px;",
        h3("TraitExplorer"),
        p("Interactive browser for the Evo-M1-Trait-Data comparative trait database and harmonized specimen crosswalk."),
        p(strong("Data source: "), sprintf("github.com/%s/%s@%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH)),
        p(class = "small-note", "TraitExplorer reads only from that GitHub repository -- never from a local checkout. Every table is fetched over HTTPS and cached under .gh_cache/ next to the app for speed; it is read-only over the source data."),
        p(class = "small-note", uiOutput("about_loaded_at", inline = TRUE)),
        hr(),
        h4("Specimen Information Architecture"),
        p("TraitExplorer implements the Evo-M1 specimen identity model resolving two distinct data-integrity problems:"),
        tags$ol(
          tags$li(strong("One individual, many labels: "), "Resolvable at the specimen level (e.g. gibbon Disco / GPZ-5542 or orangutan YN85-38). A single physical individual tracked across publications under conflicting labels is given one stable canonical ID and resolved with evidence."),
          tags$li(strong("One label, many individuals: "), "Taxon concepts pooling multiple animals (such as pre-2001 Pongo pygmaeus s.l. pooling Bornean and Sumatran animals). A pooled mean under a broad concept cannot be un-averaged and is pinned to its concept.")
        ),
        h4("Fossil Hominin Rule"),
        p("Fossil specimens (such as Late Pleistocene early Homo sapiens and Neanderthals) carry ", code("specimen_kind = fossil_specimen"), " and must never be pooled into extant species means."),
        h4("Search Behavior"),
        p("All search boxes perform multi-term matching (AND logic): entering 'Pan troglodytes sleep' returns records that match all terms across all fields.")
      )
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    index_tbl = initial_data$index_tbl,
    specimen_data = initial_data$specimen_data,
    trait_data = initial_data$trait_data,
    loaded_at = initial_data$loaded_at
  )

  # Repopulates every dropdown that depends on loaded data. Called once at
  # startup and again after a refresh, so a data-shape change (a removed
  # collection, a new domain) is reflected without restarting the app.
  sync_choices <- function() {
    if (nrow(rv$specimen_data$crosswalk)) {
      updateSelectInput(session, "spec_filter_kind",
        choices = c("All", sort(unique(na.omit(rv$specimen_data$crosswalk$specimen_kind)))))
      # Species choices use the harmonised accepted_name (one animal, one
      # name: 'Rattus norwegicus' and 'Rattus norvegicus' are the same entry)
      # plus the published binomials, so an old label can still be chosen.
      updateSelectInput(session, "spec_filter_taxon",
        choices = c("All", sort(unique(c(na.omit(rv$specimen_data$crosswalk$accepted_name), na.omit(rv$specimen_data$crosswalk$resolved_taxon), na.omit(rv$specimen_data$crosswalk$published_taxon))))))
      updateSelectInput(session, "spec_filter_collection",
        choices = c("All", sort(unique(na.omit(rv$specimen_data$crosswalk$collection_group)))))
      updateSelectInput(session, "spec_filter_pub",
        choices = c("All", sort(unique(na.omit(rv$specimen_data$crosswalk$source_publication)))))
      updateSelectInput(session, "spec_filter_match",
        choices = c("All", sort(unique(na.omit(rv$specimen_data$crosswalk$match)))))
    }

    updateSelectInput(session, "paper_spec_table_select",
      choices = names(rv$specimen_data$paper_tables))
    updateSelectInput(session, "dossier_select",
      choices = names(rv$specimen_data$notes))
    updateSelectInput(session, "trait_domain",
      choices = c("All Domains (Combined)", sort(names(rv$trait_data$domain_tables))))

    updateSelectInput(session, "file_extension",
      choices = c("All", sort(unique(rv$index_tbl$extension[rv$index_tbl$extension != ""]))))
    updateSelectInput(session, "file_year",
      choices = c("All", sort(unique(na.omit(rv$index_tbl$year)), decreasing = TRUE)))

    species_choices <- species_choices_from(rv$trait_data$combined$Species)
    keep_selected <- if (isTRUE(input$coverage_species %in% species_choices)) input$coverage_species else ""
    sp_labels <- species_display_label(species_choices, rv$trait_data$species_keys)
    updateSelectizeInput(session, "coverage_species",
      choices = c("", stats::setNames(species_choices, sp_labels)), selected = keep_selected, server = TRUE)
  }
  # This first call runs at server startup, before any reactive consumer
  # (observer/reactive/render) exists -- reading a reactiveValues field
  # there needs isolate(). The call inside the refresh observeEvent() below
  # is already inside a reactive consumer and needs no isolate().
  isolate(sync_choices())

  observeEvent(input$refresh_data, {
    withProgress(message = "Refreshing from GitHub...", {
      fresh <- load_all_data(force_tree = TRUE, force_content = TRUE)
      rv$index_tbl <- fresh$index_tbl
      rv$specimen_data <- fresh$specimen_data
      rv$trait_data <- fresh$trait_data
      rv$loaded_at <- fresh$loaded_at
    })
    sync_choices()
  })

  output$data_status <- renderUI({
    tagList(
      tags$span(
        class = "small-note",
        sprintf("github.com/%s/%s@%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH),
        " | last loaded: ", format(rv$loaded_at, "%Y-%m-%d %H:%M:%S"),
        " | ", format(nrow(rv$index_tbl), big.mark = ","), " files, ",
        format(nrow(rv$specimen_data$crosswalk), big.mark = ","), " specimen records, ",
        format(nrow(rv$trait_data$combined), big.mark = ","), " trait records"
      )
    )
  })

  output$about_loaded_at <- renderUI({
    paste0("Last loaded from GitHub: ", format(rv$loaded_at, "%Y-%m-%d %H:%M:%S"))
  })

  observeEvent(input$reset_spec_filters, {
    updateTextInput(session, "specimen_search", value = "")
    updateSelectInput(session, "spec_filter_kind", selected = "All")
    updateSelectInput(session, "spec_filter_taxon", selected = "All")
    updateSelectInput(session, "spec_filter_collection", selected = "All")
    updateSelectInput(session, "spec_filter_pub", selected = "All")
    updateSelectInput(session, "spec_filter_match", selected = "All")
    updateSelectInput(session, "spec_filter_conflict", selected = "All")
  })

  # Filtered specimen crosswalk
  filtered_specimens <- reactive({
    dat <- rv$specimen_data$crosswalk
    if (!nrow(dat)) return(dat)

    # Note: `col %in% x` (not `col == x`) throughout -- `==` against a column
    # that has NA in OTHER rows returns NA for those rows, and R's row
    # indexing keeps an NA-indexed row as a spurious all-NA row rather than
    # dropping it. `%in%` treats NA as simply not matching, which is what a
    # dropdown filter should do.
    f_kind <- input$spec_filter_kind %||% "All"
    if (f_kind != "All") {
      dat <- dat[dat$specimen_kind %in% f_kind, , drop = FALSE]
    }
    f_taxon <- input$spec_filter_taxon %||% "All"
    if (f_taxon != "All") {
      dat <- dat[dat$accepted_name %in% f_taxon | dat$resolved_taxon %in% f_taxon | dat$published_taxon %in% f_taxon, , drop = FALSE]
    }
    f_coll <- input$spec_filter_collection %||% "All"
    if (f_coll != "All") {
      dat <- dat[dat$collection_group %in% f_coll, , drop = FALSE]
    }
    f_pub <- input$spec_filter_pub %||% "All"
    if (f_pub != "All") {
      dat <- dat[dat$source_publication %in% f_pub, , drop = FALSE]
    }
    f_match <- input$spec_filter_match %||% "All"
    if (f_match != "All") {
      dat <- dat[dat$match %in% f_match, , drop = FALSE]
    }
    # isTRUE() only ever evaluates a single value; called on a whole column it
    # silently collapsed to one FALSE for every row, so this filter never
    # actually filtered. `%in% TRUE` checks each row and treats NA as "no".
    f_conflict <- input$spec_filter_conflict %||% "All"
    if (f_conflict == "Has taxon conflict") {
      dat <- dat[as.logical(dat$taxon_conflict) %in% TRUE, , drop = FALSE]
    } else if (f_conflict == "No conflict") {
      dat <- dat[!(as.logical(dat$taxon_conflict) %in% TRUE), , drop = FALSE]
    }

    query <- str_squish(input$specimen_search %||% "")
    if (nzchar(query)) {
      row_indices <- match(rownames(dat), rownames(rv$specimen_data$crosswalk))
      sub_blob <- rv$specimen_data$search_blob[row_indices]
      keep <- match_terms(sub_blob, query)
      dat <- dat[keep, , drop = FALSE]
    }

    dat
  })

  output$specimen_status <- renderUI({
    dat <- filtered_specimens()
    n_unique <- length(unique(dat$canonical_specimen))
    tagList(
      strong(format(nrow(dat), big.mark = ",")),
      " matching specimen records",
      span(paste0(" (representing ", n_unique, " unique canonical individuals)")),
      if (length(unique(dat$canonical_specimen)) < nrow(rv$specimen_data$crosswalk)) {
        span(class = "small-note", " | Select a row in the table to inspect details, cross-study tracking, and measurements.")
      }
    )
  })

  output$specimen_table <- renderDT({
    dat <- filtered_specimens()
    if (!nrow(dat)) {
      return(datatable(data.frame(Message = "No matching specimens found.")))
    }

    display_df <- dat %>%
      transmute(
        `Canonical ID` = canonical_specimen,
        `Name / Label` = specimen_name,
        `Primary ID` = primary_identifier,
        `Alternate IDs` = alternate_identifiers,
        `Species (accepted)` = accepted_name,
        `Resolved Taxon` = resolved_taxon,
        `Published Taxon` = published_taxon,
        `Concept` = taxon_concept,
        `Kind` = specimen_kind,
        `Collection Group` = collection_group,
        `Collection (as printed)` = collection,
        `Origin / Qualifier` = collection_qualifier,
        `Publication` = source_publication,
        `Match` = match,
        `Sex` = sex
      )

    datatable(
      display_df,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        searchHighlight = TRUE
      )
    )
  })

  # Selected specimen
  selected_specimen_row <- reactive({
    idx <- input$specimen_table_rows_selected
    dat <- filtered_specimens()
    if (!length(idx) || !nrow(dat) || idx > nrow(dat)) return(NULL)
    dat[idx, , drop = FALSE]
  })

  # Specimen Inspector Panel
  output$specimen_inspector <- renderUI({
    sel <- selected_specimen_row()
    if (is.null(sel)) {
      return(div(
        class = "specimen-card",
        p(class = "small-note", "Click on any specimen row in the table above to open its comprehensive identity inspector, cross-study tracking, published measurements, and dossier notes.")
      ))
    }

    canon <- sel$canonical_specimen
    all_recs <- rv$specimen_data$crosswalk[rv$specimen_data$crosswalk$canonical_specimen == canon, , drop = FALSE]

    # Look up taxon concept
    tc_row <- rv$specimen_data$taxon_concepts[rv$specimen_data$taxon_concepts$taxon_concept == sel$taxon_concept, , drop = FALSE]

    # Look up evidence sources
    ev_ids <- strsplit(sel$evidence_source_ids %||% "", "[; ]+")[[1]]
    ev_ids <- ev_ids[nzchar(ev_ids)]
    ev_df <- rv$specimen_data$sources[rv$specimen_data$sources$source_id %in% ev_ids, , drop = FALSE]

    # Look up physical measurements
    measurements <- lookup_specimen_measurements(
      canon, sel$primary_identifier, sel$alternate_identifiers, sel$specimen_name,
      paper_tables = rv$specimen_data$paper_tables
    )

    # Look up related dossier note
    dossier_name <- lookup_specimen_dossier(
      canon, sel$note, sel$published_taxon, sel$resolved_taxon,
      primary_id = sel$primary_identifier, specimen_name = sel$specimen_name,
      notes = rv$specimen_data$notes
    )

    tagList(
      div(
        class = "specimen-card",
        div(class = "detail-header",
          span(paste("Specimen Inspector: ", canon)),
          if (!is.na(sel$specimen_name) && nzchar(sel$specimen_name)) span(paste0(" ('", sel$specimen_name, "')"), style = "color: #4b5563; font-weight: 500;"),
          span(
            class = if (sel$specimen_kind == "fossil_specimen") "badge-fossil" else if (sel$specimen_kind == "in_vivo_subject") "badge-invivo" else "badge-bio",
            style = "margin-left: 10px;",
            sel$specimen_kind
          )
        ),

        # Taxon Conflict Alert if present
        if (isTRUE(as.logical(sel$taxon_conflict))) {
          div(class = "conflict-alert",
            strong("Taxon Conflict Detected: "),
            "Sources disagree on the taxonomic assignment of this physical animal across studies or classifications. See resolved taxon and provenance notes below."
          )
        },

        tabsetPanel(
          # Inspector Tab 1: Identity & Taxonomy
          tabPanel(
            "Identity & Taxonomy",
            br(),
            div(class = "row",
              div(class = "col-md-6",
                p(span(class = "prop-label", "Canonical Specimen:"), strong(canon)),
                p(span(class = "prop-label", "House / Specimen Name:"), sel$specimen_name %||% "N/A"),
                p(span(class = "prop-label", "Primary Identifier:"), strong(sel$primary_identifier %||% "N/A")),
                p(span(class = "prop-label", "Alternate Identifiers:"), sel$alternate_identifiers %||% "None"),
                p(span(class = "prop-label", "Collection Group:"), strong(sel$collection_group %||% "N/A")),
                p(span(class = "prop-label", "Collection (as printed):"), sel$collection %||% "N/A"),
                if (!is.na(sel$collection_qualifier %||% NA) && nzchar(sel$collection_qualifier %||% "")) {
                  p(span(class = "prop-label", "Origin / Qualifier:"), em(sel$collection_qualifier),
                    span(class = "small-note", " (e.g. the source colony an animal came from -- not necessarily who holds its brain now)"))
                },
                p(span(class = "prop-label", "Sex:"), sel$sex %||% "Not recorded"),
                p(span(class = "prop-label", "Match Status:"), span(class = if (sel$match == "matched") "badge-matched" else "badge-probable", sel$match %||% "N/A"))
              ),
              div(class = "col-md-6",
                p(span(class = "prop-label", "Resolved Taxon:"), strong(sel$resolved_taxon %||% "Unresolved", style = "color: #1e3a8a;")),
                p(span(class = "prop-label", "Species (accepted name):"), strong(sel$accepted_name %||% "Unresolved"),
                  if (!is.na(sel$taxon_resolution_basis %||% NA) && !(sel$taxon_resolution_basis %in% c("exact", "exact_taxonomy_key"))) {
                    span(class = "small-note", paste0(" (via ", sel$taxon_resolution_basis, " -- see specimen_taxon_resolution.csv)"))
                  }),
                p(span(class = "prop-label", "Published Taxon:"), em(sel$published_taxon %||% "N/A")),
                p(span(class = "prop-label", "Verbatim Printed Label:"), code(sel$printed_name %||% "N/A")),
                p(span(class = "prop-label", "Taxon Concept:"), sel$taxon_concept %||% "N/A"),
                if (nrow(tc_row)) {
                  tagList(
                    p(span(class = "prop-label", "Concept Decomposable:"), if (isTRUE(as.logical(tc_row$decomposable))) "Yes (named individuals)" else "NO (pooled mean - do not un-average)"),
                    p(span(class = "prop-label", "Believed Composition:"), tc_row$believed_composition %||% "N/A")
                  )
                }
              )
            ),
            hr(),
            h5("Provenance & Evidence Note:", style = "font-weight: 700;"),
            p(sel$note %||% "No specific notes recorded.")
          ),

          # Inspector Tab 2: Cross-Study Occurrences
          tabPanel(
            paste0("Cross-Study Tracking (", nrow(all_recs), " citations)"),
            br(),
            p("All recorded occurrences and citations of this same physical individual across published studies in the crosswalk:"),
            renderDT({
              datatable(
                all_recs %>%
                  transmute(
                    Publication = source_publication,
                    Reference = item_reference,
                    `Printed Taxon` = printed_name,
                    `Published Taxon` = published_taxon,
                    `Resolved Taxon` = resolved_taxon,
                    `Collection Group` = collection_group,
                    Collection = collection,
                    Match = match,
                    Note = note
                  ),
                rownames = FALSE,
                options = list(pageLength = 5, dom = "tip", scrollX = TRUE)
              )
            })
          ),

          # Inspector Tab 3: Backing Evidence Sources
          tabPanel(
            paste0("Evidence Sources (", nrow(ev_df), ")"),
            br(),
            if (nrow(ev_df)) {
              renderDT({
                datatable(
                  ev_df %>%
                    transmute(
                      `Source ID` = source_id,
                      `Label` = source_label,
                      `Type` = source_type,
                      `Publication Status` = publication_status,
                      `Access Class` = access_class,
                      `Repository Location` = repository_location,
                      Note = note
                    ),
                  rownames = FALSE,
                  options = list(pageLength = 5, dom = "tip", scrollX = TRUE)
                )
              })
            } else {
              p("No registered evidence sources linked.")
            }
          ),

          # Inspector Tab 4: Physical Measurements
          tabPanel(
            paste0("Measurements (", length(measurements), " sources)"),
            br(),
            if (length(measurements)) {
              tagList(
                p("Published physical and anatomical measurements matched for this specimen:"),
                lapply(names(measurements), function(m_lbl) {
                  tagList(
                    h6(strong(m_lbl), style = "color: #1e3a8a; margin-top: 12px;"),
                    renderDT({
                      datatable(
                        measurements[[m_lbl]],
                        rownames = FALSE,
                        options = list(dom = "t", scrollX = TRUE)
                      )
                    })
                  )
                })
              )
            } else {
              p("No direct row matches found in the curated paper measurement tables for this identifier.")
            }
          ),

          # Inspector Tab 5: Dossier Note
          if (!is.null(dossier_name) && dossier_name %in% names(rv$specimen_data$notes)) {
            tabPanel(
              "Specimen Dossier",
              br(),
              div(class = "dossier-box",
                shiny::markdown(rv$specimen_data$notes[[dossier_name]])
              )
            )
          }
        )
      )
    )
  })

  # ---- Fossil Comparison Tables ----
  output$fossil_comparison_table <- renderDT({
    fc <- rv$specimen_data$fossil_comp
    if (!nrow(fc)) return(datatable(data.frame(Message = "No comparison records found.")))
    datatable(
      fc,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$fossil_crosswalk_table <- renderDT({
    fcw <- rv$specimen_data$fossil_crosswalk
    if (!nrow(fcw)) return(datatable(data.frame(Message = "No fossil crosswalk records found.")))
    datatable(
      fcw,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  # ---- Published Specimen Tables ----
  selected_paper_table <- reactive({
    tbl_name <- input$paper_spec_table_select
    if (!tbl_name %in% names(rv$specimen_data$paper_tables)) return(data.frame())
    dat <- rv$specimen_data$paper_tables[[tbl_name]]
    query <- str_squish(input$paper_spec_search %||% "")
    if (nzchar(query) && nrow(dat)) {
      blob <- make_fast_search_blob(dat)
      keep <- match_terms(blob, query)
      dat <- dat[keep, , drop = FALSE]
    }
    dat
  })

  output$paper_spec_status <- renderUI({
    dat <- selected_paper_table()
    tagList(
      strong(format(nrow(dat), big.mark = ",")),
      " records in table ",
      strong(input$paper_spec_table_select)
    )
  })

  output$paper_spec_table <- renderDT({
    dat <- selected_paper_table()
    if (!nrow(dat)) return(datatable(data.frame(Message = "No records found.")))
    datatable(
      dat,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50),
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    )
  })

  output$download_paper_spec_table <- downloadHandler(
    filename = function() {
      paste0("SpecimenTable_", gsub("[^A-Za-z0-9_]+", "_", input$paper_spec_table_select), "_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(selected_paper_table(), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  output$download_specimens <- downloadHandler(
    filename = function() {
      paste0("SpecimenCrosswalk_Search_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(filtered_specimens(), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # ---- Registries Tables ----
  output$taxon_concepts_table <- renderDT({
    datatable(
      rv$specimen_data$taxon_concepts,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$collections_table <- renderDT({
    datatable(
      rv$specimen_data$collections,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$sources_table <- renderDT({
    datatable(
      rv$specimen_data$sources,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$dossier_content <- renderUI({
    d_name <- input$dossier_select
    if (!d_name %in% names(rv$specimen_data$notes)) {
      return(p("Selected note not found."))
    }
    shiny::markdown(rv$specimen_data$notes[[d_name]])
  })

  # ---- Trait Search Tab ----
  observe({
    dom <- input$trait_domain %||% "All Domains (Combined)"
    if (dom == "All Domains (Combined)") {
      cols <- c("All fields", names(rv$trait_data$combined))
    } else if (dom %in% names(rv$trait_data$domain_tables)) {
      cols <- c("All fields", names(rv$trait_data$domain_tables[[dom]]))
    } else {
      cols <- "All fields"
    }
    updateSelectInput(session, "trait_column", choices = cols)
  })

  observeEvent(input$reset_trait_filters, {
    updateTextInput(session, "trait_search", value = "")
    updateSelectInput(session, "trait_domain", selected = "All Domains (Combined)")
    updateSelectInput(session, "trait_column", selected = "All fields")
    updateNumericInput(session, "trait_limit", value = 1000)
  })

  trait_results <- reactive({
    dom <- input$trait_domain
    dat <- if (dom == "All Domains (Combined)") {
      rv$trait_data$combined
    } else if (dom %in% names(rv$trait_data$domain_tables)) {
      rv$trait_data$domain_tables[[dom]]
    } else {
      data.frame()
    }

    if (!nrow(dat)) {
      attr(dat, "match_count") <- 0
      return(dat)
    }

    query <- str_squish(input$trait_search %||% "")
    if (!nzchar(query)) {
      out <- head(dat, input$trait_limit)
      attr(out, "match_count") <- nrow(dat)
      return(out)
    }

    # Search within specific column or all fields
    if (input$trait_column == "All fields" || !input$trait_column %in% names(dat)) {
      blob <- if (dom == "All Domains (Combined)") rv$trait_data$search_blob else make_fast_search_blob(dat)
      keep <- match_terms(blob, query)
    } else {
      col_val <- tolower(ifelse(is.na(dat[[input$trait_column]]), "", as.character(dat[[input$trait_column]])))
      keep <- match_terms(col_val, query)
    }

    hits <- dat[keep, , drop = FALSE]
    total_hits <- nrow(hits)
    out <- head(hits, input$trait_limit)
    attr(out, "match_count") <- total_hits
    out
  })

  output$trait_status <- renderUI({
    dat <- trait_results()
    total <- attr(dat, "match_count") %||% nrow(dat)
    tagList(
      strong(format(total, big.mark = ",")),
      " matching trait records in ",
      strong(input$trait_domain),
      if (total > nrow(dat)) paste0(" (displaying first ", format(nrow(dat), big.mark = ","), ")") else NULL
    )
  })

  output$trait_table <- renderDT({
    dat <- trait_results()
    if (!nrow(dat)) return(datatable(data.frame(Message = "No matching trait records found.")))
    datatable(
      dat,
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    )
  })

  output$trait_detail_card <- renderUI({
    idx <- input$trait_table_rows_selected
    dat <- trait_results()
    if (!length(idx) || !nrow(dat) || idx > nrow(dat)) return(NULL)
    sel <- dat[idx, ]

    div(
      class = "specimen-card",
      h5("Selected Record Inspector", style = "font-weight: 700; color: #1e3a8a;"),
      div(class = "row",
        lapply(names(sel), function(col) {
          val <- as.character(sel[[col]])
          if (is.na(val) || !nzchar(val)) val <- "(empty)"
          div(class = "col-md-4", style = "margin-bottom: 8px;",
            span(class = "prop-label", paste0(col, ":")),
            strong(val)
          )
        })
      )
    )
  })

  output$download_traits <- downloadHandler(
    filename = function() {
      paste0("TraitExplorer_Search_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(trait_results(), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # ---- Species Coverage Tab ----
  coverage_summary <- reactive({
    sp <- input$coverage_species
    combined <- rv$trait_data$combined
    if (!nzchar(sp %||% "") || !nrow(combined)) return(data.frame())
    rows <- combined[!is.na(combined$Species) & combined$Species == sp, , drop = FALSE]
    if (!nrow(rows)) return(data.frame())
    rows <- rows[!is.na(rows$Domain), , drop = FALSE]
    if (!nrow(rows)) return(data.frame())
    rows %>%
      group_by(Domain) %>%
      summarise(
        n_variables = n_distinct(Variable),
        n_records = n(),
        variables = paste(sort(unique(Variable)), collapse = "; "),
        .groups = "drop"
      ) %>%
      arrange(desc(n_variables))
  })

  output$coverage_status <- renderUI({
    sp <- input$coverage_species
    if (!nzchar(sp %||% "")) {
      return(p(class = "small-note",
                "Select a species to see how many variables are available for it, by domain."))
    }
    dat <- coverage_summary()
    if (!nrow(dat)) {
      return(div(class = "conflict-alert",
                  strong("No data found for: "), sp,
                  ". Try a different spelling -- species names are not harmonized across domains."))
    }
    tagList(
      strong(sp, style = "font-style: italic;"), " -- ",
      strong(format(sum(dat$n_variables), big.mark = ",")), " distinct variables across ",
      strong(nrow(dat)), " of ", strong(length(rv$trait_data$domain_tables)), " domains",
      " (", format(sum(dat$n_records), big.mark = ","), " total records)"
    )
  })

  output$coverage_pie <- plotly::renderPlotly({
    dat <- coverage_summary()
    if (!nrow(dat)) return(plotly::plotly_empty(type = "pie"))
    plotly::plot_ly(
      dat, labels = ~Domain, values = ~n_variables, type = "pie",
      textinfo = "label+value", hoverinfo = "label+value+percent",
      marker = list(line = list(color = "#ffffff", width = 1))
    ) %>%
      plotly::layout(showlegend = TRUE, margin = list(t = 10, b = 10))
  })

  output$coverage_table <- renderDT({
    dat <- coverage_summary()
    if (!nrow(dat)) return(datatable(data.frame(Message = "No data for this species.")))
    datatable(
      dat %>%
        transmute(Domain = Domain, Variables = n_variables, Records = n_records,
                  `Variable Names` = variables),
      rownames = FALSE, filter = "top",
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })


  # ---- Start Here Tab ----
  output$home_counts <- renderUI({
    comb <- rv$trait_data$combined
    rg <- rv$trait_data$region
    n_species <- length(species_choices_from(comb$Species))
    n_dom <- length(rv$trait_data$domain_tables)
    n_spec <- length(unique(rv$specimen_data$crosswalk$canonical_specimen))
    n_regions <- if (nrow(rg)) length(unique(rg$Concept)) else 0
    tagList(
      span(class = "stat-pill", paste0(format(nrow(comb), big.mark = ","), " trait values")),
      span(class = "stat-pill", paste0(n_species, " species")),
      span(class = "stat-pill", paste0(n_dom, " data domains")),
      span(class = "stat-pill", paste0(format(n_spec, big.mark = ","), " individual specimens tracked")),
      span(class = "stat-pill", paste0(n_regions, " brain-region concepts")),
      span(class = "stat-pill", paste0(format(nrow(rv$index_tbl), big.mark = ","), " source files")),
      p(class = "small-note", style = "margin-top: 8px;",
        "Source: ", sprintf("github.com/%s/%s@%s", GITHUB_OWNER, GITHUB_REPO, GITHUB_BRANCH),
        if (USING_LOCAL_REPO) span(style = "color:#b45309; font-weight:600;", " | PREVIEWING LOCAL CHECKOUT (TRAIT_DATA_LOCAL_DIR) -- not the published GitHub data") else NULL,
        " | last loaded ", format(rv$loaded_at, "%Y-%m-%d %H:%M"), ". Use 'Refresh data from GitHub' after the repository changes.")
    )
  })

  observeEvent(input$ex_specimen, {
    updateTabsetPanel(session, "main_tabs", selected = "Specimen Search")
    updateTabsetPanel(session, "specimen_subtabs", selected = "Specimen Explorer")
    updateTextInput(session, "specimen_search", value = "Disco")
  })
  observeEvent(input$ex_trait, {
    updateTabsetPanel(session, "main_tabs", selected = "Trait Search")
    updateSelectInput(session, "trait_domain", selected = "All Domains (Combined)")
    updateTextInput(session, "trait_search", value = "Pan troglodytes Cerebellum_Vol")
  })
  observeEvent(input$ex_region, {
    updateTabsetPanel(session, "main_tabs", selected = "Region Comparison")
    updateRadioButtons(session, "rc_mode", selected = "regions")
    rc_pending$species <- "Mus musculus"
    rc_pending$x <- "CMR: CMRgl (umol/100g/min)"
    rc_pending$y <- "cellcounts: N.n (count)"
    updateSelectizeInput(session, "rc_species", selected = "Mus musculus")
  })

  # ---- Region Comparison Tab ----
  # Pending selections let the example button set species, X and Y even
  # though the X/Y choices are only computed once the species is known.
  rc_pending <- reactiveValues(species = NULL, x = NULL, y = NULL)

  rc_base <- reactive({
    rg <- rv$trait_data$region
    if (!nrow(rg)) return(rg)
    if (!isTRUE(input$rc_proxy)) rg <- rg[rg$link_class != "proxy", , drop = FALSE]
    rg
  })

  # Collapse to one value per (Species, Concept, VariableKey): prefer direct
  # links over proxy over own-name; within the preferred class take the
  # median of what remains (several sources, or several terms that map to
  # the same concept) and carry the count.
  rc_collapsed <- reactive({
    rg <- rc_base()
    if (!nrow(rg)) return(rg)
    rank <- c(direct = 1L, proxy = 2L, `own name` = 3L)
    rg$link_rank <- rank[rg$link_class]
    rg$link_rank[is.na(rg$link_rank)] <- 4L
    rg %>%
      group_by(Species, Concept, VariableKey) %>%
      filter(link_rank == min(link_rank)) %>%
      summarise(
        Value = stats::median(Value, na.rm = TRUE),
        n_values = dplyr::n(),
        Terms = paste(sort(unique(Term)), collapse = "; "),
        Structures = paste(sort(unique(Structure)), collapse = "; "),
        link_class = dplyr::first(link_class),
        link_relation = paste(sort(unique(link_relation)), collapse = "; "),
        Sources = paste(sort(unique(na.omit(Source))), collapse = " | "),
        Domain = dplyr::first(Domain),
        .groups = "drop"
      )
  })

  # Populate species / region pickers (species with >= 2 variables that each
  # cover >= 2 regions are the ones where a within-species plot is possible).
  observe({
    cd <- rc_collapsed()
    if (!nrow(cd)) return()
    sp_ok <- cd %>%
      group_by(Species, VariableKey) %>% summarise(n = dplyr::n(), .groups = "drop") %>%
      filter(n >= 2) %>% group_by(Species) %>% summarise(k = dplyr::n(), .groups = "drop") %>%
      filter(k >= 2) %>% pull(Species)
    sp_ok <- species_choices_from(sp_ok)
    labels <- species_display_label(sp_ok, rv$trait_data$species_keys)
    sel <- isolate(input$rc_species)
    if (!is.null(isolate(rc_pending$species))) sel <- isolate(rc_pending$species)
    if (is.null(sel) || !sel %in% sp_ok) sel <- if ("Mus musculus" %in% sp_ok) "Mus musculus" else sp_ok[1]
    updateSelectizeInput(session, "rc_species", choices = stats::setNames(sp_ok, labels), selected = sel, server = TRUE)

    rg_ok <- cd %>%
      group_by(Concept, VariableKey) %>% summarise(n = dplyr::n(), .groups = "drop") %>%
      filter(n >= 3) %>% group_by(Concept) %>% summarise(k = dplyr::n(), .groups = "drop") %>%
      filter(k >= 2) %>% pull(Concept) %>% sort()
    sel_r <- isolate(input$rc_region)
    if (is.null(sel_r) || !sel_r %in% rg_ok) sel_r <- if ("Cerebellum" %in% rg_ok) "Cerebellum" else rg_ok[1]
    updateSelectizeInput(session, "rc_region", choices = rg_ok, selected = sel_r, server = TRUE)
  })

  # Rows in scope for the current mode: one species (points = regions) or
  # one region concept (points = species).
  rc_scope <- reactive({
    cd <- rc_collapsed()
    if (!nrow(cd)) return(cd)
    if (identical(input$rc_mode, "species")) {
      req(input$rc_region)
      out <- cd[cd$Concept %in% input$rc_region, , drop = FALSE]
      out$Point <- out$Species
    } else {
      req(input$rc_species)
      out <- cd[cd$Species %in% input$rc_species, , drop = FALSE]
      out$Point <- out$Concept
    }
    out
  })

  # Variable choices: those with >= 2 points in scope. Keep the current
  # selection when still valid; apply a pending example selection once.
  observe({
    sc <- rc_scope()
    vars <- if (nrow(sc)) {
      sc %>% group_by(VariableKey) %>% summarise(n = dplyr::n(), .groups = "drop") %>%
        filter(n >= 2) %>% arrange(VariableKey)
    } else data.frame(VariableKey = character(), n = integer())
    choices <- vars$VariableKey
    labels <- paste0(vars$VariableKey, "  [", vars$n, " points]")
    cur_x <- isolate(input$rc_x); cur_y <- isolate(input$rc_y)
    px <- isolate(rc_pending$x); py <- isolate(rc_pending$y)
    if (!is.null(px) && px %in% choices) cur_x <- px
    if (!is.null(py) && py %in% choices) cur_y <- py
    if (!is.null(px) || !is.null(py) || !is.null(isolate(rc_pending$species))) {
      rc_pending$x <- NULL; rc_pending$y <- NULL; rc_pending$species <- NULL
    }
    if (is.null(cur_x) || !cur_x %in% choices) {
      pref <- grep("^CMR: CMRgl", choices, value = TRUE)
      cur_x <- if (length(pref)) pref[1] else choices[1]
    }
    if (is.null(cur_y) || !cur_y %in% choices || identical(cur_y, cur_x)) {
      pref <- grep("^cellcounts: N.n", choices, value = TRUE)
      cur_y <- if (length(pref) && !identical(pref[1], cur_x)) pref[1] else setdiff(choices, cur_x)[1]
    }
    updateSelectizeInput(session, "rc_x", choices = stats::setNames(choices, labels), selected = cur_x)
    updateSelectizeInput(session, "rc_y", choices = stats::setNames(choices, labels), selected = cur_y)
  })

  rc_pairs <- reactive({
    sc <- rc_scope()
    req(input$rc_x, input$rc_y)
    if (!nrow(sc)) return(data.frame())
    x <- sc[sc$VariableKey %in% input$rc_x, , drop = FALSE]
    y <- sc[sc$VariableKey %in% input$rc_y, , drop = FALSE]
    if (!nrow(x) || !nrow(y)) return(data.frame())
    out <- dplyr::inner_join(
      x %>% transmute(Point, Species, Concept, X = Value, X_structure = Structures, X_terms = Terms, X_link = link_class, X_relation = link_relation, X_n = n_values, X_sources = Sources),
      y %>% transmute(Point, Y = Value, Y_structure = Structures, Y_terms = Terms, Y_link = link_class, Y_relation = link_relation, Y_n = n_values, Y_sources = Sources),
      by = "Point"
    )
    out <- out[is.finite(out$X) & is.finite(out$Y), , drop = FALSE]
    if (isTRUE(input$rc_log)) out <- out[out$X > 0 & out$Y > 0, , drop = FALSE]
    out
  })

  rc_fit <- reactive({
    d <- rc_pairs()
    if (nrow(d) < 3) return(NULL)
    xx <- if (isTRUE(input$rc_log)) log10(d$X) else d$X
    yy <- if (isTRUE(input$rc_log)) log10(d$Y) else d$Y
    fit <- stats::lm(yy ~ xx)
    sm <- summary(fit)
    list(
      fit = fit,
      n = nrow(d),
      slope = unname(coef(fit)[2]),
      intercept = unname(coef(fit)[1]),
      slope_se = sm$coefficients[2, 2],
      r2 = sm$r.squared,
      p = sm$coefficients[2, 4],
      pearson = suppressWarnings(stats::cor(xx, yy)),
      spearman = suppressWarnings(stats::cor(d$X, d$Y, method = "spearman"))
    )
  })

  output$rc_status <- renderUI({
    d <- rc_pairs()
    if (!nrow(rv$trait_data$region)) return(p(class = "small-note", "No region-level data could be built from the loaded domains."))
    if (is.null(input$rc_x) || is.null(input$rc_y) || !nzchar(input$rc_x %||% "") || !nzchar(input$rc_y %||% "")) {
      return(p(class = "small-note", "Choose a species (or region) and two variables."))
    }
    scope_lbl <- if (identical(input$rc_mode, "species")) paste0("region ", strong(input$rc_region), "; points = species") else tagList("species ", em(input$rc_species), "; points = brain regions")
    if (!nrow(d)) {
      return(div(class = "conflict-alert", strong("No brain regions carry both variables"), " for ", scope_lbl,
        ". Pick another pair, or allow proxy links. The list below shows which regions have only one of the two."))
    }
    f <- rc_fit()
    tagList(
      strong(nrow(d)), " paired points for ", scope_lbl, ". ",
      tags$br(),
      if (!is.null(f)) {
        scale_lbl <- if (isTRUE(input$rc_log)) "log10-log10" else "linear"
        span(
          sprintf("OLS (%s): slope = %.3f +/- %.3f (SE), intercept = %.3f, R\u00b2 = %.3f, p = %.3g. Pearson r = %.3f; Spearman rho = %.3f.",
                  scale_lbl, f$slope, f$slope_se, f$intercept, f$r2, f$p, f$pearson, f$spearman)
        )
      } else span(class = "small-note", "At least 3 paired points are needed for a regression."),
      tags$br(),
      span(class = "small-note",
        "Pairings: ", paste(names(table(paste(d$X_link, "x", d$Y_link))), table(paste(d$X_link, "x", d$Y_link)), sep = " = ", collapse = "; "),
        ". Values are compiled species means (median where several sources/terms map to one concept); ",
        "points are not phylogenetically independent in species mode, and regional values within a species share one animal sample -- treat the fit as descriptive.")
    )
  })

  output$rc_plot <- plotly::renderPlotly({
    d <- rc_pairs()
    if (!nrow(d)) return(plotly::plotly_empty())
    f <- rc_fit()
    xlab <- input$rc_x; ylab <- input$rc_y
    hover <- paste0(
      "<b>", d$Point, "</b>",
      if (identical(input$rc_mode, "species")) "" else paste0("<br>Species: ", d$Species),
      "<br>X: ", signif(d$X, 4), "  [", d$X_structure, "; ", d$X_link, "]",
      "<br>Y: ", signif(d$Y, 4), "  [", d$Y_structure, "; ", d$Y_link, "]",
      "<br>X source: ", substr(d$X_sources, 1, 80),
      "<br>Y source: ", substr(d$Y_sources, 1, 80)
    )
    col <- ifelse(d$X_link == "direct" & d$Y_link == "direct", "#1d4ed8", ifelse(d$X_link == "own name" | d$Y_link == "own name", "#6b7280", "#f59e0b"))
    pl <- plotly::plot_ly(
      d, x = ~X, y = ~Y, type = "scatter",
      mode = if (isTRUE(input$rc_labels)) "markers+text" else "markers",
      text = if (isTRUE(input$rc_labels)) d$Point else NULL, textposition = "top center",
      textfont = list(size = 10, color = "#374151"),
      hovertext = hover, hoverinfo = "text",
      marker = list(size = 10, color = col, line = list(color = "#ffffff", width = 1)),
      name = "paired points"
    )
    if (!is.null(f)) {
      xs <- seq(min(d$X), max(d$X), length.out = 100)
      ys <- if (isTRUE(input$rc_log)) 10^(f$intercept + f$slope * log10(xs)) else f$intercept + f$slope * xs
      pl <- pl %>% plotly::add_trace(x = xs, y = ys, type = "scatter", mode = "lines", inherit = FALSE,
        line = list(color = "#b91c1c", width = 2), hoverinfo = "skip",
        name = sprintf("OLS fit (slope %.2f, R\u00b2 %.2f)", f$slope, f$r2))
    }
    ax_type <- if (isTRUE(input$rc_log)) "log" else "linear"
    pl %>% plotly::layout(
      xaxis = list(title = xlab, type = ax_type),
      yaxis = list(title = ylab, type = ax_type),
      legend = list(orientation = "h", y = -0.2),
      margin = list(t = 20)
    )
  })

  output$rc_table <- renderDT({
    d <- rc_pairs()
    if (!nrow(d)) return(datatable(data.frame(Message = "No paired observations.")))
    datatable(
      d %>% transmute(
        Point, Species,
        `X value` = signif(X, 5), `X structure(s)` = X_structure, `X link` = X_link, `X n` = X_n,
        `Y value` = signif(Y, 5), `Y structure(s)` = Y_structure, `Y link` = Y_link, `Y n` = Y_n,
        `X sources` = X_sources, `Y sources` = Y_sources
      ),
      rownames = FALSE, filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$rc_unpaired <- renderUI({
    sc <- rc_scope()
    if (!nrow(sc) || is.null(input$rc_x) || is.null(input$rc_y)) return(NULL)
    x_pts <- unique(sc$Point[sc$VariableKey %in% input$rc_x])
    y_pts <- unique(sc$Point[sc$VariableKey %in% input$rc_y])
    only_x <- setdiff(x_pts, y_pts); only_y <- setdiff(y_pts, x_pts)
    if (!length(only_x) && !length(only_y)) return(NULL)
    tags$details(
      tags$summary(style = "cursor:pointer; font-weight:600; color:#475569;",
        sprintf("Unpaired: %d with X only, %d with Y only (click to show)", length(only_x), length(only_y))),
      if (length(only_x)) p(class = "small-note", strong("X only: "), paste(sort(only_x), collapse = ", ")),
      if (length(only_y)) p(class = "small-note", strong("Y only: "), paste(sort(only_y), collapse = ", ")),
      p(class = "small-note", "A structure measured in both domains under names the anatomy crosswalk does not yet link will appear here on both sides; add the link upstream in _keys/anatomy_crosswalk.csv.")
    )
  })

  output$rc_download <- downloadHandler(
    filename = function() {
      lbl <- if (identical(input$rc_mode, "species")) input$rc_region else input$rc_species
      paste0("TraitExplorer_RegionComparison_", gsub("[^A-Za-z0-9]+", "_", lbl %||% "data"), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      d <- rc_pairs()
      d$X_variable <- input$rc_x; d$Y_variable <- input$rc_y
      write.csv(d, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  # ---- Repository Explorer Tab ----
  observeEvent(input$reset_file_filters, {
    updateTextInput(session, "file_search", value = "")
    updateSelectInput(session, "file_extension", selected = "All")
    updateSelectInput(session, "file_year", selected = "All")
    updateCheckboxInput(session, "file_papers_only", value = FALSE)
    updateCheckboxInput(session, "file_public_only", value = FALSE)
  })

  filtered_files <- reactive({
    dat <- rv$index_tbl

    if ((input$file_extension %||% "All") != "All") {
      dat <- filter(dat, extension == input$file_extension)
    }

    if ((input$file_year %||% "All") != "All") {
      dat <- filter(dat, year == input$file_year)
    }

    if (isTRUE(input$file_papers_only)) {
      dat <- filter(dat, is_paper_folder)
    }

    if (isTRUE(input$file_public_only)) {
      dat <- filter(dat, str_detect(relative_path, "(^|/)__Public(/|$)"))
    }

    query <- str_squish(input$file_search %||% "")
    if (nzchar(query)) {
      search_blob <- str_to_lower(paste(dat$relative_path, dat$filename, dat$study_folder, dat$author))
      keep <- match_terms(search_blob, query)
      dat <- dat[keep, , drop = FALSE]
    }

    dat
  })

  output$repo_status <- renderUI({
    dat <- filtered_files()
    tagList(
      strong(format(nrow(dat), big.mark = ",")),
      " matching files in repository"
    )
  })

  output$file_table <- renderDT({
    dat <- filtered_files() %>%
      transmute(
        Study = study_folder,
        Author = author,
        Year = year,
        File = filename,
        Type = extension,
        Size_kB = size_kb,
        Folder = folder,
        GitHub = github_url
      )

    datatable(
      dat,
      rownames = FALSE,
      selection = "single",
      escape = FALSE,
      options = list(
        pageLength = 20,
        lengthMenu = c(10, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = 7, render = JS("function(data){ return '<a href=\"' + data + '\" target=\"_blank\">open</a>'; }"))
        )
      )
    )
  })

  selected_file <- reactive({
    row <- input$file_table_rows_selected
    dat <- filtered_files()
    if (!length(row) || !nrow(dat) || row > nrow(dat)) return(NULL)
    dat[row, , drop = FALSE]
  })

  output$file_details <- renderPrint({
    row <- selected_file()
    if (is.null(row)) {
      cat("Select a file to see its provenance details.
")
      return(invisible())
    }
    print(row %>% select(relative_path, filename, extension, year, author, size_kb, github_url), row.names = FALSE)
  })

  output$download_selected_file <- downloadHandler(
    filename = function() {
      row <- selected_file()
      if (is.null(row)) "selected_file" else row$filename
    },
    content = function(file) {
      row <- selected_file()
      if (is.null(row)) stop("Select a file first.", call. = FALSE)
      cached <- gh_fetch(row$relative_path)
      if (is.null(cached)) stop("Could not download the selected file from GitHub.", call. = FALSE)
      ok <- file.copy(cached, file, overwrite = TRUE)
      if (!ok) stop("Could not copy the selected file.", call. = FALSE)
    }
  )
}

shinyApp(ui, server)
