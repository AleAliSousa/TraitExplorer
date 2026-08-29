# TraitExplorer
# Shiny interface for exploring the Evo-M1-Trait-Data repository.

required_packages <- c(
  "shiny", "DT", "dplyr", "stringr", "readr", "readxl", "janitor"
)

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
library(readr)
library(readxl)
library(janitor)

# --------------------------------------------------
# CONFIG
# --------------------------------------------------

app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(app_file) || !nzchar(app_file)) app_file <- "app.R"
app_dir <- dirname(normalizePath(app_file, winslash = "/", mustWork = FALSE))
source(file.path(app_dir, "config.R"))

find_data_repo <- function(configured_path) {
  cloud_candidates <- Sys.glob(
    path.expand("~/Library/CloudStorage/*/Species/Evo-M1-Trait-Data")
  )
  candidates <- unique(c(
    configured_path,
    Sys.getenv("TRAIT_DATA_REPO", unset = ""),
    cloud_candidates,
    path.expand("~/Species/Evo-M1-Trait-Data"),
    file.path(getwd(), "Evo-M1-Trait-Data"),
    file.path(dirname(getwd()), "Evo-M1-Trait-Data")
  ))
  candidates <- candidates[nzchar(candidates)]
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  existing <- candidates[dir.exists(candidates)]
  if (length(existing)) existing[[1]] else NA_character_
}

repo_root <- find_data_repo(DATA_REPO)
if (is.na(repo_root)) {
  stop(
    paste0(
      "Trait data repository not found.\n\n",
      "Set TRAIT_DATA_REPO or edit config.R.\n",
      "Current configured path: ", DATA_REPO
    ),
    call. = FALSE
  )
}

GITHUB_BASE_URL <- if (exists("GITHUB_DATA_REPO", inherits = FALSE)) {
  GITHUB_DATA_REPO
} else {
  "https://github.com/AleAliSousa/Evo-M1-Trait-Data/blob/main"
}

# --------------------------------------------------
# HELPERS
# --------------------------------------------------

safe_relative_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  sub(paste0("^", stringr::fixed(prefix)), "", path)
}

build_index <- function(repo_root) {
  files <- list.files(
    repo_root,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  files <- files[!grepl("(^|/)(\\.git|\\.Rproj\\.user)(/|$)", files)]

  out <- tibble(
    full_path = files,
    relative_path = vapply(files, safe_relative_path, character(1), root = repo_root)
  ) %>%
    mutate(
      folder = dirname(relative_path),
      filename = basename(relative_path),
      extension = tolower(tools::file_ext(filename)),
      study_folder = vapply(
        strsplit(relative_path, "/", fixed = TRUE),
        function(x) if (length(x) >= 2) x[[1]] else "(root)",
        character(1)
      ),
      year = str_extract(study_folder, "(?:19|20)\\d{2}"),
      author = str_replace(study_folder, "_etal.*|__.*", ""),
      is_paper_folder = !startsWith(study_folder, "__"),
      size_kb = round(file.info(full_path)$size / 1024, 1)
    ) %>%
    mutate(
      github_url = paste0(
        GITHUB_BASE_URL,
        "/",
        relative_path
      )
    )

  out
}

read_data_file <- function(f) {
  ext <- tolower(tools::file_ext(f))
  switch(
    ext,
    csv = read_csv(f, show_col_types = FALSE),
    tsv = read_tsv(f, show_col_types = FALSE),
    xls = read_excel(f),
    xlsx = read_excel(f),
    NULL
  )
}

load_merged_datasets <- function(repo_root) {
  merge_dirs <- list.dirs(repo_root, recursive = FALSE, full.names = TRUE)
  merge_dirs <- merge_dirs[grepl("__merging", basename(merge_dirs), fixed = TRUE)]

  datasets <- list()
  problems <- character()

  for (dir in merge_dirs) {
    files <- list.files(
      dir,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.(csv|tsv|xlsx?|xls)$",
      ignore.case = TRUE
    )

    for (f in files) {
      dat <- tryCatch(read_data_file(f), error = function(e) {
        problems <<- c(problems, paste(basename(f), ":", conditionMessage(e)))
        NULL
      })
      if (is.null(dat)) next

      dat <- janitor::clean_names(dat)
      dat <- as.data.frame(lapply(dat, as.character), stringsAsFactors = FALSE)
      dat$source_dataset <- basename(f)
      dat$source_folder <- basename(dir)
      datasets[[length(datasets) + 1L]] <- dat
    }
  }

  if (!length(datasets)) {
    return(list(data = data.frame(), problems = problems))
  }

  all_data <- bind_rows(lapply(datasets, as.data.frame, stringsAsFactors = FALSE))
  list(data = all_data, problems = problems)
}

make_search_text <- function(dat) {
  if (!nrow(dat)) return(character())
  vapply(seq_len(nrow(dat)), function(i) {
    str_to_lower(paste(dat[i, ], collapse = " | "))
  }, character(1))
}

match_terms <- function(values, query) {
  terms <- str_split(str_squish(str_to_lower(query)), "\\s+")[[1]]
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(rep(TRUE, length(values)))
  Reduce(`&`, lapply(terms, function(term) str_detect(values, fixed(term))))
}

# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------

index_tbl <- build_index(repo_root)
merged_result <- load_merged_datasets(repo_root)
merged_data <- merged_result$data
merged_search_text <- make_search_text(merged_data)

message(
  "TraitExplorer data root: ", repo_root,
  " | files: ", nrow(index_tbl),
  " | merged rows: ", nrow(merged_data),
  " | merged columns: ", ncol(merged_data)
)

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(
  titlePanel("Trait Explorer"),

  tags$head(
    tags$style(HTML("\n      .small-note { color: #666; font-size: 0.9em; }\n      .status-box { padding: 10px 14px; border: 1px solid #ddd; border-radius: 6px; background: #fafafa; margin-bottom: 12px; }\n      .download-row { margin-top: 8px; }\n    "))
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Repository Explorer",
      sidebarLayout(
        sidebarPanel(
          textInput("file_search", "Search files", placeholder = "species, Stephan, TableS1, brain..."),
          selectInput("file_extension", "File type", choices = "All"),
          selectInput("file_year", "Year", choices = "All"),
          checkboxInput("file_papers_only", "Paper folders only", FALSE),
          checkboxInput("file_public_only", "Public/comparative data only", FALSE),
          br(),
          actionButton("reset_file_filters", "Reset filters"),
          width = 3
        ),
        mainPanel(
          div(class = "status-box", uiOutput("repo_status")),
          DTOutput("file_table"),
          div(class = "download-row", downloadButton("download_selected_file", "Download selected file")),
          br(),
          verbatimTextOutput("file_details")
        )
      )
    ),

    tabPanel(
      "Trait Search",
      sidebarLayout(
        sidebarPanel(
          textInput(
            "trait_search",
            "Search traits / records",
            placeholder = "Pan troglodytes sleep M1"
          ),
          selectInput("trait_column", "Search within", choices = "All fields"),
          numericInput("trait_limit", "Maximum rows", value = 1000, min = 50, max = 100000, step = 50),
          br(),
          downloadButton("download_traits", "Download search results"),
          width = 3
        ),
        mainPanel(
          div(class = "status-box", uiOutput("trait_status")),
          DTOutput("trait_table")
        )
      )
    ),

    tabPanel(
      "About",
      h3("TraitExplorer"),
      p("Interactive browser for the Evo-M1-Trait-Data comparative trait database."),
      p(strong("Local data root: "), repo_root),
      p(class = "small-note", "The app reads your local repository; it does not modify source data."),
      h4("Search behavior"),
      p("Search terms are case-insensitive. Multiple words are treated as AND terms, so a search for 'Pan troglodytes sleep' returns records containing all three terms."),
      h4("Provenance"),
      p("Merged records retain source_dataset and source_folder columns so search results can be traced back to the source dataset.")
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {

  updateSelectInput(
    session,
    "file_extension",
    choices = c("All", sort(unique(index_tbl$extension[index_tbl$extension != ""])))
  )

  updateSelectInput(
    session,
    "file_year",
    choices = c("All", sort(unique(na.omit(index_tbl$year)), decreasing = TRUE))
  )

  if (ncol(merged_data)) {
    updateSelectInput(
      session,
      "trait_column",
      choices = c("All fields", names(merged_data))
    )
  }

  observeEvent(input$reset_file_filters, {
    updateTextInput(session, "file_search", value = "")
    updateSelectInput(session, "file_extension", selected = "All")
    updateSelectInput(session, "file_year", selected = "All")
    updateCheckboxInput(session, "file_papers_only", value = FALSE)
    updateCheckboxInput(session, "file_public_only", value = FALSE)
  })

  filtered_files <- reactive({
    dat <- index_tbl

    if (input$file_extension != "All") {
      dat <- filter(dat, extension == input$file_extension)
    }

    if (input$file_year != "All") {
      dat <- filter(dat, year == input$file_year)
    }

    if (isTRUE(input$file_papers_only)) {
      dat <- filter(dat, is_paper_folder)
    }

    if (isTRUE(input$file_public_only)) {
      dat <- filter(
        dat,
        str_detect(relative_path, "(^|/)__Public(/|$)")
      )
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
      " matching files",
      if (length(merged_result$problems)) {
        tags$span(" | ", style = "color:#a00", paste(length(merged_result$problems), "file(s) could not be read"))
      }
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
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = 7, render = JS("function(data){ return '<a href=\\\"' + data + '\\\" target=\\\"_blank\\\">open</a>'; }"))
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
      cat("Select a file to see its provenance and download it.\n")
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
      if (is.null(row)) {
        stop("Select a file first.", call. = FALSE)
      }
      ok <- file.copy(row$full_path, file, overwrite = TRUE)
      if (!ok) stop("Could not copy the selected file.", call. = FALSE)
    }
  )

  trait_results <- reactive({
    req(ncol(merged_data) > 0)

    query <- str_squish(input$trait_search %||% "")
    if (!nzchar(query)) {
      out <- head(merged_data, input$trait_limit)
      attr(out, "match_count") <- nrow(merged_data)
      return(out)
    }

    if (input$trait_column == "All fields") {
      keep <- match_terms(merged_search_text, query)
    } else {
      column_values <- str_to_lower(ifelse(is.na(merged_data[[input$trait_column]]), "", merged_data[[input$trait_column]]))
      keep <- match_terms(column_values, query)
    }

    hits <- merged_data[keep, , drop = FALSE]
    total_hits <- nrow(hits)
    hits <- head(hits, input$trait_limit)
    attr(hits, "match_count") <- total_hits
    hits
  })

  output$trait_status <- renderUI({
    dat <- trait_results()
    total <- attr(dat, "match_count") %||% nrow(dat)
    tagList(
      strong(format(total, big.mark = ",")),
      " matching records",
      if (total > nrow(dat)) paste0(" (showing first ", format(nrow(dat), big.mark = ","), ")") else NULL,
      if (length(merged_result$problems)) tags$span(" | ", style = "color:#a00", "some source files could not be read")
    )
  })

  output$trait_table <- renderDT({
    datatable(
      trait_results(),
      rownames = FALSE,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    )
  })

  output$download_traits <- downloadHandler(
    filename = function() {
      paste0("TraitExplorer_search_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(trait_results(), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
