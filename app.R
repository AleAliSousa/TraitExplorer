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

source("config.R")

repo_root <- normalizePath(DATA_REPO)

# --------------------------------------------------
# BUILD FILE INDEX
# --------------------------------------------------

build_index <- function(repo_root){

  files <- list.files(
    repo_root,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE
  )

  tibble(
    full_path = files
  ) %>%
    mutate(

      relative_path =
        sub(
          paste0(normalizePath(repo_root), "/"),
          "",
          normalizePath(full_path)
        ),

      folder = dirname(relative_path),

      filename = basename(relative_path),

      extension = tools::file_ext(filename),

      study_folder = sapply(
        strsplit(folder, "/"),
        function(x) x[1]
      ),

      year = str_extract(
        study_folder,
        "(19|20)\\d{2}"
      ),

      author = str_replace(
        study_folder,
        "_etal.*|__.*",
        ""
      ),

      github_url = paste0(
        "https://github.com/AleAliSousa/Evo-M1-Trait-Data/blob/main/",
        relative_path
      )
    )
}

# --------------------------------------------------
# LOAD MERGED DATASETS
# --------------------------------------------------

load_merged_datasets <- function(repo_root){

  merge_dirs <- list.dirs(
    repo_root,
    recursive = FALSE,
    full.names = TRUE
  )

  merge_dirs <- merge_dirs[
    grepl("__merging", basename(merge_dirs))
  ]

  datasets <- list()

  for(dir in merge_dirs){

    cat("Reading:", basename(dir), "\n")

    files <- list.files(
      dir,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "_long\\.csv$"
    )

    for(f in files){

      dat <- tryCatch({

        if(grepl("\\.csv$", f, ignore.case = TRUE)){

          read_csv(
            f,
            show_col_types = FALSE
          )

        } else if(grepl("\\.tsv$", f, ignore.case = TRUE)){

          read_tsv(
            f,
            show_col_types = FALSE
          )

        } else {

          read_excel(f)

        }

      }, error = function(e){

        message(
          "Failed: ",
          basename(f)
        )

        NULL

      })

      if(is.null(dat))
        next

      dat <- janitor::clean_names(dat)
      
      # Force everything to character
      dat <- data.frame(
        lapply(dat, as.character),
        stringsAsFactors = FALSE
      )
      
      dat$source_dataset <- basename(f)
      dat$source_folder <- basename(dir)
      cat(
        "Loaded:",
        basename(f),
        "\n"
      )
      datasets[[length(datasets) + 1]] <- dat
    }
  }

  if(length(datasets) == 0){
    
    return(data.frame())
  }
  
  merged <- dplyr::bind_rows(
    lapply(
      datasets,
      function(x){
        
        x[] <- lapply(x, as.character)
        
        as.data.frame(
          x,
          stringsAsFactors = FALSE
        )
      }
    )
  )
  
  return(merged)
}  

# --------------------------------------------------
# LOAD DATA
# --------------------------------------------------

index_tbl <- build_index(repo_root)

merged_data <- load_merged_datasets(repo_root)
cat(
  "\nMerged rows:",
  nrow(merged_data),
  "\nMerged columns:",
  ncol(merged_data),
  "\n\n"
)
# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(

  titlePanel("Trait Explorer"),

  tabsetPanel(

    tabPanel(

      "Repository Explorer",

      sidebarLayout(

        sidebarPanel(

          textInput(
            "search",
            "Search files"
          ),

          selectInput(
            "extension",
            "File type",
            choices = c(
              "All",
              sort(unique(index_tbl$extension))
            )
          ),

          selectInput(
            "year",
            "Year",
            choices = c(
              "All",
              sort(unique(
                na.omit(index_tbl$year)
              ))
            )
          ),

          checkboxInput(
            "papers_only",
            "Paper folders only",
            FALSE
          )
        ),

        mainPanel(

          DTOutput("file_table"),

          br(),

          verbatimTextOutput("details")
        )
      )
    ),

    tabPanel(

      "Trait Search",

      textInput(
        "trait_search",
        "Search all merged datasets",
        placeholder =
          "Pan troglodytes, sleep, M1, gyrification..."
      ),

      DTOutput("trait_table")
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session){

  filtered <- reactive({

    dat <- index_tbl

    if(input$extension != "All"){

      dat <- dat %>%
        filter(
          extension ==
            input$extension
        )
    }

    if(input$year != "All"){

      dat <- dat %>%
        filter(
          year ==
            input$year
        )
    }

    if(nchar(input$search) > 0){

      query <- tolower(input$search)

      dat <- dat %>%
        filter(
          grepl(query,
                tolower(relative_path)) |
          grepl(query,
                tolower(filename)) |
          grepl(query,
                tolower(folder))
        )
    }

    if(input$papers_only){

      dat <- dat %>%
        filter(
          !startsWith(
            study_folder,
            "__"
          )
        )
    }

    dat
  })

  output$file_table <- renderDT({

    filtered() %>%
      select(
        Study = study_folder,
        Author = author,
        Year = year,
        File = filename,
        Type = extension,
        Folder = folder,
        URL = github_url
      )

  },
  options = list(
    pageLength = 25,
    scrollX = TRUE
  ))

  output$details <- renderPrint({

    row <- input$file_table_rows_selected

    if(length(row) == 0){

      return(
        "Select a file"
      )
    }

    filtered()[row, ]
  })

  trait_results <- reactive({
    
    req(merged_data)
    
    if(
      is.null(input$trait_search) ||
      trimws(input$trait_search) == ""
    ){
      return(head(merged_data, 100))
    }
    
    query <- tolower(trimws(input$trait_search))
    
    keep <- apply(
      merged_data,
      1,
      function(x){
        
        values <- tolower(
          as.character(x)
        )
        
        any(
          grepl(
            query,
            values,
            fixed = TRUE
          ),
          na.rm = TRUE
        )
      }
    )
    
    merged_data[keep, , drop = FALSE]
  })

  output$trait_table <- renderDT({
    
    datatable(
      trait_results(),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })


shinyApp(ui, server)
