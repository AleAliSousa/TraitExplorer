# https://github.com/AleAliSousa/Evo-M1-Trait-Data
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(purrr)

# --------------------------------------------------
# CONFIG
# --------------------------------------------------

repo_root <- "Evo-M1-Trait-Data"

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
    full_path = files,
    relative_path = gsub(
      paste0("^", normalizePath(repo_root), "/?"),
      "",
      normalizePath(files)
    )
  ) %>%
    mutate(
      folder = dirname(relative_path),
      filename = basename(relative_path),
      extension = tools::file_ext(filename),
      
      study_folder = str_split(folder, "/", simplify = TRUE)[,1],
      
      year = str_extract(study_folder, "(19|20)\\d{2}"),
      
      author = str_replace(study_folder,
                           "_etal.*|__.*",
                           ""),
      
      github_url = paste0(
        "https://github.com/AleAliSousa/Evo-M1-Trait-Data/blob/main/",
        relative_path
      )
    )
}

index_tbl <- build_index(repo_root)

# Add a tab before the UI
library(readr)
library(readxl)
library(janitor)

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
    
    files <- list.files(
      dir,
      pattern = "\\.(csv|tsv|xlsx)$",
      full.names = TRUE,
      recursive = TRUE
    )
    
    for(f in files){
      
      dat <- tryCatch({
        
        if(grepl("\\.csv$", f, ignore.case = TRUE))
          read_csv(f, show_col_types = FALSE)
        
        else if(grepl("\\.tsv$", f, ignore.case = TRUE))
          read_tsv(f, show_col_types = FALSE)
        
        else
          read_excel(f)
        
      }, error = function(e) NULL)
      
      if(is.null(dat))
        next
      
      dat <- janitor::clean_names(dat)
      
      dat$source_dataset <- basename(f)
      dat$source_folder <- basename(dir)
      
      datasets[[length(datasets)+1]] <- dat
    }
  }
  
  bind_rows(datasets)
}

merged_data <- load_merged_datasets(repo_root)

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Evo-M1 Trait Database Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        "search",
        "Search",
        placeholder = "species, trait, author, file..."
      ),
      
      selectInput(
        "extension",
        "File Type",
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
          sort(unique(na.omit(index_tbl$year)))
        )
      ),
      
      checkboxInput(
        "papers_only",
        "Show paper folders only",
        FALSE
      ),
      
      width = 3
    ),
    
    tabsetPanel(
      
      tabPanel(
        "Repository Explorer",
        DTOutput("file_table"),
        verbatimTextOutput("details")
      ),
      
      tabPanel(
        "Trait Search",
        
        textInput(
          "trait_search",
          "Search merged datasets",
          placeholder = "species, trait, value..."
        ),
        
        DTOutput("trait_table")
      )
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
        filter(extension == input$extension)
    }
    
    if(input$year != "All"){
      dat <- dat %>%
        filter(year == input$year)
    }
    
    if(nchar(input$search) > 0){
      
      pat <- tolower(input$search)
      
      dat <- dat %>%
        filter(
          grepl(pat, tolower(relative_path)) |
            grepl(pat, tolower(filename)) |
            grepl(pat, tolower(folder))
        )
    }
    
    if(input$papers_only){
      
      dat <- dat %>%
        filter(!startsWith(study_folder, "__"))
    }
    
    dat
  })
  
  output$nfiles <- renderUI({
    h3(
      paste("Files:", nrow(filtered()))
    )
  })
  
  output$nstudies <- renderUI({
    h3(
      paste(
        "Studies:",
        n_distinct(filtered()$study_folder)
      )
    )
  })
  
  output$nextensions <- renderUI({
    h3(
      paste(
        "Types:",
        n_distinct(filtered()$extension)
      )
    )
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
  escape = FALSE,
  selection = "single",
  options = list(
    pageLength = 25,
    scrollX = TRUE
  ))
  
  output$details <- renderPrint({
    
    row <- input$file_table_rows_selected
    
    if(length(row) == 0)
      return("Select a file")
    
    filtered()[row,]
  })
}
##
trait_results <- reactive({
  
  req(input$trait_search)
  
  query <- tolower(input$trait_search)
  
  merged_data %>%
    filter(
      
      apply(
        .,
        1,
        function(x){
          
          any(
            grepl(
              query,
              tolower(as.character(x)),
              fixed = TRUE
            ),
            na.rm = TRUE
          )
        }
      )
    )
})
##
output$trait_table <- renderDT({
  
  trait_results()
  
},
options = list(
  pageLength = 25,
  scrollX = TRUE
))
##
species_columns <- c(
  "species",
  "taxon",
  "scientific_name",
  "binomial",
  "genus_species"
)
##
selectizeInput(
  "species",
  "Species",
  choices = sort(unique(all_species)),
  multiple = TRUE
)

selectInput(
  "dataset",
  "Dataset",
  choices = unique(merged_data$source_folder)
)
shinyApp(ui, server)