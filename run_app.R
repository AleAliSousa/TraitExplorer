# Convenience launcher for TraitExplorer.
# Run this file from R/RStudio, or use: Rscript run_app.R

app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(app_file) || !nzchar(app_file)) app_file <- "run_app.R"
app_dir <- dirname(normalizePath(app_file, winslash = "/", mustWork = FALSE))

shiny::runApp(app_dir, launch.browser = interactive())
