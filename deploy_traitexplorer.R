#!/usr/bin/env Rscript
# deploy_traitexplorer.R
#
# Deploys TraitExplorer to shinyapps.io.
#
# Public trait data comes from Evo-M1-Trait-Data on GitHub at runtime (see
# data_layer.R). The restricted neuron-method audit stays local. Only a
# separately reviewed projection with an APPROVED, hash-verified manifest can
# enter this deployment bundle. Redeploy when app code or that projection changes.
#
# First-time setup (once per computer):
#   1. Log into https://www.shinyapps.io -> Account -> Tokens -> Show ->
#      "Show secret" -> Copy to clipboard.
#   2. Paste the copied rsconnect::setAccountInfo(name=..., token=..., secret=...)
#      line into an R console once. This is a login step only I (the agent)
#      cannot do for you -- it needs your account.
#
# Usage:  Rscript deploy_traitexplorer.R

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    "Install rsconnect first: install.packages('rsconnect')",
    call. = FALSE
  )
}

app_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(app_file) || !nzchar(app_file)) app_file <- "deploy_traitexplorer.R"
app_dir <- dirname(normalizePath(app_file, winslash = "/", mustWork = FALSE))

accounts <- tryCatch(rsconnect::accounts(), error = function(e) NULL)
if (is.null(accounts) || !nrow(accounts)) {
  stop(
    "No shinyapps.io account configured on this machine.\n\n",
    "Log into https://www.shinyapps.io -> Account -> Tokens -> Show ->\n",
    '"Show secret" -> Copy, then paste the copied rsconnect::setAccountInfo(...)\n',
    "line into an R console once. Re-run this script after that.",
    call. = FALSE
  )
}

message("Deploying TraitExplorer to shinyapps.io as account: ", accounts$name[[1]])

source(file.path(app_dir, "neuron_methods.R"), local = TRUE)
public_directory <- file.path(app_dir, "data", "neuron_methods_public")
public_check <- nm_public_manifest_check(public_directory)
app_files <- c("app.R", "data_layer.R", "neuron_methods.R", "structure_reconcile.R", "config.R")
if (isTRUE(public_check$approved)) {
  app_files <- c(app_files, nm_public_bundle_files(public_directory))
  message("Including approved, hash-verified published-source projection: ",
          paste(public_check$files, collapse = ", "))
} else {
  message("Deploying code only; Neuron methods will be absent online. ", public_check$reason)
}

rsconnect::deployApp(
  appDir = app_dir,
  appName = "TraitExplorer",
  appTitle = "TraitExplorer",
  appFiles = app_files,
  forceUpdate = TRUE
)

message(
  "Done. Public trait data still comes from GitHub. A change to app code\n",
  "or the approved neuron-method projection requires redeployment."
)
