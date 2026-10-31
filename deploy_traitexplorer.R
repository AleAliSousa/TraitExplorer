#!/usr/bin/env Rscript
# deploy_traitexplorer.R
#
# Deploys TraitExplorer to shinyapps.io.
#
# TraitExplorer has no local data of its own to push -- it reads
# Evo-M1-Trait-Data straight from GitHub at runtime (see data_layer.R), so
# the live app always sees current GitHub data with no redeploy needed for
# a pure data change. Redeploy only when app.R, data_layer.R, or config.R
# themselves change.
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

rsconnect::deployApp(
  appDir = app_dir,
  appName = "TraitExplorer",
  appTitle = "TraitExplorer",
  appFiles = c("app.R", "data_layer.R", "config.R"),
  forceUpdate = TRUE
)

message(
  "Done. TraitExplorer reads Evo-M1-Trait-Data live from GitHub, so a pure\n",
  "data change on GitHub needs no redeploy -- only a change to app.R,\n",
  "data_layer.R, or config.R does."
)
