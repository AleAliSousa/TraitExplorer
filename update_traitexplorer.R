# =============================================================================
# update_traitexplorer.R  —  one program to check, commit and publish
# TraitExplorer from your own computer. Run it after you change the app code
# (app.R, data_layer.R, structure_reconcile.R, ...) and it will:
#
#   0. EVO-M1  (optional, --with-evom1) rebuild Evo-M1-Trait-Data and update
#              its app first: run_all_scripts_v2.R, run_all_scripts_v3.py,
#              then update_shinyapp.R (build -> push -> deploy). Done FIRST so
#              the TraitExplorer test below reads the freshly pushed data.
#              Any failure stops everything.
#   1. TEST    run test_app.R headless against GitHub. If anything fails,
#              NOTHING is committed, pushed or deployed.
#   2. COMMIT  stage the app files (+ any other file git already tracks) and
#              commit them, then PUSH to GitHub (AleAliSousa/TraitExplorer)
#   3. DEPLOY  re-upload the app to shinyapps.io via deploy_traitexplorer.R
#              (needed whenever app code changes; a pure DATA change in
#              Evo-M1-Trait-Data never needs this -- the live app reads GitHub)
#
# Also checks that structure_reconcile.R is identical to the Evo-M1 app's copy
# (__ShinyApp/structure_reconcile.R) and warns if the two have drifted.
#
# ---------------------------------------------------------------------------
# HOW TO RUN IT
# ---------------------------------------------------------------------------
#   • Easiest — double-click  Update_TraitExplorer.command  in Finder.
#     It asks for a commit message (Return = a dated default).
#   • In a terminal:   Rscript update_traitexplorer.R --message "what changed"
#   • In R / RStudio:  source("/full/path/to/update_traitexplorer.R")
#
# ---------------------------------------------------------------------------
# OPTIONS  (append to the Rscript / launcher command, in any order)
# ---------------------------------------------------------------------------
#   --message "text"  commit message (also --message=text, or -m "text")
#   --with-evom1  first rebuild Evo-M1-Trait-Data and update its Shiny app
#                 (step 0; the launcher asks about this)
#   --no-test     skip test_app.R (not recommended)
#   --no-push     commit locally but do not push to GitHub
#   --no-deploy   commit + push only; skip the shinyapps.io upload
#   --no-commit   test (and deploy, unless --no-deploy) without committing
#   --all         stage EVERY change in the folder, including untracked files
#                 (by default only the app files + already-tracked files)
#   --help        show this list and exit
#
# Set EVOM1_REPO to the Evo-M1-Trait-Data folder if it is not at
# ~/Library/CloudStorage/OneDrive-AllenInstitute/Species/Evo-M1-Trait-Data
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

.step <- function(...) message("\n==> ", ...)
.ok   <- function(...) message("    \u2713 ", ...)
.warn <- function(...) message("    ! ", ...)
.die  <- function(...) stop(paste0(...), call. = FALSE)

# ---- flags --------------------------------------------------------------------
.argv <- commandArgs(trailingOnly = TRUE)
.self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1] %||% "")
if ("--help" %in% .argv || "-h" %in% .argv) {
  if (nzchar(.self)) cat(readLines(.self)[seq_len(45)], sep = "\n")
  quit(save = "no", status = 0)
}
DO_TEST   <- !("--no-test"   %in% .argv)
DO_COMMIT <- !("--no-commit" %in% .argv)
DO_PUSH   <- !("--no-push"   %in% .argv) && DO_COMMIT
DO_DEPLOY <- !("--no-deploy" %in% .argv)
STAGE_ALL <-  ("--all"       %in% .argv)
DO_EVOM1  <-  ("--with-evom1" %in% .argv)
EVOM1_REPO <- path.expand(Sys.getenv("EVOM1_REPO",
  "~/Library/CloudStorage/OneDrive-AllenInstitute/Species/Evo-M1-Trait-Data"))
MSG <- {
  m <- sub("^--message=", "", grep("^--message=", .argv, value = TRUE))
  i <- which(.argv %in% c("--message", "-m"))
  if (!length(m) && length(i) && i[1] < length(.argv)) m <- .argv[i[1] + 1]
  m <- trimws(m %||% "")
  if (length(m) && nzchar(m[1])) m[1] else
    paste0("Update TraitExplorer ", format(Sys.time(), "%Y-%m-%d %H:%M"))
}

# ---- locate the TraitExplorer folder (this script lives in it) -------------------
find_app <- function() {
  if (nzchar(.self)) return(normalizePath(dirname(.self)))                   # Rscript
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p)))
  }
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)                # source()
  if (!is.null(of) && nzchar(of)) return(normalizePath(dirname(of)))
  if (file.exists(file.path(getwd(), "data_layer.R"))) return(normalizePath(getwd()))
  .die("Could not locate the TraitExplorer folder. Double-click ",
       "Update_TraitExplorer.command, or run this script from inside that folder.")
}
APP <- find_app()
if (!all(file.exists(file.path(APP, c("app.R", "data_layer.R")))))
  .die("app.R / data_layer.R not found in: ", APP)
message("TraitExplorer folder: ", APP)
message("Commit message: ", MSG)

R_BIN <- file.path(R.home("bin"), "Rscript")
git <- function(...) suppressWarnings(
  system2("git", c("-C", shQuote(APP), ...), stdout = TRUE, stderr = TRUE))
git_ok <- function(out) is.null(attr(out, "status")) || attr(out, "status") == 0

# ---- 0. (optional) rebuild Evo-M1-Trait-Data and update its app -----------------
run_r <- function(path) {
  runner <- path.expand(path)
  if (!file.exists(runner)) stop("Runner not found: ", runner, call. = FALSE)
  message("\n=== Running: ", runner, " ===")
  status <- system2(R_BIN, c("--vanilla", shQuote(runner)))
  if (!identical(status, 0L)) {
    stop("Runner failed (exit status ", status, "): ", runner, call. = FALSE)
  }
}

run_py <- function(path, python = "python3") {
  script <- path.expand(path)
  if (!file.exists(script)) stop("Python script not found: ", script, call. = FALSE)
  message("\n=== Running: ", script, " ===")
  status <- system2(python, c(shQuote(script)))
  if (!identical(status, 0L)) {
    stop("Python runner failed (exit status ", status, "): ", script, call. = FALSE)
  }
}

if (DO_EVOM1) {
  .step("0/3  Rebuilding Evo-M1-Trait-Data and updating its app (", EVOM1_REPO, ") …")
  if (!dir.exists(EVOM1_REPO))
    .die("Evo-M1-Trait-Data folder not found: ", EVOM1_REPO,
         "\n    Set EVOM1_REPO to its location, or run without --with-evom1.")
  tryCatch({
    run_r(file.path(EVOM1_REPO, "run_all_scripts_v2.R"))
    run_py(file.path(EVOM1_REPO, "run_all_scripts_v3.py"))
    run_r(file.path(EVOM1_REPO, "update_shinyapp.R"))
  }, error = function(e) .die(conditionMessage(e),
       "\n    Stopped before TraitExplorer: nothing in TraitExplorer was tested, ",
       "committed or deployed."))
  .ok("Evo-M1 data rebuilt, pushed and its app updated.")
}

# ---- structure_reconcile.R must match the Evo-M1 app's copy ----------------------
.step("Checking structure_reconcile.R against the Evo-M1 app's copy …")
evo_candidates <- unique(c(
  file.path(EVOM1_REPO, "__ShinyApp"),
  file.path(dirname(APP), "Species", "Evo-M1-Trait-Data", "__ShinyApp"),
  file.path(dirname(APP), "Evo-M1-Trait-Data", "__ShinyApp"),
  Sys.glob(file.path(path.expand("~/Library/CloudStorage"), "*", "*", "Evo-M1-Trait-Data", "__ShinyApp")),
  Sys.glob(file.path(path.expand("~/Library/CloudStorage"), "*", "*", "*", "Evo-M1-Trait-Data", "__ShinyApp"))
))
evo_copy <- file.path(evo_candidates, "structure_reconcile.R")
evo_copy <- evo_copy[file.exists(evo_copy)][1]
mine <- file.path(APP, "structure_reconcile.R")
if (!file.exists(mine)) {
  .warn("structure_reconcile.R is missing from TraitExplorer; the Structure Profile ",
        "and Trait Reconciliation tabs need it.")
} else if (is.na(evo_copy)) {
  .warn("Could not find the Evo-M1 app's copy to compare (set EVOM1_REPO to the ",
        "Evo-M1-Trait-Data folder). Skipping the drift check.")
} else if (identical(unname(tools::md5sum(mine)), unname(tools::md5sum(evo_copy)))) {
  .ok("Identical to ", evo_copy)
} else {
  .warn("structure_reconcile.R DIFFERS from ", evo_copy,
        "\n      The two apps share this file; copy the newer one over the other ",
        "before publishing. (Continuing.)")
}

# ---- 1. TEST ------------------------------------------------------------------
if (DO_TEST) {
  .step("1/3  Running test_app.R (reads Evo-M1-Trait-Data from GitHub; ~1 min) …")
  if (!file.exists(file.path(APP, "test_app.R"))) .die("test_app.R not found in ", APP)
  out <- suppressWarnings(system2(R_BIN, shQuote(file.path(APP, "test_app.R")),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  test_lines <- grep("^\\[TEST\\]", out, value = TRUE)
  cat(paste0("    ", test_lines), sep = "\n")
  sweep <- regmatches(out, regexpr("[0-9]+ failures", out))
  n_fail <- if (length(sweep)) sum(as.integer(sub(" failures", "", sweep))) else 0L
  build_err <- grep("REGION TABLE BUILD ERROR", out, value = TRUE)
  if (status != 0 || n_fail > 0 || length(build_err)) {
    message("\n---- test output (last 40 lines) ----")
    cat(utils::tail(out, 40), sep = "\n")
    .die("The test failed",
         if (n_fail > 0) paste0(" (", n_fail, " inspector failure(s))") else "",
         if (length(build_err)) " (region table could not be built)" else "",
         ". Nothing was committed, pushed or deployed.\n",
         "    Fix the problem (paste the output above to Copilot if useful) and re-run.")
  }
  if (any(grepl("missing columns", test_lines)))
    .warn("A merge file is missing a column (see above). Not blocking; run ",
          "`Rscript refresh_cache.R`, and if it persists push the data repo.")
  .ok("All checks passed.")
} else .step("1/3  Skipping the test (--no-test).")

# ---- 2. COMMIT + PUSH -------------------------------------------------------------
if (DO_COMMIT) {
  if (Sys.which("git") == "")
    .die("git is not installed. Install it from https://git-scm.com and re-run, ",
         "or use --no-commit.")
  if (!dir.exists(file.path(APP, ".git")))
    .die("No git repository in ", APP, ". (Expected a clone of AleAliSousa/TraitExplorer.)")
  .step("2/3  Committing", if (DO_PUSH) " + pushing to GitHub" else "", " …")

  # Launchers must stay executable for whoever clones the repo.
  launchers <- c("Update_TraitExplorer.command", "Deploy_TraitExplorer.command")
  for (l in launchers[file.exists(file.path(APP, launchers))]) Sys.chmod(file.path(APP, l), "755")

  if (STAGE_ALL) {
    invisible(git("add", "-A"))
  } else {
    app_files <- c("app.R", "data_layer.R", "structure_reconcile.R", "config.R",
                   "check_setup.R", "test_app.R", "refresh_cache.R", "run_app.R",
                   "deploy_traitexplorer.R", "update_traitexplorer.R", launchers,
                   "README.md", "TraitExplorer_fix_notes.txt", ".gitignore")
    app_files <- app_files[file.exists(file.path(APP, app_files))]
    invisible(git("add", "--", app_files))
    invisible(git("add", "-u"))                     # other files git already tracks
    for (l in launchers[file.exists(file.path(APP, launchers))])
      invisible(git("update-index", "--chmod=+x", l))
    untracked <- git("ls-files", "--others", "--exclude-standard")
    if (length(untracked)) {
      top <- unique(sub("/.*$", "/", untracked))
      .warn(length(untracked), " untracked file(s) NOT committed: ",
            paste(utils::head(top, 6), collapse = ", "),
            if (length(top) > 6) " …" else "",
            "\n      Add them to .gitignore, or re-run with --all to include them.")
    }
  }

  staged <- git("diff", "--cached", "--quiet")
  if (!git_ok(staged)) {
    cat(paste0("    ", git("diff", "--cached", "--stat")), sep = "\n")
    out <- git("commit", "-m", shQuote(MSG))
    if (!git_ok(out)) {
      cat(out, sep = "\n")
      .die("git commit failed (see above). If git asks who you are, run once:\n",
           '      git config --global user.name "Your Name"\n',
           '      git config --global user.email "you@alleninstitute.org"')
    }
    .ok("Committed: ", MSG)
  } else .ok("Nothing new to commit.")

  if (DO_PUSH) {
    has_up <- git_ok(git("rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{u}"))
    push <- function() if (has_up) git("push") else git("push", "-u", "origin", "HEAD")
    pushed <- FALSE
    for (attempt in 1:3) {
      out <- push()
      if (git_ok(out)) { pushed <- TRUE; break }
      if (any(grepl("non-fast-forward|fetch first|rejected", out))) {
        .warn("GitHub has commits this computer does not (pushed from elsewhere). ",
              "Rebasing onto them …")
        rb <- git("pull", "--rebase")
        if (!git_ok(rb)) {
          invisible(git("rebase", "--abort"))
          cat(rb, sep = "\n")
          .die("Your commit and the GitHub version change the same lines. Nothing was ",
               "pushed; your commit is safe locally.\n",
               "    Resolve it by hand (git pull --rebase, fix conflicts, git push) and re-run.")
        }
        next
      }
      .warn("push attempt ", attempt, " failed; retrying in ", 2^attempt, "s …")
      Sys.sleep(2^attempt)
    }
    if (pushed) .ok("Pushed to GitHub (AleAliSousa/TraitExplorer).")
    else {
      cat(out, sep = "\n")
      .warn("Could not push. Check your connection / GitHub credentials and run ",
            "`git push` in the TraitExplorer folder. (Deploy will still proceed.)")
    }
  } else .ok("Not pushed (--no-push).")
} else .step("2/3  Skipping commit (--no-commit).")

# ---- 3. DEPLOY ------------------------------------------------------------------
if (DO_DEPLOY) {
  .step("3/3  Deploying to shinyapps.io (deploy_traitexplorer.R) …")
  rc <- system2(R_BIN, shQuote(file.path(APP, "deploy_traitexplorer.R")))
  if (rc != 0) .die("Deploy failed (see above). The commit/push above still stands.")
  .ok("Deployed.")
} else .step("3/3  Skipping shinyapps.io deploy (--no-deploy).")

.step("All done \u2705")
