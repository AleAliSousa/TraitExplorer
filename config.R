# TraitExplorer configuration.
#
# Set TRAIT_DATA_REPO to override automatic discovery. The app will otherwise
# look for a local Evo-M1-Trait-Data repository in common locations.

GITHUB_OWNER  <- Sys.getenv("TRAIT_DATA_OWNER",     unset = "AleAliSousa")
GITHUB_REPO   <- Sys.getenv("TRAIT_DATA_REPO_NAME", unset = "Evo-M1-Trait-Data")
GITHUB_BRANCH <- Sys.getenv("TRAIT_DATA_BRANCH",    unset = "main")

# Where downloaded files are cached (mirrors the repo's folder layout). Never
# the user's local Evo-M1-Trait-Data checkout -- this is a disposable cache
# the app manages itself. Override with TRAITEXPLORER_CACHE_DIR if you want
# it somewhere other than TraitExplorer/.gh_cache.
# CACHE_DIR <- Sys.getenv("TRAITEXPLORER_CACHE_DIR", unset = file.path(app_dir, ".gh_cache"))
