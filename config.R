# TraitExplorer configuration.
#
# Set TRAIT_DATA_REPO to override automatic discovery. The app will otherwise
# look for a local Evo-M1-Trait-Data repository in common locations.

DATA_REPO <- Sys.getenv("TRAIT_DATA_REPO", unset = "")

GITHUB_DATA_REPO <- "https://github.com/AleAliSousa/Evo-M1-Trait-Data/blob/main"
