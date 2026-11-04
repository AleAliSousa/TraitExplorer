#!/bin/bash
# =============================================================================
# Update TraitExplorer — macOS one-click launcher.
#
#   • DOUBLE-CLICK this file in Finder, or run:  bash Update_TraitExplorer.command
#
# It asks whether to rebuild Evo-M1-Trait-Data first, and for a commit message,
# then hands off to update_traitexplorer.R, which does
#   [Evo-M1 rebuild + app update] -> test -> commit -> push -> deploy
# stopping at the first failure.
# Pass options straight through, e.g.:  bash Update_TraitExplorer.command --no-deploy
#
# First time only: Finder may say the file "can't be opened". Right-click it ->
# Open -> Open, or run once:  chmod +x Update_TraitExplorer.command
# =============================================================================
set -uo pipefail

# app folder = the folder this launcher lives in (handles spaces in the path)
cd "$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
echo "TraitExplorer folder: $(pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "❌ Rscript not found. Install R from https://cran.r-project.org, then re-run."
  echo "   (Press Return to close.)"; read -r _ || true; exit 1
fi

args=("$@")
has_evo=0
for a in "$@"; do
  case "$a" in --with-evom1|--help|-h) has_evo=1 ;; esac
done
if [ "$has_evo" -eq 0 ]; then
  echo
  read -r -p "Also rebuild Evo-M1-Trait-Data and update its app first? This runs all build scripts and can take a while. [y/N] " evo || evo=""
  case "$evo" in [yY]*) args+=("--with-evom1") ;; esac
fi

has_msg=0
for a in "$@"; do
  case "$a" in --message|--message=*|-m|--no-commit|--help|-h) has_msg=1 ;; esac
done
if [ "$has_msg" -eq 0 ]; then
  echo
  read -r -p "Commit message (Return for a dated default): " msg || msg=""
  if [ -n "$msg" ]; then args+=("--message=$msg"); fi
fi

status=0
Rscript "./update_traitexplorer.R" "${args[@]}" || status=$?

echo
if [ "$status" -eq 0 ]; then echo "✅ Finished."; else echo "❌ Finished with errors (see above)."; fi
echo "(Press Return to close this window.)"
read -r _ || true
exit "$status"
