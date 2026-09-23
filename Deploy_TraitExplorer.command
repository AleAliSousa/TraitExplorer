#!/bin/bash
# =============================================================================
# Deploy TraitExplorer to shinyapps.io — macOS one-click launcher.
#
#   • DOUBLE-CLICK this file in Finder, or run:  bash Deploy_TraitExplorer.command
#
# It just hands off to deploy_traitexplorer.R.
#
# First time only: Finder may say the file "can't be opened". Right-click it ->
# Open -> Open, or run once:  chmod +x Deploy_TraitExplorer.command
# =============================================================================
set -euo pipefail

# app dir = the folder this launcher lives in (handles spaces in the path)
cd "$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"
echo "App dir: $(pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "❌ Rscript not found. Install R from https://cran.r-project.org, then re-run."
  echo "   (Press Return to close.)"; read -r _ || true; exit 1
fi

Rscript "./deploy_traitexplorer.R"
status=$?

echo
if [ "$status" -eq 0 ]; then echo "✅ Finished."; else echo "❌ Finished with errors (see above)."; fi
echo "(Press Return to close this window.)"
read -r _ || true
exit "$status"
