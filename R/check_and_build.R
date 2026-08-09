# Entry point for .github/workflows/check-tidytuesday.yml
#
# Finds the newest TidyTuesday week upstream and, if it doesn't have a post
# yet, scaffolds posts/<date>/index.qmd via tt_build_post() (R/tt_helpers.R).
# The actual data fetching/plotting happens later, at `quarto render` time
# (see R/tt_helpers.R and .github/workflows/publish.yml) — this script only
# writes the thin qmd template. The workflow decides whether to commit based
# on `git status`, so this script just prints what it did and exits 0 either
# way.

suppressPackageStartupMessages({
  library(glue)
})

source("R/tt_helpers.R")

latest <- tt_latest_available_date()
if (is.null(latest)) {
  message("Could not determine the latest TidyTuesday week upstream. Nothing to do.")
  quit(status = 0)
}

built <- tt_build_post(latest)

if (isTRUE(built)) {
  # Let the calling workflow write a commit message that names the actual
  # TidyTuesday week date, not just "today".
  github_env <- Sys.getenv("GITHUB_ENV")
  if (nzchar(github_env)) {
    cat(glue("POST_DATE={latest}\n"), file = github_env, append = TRUE)
  }
}
