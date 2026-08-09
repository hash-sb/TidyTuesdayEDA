# Entry point for .github/workflows/backfill-tidytuesday.yml (manual only).
#
# Builds posts for every upstream TidyTuesday week whose own published date
# falls within [start_date, end_date] (inclusive). The dates you provide
# don't need to land exactly on a week's folder date — they're just the
# window's edges — which avoids having to know upstream's exact week
# numbering or folder dates ahead of time.
#
# Reads its inputs from env vars (set by the workflow from workflow_dispatch
# inputs) rather than args, to match how check_and_build.R is invoked:
#   TT_START_DATE  required, YYYY-MM-DD
#   TT_END_DATE    optional, YYYY-MM-DD, defaults to TT_START_DATE (single week)
#
# Only understands the meta.yaml + per-dataset .md dictionary format
# TidyTuesday adopted in late 2025 (first seen the week of 2025-12-02; weeks
# before that have meta.yaml but no per-dataset dictionary, just one combined
# readme.md). tt_week_is_ready() (R/tt_helpers.R) checks for the per-dataset
# .md files specifically, so older weeks are skipped by tt_build_post(), same
# as any other not-ready/unsupported week — check this script's log output
# for which requested weeks were actually built vs. skipped.

suppressPackageStartupMessages({
  library(stringr)
  library(glue)
  library(purrr)
})

source("R/tt_helpers.R")

parse_date_input <- function(raw, label) {
  raw <- str_trim(raw)
  if (!str_detect(raw, "^\\d{4}-\\d{2}-\\d{2}$")) {
    stop(glue("{label} must be in YYYY-MM-DD format, got: '{raw}'"))
  }
  parsed <- suppressWarnings(as.Date(raw))
  if (is.na(parsed)) {
    stop(glue("{label} is not a valid calendar date: '{raw}'"))
  }
  parsed
}

start_date <- parse_date_input(Sys.getenv("TT_START_DATE"), "start_date")

end_date_raw <- str_trim(Sys.getenv("TT_END_DATE"))
end_date <- if (nzchar(end_date_raw)) parse_date_input(end_date_raw, "end_date") else start_date

if (start_date > end_date) {
  stop(glue("start_date ({start_date}) must be <= end_date ({end_date})."))
}

if (end_date > Sys.Date()) {
  message(glue("end_date {end_date} is in the future; clamping to today ({Sys.Date()})."))
  end_date <- Sys.Date()
}

years <- as.integer(format(start_date, "%Y")):as.integer(format(end_date, "%Y"))
all_dates <- sort(unique(unlist(map(years, tt_list_week_dates))))
selected_dates <- all_dates[as.Date(all_dates) >= start_date & as.Date(all_dates) <= end_date]

if (length(selected_dates) == 0) {
  stop(glue("No TidyTuesday weeks found upstream between {start_date} and {end_date}."))
}

message(glue(
  "Backfilling {length(selected_dates)} week(s) between {start_date} and {end_date}: ",
  "{paste(selected_dates, collapse = ', ')}"
))

# One bad week (malformed meta.yaml, a transient network error) shouldn't
# throw away every other week already built in this run: an uncaught error
# here would abort the whole script, the workflow's commit step would never
# run (it doesn't use `if: always()`), and everything built so far in this
# run would be silently discarded, not just the failing week.
safe_build_post <- function(date) {
  tryCatch(
    tt_build_post(date),
    error = function(e) {
      message(glue("Failed to build post for {date}: {conditionMessage(e)}"))
      FALSE
    }
  )
}

results <- map_lgl(selected_dates, safe_build_post)
built_dates <- selected_dates[results]

message(glue("Built {length(built_dates)} of {length(selected_dates)} requested week(s)."))

github_env <- Sys.getenv("GITHUB_ENV")
if (nzchar(github_env)) {
  cat(glue("BACKFILL_SUMMARY={start_date} to {end_date} ({length(built_dates)} post(s))\n"),
      file = github_env, append = TRUE)
}
