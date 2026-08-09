# Entry point for .github/workflows/backfill-tidytuesday.yml (manual only).
#
# Builds posts for a range of past TidyTuesday weeks in a given year. "Week
# N" means the Nth weekly data folder published that year, in date order
# (i.e. the same numbering TidyTuesday's own per-year readme uses) — not an
# ISO calendar week number.
#
# Reads its inputs from env vars (set by the workflow from workflow_dispatch
# inputs) rather than args, to match how check_and_build.R is invoked:
#   TT_YEAR        required, e.g. "2024"
#   TT_START_WEEK  required, 1-based
#   TT_END_WEEK    optional, defaults to TT_START_WEEK (single week)
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

year <- str_trim(Sys.getenv("TT_YEAR"))
if (!str_detect(year, "^[0-9]{4}$")) {
  stop(glue("TT_YEAR must be a 4-digit year, got: '{year}'"))
}

this_year <- as.integer(format(Sys.Date(), "%Y"))
if (as.integer(year) < 2018 || as.integer(year) > this_year) {
  stop(glue("TT_YEAR {year} is outside TidyTuesday's history (2018-{this_year})."))
}

start_week <- suppressWarnings(as.integer(str_trim(Sys.getenv("TT_START_WEEK"))))
if (is.na(start_week) || start_week < 1) {
  stop(glue("TT_START_WEEK must be a positive integer, got: '{Sys.getenv('TT_START_WEEK')}'"))
}

end_week_raw <- str_trim(Sys.getenv("TT_END_WEEK"))
end_week <- if (nzchar(end_week_raw)) suppressWarnings(as.integer(end_week_raw)) else start_week
if (is.na(end_week)) {
  stop(glue("TT_END_WEEK must be a positive integer if set, got: '{end_week_raw}'"))
}
if (start_week > end_week) {
  stop(glue("start_week ({start_week}) must be <= end_week ({end_week})."))
}

week_entries <- tryCatch(tt_gh_api(glue("data/{year}")), error = function(e) NULL)
if (is.null(week_entries)) {
  stop(glue("Could not list data/{year} upstream — check the year is correct."))
}

dirs <- keep(week_entries, ~ .x$type == "dir" && str_detect(.x$name, "^\\d{4}-\\d{2}-\\d{2}$"))
dates <- sort(map_chr(dirs, "name"))
dates <- dates[as.Date(dates) <= Sys.Date()]

n_weeks <- length(dates)
if (n_weeks == 0) {
  stop(glue("No weekly data folders found under data/{year}."))
}
if (start_week > n_weeks) {
  stop(glue("start_week {start_week} is out of range — {year} only has {n_weeks} week(s)."))
}
if (end_week > n_weeks) {
  message(glue("end_week {end_week} exceeds {year}'s {n_weeks} week(s); clamping to {n_weeks}."))
  end_week <- n_weeks
}

selected_dates <- dates[start_week:end_week]
message(glue(
  "{year} has {n_weeks} week(s) upstream. Backfilling weeks {start_week}-{end_week}: ",
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
  cat(glue("BACKFILL_SUMMARY={year} weeks {start_week}-{end_week} ({length(built_dates)} post(s))\n"),
      file = github_env, append = TRUE)
}
