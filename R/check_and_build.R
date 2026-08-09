# Entry point for .github/workflows/check-tidytuesday.yml
#
# Finds the newest TidyTuesday week upstream, and if it doesn't have a post
# yet, scaffolds posts/<date>/index.qmd. The actual data fetching/plotting
# happens later, at `quarto render` time (see R/tt_helpers.R and
# .github/workflows/publish.yml) — this script only writes the thin qmd
# template. The workflow decides whether to commit based on `git status`,
# so this script just prints what it did and exits 0 either way.

suppressPackageStartupMessages({
  library(stringr)
  library(glue)
  library(yaml)
  library(purrr)
})

source("R/tt_helpers.R")

latest <- tt_latest_available_date()
if (is.null(latest)) {
  message("Could not determine the latest TidyTuesday week upstream. Nothing to do.")
  quit(status = 0)
}

post_dir <- file.path("posts", latest)
if (dir.exists(post_dir)) {
  message(glue("Week {latest} already has a post at {post_dir}/. Nothing to do."))
  quit(status = 0)
}

week_files <- tt_week_files(latest)
if (!tt_week_is_ready(week_files)) {
  message(glue("Week {latest} exists upstream but isn't fully populated yet (no meta.yaml/.csv). Skipping."))
  quit(status = 0)
}

basenames <- tt_dataset_basenames(week_files)
message(glue("Building post for {latest}: datasets = {paste(basenames, collapse = ', ')}"))

meta <- tryCatch(
  read_yaml(tt_raw_url(latest, "meta.yaml")),
  error = function(e) list()
)

intro_text <- tryCatch(
  paste(readLines(tt_raw_url(latest, "intro.md"), warn = FALSE), collapse = "\n"),
  error = function(e) ""
)

title <- as.character(meta$title %||% glue("TidyTuesday {latest}"))

frontmatter <- list(
  title = title,
  date = latest,
  categories = as.list(str_to_title(str_replace_all(basenames, "_", " ")))
)
if (!is.null(meta$article$title)) {
  frontmatter$description <- meta$article$title
}

front_yaml <- as.yaml(frontmatter)

source_lines <- c()
if (!is.null(meta$data_source$title)) {
  src <- meta$data_source$title
  if (!is.null(meta$data_source$url)) {
    src <- glue("[{src}]({meta$data_source$url})")
  }
  source_lines <- c(source_lines, glue("**Data source:** {src}  "))
}
if (!is.null(meta$article$title)) {
  art <- meta$article$title
  if (!is.null(meta$article$url)) {
    art <- glue("[{art}]({meta$article$url})")
  }
  source_lines <- c(source_lines, glue("**Original article:** {art}  "))
}
if (!is.null(meta$credit$post)) {
  source_lines <- c(source_lines, glue("**Curated by:** {meta$credit$post}  "))
}

dataset_sections <- map_chr(basenames, function(basename) {
  heading <- str_to_title(str_replace_all(basename, "_", " "))
  glue(
    "## {heading}\n\n",
    "```{{r}}\n",
    "#| output: asis\n",
    "tt_dataset_section(\"{latest}\", \"{basename}\")\n",
    "```\n"
  )
})

body <- glue(
  "---\n{front_yaml}---\n\n",
  "```{{r}}\n",
  "#| include: false\n",
  "source(\"R/tt_helpers.R\")\n",
  "```\n\n",
  "{if (nzchar(intro_text)) intro_text else ''}\n\n",
  "{paste(source_lines, collapse = '\\n')}\n\n",
  "{paste(dataset_sections, collapse = '\\n')}"
)

dir.create(post_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(body, file.path(post_dir, "index.qmd"))

message(glue("Wrote {post_dir}/index.qmd"))

# Let the calling workflow write a commit message that names the actual
# TidyTuesday week date, not just "today".
github_env <- Sys.getenv("GITHUB_ENV")
if (nzchar(github_env)) {
  cat(glue("POST_DATE={latest}\n"), file = github_env, append = TRUE)
}
