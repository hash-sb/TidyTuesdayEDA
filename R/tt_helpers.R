# Shared helpers for the TidyTuesday auto-publishing pipeline.
#
# Sourced both by R/check_and_build.R (finds the newest upstream week, scaffolds
# a post) and by every generated posts/<date>/index.qmd (fetches data + the
# variable dictionary and draws distribution plots at render time).

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(purrr)
  library(glue)
  library(scales)
  library(yaml)
})

TT_REPO <- "rfordatascience/tidytuesday"
TT_BLUE <- "#2a78d6"

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- GitHub access -----------------------------------------------------

tt_gh_api <- function(path) {
  req <- request(glue("https://api.github.com/repos/{TT_REPO}/contents/{path}")) |>
    req_headers(
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28"
    ) |>
    req_user_agent("tidytuesday-eda-site")

  token <- Sys.getenv("GITHUB_TOKEN")
  if (nzchar(token)) {
    req <- req_headers(req, Authorization = paste("Bearer", token))
  }

  resp_body_json(req_perform(req))
}

tt_raw_url <- function(date, filename) {
  year <- str_sub(date, 1, 4)
  glue("https://raw.githubusercontent.com/{TT_REPO}/main/data/{year}/{date}/{filename}")
}

# ---- Week discovery (used by R/check_and_build.R) -----------------------

# Newest YYYY-MM-DD week folder in the upstream repo that is not in the
# future, checking the current year and falling back to the previous year
# (handles the turn of a new year before that year's folder exists).
tt_latest_available_date <- function() {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  for (yr in c(this_year, this_year - 1)) {
    entries <- tryCatch(tt_gh_api(glue("data/{yr}")), error = function(e) NULL)
    if (is.null(entries)) next

    dirs <- keep(entries, ~ .x$type == "dir" && str_detect(.x$name, "^\\d{4}-\\d{2}-\\d{2}$"))
    dates <- map_chr(dirs, "name")
    dates <- dates[as.Date(dates) <= Sys.Date()]
    if (length(dates) > 0) return(max(dates))
  }
  NULL
}

tt_week_files <- function(date) {
  year <- str_sub(date, 1, 4)
  tt_gh_api(glue("data/{year}/{date}"))
}

tt_dataset_basenames <- function(week_files) {
  names_ <- map_chr(week_files, "name")
  csvs <- names_[str_detect(names_, "\\.csv$")]
  str_remove(csvs, "\\.csv$")
}

# A week is only safe to build once it has metadata, at least one dataset,
# AND a per-dataset `<name>.md` data dictionary for every dataset — that
# dictionary is where variable descriptions come from, and it's only been
# part of the upstream format since late 2025. Older weeks have meta.yaml
# and CSVs but no per-dataset dictionary (just one combined readme.md), so
# checking for meta.yaml alone was wrongly treating them as buildable and
# silently producing posts with every subtitle blank.
tt_week_is_ready <- function(week_files) {
  names_ <- map_chr(week_files, "name")
  basenames <- tt_dataset_basenames(week_files)
  if (!("meta.yaml" %in% names_) || length(basenames) == 0) return(FALSE)
  all(glue("{basenames}.md") %in% names_)
}

# ---- Post scaffolding (used by R/check_and_build.R and R/backfill.R) -----

# Scaffolds posts/<date>/index.qmd for a given upstream week, unless it
# already exists or the week isn't fully populated upstream yet. Returns
# TRUE if a post was written, FALSE if it was skipped (already existed / not
# ready / not found upstream) — never errors on a merely-missing week, so a
# caller can loop this over many dates.
tt_build_post <- function(date) {
  post_dir <- file.path("posts", date)
  if (dir.exists(post_dir)) {
    message(glue("Week {date} already has a post at {post_dir}/. Skipping."))
    return(invisible(FALSE))
  }

  week_files <- tryCatch(tt_week_files(date), error = function(e) NULL)
  if (is.null(week_files) || !tt_week_is_ready(week_files)) {
    message(glue("Week {date} isn't available upstream, or isn't fully populated yet ",
                  "(no meta.yaml/.csv — older TidyTuesday weeks used a different format ",
                  "that this pipeline doesn't parse). Skipping."))
    return(invisible(FALSE))
  }

  basenames <- tt_dataset_basenames(week_files)
  message(glue("Building post for {date}: datasets = {paste(basenames, collapse = ', ')}"))

  meta <- tryCatch(read_yaml(tt_raw_url(date, "meta.yaml")), error = function(e) list())
  intro_text <- tryCatch(
    paste(readLines(tt_raw_url(date, "intro.md"), warn = FALSE), collapse = "\n"),
    error = function(e) ""
  )

  title <- as.character(meta$title %||% glue("TidyTuesday {date}"))

  frontmatter <- list(
    title = title,
    date = date,
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
      "tt_dataset_section(\"{date}\", \"{basename}\")\n",
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
  invisible(TRUE)
}

# ---- Data dictionary parsing --------------------------------------------

# Parses a standard `knitr::kable` pipe table (the format TidyTuesday's
# per-dataset `<name>.md` files use) into a variable/class/description
# tibble-like data.frame.
tt_parse_dictionary <- function(md_text) {
  empty <- data.frame(variable = character(), class = character(),
                       description = character(), stringsAsFactors = FALSE)
  if (!nzchar(str_trim(md_text))) return(empty)

  lines <- str_split(md_text, "\n")[[1]]
  lines <- lines[str_starts(str_trim(lines), "\\|")]
  if (length(lines) < 2) return(empty)

  split_row <- function(line) {
    cells <- str_split(line, "\\|")[[1]]
    # a well-formed "|a|b|c|" row has an empty string before the first and
    # after the last pipe; drop those, keep the real cells in between
    if (length(cells) > 0 && cells[1] == "") cells <- cells[-1]
    if (length(cells) > 0 && cells[length(cells)] == "") cells <- cells[-length(cells)]
    str_trim(cells)
  }

  rows <- map(lines, split_row)
  header <- str_to_lower(rows[[1]])
  body <- rows[-1]

  is_separator <- function(cells) all(str_detect(cells, "^:?-+:?$"))
  if (length(body) > 0 && is_separator(body[[1]])) body <- body[-1]

  body <- keep(body, ~ length(.x) == length(header))
  if (length(body) == 0) return(empty)

  df <- as.data.frame(do.call(rbind, body), stringsAsFactors = FALSE)
  names(df) <- header
  if (!all(c("variable", "class", "description") %in% names(df))) return(empty)
  df[, c("variable", "class", "description")]
}

tt_read_dictionary <- function(date, basename) {
  txt <- tryCatch(
    paste(readLines(tt_raw_url(date, glue("{basename}.md")), warn = FALSE), collapse = "\n"),
    error = function(e) ""
  )
  tt_parse_dictionary(txt)
}

tt_variable_description <- function(dict, variable) {
  hit <- dict$description[str_to_lower(dict$variable) == str_to_lower(variable)]
  if (length(hit) == 0 || is.na(hit[1]) || !nzchar(str_trim(hit[1]))) {
    return("No description provided.")
  }
  str_trim(hit[1])
}

# ---- Data access at render time -----------------------------------------

tt_read_csv <- function(date, basename) {
  read_csv(tt_raw_url(date, glue("{basename}.csv")), show_col_types = FALSE, guess_max = 50000)
}

# ---- Plot styling ---------------------------------------------------------

theme_tt <- function() {
  theme_minimal(base_size = 12) %+replace%
    theme(
      plot.title = element_text(face = "bold", size = rel(1.15), colour = "#0b0b0b",
                                 margin = margin(b = 4), hjust = 0),
      plot.subtitle = element_text(colour = "#52514e", size = rel(0.95),
                                    margin = margin(b = 10), hjust = 0),
      plot.caption = element_text(colour = "#898781", size = rel(0.75), hjust = 0),
      axis.title = element_text(colour = "#52514e", size = rel(0.85)),
      axis.text = element_text(colour = "#898781"),
      panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "#c3c2b7", linewidth = 0.4),
      plot.background = element_rect(fill = "#fcfcfb", colour = NA),
      panel.background = element_rect(fill = "#fcfcfb", colour = NA),
      legend.position = "none"
    )
}

# ---- Rendering one variable's distribution --------------------------------

tt_emit_note <- function(msg) {
  cat("\n\n::: {.tt-note}\n", msg, "\n:::\n\n", sep = "")
}

tt_render_variable <- function(x, variable, description) {
  n <- length(x)
  n_na <- sum(is.na(x))
  if (n_na == n) {
    tt_emit_note(glue("**{variable}** — all values are missing; distribution not shown."))
    return(invisible(NULL))
  }

  cls <- class(x)[1]
  p <- NULL
  note <- NULL

  if (cls %in% c("integer", "numeric", "double")) {
    d <- data.frame(x = x)
    p <- ggplot(d, aes(x = x)) +
      geom_histogram(fill = TT_BLUE, bins = 30, na.rm = TRUE) +
      labs(title = variable, subtitle = description, x = NULL, y = "count") +
      theme_tt()
  } else if (inherits(x, "Date") || inherits(x, "POSIXct")) {
    d <- data.frame(x = x)
    p <- ggplot(d, aes(x = x)) +
      geom_histogram(fill = TT_BLUE, bins = 30, na.rm = TRUE) +
      labs(title = variable, subtitle = description, x = NULL, y = "count") +
      theme_tt()
  } else if (cls == "logical") {
    d <- data.frame(x = factor(ifelse(is.na(x), "NA", as.character(x)),
                                levels = c("TRUE", "FALSE", "NA")))
    p <- ggplot(d, aes(x = x)) +
      geom_bar(fill = TT_BLUE, na.rm = TRUE) +
      labs(title = variable, subtitle = description, x = NULL, y = "count") +
      theme_tt()
  } else if (cls %in% c("character", "factor")) {
    xc <- as.character(x)
    xc <- xc[!is.na(xc)]
    n_unique <- length(unique(xc))

    if (n_unique > 30 && n_unique >= 0.9 * n) {
      note <- glue(
        "**{variable}** — {comma(n_unique)} unique values out of {comma(n)}; ",
        "looks like a free-text or identifier column, distribution not shown."
      )
    } else {
      counts <- sort(table(xc), decreasing = TRUE)
      caption <- NULL
      if (length(counts) > 30) {
        total_categories <- length(counts)
        top <- counts[1:15]
        other_n <- sum(counts[-(1:15)])
        counts <- c(top, `(other)` = other_n)
        caption <- glue("Top 15 of {total_categories} categories shown; the rest are grouped as \"(other)\".")
      }
      d <- data.frame(level = factor(names(counts), levels = names(counts)),
                       n = as.integer(counts))
      p <- ggplot(d, aes(x = level, y = n)) +
        geom_col(fill = TT_BLUE) +
        labs(title = variable, subtitle = description, x = NULL, y = "count", caption = caption) +
        theme_tt() +
        theme(axis.text.x = element_text(angle = 40, hjust = 1))
    }
  } else {
    note <- glue("**{variable}** — unsupported column type (`{cls}`); distribution not shown.")
  }

  if (!is.null(p)) {
    print(p)
  } else if (!is.null(note)) {
    tt_emit_note(note)
  }
  invisible(NULL)
}

# ---- One dataset section: called directly from a generated post ----------

tt_dataset_section <- function(date, basename) {
  df <- tryCatch(tt_read_csv(date, basename), error = function(e) NULL)
  if (is.null(df)) {
    tt_emit_note(glue("Could not load `{basename}.csv` for {date}."))
    return(invisible(NULL))
  }

  dict <- tt_read_dictionary(date, basename)

  cat(glue("\n\n{comma(nrow(df))} rows x {ncol(df)} columns.\n\n"))

  for (variable in names(df)) {
    description <- tt_variable_description(dict, variable)
    tt_render_variable(df[[variable]], variable, description)
  }
  invisible(NULL)
}
