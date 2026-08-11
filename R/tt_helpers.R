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

# ---- Week discovery (used by R/check_and_build.R and R/backfill.R) -------

# All YYYY-MM-DD week folder names published under data/<year> upstream,
# sorted ascending. Returns character(0) if the year doesn't exist upstream
# (e.g. before TidyTuesday started, or a typo) rather than erroring, so
# callers can just combine results across several years.
tt_list_week_dates <- function(year) {
  entries <- tryCatch(tt_gh_api(glue("data/{year}")), error = function(e) NULL)
  if (is.null(entries)) return(character())
  dirs <- keep(entries, ~ .x$type == "dir" && str_detect(.x$name, "^\\d{4}-\\d{2}-\\d{2}$"))
  sort(map_chr(dirs, "name"))
}

# Newest week folder in the upstream repo that is not in the future,
# checking the current year and falling back to the previous year (handles
# the turn of a new year before that year's folder exists).
tt_latest_available_date <- function() {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  for (yr in c(this_year, this_year - 1)) {
    dates <- tt_list_week_dates(yr)
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

  # No categories field: dataset names were unique per week almost by
  # definition, so the category filter/badges never actually grouped more
  # than one post together -- pure clutter, not a useful browsing aid.
  frontmatter <- list(
    title = title,
    date = date
  )
  if (!is.null(meta$article$title)) {
    frontmatter$description <- meta$article$title
  }
  # meta.yaml often ships one sample community visualization for the week;
  # use it as this post's card thumbnail on the grid listing (index.qmd) if
  # present. Not every week has one -- those cards just render without an
  # image, still fine in the grid layout.
  if (!is.null(meta$images) && length(meta$images) > 0 && !is.null(meta$images[[1]]$file)) {
    frontmatter$image <- tt_raw_url(date, meta$images[[1]]$file)
    if (!is.null(meta$images[[1]]$alt)) {
      frontmatter[["image-alt"]] <- meta$images[[1]]$alt
    }
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

  # The "## <heading>" for each dataset is emitted by tt_dataset_section()
  # itself (not written here as static markdown) so it can include the
  # dataset's row/column count, which is only known once the CSV is
  # actually fetched at render time.
  dataset_sections <- map_chr(basenames, function(basename) {
    heading <- str_to_title(str_replace_all(basename, "_", " "))
    glue(
      "```{{r}}\n",
      "#| output: asis\n",
      "tt_dataset_section(\"{date}\", \"{basename}\", \"{heading}\")\n",
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
                                 margin = margin(b = 4), hjust = 0, lineheight = 1.1),
      plot.subtitle = element_text(colour = "#52514e", size = rel(0.95),
                                    margin = margin(b = 10), hjust = 0, lineheight = 1.15),
      plot.caption = element_text(colour = "#898781", size = rel(0.75), hjust = 0, lineheight = 1.1),
      axis.title = element_text(colour = "#52514e", size = rel(0.85)),
      axis.text = element_text(colour = "#898781"),
      panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "#c3c2b7", linewidth = 0.4),
      plot.background = element_rect(fill = "#fcfcfb", colour = NA),
      panel.background = element_rect(fill = "#fcfcfb", colour = NA),
      legend.position = "none",
      # A little extra right-hand breathing room so the last axis tick's
      # label (e.g. a wide number on a histogram's x-axis) doesn't get
      # clipped at the panel edge -- confirmed by an actual render, not a
      # blind guess: ref_period_id's rightmost "20250000" tick was visibly
      # cut off to "202500" without this.
      plot.margin = margin(t = 6, r = 16, b = 6, l = 6)
    )
}

# ---- Rendering one variable's distribution --------------------------------

tt_emit_note <- function(msg) {
  cat("\n\n::: {.tt-note}\n", msg, "\n:::\n\n", sep = "")
}

# Subtitles are variable descriptions pulled verbatim from TidyTuesday's data
# dictionary and can run to a full sentence; ggplot doesn't auto-wrap title/
# subtitle text to the plot width, so long ones get clipped/overflow unless
# wrapped into explicit line breaks first. 90 was picked blind before this
# pipeline could be rendered locally and was confirmed too wide by an actual
# render: an 84-character single line (no break inserted, since it was under
# the old width) visibly overflowed past the plot's right edge. 65 leaves
# real margin rather than sitting right at the failure boundary.
TT_WRAP_WIDTH <- 65

tt_wrap <- function(x, width = TT_WRAP_WIDTH) {
  if (is.null(x)) return(NULL)
  str_wrap(x, width = width)
}

tt_fmt_num <- function(x) {
  if (is.na(x)) return("NA")
  format(signif(x, 3), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# NULL (not "") when there's nothing missing, so callers can drop it from a
# caption with c(...) without leaving a stray separator.
tt_missing_stat <- function(n_na, n) {
  if (n_na == 0) return(NULL)
  glue("{round(100 * n_na / n)}% missing ({comma(n_na)} of {comma(n)})")
}

TT_LABEL_WIDTH <- 20

# Fixed-width category axis labels: right-aligned within TT_LABEL_WIDTH
# characters (space-padded on the left), truncated with an ellipsis if
# longer -- every categorical chart's labels occupy exactly the same width
# instead of each chart's left edge being as ragged as its longest category
# name happens to be. Only formats the axis label; caption text (e.g. "Most
# common: ...") keeps the real, untruncated category name.
tt_fixed_width_label <- function(x, width = TT_LABEL_WIDTH) {
  x <- str_trunc(x, width = width, side = "right", ellipsis = "...")
  str_pad(x, width = width, side = "left")
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
  fig_height <- NULL
  missing_stat <- tt_missing_stat(n_na, n)

  if (cls %in% c("integer", "numeric", "double")) {
    d <- data.frame(x = x)
    stats <- glue(
      "Mean: {tt_fmt_num(mean(x, na.rm = TRUE))} · ",
      "Median: {tt_fmt_num(median(x, na.rm = TRUE))} · ",
      "SD: {tt_fmt_num(sd(x, na.rm = TRUE))}"
    )
    caption <- paste(c(stats, missing_stat), collapse = " · ")
    p <- ggplot(d, aes(x = x)) +
      geom_histogram(fill = TT_BLUE, bins = 30, na.rm = TRUE) +
      labs(title = variable, subtitle = tt_wrap(description), x = NULL, y = "count",
           caption = tt_wrap(caption)) +
      # Bars sit flush on a zero baseline (no expansion below zero) with a
      # little headroom above the tallest bar; gridlines only run parallel to
      # the count axis -- vertical ones between bins don't align with
      # anything meaningful and are just visual noise.
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
      theme_tt() +
      theme(panel.grid.major.x = element_blank())
  } else if (inherits(x, "Date") || inherits(x, "POSIXct")) {
    d <- data.frame(x = x)
    stats <- glue("Range: {format(min(x, na.rm = TRUE))} to {format(max(x, na.rm = TRUE))}")
    caption <- paste(c(stats, missing_stat), collapse = " · ")
    p <- ggplot(d, aes(x = x)) +
      geom_histogram(fill = TT_BLUE, bins = 30, na.rm = TRUE) +
      labs(title = variable, subtitle = tt_wrap(description), x = NULL, y = "count",
           caption = tt_wrap(caption)) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
      theme_tt() +
      theme(panel.grid.major.x = element_blank())
  } else if (cls == "logical") {
    d <- data.frame(x = factor(ifelse(is.na(x), "NA", as.character(x)),
                                levels = c("TRUE", "FALSE", "NA")))
    p <- ggplot(d, aes(x = x)) +
      geom_bar(fill = TT_BLUE, na.rm = TRUE) +
      labs(title = variable, subtitle = tt_wrap(description), x = NULL, y = "count",
           caption = tt_wrap(missing_stat)) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
      theme_tt() +
      theme(panel.grid.major.x = element_blank())
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
      mode_val <- names(counts)[1]
      cat_stats <- glue("{comma(n_unique)} categories · Most common: \"{mode_val}\"")

      trunc_note <- NULL
      if (length(counts) > 30) {
        total_categories <- length(counts)
        top <- counts[1:15]
        other_n <- sum(counts[-(1:15)])
        # Deliberately pinned last rather than sorted by size: "(other)" is a
        # catch-all summary row, not a real category competing on its own
        # merits, so it reads as "everything else" regardless of how its
        # magnitude compares to the top 15 shown above it.
        counts <- c(top, `(other)` = other_n)
        trunc_note <- glue("top 15 of {total_categories} categories shown, rest grouped as \"(other)\"")
      }
      caption <- paste(c(cat_stats, trunc_note, missing_stat), collapse = " · ")

      # Always horizontal (labels on the y-axis, read left to right, largest
      # on top) rather than switching to vertical bars for short labels --
      # one consistent chart shape for every categorical variable on the
      # site, instead of the shape varying per-variable by label length.
      # factor()'s levels must be unique, but two different category names
      # can truncate to the identical fixed-width label -- e.g. "United
      # States of America" and "United States of Europe" both collapse to
      # "United States of ...". Keying the factor on row position (always
      # unique) rather than the label text itself avoids that crash; labels=
      # supplies the (possibly duplicated) display text separately.
      labels <- tt_fixed_width_label(names(counts))
      d <- data.frame(
        level = factor(seq_along(labels), levels = rev(seq_along(labels)), labels = rev(labels)),
        n = as.integer(counts)
      )

      # Vertical space scales with the number of bars shown (up to 16, after
      # top-15-plus-other truncation) so bars stay a consistent thickness
      # instead of a 3-category chart and a 16-category chart rendering at
      # the same fixed height.
      fig_height <- max(2.5, 0.9 + 0.35 * nrow(d))

      p <- ggplot(d, aes(x = n, y = level)) +
        geom_col(fill = TT_BLUE) +
        labs(title = variable, subtitle = tt_wrap(description), x = "count", y = NULL,
             caption = tt_wrap(caption)) +
        scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
        theme_tt() +
        theme(
          # Fixed-width padding only lines up visually under a monospace font
          # -- under the site's normal proportional font, equal character
          # counts don't render to equal pixel widths.
          axis.text.y = element_text(family = "mono"),
          # Gridlines only run parallel to the count axis (vertical here);
          # one horizontal line per category is redundant with the bars
          # themselves and just adds noise.
          panel.grid.major.y = element_blank()
        )
    }
  } else {
    note <- glue("**{variable}** — unsupported column type (`{cls}`); distribution not shown.")
  }

  if (!is.null(p)) {
    if (!is.null(fig_height)) {
      # Overrides the figure height for just this plot -- knitr reads
      # opts_current at capture time, so this only affects the very next
      # plot printed in the chunk, not the whole loop tt_dataset_section()
      # runs through.
      knitr::opts_current$set(fig.height = fig_height)
    }
    print(p)
  } else if (!is.null(note)) {
    tt_emit_note(note)
  }
  invisible(NULL)
}

# ---- One dataset section: called directly from a generated post ----------

# Emits the "## <heading>" itself (rather than the qmd template writing it as
# static markdown) so the row x column count can be appended right after the
# title, and so it's always preceded by a blank line — guaranteeing it's
# recognized as a real H2 regardless of whatever content came right before it
# in the rendered output.
tt_dataset_section <- function(date, basename, heading) {
  df <- tryCatch(tt_read_csv(date, basename), error = function(e) NULL)
  if (is.null(df)) {
    cat(glue("\n\n## {heading}\n\n"))
    tt_emit_note(glue("Could not load `{basename}.csv` for {date}."))
    return(invisible(NULL))
  }

  dict <- tt_read_dictionary(date, basename)

  cat(glue("\n\n## {heading} ({comma(nrow(df))}x{ncol(df)})\n\n"))

  for (variable in names(df)) {
    description <- tt_variable_description(dict, variable)
    tt_render_variable(df[[variable]], variable, description)
  }
  invisible(NULL)
}
