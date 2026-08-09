# TidyTuesday EDA

An automatically updating [Quarto](https://quarto.org) portfolio site built on
top of [TidyTuesday](https://github.com/rfordatascience/tidytuesday). A new
page is generated for every weekly TidyTuesday challenge, with one section per
dataset and one distribution plot per variable — histogram or bar chart
depending on the variable's type, titled with the variable name and subtitled
with its official data-dictionary description.

## How it works

- **`.github/workflows/check-tidytuesday.yml`** polls the upstream
  TidyTuesday repo every 4 hours (and can be run manually). When it finds a
  week that doesn't have a page yet, `R/check_and_build.R` scaffolds
  `posts/<date>/index.qmd` and the workflow commits it to `main`.
- **`.github/workflows/publish.yml`** runs on every push to `main`. It renders
  the whole site with Quarto and deploys it to GitHub Pages. Posts fetch their
  data straight from `raw.githubusercontent.com` at render time (see
  `R/tt_helpers.R`), so no raw CSVs are stored in this repo — only the
  generated `.qmd` files and Quarto's `_freeze/` render cache (kept so
  already-built weeks aren't re-downloaded/re-rendered on every run).
- **`.github/workflows/backfill-tidytuesday.yml`** is manual-only
  (**Actions → Backfill past TidyTuesday challenges → Run workflow**). It
  prompts for a `year` and a `start_week`/`end_week` range and builds a post
  for each week in that range, the same way the weekly poller does. "Week N"
  means the Nth weekly data folder published that year (date order), matching
  the numbering in TidyTuesday's own per-year readme — not an ISO calendar
  week. Leave `end_week` blank to build a single week.

  This only understands the `meta.yaml` + per-dataset `.md` dictionary format
  TidyTuesday has used since 2025 (see `R/tt_helpers.R`). Weeks from earlier
  years use a different layout (a single `readme.md` with all dictionaries
  embedded) and are skipped, not converted — check the Action's run log for
  which weeks were actually built.

Both `check_and_build.R` and `backfill.R` scaffold posts through the same
`tt_build_post()` function in `R/tt_helpers.R`, so a manually backfilled post
and a normally-detected weekly post are identical in structure.

## Local development

Requires [R](https://www.r-project.org/), the packages listed in
`DESCRIPTION`, and [Quarto](https://quarto.org/docs/get-started/).

```sh
Rscript -e 'install.packages("pak"); pak::local_install_deps()'
Rscript R/check_and_build.R   # scaffold the newest missing week, if any
quarto preview                # live-reload local preview
```

## One-time repo setup

1. Push this repo to GitHub.
2. Settings → **Actions → General → Workflow permissions** → "Read and write
   permissions" (`check-tidytuesday.yml` commits directly to `main`).
3. Settings → **Pages → Build and deployment → Source** → "GitHub Actions".
4. Run `check-tidytuesday.yml` once via **Actions → Run workflow** to build
   the first post.
