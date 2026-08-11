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
- **`.github/workflows/publish.yml`** renders the whole site with Quarto and
  deploys it to GitHub Pages. It runs on `workflow_dispatch` and on pushes to
  `main`, but a push made with a workflow's default `GITHUB_TOKEN` does
  **not** trigger other workflows' `push` events (a GitHub Actions safeguard
  against recursive workflow chains) — so `check-tidytuesday.yml` and
  `backfill-tidytuesday.yml` each explicitly dispatch `publish.yml` via
  `gh workflow run publish.yml` right after they push a new post, rather than
  relying on the push event to cascade. Posts fetch their data straight from
  `raw.githubusercontent.com` at render time (see `R/tt_helpers.R`), so no
  raw CSVs are stored in this repo — only the generated `.qmd` files and
  Quarto's `_freeze/` render cache (kept so already-built weeks aren't
  re-downloaded/re-rendered on every run).
- **`.github/workflows/backfill-tidytuesday.yml`** is manual-only
  (**Actions → Backfill past TidyTuesday challenges → Run workflow**). It
  prompts for a `start_date`/`end_date` (YYYY-MM-DD) and builds a post for
  every upstream week whose own published date falls within that range,
  inclusive, the same way the weekly poller does. The dates are just the
  window's edges — they don't need to land exactly on a week's folder date,
  so e.g. `2025-06-01` to `2025-08-01` picks up every week published in
  between. Leave `end_date` blank to build just the single week covering
  `start_date`.

  This only understands the `meta.yaml` + per-dataset `.md` dictionary format
  TidyTuesday adopted in late 2025 (first seen the week of 2025-12-02; see
  `tt_week_is_ready()` in `R/tt_helpers.R`). Earlier weeks have `meta.yaml`
  and CSVs but no per-dataset dictionary — just one combined `readme.md` —
  so they're skipped, not converted. Check the Action's run log to see which
  requested weeks were actually built vs. skipped and why.

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
