# Example Thesis Repository

This repository shows a clean thesis-style workflow in Quarto. It is organized so that a reader can move from project overview, to cleaning, to exploration, to modeling, to figures. The same structure also renders as a GitHub Pages website.

## Repository structure

```text
index.qmd          <- Project overview and data description
analysis/
  01-clean.qmd     <- Data cleaning and decisions
  02-explore.qmd   <- Exploratory analysis
  03-model.qmd     <- Statistical modeling
  04-figures.qmd   <- Publication figures

data/              <- Raw data (or link to repository)
figs/              <- Saved figure outputs
R/
  analysis_functions.R
about.qmd
references.bib
styles.css
```

## Why this is useful for teaching

Students can see, in one place, how to:

- separate raw data from analysis,
- present analytical work in a clear order,
- save figure outputs in a predictable location,
- and publish a project website from the same repository.

## Build the site locally

```bash
quarto render
```

## Publish with GitHub Pages

1. Push the repository to GitHub.
2. In the repository, go to **Settings > Pages**.
3. Under **Build and deployment**, choose **GitHub Actions**.
4. Keep the included workflow in `.github/workflows/publish.yml`.
5. Each push to `main` will rebuild the site.
