# Africa Adaptation Grey Literature Review — Scraper Pipeline

Systematic retrieval pipeline for grey literature (project evaluations, completion reports, funding proposals) from 20+ institutional sources supporting an evidence synthesis on **climate adaptation in Africa's food and agriculture sector** (2000–2025).

## What it does

Scrapes, downloads, and catalogues project documents from multilateral development banks, UN agencies, climate finance funds, NGOs, and African research organizations. Each institutional source has its own R script that:

1. Queries/scrapes the source (via API or web)
2. Parses results into a standardised tibble
3. Filters for relevance (Africa + agriculture/adaptation keywords)
4. Saves a metadata CSV
5. Downloads PDFs to `downloads/{source}/`
6. Logs every attempt (success/failed/skipped) to `data/download_log.csv`

## Source status

| Source | Script | Status | Strategy |
|--------|--------|--------|---------|
| World Bank | `R/worldbank.R` | ✅ Working | REST API |
| Green Climate Fund | `R/gcf.R` | ✅ Working | Drupal AJAX |
| Global Environment Facility | `R/gef.R` | ✅ Working | HTML scraping |
| African Development Bank | `R/afdb.R` | 🔲 To build | TBD |
| IFAD | `R/ifad.R` | 🔲 To build | TBD |
| Adaptation Fund | `R/af.R` | 🔲 To build | TBD |
| FAO | `R/fao.R` | 🔲 To build | TBD |
| UNDP | `R/undp.R` | 🔲 To build | TBD |

## Setup

### 1. Install R dependencies

```r
Rscript R/install_packages.R
```

Required packages: `httr`, `rvest`, `dplyr`, `stringr`, `readr`, `jsonlite`, `xml2`, `glue`, `fs`, `cli`, `lubridate`, `tibble`, `curl`, `purrr`

### 2. Run a single scraper

```r
Rscript R/worldbank.R
Rscript R/gcf.R
Rscript R/gef.R
```

### 3. Run all scrapers

```r
Rscript R/run_all.R
```

## Project structure

```
AI_grey_litterature/
├── CLAUDE.md               # Project memory for Claude Code
├── README.md
├── .gitignore
├── R/
│   ├── 00_config.R         # Paths, HTTP config, country lists, keyword lists
│   ├── 01_utils.R          # Shared utilities (HTTP, PDF download, logging)
│   ├── worldbank.R         # World Bank Documents API
│   ├── gcf.R               # Green Climate Fund
│   ├── gef.R               # Global Environment Facility
│   ├── install_packages.R  # Package installer
│   └── run_all.R           # Master runner
├── data/                   # Metadata CSVs + download_log.csv
├── downloads/              # PDFs organised by source
│   ├── worldbank/
│   ├── gcf/
│   └── gef/
├── docs/                   # Protocol and notes
└── tests/                  # Validation scripts
```

## Output files

| File | Description |
|------|-------------|
| `data/{source}_metadata.csv` | All documents found (whether downloaded or not) |
| `data/download_log.csv` | Every download attempt with status + notes |
| `downloads/{source}/*.pdf` | Downloaded PDFs |

## Configuration

Edit `R/00_config.R` to adjust:
- `HTTP_CONFIG` — delays, timeouts, retries, user-agent
- `AFRICA_COUNTRIES_*` — country name lists (EN/FR/PT)
- `AGRICULTURE_KEYWORDS` — sector keywords
- `ADAPTATION_KEYWORDS` — climate adaptation keywords

## Contributing

1. Create a feature branch: `git checkout -b feat/{source-name}`
2. Build the scraper following the pattern in `worldbank.R`
3. Test it, commit with a conventional message (`feat: add {source} scraper`)
4. Push and open a PR

See `CLAUDE.md` for the full scraper-building checklist.

## Scope

- **Geographic**: All 54 African Union member states
- **Sector**: Agriculture, livestock, fisheries, agroforestry, food systems
- **Document types**: ICRs, PPARs, PADs, terminal evaluations, mid-term evaluations, impact evaluations, project completion reports, funding proposals
- **Timeframe**: 2000–2025
