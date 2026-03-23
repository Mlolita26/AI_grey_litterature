---
name: new-scraper
description: Guide for building a new institutional source scraper for the grey literature pipeline
---

When building a new scraper for this project:

## 1. Research first

Before writing any code, investigate the source:
- Does it have a public API? Check `/api`, developer docs, network traffic
- How is content paginated? (offset, page number, cursor, AJAX POST)
- What filters are available? (country, sector, date, document type)
- What document types exist and which are relevant?
- Check `robots.txt` for scraping restrictions
- Document findings in the script header block

## 2. Follow the established pattern

Use `R/worldbank.R` (API-based) or `R/gcf.R` (AJAX scraping) as the template.

Required structure:
```r
##############################################################################
# {source}.R — {Institution Name} Scraper
#
# Targets: [document types]
# Website: [URL]
# Strategy: [API / rvest / AJAX POST / etc.]
# Pagination: [how pages work]
# Quirks: [anything unusual]
##############################################################################

# ── Setup ──────────────────────────────────────────────────────────────────
.find_r_dir <- function() { ... }  # standard boilerplate
if (!exists("PATHS")) { source config + utils }

DOWNLOAD_DIR <- file.path(PATHS$downloads, "{source}")
SOURCE_NAME  <- "{source}"

# ── Constants ──────────────────────────────────────────────────────────────
# Base URLs, doc type filters, API keys, etc.

# ── Helpers ────────────────────────────────────────────────────────────────
# Source-specific query/parse functions

# ── Main scraper function ──────────────────────────────────────────────────
run_{source}_scraper <- function() {
  # 1. Query/scrape → tibble
  # 2. Deduplicate by document ID
  # 3. Filter relevance (passes_relevance_fast)
  # 4. save_metadata (write CSV to data/)
  # 5. download_results (download_pdf + log_download)
  # 6. print_source_summary
}

if (sys.nframe() == 0 || !interactive()) run_{source}_scraper()
```

## 3. Required metadata columns (minimum)

| Column | Description |
|--------|-------------|
| `id` | Unique document identifier |
| `title` | Document title |
| `pdf_url` | Direct PDF download URL |
| `doc_date` | Publication date |
| `doc_type` | Document type (ICR, evaluation, etc.) |
| `country` | Country/countries |
| `project_id` | Project identifier |
| `web_url` | Human-readable web page URL |

## 4. Use shared utilities — never reimplement

From `R/01_utils.R`:
- `polite_get(url)` — all HTTP requests
- `safe_get_json(url)` — for JSON APIs
- `safe_read_html(url)` — for HTML pages
- `download_pdf(url, dest)` — download + validate
- `log_download(...)` — log every attempt
- `passes_relevance_fast(text, ...)` — relevance filter
- `safe_filename(x)` — sanitize filenames

From `R/00_config.R`:
- `HTTP_CONFIG` — delays, timeout, user-agent
- `AFRICA_ALL` — all Africa country/region terms
- `AGRICULTURE_ALL`, `ADAPTATION_ALL` — sector keywords

## 5. Testing checklist

- [ ] Script sources without error
- [ ] Query returns data (check `nrow(results) > 0`)
- [ ] Metadata CSV written to `data/{source}_metadata.csv`
- [ ] At least one PDF downloads successfully
- [ ] `data/download_log.csv` has entries for this source
- [ ] No hardcoded absolute paths

## 6. Git workflow

```bash
git checkout -b feat/{source-name}
# build + test
git add R/{source}.R
git commit -m "feat: add {Institution} scraper"
git push origin feat/{source-name}
# then merge to main after review
```

Update the source status table in `CLAUDE.md`.
