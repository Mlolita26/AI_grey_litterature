# Africa Adaptation Grey Literature Review — Scraper Pipeline

## What this project does
Systematic retrieval of grey literature (project evaluations, completion reports, funding proposals) from 20+ institutional sources for an evidence synthesis on climate adaptation in Africa's food and agriculture sector. Covers 2000–2025.

## Architecture
- One R script per institutional source (`worldbank.R`, `gcf.R`, `gef.R`, `afdb.R`, etc.)
- Shared config in `R/00_config.R` — paths, HTTP settings, African country lists, keyword lists
- Shared utilities in `R/01_utils.R` — HTTP helpers, download/log functions, relevance filters
- Each scraper follows the same pattern: **query → parse to tibble → filter → save metadata CSV → download PDFs → log**
- PDFs go to `downloads/{source_name}/`, metadata CSVs to `data/`, logs to `data/download_log.csv`
- Master runner: `R/run_all.R` (runs all scrapers sequentially with error isolation)

## Project structure
```
AI_grey_litterature/
├── CLAUDE.md
├── README.md
├── .gitignore
├── R/
│   ├── 00_config.R       # Paths, HTTP config, country lists, keyword lists
│   ├── 01_utils.R        # Shared helpers
│   ├── worldbank.R       # ✅ Working
│   ├── gcf.R             # ✅ Working
│   ├── gef.R             # ✅ Working
│   ├── afdb.R            # 🔲 To build
│   ├── ifad.R            # 🔲 To build
│   └── run_all.R         # Master runner
├── data/                 # Metadata CSVs + download_log.csv
├── downloads/            # PDFs by source name
├── logs/
├── docs/                 # Protocol, notes
└── tests/
```

## Git conventions
- Conventional commits: `feat:`, `fix:`, `docs:`, `refactor:`
- One feature branch per new scraper: `feat/{source-name}`
- Commit to branch, push, then merge to main after testing
- Remote: https://github.com/Mlolita26/AI_grey_litterature

## Coding conventions
- Every scraper file starts with a header block (##########) documenting: source, URL, strategy, document types targeted, quirks
- All HTTP via `polite_get()` — never raw `httr::GET()`
- All requests use `HTTP_CONFIG$user_agent` and delays from `HTTP_CONFIG$delay_min/max`
- Never crash the pipeline — wrap everything in `tryCatch`, log failures, keep going
- Deduplication before downloading (by document ID or URL)
- PDF validation: check magic bytes `%PDF-` and file size > `HTTP_CONFIG$min_pdf_bytes`
- Metadata tibble minimum columns: `id`, `title`, `pdf_url`, `doc_date`, `doc_type`, `country`, `project_id`, `web_url`

## Key shared functions (R/01_utils.R)
| Function | Purpose |
|----------|---------|
| `safe_get_json(url)` | GET + JSON parse with retry |
| `polite_get(url)` | GET with delay + user-agent + exponential backoff |
| `safe_read_html(url)` | GET + HTML parse with JS-page detection |
| `download_pdf(url, dest)` | Download + validate PDF (magic bytes + size) |
| `log_download(source, project_code, doc_type, title, url, filepath, status, notes)` | Append to CSV log |
| `passes_relevance_fast(text, require_africa, require_sector)` | Keyword relevance check using pre-compiled regex |
| `safe_filename(x)` | Sanitize strings for filenames |
| `print_source_summary(source_name)` | Print log summary for a source |

## Source status
| Source | Script | Status | Strategy |
|--------|--------|--------|---------|
| World Bank | `worldbank.R` | ✅ Working | REST API (search.worldbank.org/api/v3/wds) |
| GCF | `gcf.R` | ✅ Working | Drupal AJAX scraping |
| GEF | `gef.R` | ✅ Working | HTML table scraping |
| AfDB | `afdb.R` | 🔲 Not started | TBD (research needed) |
| IFAD | `ifad.R` | 🔲 Not started | TBD |
| Adaptation Fund | `af.R` | 🔲 Not started | TBD |
| FAO | `fao.R` | 🔲 Not started | TBD |
| UNDP | `undp.R` | 🔲 Not started | TBD |
| UNEP | `unep.R` | 🔲 Not started | TBD |
| WFP | `wfp.R` | 🔲 Not started | TBD |
| IFPRI | `ifpri.R` | 🔲 Not started | TBD |

## Reference
- Full protocol: `docs/AIs_WP3_EvidenceSynthesis_GreyLit.pdf`
- Target document types: ICRs, PPARs, PADs, funding proposals, terminal evaluations, mid-term evaluations, impact evaluations, project completion reports
- Geographic scope: All 54 African countries
- Sector: Agriculture, livestock, fisheries, agroforestry, food systems
- Timeframe: 2000–2025
- Keywords: see `AGRICULTURE_KEYWORDS`, `ADAPTATION_KEYWORDS`, `DOC_TYPE_KEYWORDS` in `00_config.R`

## Building a new scraper — checklist
1. Research the source website (API?, pagination, filters, doc types) — document in script header
2. Create `R/{source}.R` following the worldbank.R/gcf.R pattern
3. Test: run the scraper, check metadata CSV, verify PDF downloads
4. Update the source status table in this CLAUDE.md
5. Commit on branch `feat/{source-name}`, push, merge to main
