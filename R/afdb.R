##############################################################################
# afdb.R — African Development Bank (AfDB) Document Scraper
#
# Targets: One document per project (most complete type available) for
#          Environment and Climate Change sector projects in Africa.
#
# Source: https://www.afdb.org/en/all-documents
#   Filter: tid_1=89 (Environment), tid_1=98 (Climate Change)
#   Pagination: ?page=0, ?page=1, ... (18 docs per page)
#
# Strategy:
#   1. Scrape sector-filtered document listings (Environment + Climate Change)
#   2. Parse doc type from title suffix (EER, IPR, PCR, PAR, etc.)
#   3. Group by project name, keep ONE doc per project (priority ranking)
#   4. Visit document pages to resolve PDF download URLs
#   5. Download PDFs
#
# Document type priority (most → least complete):
#   1. EER   — Extended/External Evaluation Report (post-completion)
#   2. PCREN — PCR Evaluation Note
#   3. PCR   — Project Completion Report
#   4. MTR / MTEv — Mid-Term Review / Evaluation
#   5. IPR / ISR  — Implementation Progress / Status Report
#   6. PAR   — Project Appraisal Report (baseline)
#   7. ESIA  — Environmental & Social Impact Assessment
#
# Technical notes:
#   - afdb.org does NOT use Cloudflare — browser UA + gzip-only encoding works
#   - MapAfrica (mapafrica.afdb.org) is Cloudflare-protected and cannot be
#     scraped with standard R HTTP libraries; use afdb.org instead
#   - IDEV portal (idev.afdb.org) has no accessible listing pages (all 404)
#   - Agriculture sector (tid_1=7) excluded: all AfDB environment projects
#     already cover agriculture/food systems in Africa
#   - Doc type is encoded in the title suffix, not a separate field
#   - HTML structure: Drupal 7 Views Bootstrap grid
##############################################################################

# ── Setup ──────────────────────────────────────────────────────────────────
.find_r_dir <- function() {
  tryCatch({
    d <- dirname(sys.frame(2)$ofile)
    if (!is.null(d) && nzchar(d)) return(normalizePath(file.path(d, ".."), mustWork = FALSE))
  }, error = function(e) NULL)
  tryCatch({
    args <- commandArgs(trailingOnly = FALSE)
    fa <- grep("^--file=", args, value = TRUE)
    if (length(fa)) return(normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), ".."), mustWork = FALSE))
  }, error = function(e) NULL)
  wd <- getwd()
  if (file.exists(file.path(wd, "R", "00_config.R"))) return(file.path(wd, "R"))
  if (file.exists(file.path(wd, "00_config.R"))) return(wd)
  return(wd)
}
if (!exists("PATHS")) {
  .r_dir <- .find_r_dir()
  source(file.path(.r_dir, "00_config.R"))
  source(file.path(.r_dir, "01_utils.R"))
}

DOWNLOAD_DIR <- file.path(PATHS$downloads, "afdb")
dir.create(DOWNLOAD_DIR, recursive = TRUE, showWarnings = FALSE)

SOURCE_NAME <- "afdb"

# ── AfDB Configuration ─────────────────────────────────────────────────────
AFDB_BASE      <- "https://www.afdb.org"
AFDB_DOCS_BASE <- paste0(AFDB_BASE, "/en/all-documents")
AFDB_DOCS_PER_PAGE <- 18

# Sector filter taxonomy IDs (confirmed from live HTML inspection)
# Environment = 89 (304 docs), Climate Change = 98 (492 docs)
# Agriculture (7) excluded: AfDB environment projects already cover it
AFDB_SECTOR_FILTERS <- list(
  environment    = list(tid = 89, label = "Environment"),
  climate_change = list(tid = 98, label = "Climate Change")
)

# Document type priority (lower = more complete; used for one-per-project selection)
# Based on AfDB document taxonomy observed in titles
AFDB_DOC_TYPE_PRIORITY <- c(
  EER   = 1L,  # Extended/External Evaluation Report (post-completion, most rigorous)
  PCREN = 2L,  # PCR Evaluation Note
  PCR   = 3L,  # Project Completion Report (self-assessment)
  MTR   = 4L,  # Mid-Term Review
  MTEV  = 4L,  # Mid-Term Evaluation (alternate abbreviation)
  IPR   = 5L,  # Implementation Progress Report (during implementation)
  ISR   = 5L,  # Implementation Status Report (during implementation)
  PAR   = 6L,  # Project Appraisal Report (at approval)
  ESIA  = 7L   # Environmental & Social Impact Assessment
)

# Browser User-Agent (required: afdb.org checks UA)
AFDB_BROWSER_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/120.0.0.0 Safari/537.36"
)

AFDB_MAX_PAGES <- 60  # safety cap per sector (~1080 docs max)

# AfDB is Africa-focused by mandate; require sector keywords only
AFDB_REQUIRE_AFRICA  <- FALSE
AFDB_REQUIRE_SECTOR  <- TRUE

# ── AfDB session handle (persistent cookies across requests) ──────────────
# The site requires a session cookie set by the homepage before serving
# filtered document pages. We use a single httr handle (cookie jar) for the
# entire scraping run and warm it up by visiting the homepage first.
AFDB_HANDLE <- NULL  # initialised in afdb_init_session()

afdb_init_session <- function() {
  cli::cli_alert_info("Initialising AfDB session (visiting homepage for cookies)...")
  AFDB_HANDLE <<- httr::handle(AFDB_BASE)

  resp <- tryCatch(
    httr::GET(
      AFDB_BASE,
      httr::handle(AFDB_HANDLE),
      httr::user_agent(AFDB_BROWSER_UA),
      httr::timeout(HTTP_CONFIG$timeout_sec),
      httr::add_headers(
        Accept            = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "en-US,en;q=0.9",
        `Accept-Encoding` = "gzip, deflate",
        `Upgrade-Insecure-Requests` = "1",
        `Sec-Fetch-Dest`  = "document",
        `Sec-Fetch-Mode`  = "navigate",
        `Sec-Fetch-Site`  = "none",
        `Sec-Fetch-User`  = "?1"
      )
    ),
    error = function(e) NULL
  )

  if (!is.null(resp) && httr::status_code(resp) == 200) {
    cli::cli_alert_success("Session initialised (homepage returned 200)")
    Sys.sleep(runif(1, 1.5, 3.0))  # brief pause before first document request
    return(TRUE)
  }
  status_str <- if (is.null(resp)) "no response (network error)" else httr::status_code(resp)
  cli::cli_alert_warning("Homepage returned HTTP {status_str} — continuing anyway (cookies may be missing)")
  FALSE
}

# ── Helper: AfDB-specific polite GET ──────────────────────────────────────
#' Uses persistent cookie handle + browser UA + gzip-only encoding.
#' Brotli ('br') is omitted — libcurl on Windows cannot decode it.
afdb_get <- function(url, referer = AFDB_BASE) {
  Sys.sleep(runif(1, HTTP_CONFIG$delay_min, HTTP_CONFIG$delay_max))

  for (attempt in seq_len(HTTP_CONFIG$max_retries)) {
    resp <- tryCatch({
      args <- list(
        url,
        httr::user_agent(AFDB_BROWSER_UA),
        httr::timeout(HTTP_CONFIG$timeout_sec),
        httr::add_headers(
          Accept            = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "en-US,en;q=0.9",
          `Accept-Encoding` = "gzip, deflate",
          Referer           = referer,
          `Upgrade-Insecure-Requests` = "1",
          `Sec-Fetch-Dest`  = "document",
          `Sec-Fetch-Mode`  = "navigate",
          `Sec-Fetch-Site`  = "same-origin",
          `Sec-Fetch-User`  = "?1",
          `Cache-Control`   = "max-age=0"
        )
      )
      # Attach cookie handle if session was initialised
      if (!is.null(AFDB_HANDLE)) args <- c(args, list(httr::handle(AFDB_HANDLE)))
      do.call(httr::GET, args)
    },
    error = function(e) {
      cli::cli_alert_warning("AfDB GET error (attempt {attempt}): {e$message}")
      NULL
    })

    if (is.null(resp)) { Sys.sleep(2^attempt); next }
    status <- httr::status_code(resp)
    if (status == 200) return(resp)
    if (status == 429) { Sys.sleep(30); next }
    if (status >= 500) { Sys.sleep(2^attempt); next }
    cli::cli_alert_warning("HTTP {status} for: {url}")
    return(resp)
  }
  NULL
}

# ── Helper: Read HTML with AfDB GET ───────────────────────────────────────
afdb_read_html <- function(url, referer = paste0(AFDB_BASE, "/en/documents")) {
  resp <- afdb_get(url, referer = referer)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  tryCatch({
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (nchar(txt) < 200) return(NULL)
    xml2::read_html(txt)
  }, error = function(e) {
    cli::cli_alert_warning("HTML parse error: {e$message}")
    NULL
  })
}

# ── Helper: Parse document type from title suffix ─────────────────────────
#' AfDB titles embed doc type as " - EER October 2025" or " - IPR novembre 2024"
#' Returns the type abbreviation in uppercase, or NA if not detected.
parse_doc_type_from_title <- function(title) {
  if (is.na(title) || nchar(title) == 0) return(NA_character_)
  pattern <- "\\b(EER|PCREN|PCR|MTEv|MTEV|MTR|IPR|ISR|PAR|ESIA)\\b"
  m <- regmatches(title, regexpr(pattern, title, ignore.case = TRUE))
  if (length(m) == 0 || nchar(m) == 0) return(NA_character_)
  toupper(m)
}

# ── Helper: Strip doc type suffix to get project name ────────────────────
#' Removes " - EER October 2025" style suffix from title to get the base project name.
#' Used for grouping multiple documents that belong to the same project.
extract_project_name <- function(title) {
  if (is.na(title) || nchar(title) == 0) return(NA_character_)
  # Match " - TYPE [anything to end]"
  pattern <- "\\s*-\\s*(EER|PCREN|PCR|MTEv|MTEV|MTR|IPR|ISR|PAR|ESIA)(\\s.*)?$"
  trimws(sub(pattern, "", title, ignore.case = TRUE))
}

# ── Helper: Parse a single AfDB document listing page ─────────────────────
#' Uses confirmed Drupal 7 Views Bootstrap grid selectors, with generic fallbacks.
parse_afdb_listing <- function(page_html, sector_label) {
  if (is.null(page_html)) return(tibble())

  # Primary selectors (confirmed from live HTML)
  entries <- tryCatch({
    rows <- rvest::html_elements(page_html, "div.col-xs-12.col-sm-12")
    if (length(rows) == 0)
      rows <- rvest::html_elements(page_html, ".views-bootstrap-grid-plugin-style .col-xs-12")
    if (length(rows) == 0)
      rows <- rvest::html_elements(page_html, ".views-row, .view-content > div")
    rows
  }, error = function(e) list())

  if (length(entries) == 0) {
    cli::cli_alert_warning("No document entries found on page.")
    return(tibble())
  }

  records <- purrr::map(entries, function(entry) {
    tryCatch({
      # Title link (confirmed: .views-field-title a)
      title_el <- rvest::html_element(entry,
        ".views-field-title a, .field-content a, h3 a, h2 a")
      if (is.na(title_el)) return(NULL)

      title <- trimws(rvest::html_text2(title_el))
      href  <- rvest::html_attr(title_el, "href")
      if (is.na(title) || nchar(title) < 5) return(NULL)

      web_url <- if (!is.na(href)) {
        if (startsWith(href, "http")) href else paste0(AFDB_BASE, href)
      } else NA_character_

      # Date (confirmed: span.date-display-single)
      date_el  <- rvest::html_element(entry,
        "span.date-display-single, time, .views-field-created")
      doc_date <- if (!is.na(date_el)) trimws(rvest::html_text2(date_el)) else NA_character_

      # Standardise date to ISO (AfDB uses "02-Mar-2026" format)
      doc_date_iso <- tryCatch(
        format(as.Date(doc_date, format = "%d-%b-%Y"), "%Y-%m-%d"),
        error = function(e) doc_date
      )

      # Direct PDF link on listing (rare but possible)
      pdf_el  <- rvest::html_element(entry, "a[href$='.pdf'], a[href$='.PDF']")
      pdf_url <- if (!is.na(pdf_el)) {
        h <- rvest::html_attr(pdf_el, "href")
        if (!is.na(h)) { if (startsWith(h, "http")) h else paste0(AFDB_BASE, h) }
      } else NA_character_

      # Country: first Africa country found in title
      title_lower <- tolower(title)
      country <- NA_character_
      for (cn in AFRICA_COUNTRIES_EN) {
        if (grepl(tolower(cn), title_lower, fixed = TRUE)) {
          country <- cn; break
        }
      }

      tibble(
        id         = safe_filename(title),
        title      = title,
        pdf_url    = pdf_url,
        doc_date   = doc_date_iso,
        doc_type   = sector_label,   # broad sector label (refined later by parse_doc_type_from_title)
        country    = country,
        project_id = NA_character_,
        web_url    = web_url
      )
    }, error = function(e) NULL)
  })

  purrr::compact(records) %>% bind_rows()
}

# ── Helper: Visit a document page to resolve its PDF URL ─────────────────
resolve_pdf_url <- function(web_url) {
  if (is.na(web_url) || nchar(web_url) == 0) return(NA_character_)
  page <- afdb_read_html(web_url)
  if (is.null(page)) return(NA_character_)
  # Look for PDF links (AfDB stores at /fileadmin/uploads/...)
  pdf_links <- rvest::html_elements(page,
    "a[href$='.pdf'], a[href$='.PDF'], a[href*='fileadmin']")
  if (length(pdf_links) == 0) return(NA_character_)
  href <- rvest::html_attr(pdf_links[[1]], "href")
  if (is.na(href)) return(NA_character_)
  if (startsWith(href, "http")) href else paste0(AFDB_BASE, href)
}

# ── Strategy: Scrape all documents by sector ──────────────────────────────
scrape_afdb_by_sector <- function() {
  cli::cli_h2("Scraping AfDB documents by sector (Environment + Climate Change)")
  all_results <- tibble()

  for (sector_name in names(AFDB_SECTOR_FILTERS)) {
    tid   <- AFDB_SECTOR_FILTERS[[sector_name]]$tid
    label <- AFDB_SECTOR_FILTERS[[sector_name]]$label
    cli::cli_alert_info("Sector: {label} (tid_1={tid})")

    for (page_num in 0:AFDB_MAX_PAGES) {
      url <- paste0(AFDB_DOCS_BASE, "?tid_1=", tid, "&page=", page_num)
      page_html <- afdb_read_html(url)

      if (is.null(page_html)) {
        cli::cli_alert_warning("  Could not fetch {label} page {page_num} — stopping sector.")
        break
      }

      records <- parse_afdb_listing(page_html, label)

      if (nrow(records) == 0) {
        cli::cli_alert_info("  No entries on page {page_num} — end of {label} listing.")
        break
      }

      all_results <- bind_rows(all_results, records)
      cli::cli_alert_success("  Page {page_num}: {nrow(records)} entries (total so far: {nrow(all_results)})")

      # Check for "next page" pager link
      next_link <- rvest::html_element(page_html,
        "a[rel='next'], .pager__item--next a, li.pager-next a, li.next a")
      if (is.na(next_link)) {
        cli::cli_alert_info("  No next-page link found — end of {label} listing.")
        break
      }
    }
  }

  cli::cli_alert_success("Total scraped: {nrow(all_results)} documents across all sectors")
  all_results
}

# ── One document per project: select highest-priority type ────────────────
#' Groups by extracted project name, keeps the most complete document type.
#' Within the same type, keeps the most recent document.
select_best_document <- function(results) {
  if (nrow(results) == 0) return(results)

  cli::cli_h2("Selecting one document per project (priority ranking)")

  results <- results %>%
    mutate(
      doc_type_abbr = sapply(title, parse_doc_type_from_title),
      project_name  = sapply(title, extract_project_name),
      type_priority = {
        p <- AFDB_DOC_TYPE_PRIORITY[doc_type_abbr]
        ifelse(is.na(p), 99L, as.integer(p))
      }
    )

  # Count before selection
  n_total    <- nrow(results)
  n_projects <- dplyr::n_distinct(results$project_name)
  cli::cli_alert_info("Before: {n_total} documents, {n_projects} distinct projects")

  # Show doc type distribution
  type_dist <- results %>% count(doc_type_abbr, sort = TRUE)
  cli::cli_alert_info("Doc type breakdown:")
  for (i in seq_len(nrow(type_dist))) {
    cli::cli_text("  {type_dist$doc_type_abbr[i]}: {type_dist$n[i]}")
  }

  # Select one per project
  best <- results %>%
    group_by(project_name) %>%
    arrange(type_priority, desc(doc_date)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-type_priority)

  cli::cli_alert_success("After selection: {nrow(best)} documents (one per project)")
  best
}

# ── Relevance filtering ────────────────────────────────────────────────────
filter_relevant <- function(results) {
  cli::cli_h2("Filtering for relevance (agriculture/adaptation keywords)")
  if (nrow(results) == 0) return(results)

  results <- results %>%
    distinct(web_url, .keep_all = TRUE) %>%
    distinct(title, .keep_all = TRUE)
  cli::cli_alert_info("After dedup: {nrow(results)} documents")

  results %>%
    rowwise() %>%
    mutate(relevant = passes_relevance_fast(
      paste(coalesce(title, ""), coalesce(project_name, ""), sep = " "),
      require_africa = AFDB_REQUIRE_AFRICA,
      require_sector = AFDB_REQUIRE_SECTOR
    )) %>%
    ungroup() %>%
    filter(relevant) %>%
    select(-relevant)
}

# ── Resolve missing PDF URLs ───────────────────────────────────────────────
enrich_pdf_urls <- function(results) {
  cli::cli_h2("Resolving PDF URLs (visiting document pages)")
  needs_pdf <- results %>% filter(is.na(pdf_url) & !is.na(web_url))
  has_pdf   <- results %>% filter(!is.na(pdf_url) | is.na(web_url))
  cli::cli_alert_info("{nrow(needs_pdf)} documents need PDF URL resolution")
  if (nrow(needs_pdf) == 0) return(results)

  resolved <- needs_pdf %>%
    rowwise() %>%
    mutate(pdf_url = {
      cli::cli_alert_info("  {substr(title, 1, 65)}")
      resolve_pdf_url(web_url)
    }) %>%
    ungroup()

  bind_rows(has_pdf, resolved)
}

# ── Download PDFs ─────────────────────────────────────────────────────────
download_results <- function(results) {
  cli::cli_h2("Downloading PDFs")
  with_pdf <- results %>% filter(!is.na(pdf_url) & nchar(pdf_url) > 0)
  cli::cli_alert_info("{nrow(with_pdf)} documents have PDF URLs")

  success <- 0L; failed <- 0L; skipped <- 0L

  for (i in seq_len(nrow(with_pdf))) {
    row <- with_pdf[i, ]

    year <- tryCatch(
      format(as.Date(row$doc_date), "%Y"),
      error = function(e) "XXXX"
    )
    type_str <- safe_filename(coalesce(row$doc_type_abbr, "doc"))
    proj_str <- safe_filename(coalesce(row$project_name, coalesce(row$title, "notitle")))
    fname <- paste0(substr(
      glue("{SOURCE_NAME}_{type_str}_{proj_str}_{year}"),
      1, 150), ".pdf")
    dest <- file.path(DOWNLOAD_DIR, fname)

    if (file.exists(dest)) {
      log_download(SOURCE_NAME, row$project_id, row$doc_type_abbr,
                   row$title, row$pdf_url, dest, "skipped", "Already exists")
      skipped <- skipped + 1L
      next
    }

    resp  <- afdb_get(row$pdf_url)
    dl_ok <- FALSE

    if (!is.null(resp) && httr::status_code(resp) == 200) {
      tryCatch({
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        writeBin(httr::content(resp, as = "raw"), dest)
        fsize <- file.info(dest)$size
        if (!is.na(fsize) && fsize >= HTTP_CONFIG$min_pdf_bytes) {
          con   <- file(dest, "rb")
          magic <- rawToChar(readBin(con, "raw", n = 5))
          close(con)
          if (magic == "%PDF-") dl_ok <- TRUE
        }
        if (!dl_ok && file.exists(dest)) file.remove(dest)
      }, error = function(e) {
        if (file.exists(dest)) file.remove(dest)
      })
    }

    if (dl_ok) {
      log_download(SOURCE_NAME, row$project_id, row$doc_type_abbr,
                   row$title, row$pdf_url, dest, "success")
      success <- success + 1L
    } else {
      log_download(SOURCE_NAME, row$project_id, row$doc_type_abbr,
                   row$title, row$pdf_url, dest, "failed",
                   "Download or PDF validation failed")
      failed <- failed + 1L
    }

    if (i %% 10 == 0)
      cli::cli_alert_info("Progress: {i}/{nrow(with_pdf)} (OK:{success} fail:{failed} skip:{skipped})")
  }

  cli::cli_h3("Download Summary")
  cli::cli_alert_success("Success: {success}")
  cli::cli_alert_danger("Failed:  {failed}")
  cli::cli_alert_info("Skipped: {skipped}")
  cli::cli_alert_info("No PDF:  {nrow(results) - nrow(with_pdf)}")
}

# ── Save metadata ─────────────────────────────────────────────────────────
save_metadata <- function(results) {
  meta_path <- file.path(PATHS$data, "afdb_metadata.csv")
  readr::write_csv(results, meta_path)
  cli::cli_alert_success("Metadata saved ({nrow(results)} rows): {meta_path}")
}

# ── Main execution ────────────────────────────────────────────────────────
run_afdb_scraper <- function() {
  cli::cli_h1("AfDB Grey Literature Scraper")
  cli::cli_alert_info("Download dir: {DOWNLOAD_DIR}")

  # Step 0: Warm up session (get homepage cookies)
  afdb_init_session()

  # Step 1: Collect all documents from Environment + Climate Change sectors
  all_docs <- tryCatch(scrape_afdb_by_sector(), error = function(e) {
    cli::cli_alert_danger("Scraping failed: {e$message}")
    tibble()
  })

  if (nrow(all_docs) == 0) {
    cli::cli_alert_danger("No documents found. Check network and URL structure.")
    return(invisible(NULL))
  }

  # Step 2: Deduplicate + relevance filter
  relevant <- filter_relevant(all_docs)
  cli::cli_alert_info("After relevance filter: {nrow(relevant)} documents")

  if (nrow(relevant) == 0) {
    cli::cli_alert_warning("No documents passed relevance filter.")
    save_metadata(relevant)
    return(invisible(NULL))
  }

  # Step 3: One document per project
  best <- select_best_document(relevant)

  # Step 4: Resolve PDF URLs
  best <- enrich_pdf_urls(best)

  # Step 5: Save metadata
  save_metadata(best)

  # Step 6: Download PDFs
  download_results(best)

  # Step 7: Summary
  print_source_summary(SOURCE_NAME)

  cli::cli_h2("Done!")
  invisible(best)
}

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  run_afdb_scraper()
}
