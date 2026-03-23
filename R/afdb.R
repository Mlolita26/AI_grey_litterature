##############################################################################
# afdb.R — African Development Bank (AfDB) Document Scraper
#
# Targets:
#   - Project Completion Reports (PCRs) — AfDB equivalent of World Bank ICRs
#   - Project Appraisal Reports (PARs)
#   - Independent evaluations from IDEV (idev.afdb.org)
#   - Project Completion Report Evaluation Notes (PCRENs)
#
# Websites:
#   - Main documents: https://www.afdb.org/en/all-documents
#   - Category pages: https://www.afdb.org/en/documents/category/{type}
#   - IDEV evaluations: https://idev.afdb.org/en/evaluations
#
# Strategy:
#   - AfDB uses Drupal CMS with standard ?page=N pagination
#   - Two scraping strategies run in sequence:
#     1. IDEV portal (idev.afdb.org) — evaluation reports, more accessible
#     2. AfDB category pages — PCRs and PARs
#   - Both strategies parse HTML document listings, follow links to get PDF URLs
#   - PDFs stored at /fileadmin/uploads/afdb/Documents/... or
#     idev.afdb.org/sites/default/files/...
#
# Anti-bot notes:
#   - The AfDB site blocks generic user-agents with 403 Forbidden
#   - We use a realistic browser User-Agent as override for this source
#   - If 403 persists, try polite_get() with httr::add_headers(Cookie=...) or
#     contact idevhelpdesk@afdb.org for bulk data access
#
# Document type mapping (AfDB terminology → our standard):
#   PCR  = Project Completion Report
#   PAR  = Project Appraisal Report
#   PCREN = PCR Evaluation Note
#   ESW  = Economic and Sector Work
#   Evaluation Report = IDEV independent evaluation
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
AFDB_BASE       <- "https://www.afdb.org"
IDEV_BASE       <- "https://idev.afdb.org"

# Category pages on the main site (Drupal, paginated with ?page=N)
AFDB_CATEGORIES <- list(
  pcr      = paste0(AFDB_BASE, "/en/documents/category/project-completion-reports"),
  par      = paste0(AFDB_BASE, "/en/documents/category/project-appraisal-reports"),
  pcren    = paste0(AFDB_BASE, "/en/documents/category/project-completion-report-evaluation-notes"),
  eval     = paste0(AFDB_BASE, "/en/documents/evaluation-reports")
)

# IDEV evaluation pages (separate Drupal site)
# Candidate URLs tried in order — 404s are skipped automatically
IDEV_EVALUATIONS_URLS <- c(
  paste0(IDEV_BASE, "/en/evaluation"),           # most likely correct path
  paste0(IDEV_BASE, "/en/evaluations"),
  paste0(IDEV_BASE, "/evaluations"),
  paste0(IDEV_BASE, "/en/idev-evaluations"),
  paste0(IDEV_BASE, "/en/publications"),
  IDEV_BASE                                       # root page as last resort
)

# AfDB uses a realistic User-Agent because the site blocks generic bots
# This is used as an override for polite_get calls on this source
AFDB_BROWSER_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/120.0.0.0 Safari/537.36"
)

# Maximum pages to scrape per category (safety cap)
AFDB_MAX_PAGES <- 50

# Known African country names in AfDB document titles (for relevance check)
# AfDB primarily covers African countries, but we still filter for agriculture/adaptation
AFDB_REQUIRE_AFRICA  <- FALSE  # AfDB is Africa-focused by definition
AFDB_REQUIRE_SECTOR  <- TRUE   # Still require agriculture or adaptation keywords

# ── Helper: AfDB-specific polite GET ──────────────────────────────────────
#' Like polite_get but uses a browser User-Agent to avoid 403s on AfDB.
afdb_get <- function(url) {
  Sys.sleep(runif(1, HTTP_CONFIG$delay_min, HTTP_CONFIG$delay_max))
  for (attempt in seq_len(HTTP_CONFIG$max_retries)) {
    resp <- tryCatch(
      httr::GET(
        url,
        httr::user_agent(AFDB_BROWSER_UA),
        httr::timeout(HTTP_CONFIG$timeout_sec),
        httr::add_headers(
          Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "en-US,en;q=0.5",
          # Omit 'br' (Brotli) — libcurl on Windows does not support it;
          # without this the server sends Brotli which httr cannot decode.
          `Accept-Encoding` = "gzip, deflate"
        )
      ),
      error = function(e) {
        cli::cli_alert_warning("AfDB GET error (attempt {attempt}): {e$message}")
        NULL
      }
    )
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

# ── Helper: Parse HTML safely using AfDB GET ───────────────────────────────
afdb_read_html <- function(url) {
  resp <- afdb_get(url)
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

# ── Helper: Extract documents from a Drupal listing page ──────────────────
#' Parse a Drupal document listing page into a tibble.
#' Tries multiple CSS selector patterns to be resilient across Drupal themes.
parse_afdb_listing <- function(page_html, base_url, doc_type_label) {
  if (is.null(page_html)) return(tibble())

  # Try different container selectors used by AfDB/Drupal
  # Pattern 1: views-row (Drupal 7 Views)
  # Pattern 2: article.node (Drupal 8+)
  # Pattern 3: .document-item (custom theme)
  entries <- tryCatch({
    rows <- rvest::html_elements(page_html, ".views-row, article.node--type-publication, .document-item, li.views-row")
    if (length(rows) == 0) {
      # Fallback: any list item or article with an anchor
      rows <- rvest::html_elements(page_html, "ul.item-list li, .view-content > div")
    }
    rows
  }, error = function(e) list())

  if (length(entries) == 0) {
    cli::cli_alert_warning("No document entries found on page. Trying link-based extraction.")
    # Last resort: grab all links that look like document pages
    links <- rvest::html_elements(page_html, "a[href*='/documents/document'], a[href*='/en/documents/']")
    if (length(links) == 0) return(tibble())

    hrefs <- rvest::html_attr(links, "href")
    titles <- rvest::html_text2(links)
    hrefs <- hrefs[nchar(titles) > 10]  # filter out nav links
    titles <- titles[nchar(titles) > 10]
    if (length(hrefs) == 0) return(tibble())

    full_urls <- ifelse(startsWith(hrefs, "http"), hrefs,
                        paste0(AFDB_BASE, hrefs))
    return(tibble(
      id         = safe_filename(titles),
      title      = titles,
      pdf_url    = NA_character_,
      doc_date   = NA_character_,
      doc_type   = doc_type_label,
      country    = NA_character_,
      project_id = NA_character_,
      web_url    = full_urls
    ))
  }

  records <- purrr::map(entries, function(entry) {
    tryCatch({
      # Title and web link
      title_el <- rvest::html_element(entry, "a, h3, h2, .views-field-title")
      title <- if (!is.na(title_el)) rvest::html_text2(title_el) else NA_character_
      href  <- if (!is.na(title_el)) rvest::html_attr(title_el, "href") else NA_character_

      if (is.na(title) || nchar(trimws(title)) == 0) return(NULL)

      web_url <- if (!is.na(href)) {
        ifelse(startsWith(href, "http"), href, paste0(AFDB_BASE, href))
      } else NA_character_

      # Date
      date_el  <- rvest::html_element(entry, ".date-display-single, time, .views-field-created, .field--name-created")
      doc_date <- if (!is.na(date_el)) rvest::html_text2(date_el) else NA_character_

      # Direct PDF link (if on listing page)
      pdf_el  <- rvest::html_element(entry, "a[href$='.pdf'], a[href$='.PDF']")
      pdf_url <- if (!is.na(pdf_el)) {
        href_pdf <- rvest::html_attr(pdf_el, "href")
        ifelse(startsWith(href_pdf, "http"), href_pdf,
               paste0(AFDB_BASE, href_pdf))
      } else NA_character_

      # Country from text
      text_full <- rvest::html_text2(entry)
      country_match <- Filter(function(c) grepl(c, tolower(text_full), fixed = TRUE),
                              tolower(AFRICA_COUNTRIES_EN))[1]
      country <- if (length(country_match) > 0) country_match else NA_character_

      tibble(
        id         = safe_filename(paste0(doc_type_label, "_", title)),
        title      = trimws(title),
        pdf_url    = pdf_url,
        doc_date   = trimws(coalesce(doc_date, NA_character_)),
        doc_type   = doc_type_label,
        country    = country,
        project_id = NA_character_,
        web_url    = web_url
      )
    }, error = function(e) NULL)
  })

  purrr::compact(records) %>% bind_rows()
}

# ── Helper: Visit a document page to get its PDF URL ─────────────────────
resolve_pdf_url <- function(web_url) {
  if (is.na(web_url) || nchar(web_url) == 0) return(NA_character_)

  page <- afdb_read_html(web_url)
  if (is.null(page)) return(NA_character_)

  # Look for direct PDF links
  pdf_links <- rvest::html_elements(page, "a[href$='.pdf'], a[href$='.PDF'], a[href*='fileadmin']")
  if (length(pdf_links) == 0) return(NA_character_)

  href <- rvest::html_attr(pdf_links[[1]], "href")
  if (is.na(href)) return(NA_character_)

  if (startsWith(href, "http")) href else paste0(AFDB_BASE, href)
}

# ── Strategy 1: Scrape IDEV evaluations portal ────────────────────────────
scrape_idev <- function() {
  cli::cli_h2("Strategy 1: IDEV Evaluations Portal (idev.afdb.org)")
  all_results <- tibble()

  # Try each candidate URL until one returns 200
  idev_base_url <- NULL
  for (candidate in IDEV_EVALUATIONS_URLS) {
    cli::cli_alert_info("Trying IDEV URL: {candidate}")
    resp <- afdb_get(candidate)
    if (!is.null(resp) && httr::status_code(resp) == 200) {
      idev_base_url <- candidate
      cli::cli_alert_success("IDEV accessible at: {candidate}")
      break
    } else {
      status <- if (is.null(resp)) "no response" else httr::status_code(resp)
      cli::cli_alert_warning("  {status} — trying next...")
    }
  }

  if (is.null(idev_base_url)) {
    cli::cli_alert_warning("IDEV portal not accessible — skipping.")
    return(tibble())
  }

  for (page_num in 0:AFDB_MAX_PAGES) {
    url <- if (page_num == 0) idev_base_url else
      paste0(idev_base_url, "?page=", page_num)

    cli::cli_alert_info("IDEV page {page_num}: {url}")
    page_html <- afdb_read_html(url)

    if (is.null(page_html)) {
      cli::cli_alert_warning("Could not fetch IDEV page {page_num}, stopping.")
      break
    }

    records <- parse_afdb_listing(page_html, IDEV_BASE, "Evaluation Report")

    if (nrow(records) == 0) {
      cli::cli_alert_info("No more entries at IDEV page {page_num}, stopping.")
      break
    }

    # Fix relative links to use IDEV base
    records <- records %>%
      mutate(web_url = ifelse(!is.na(web_url) & startsWith(web_url, AFDB_BASE),
                              sub(AFDB_BASE, IDEV_BASE, web_url, fixed = TRUE),
                              web_url))

    all_results <- bind_rows(all_results, records)
    cli::cli_alert_success("IDEV page {page_num}: {nrow(records)} documents found")

    next_link <- rvest::html_element(page_html, "a[rel='next'], .pager__item--next a, li.next a")
    if (is.na(next_link)) break
  }

  cli::cli_alert_success("IDEV total: {nrow(all_results)} documents found")
  all_results
}

# ── Strategy 2: Scrape AfDB category pages ────────────────────────────────
scrape_afdb_categories <- function() {
  cli::cli_h2("Strategy 2: AfDB Category Pages (afdb.org)")
  all_results <- tibble()

  for (cat_name in names(AFDB_CATEGORIES)) {
    base_url    <- AFDB_CATEGORIES[[cat_name]]
    doc_label   <- switch(cat_name,
      pcr   = "Project Completion Report",
      par   = "Project Appraisal Report",
      pcren = "PCR Evaluation Note",
      eval  = "Evaluation Report",
      cat_name
    )

    cli::cli_alert_info("Category: {cat_name} ({doc_label})")

    for (page_num in 0:AFDB_MAX_PAGES) {
      url <- if (page_num == 0) base_url else paste0(base_url, "?page=", page_num)
      page_html <- afdb_read_html(url)

      if (is.null(page_html)) {
        cli::cli_alert_warning("  Could not fetch {cat_name} page {page_num} — skipping.")
        break
      }

      records <- parse_afdb_listing(page_html, AFDB_BASE, doc_label)

      if (nrow(records) == 0) {
        cli::cli_alert_info("  No entries on page {page_num}, moving to next category.")
        break
      }

      all_results <- bind_rows(all_results, records)
      cli::cli_alert_success("  Page {page_num}: {nrow(records)} documents")

      # Check for next page
      next_link <- rvest::html_element(page_html, "a[rel='next'], .pager__item--next a, li.next a")
      if (is.na(next_link)) break
    }
  }

  cli::cli_alert_success("AfDB categories total: {nrow(all_results)} documents found")
  all_results
}

# ── Resolve missing PDF URLs by visiting document pages ───────────────────
enrich_pdf_urls <- function(results) {
  cli::cli_h2("Resolving PDF URLs for documents without direct links")

  needs_pdf <- results %>% filter(is.na(pdf_url) & !is.na(web_url))
  has_pdf   <- results %>% filter(!is.na(pdf_url) | is.na(web_url))

  cli::cli_alert_info("{nrow(needs_pdf)} documents need PDF URL resolution")

  if (nrow(needs_pdf) == 0) return(results)

  resolved <- needs_pdf %>%
    rowwise() %>%
    mutate(pdf_url = {
      cli::cli_alert_info("  Resolving: {substr(title, 1, 60)}")
      resolve_pdf_url(web_url)
    }) %>%
    ungroup()

  bind_rows(has_pdf, resolved)
}

# ── Relevance filtering ────────────────────────────────────────────────────
filter_relevant <- function(results) {
  cli::cli_h2("Filtering for relevance (agriculture + adaptation)")

  if (nrow(results) == 0) return(results)

  # Deduplicate by web_url first, then by title
  results <- results %>%
    distinct(web_url, .keep_all = TRUE) %>%
    distinct(title, .keep_all = TRUE)

  cli::cli_alert_info("After dedup: {nrow(results)} documents")

  # AfDB is Africa-focused, so we skip Africa requirement and focus on sector
  results %>%
    rowwise() %>%
    mutate(relevant = passes_relevance_fast(
      paste(coalesce(title, ""), coalesce(doc_type, ""), sep = " "),
      require_africa = AFDB_REQUIRE_AFRICA,
      require_sector = AFDB_REQUIRE_SECTOR
    )) %>%
    ungroup() %>%
    filter(relevant) %>%
    select(-relevant)
}

# ── Download PDFs ─────────────────────────────────────────────────────────
download_results <- function(results) {
  cli::cli_h2("Downloading PDFs")

  with_pdf <- results %>% filter(!is.na(pdf_url) & nchar(pdf_url) > 0)
  cli::cli_alert_info("{nrow(with_pdf)} documents have PDF URLs")

  success <- 0; failed <- 0; skipped <- 0

  for (i in seq_len(nrow(with_pdf))) {
    row <- with_pdf[i, ]

    year <- tryCatch(
      format(as.Date(row$doc_date), "%Y"),
      error = function(e) "XXXX"
    )
    fname <- glue("{SOURCE_NAME}_{safe_filename(coalesce(row$doc_type, 'doc'))}_{safe_filename(coalesce(row$title, 'notitle'))}_{year}.pdf")
    # Truncate to safe length
    fname <- paste0(substr(fname, 1, 150), ".pdf")
    dest  <- file.path(DOWNLOAD_DIR, fname)

    if (file.exists(dest)) {
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "skipped", "File already exists")
      skipped <- skipped + 1
      next
    }

    # Use afdb_get for download (browser UA)
    resp <- afdb_get(row$pdf_url)
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
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "success")
      success <- success + 1
    } else {
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "failed", "Download or PDF validation failed")
      failed <- failed + 1
    }

    if (i %% 10 == 0) cli::cli_alert_info("Progress: {i}/{nrow(with_pdf)} (OK:{success} fail:{failed} skip:{skipped})")
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
  cli::cli_alert_info("Note: AfDB blocks generic user-agents; using browser UA override.")

  # Collect from both strategies
  idev_results  <- tryCatch(scrape_idev(), error = function(e) {
    cli::cli_alert_danger("IDEV strategy failed: {e$message}")
    tibble()
  })

  afdb_results  <- tryCatch(scrape_afdb_categories(), error = function(e) {
    cli::cli_alert_danger("AfDB categories strategy failed: {e$message}")
    tibble()
  })

  all_results <- bind_rows(idev_results, afdb_results)
  cli::cli_alert_info("Total raw: {nrow(all_results)} documents")

  if (nrow(all_results) == 0) {
    cli::cli_alert_danger("No documents found. The site may be blocking all requests.")
    cli::cli_alert_info("Consider: contacting idevhelpdesk@afdb.org for bulk data access.")
    return(invisible(NULL))
  }

  # Filter for relevance
  relevant <- filter_relevant(all_results)
  cli::cli_alert_info("After relevance filter: {nrow(relevant)} documents")

  # Resolve any missing PDF URLs (visit document pages)
  relevant <- enrich_pdf_urls(relevant)

  # Save metadata
  save_metadata(relevant)

  # Download PDFs
  download_results(relevant)

  # Summary
  print_source_summary(SOURCE_NAME)

  cli::cli_h2("Done!")
  invisible(relevant)
}

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  run_afdb_scraper()
}
