##############################################################################
# worldbank.R — World Bank Documents & Reports API Scraper
#
# Targets: Implementation Completion and Results Reports (ICRs),
#          Project Performance Assessment Reviews (PPARs),
#          Project Appraisal Documents (PADs) for Africa agriculture/adaptation
#
# API docs: https://documents.worldbank.org/en/publication/documents-reports/api
# Base URL: https://search.worldbank.org/api/v3/wds
#
# Strategy:
#   1. Query by document type (ICR, PPAR) — these are structured fields
#   2. Filter by African countries using count_exact
#   3. Also run keyword-based queries for agriculture + adaptation + Africa
#   4. Deduplicate by document ID
#   5. Download PDFs, validate, log everything
##############################################################################

# ── Setup ──────────────────────────────────────────────────────────────────
# Find and source config + utils. Works with Rscript, source(), and interactive R.
.find_r_dir <- function() {
  # Try sys.frame (works with source())
  tryCatch({
    d <- dirname(sys.frame(2)$ofile)
    if (!is.null(d) && nzchar(d)) return(normalizePath(file.path(d, ".."), mustWork = FALSE))
  }, error = function(e) NULL)
  # Try commandArgs (works with Rscript)
  tryCatch({
    args <- commandArgs(trailingOnly = FALSE)
    fa <- grep("^--file=", args, value = TRUE)
    if (length(fa)) return(normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), ".."), mustWork = FALSE))
  }, error = function(e) NULL)
  # Fall back to working directory
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

DOWNLOAD_DIR <- file.path(PATHS$downloads, "worldbank")
dir.create(DOWNLOAD_DIR, recursive = TRUE, showWarnings = FALSE)

SOURCE_NAME <- "worldbank"

# ── World Bank API Configuration ───────────────────────────────────────────
WB_API_BASE <- "https://search.worldbank.org/api/v3/wds"

# Document types to search (exact match values for docty_exact)
WB_DOC_TYPES <- c(

  "Implementation Completion and Results Report",
  "Implementation Completion Report",
  "Project Performance Assessment Review"
)

# Fields to return in API response
WB_FIELDS <- paste(
  "id", "display_title", "pdfurl", "docdt", "docty", "count",
  "repnme", "projn", "abstracts", "lang_exact", "url",
  sep = ","
)

# Keyword search queries (used for broader matching)
WB_KEYWORD_QUERIES <- c(
  "agriculture adaptation climate Africa",
  "climate resilience agriculture Africa",
  "climate smart agriculture Africa",
  "food security climate change Africa",
  "irrigation climate adaptation Africa",
  "livestock resilience Africa drought",
  "agricultural development climate Africa"
)

# ── Helper: Build WB API URL ──────────────────────────────────────────────
build_wb_url <- function(qterm = NULL, docty = NULL, country = NULL,
                         rows = 50, offset = 0) {
  params <- list(
    format = "json",
    fl     = WB_FIELDS,
    rows   = rows,
    os     = offset
  )
  if (!is.null(qterm))   params$qterm        <- qterm
  if (!is.null(docty))   params$docty_exact   <- docty
  if (!is.null(country)) params$count_exact   <- country

  url <- paste0(WB_API_BASE, "?", paste(
    mapply(function(k, v) paste0(k, "=", utils::URLencode(as.character(v), reserved = TRUE)),
           names(params), params),
    collapse = "&"
  ))
  url
}

# ── Helper: Parse WB API response ─────────────────────────────────────────
parse_wb_response <- function(json_data) {
  if (is.null(json_data)) return(tibble())

  # The API returns documents in a named list under "documents"
  docs <- json_data$documents
  if (is.null(docs) || length(docs) == 0) return(tibble())

  # Remove the "facets" entry if present
  docs[["facets"]] <- NULL

  records <- purrr::compact(purrr::map(docs, function(doc) {
    tryCatch({
      # Helper: safely flatten any field to a single character string
      flatten_field <- function(x) {
        if (is.null(x)) return(NA_character_)
        if (is.list(x)) return(paste(unlist(x), collapse = "; "))
        as.character(x)
      }
      
      tibble(
        id            = flatten_field(doc$id),
        title         = flatten_field(doc$display_title),
        pdf_url       = flatten_field(doc$pdfurl),
        doc_date      = flatten_field(doc$docdt),
        doc_type      = flatten_field(doc$docty),
        country       = flatten_field(doc$count),
        report_name   = flatten_field(doc$repnme),
        project_id    = flatten_field(doc$projn),
        abstract      = flatten_field(doc$abstracts),
        language      = flatten_field(doc$lang_exact),
        web_url       = flatten_field(doc$url)
      )
    }, error = function(e) NULL)
  }))
  
  if (length(records) == 0) return(tibble())
  bind_rows(records)
}

# ── Strategy 1: Search by document type for each African country ──────────
search_by_doctype_country <- function() {
  cli::cli_h2("Strategy 1: Search by document type × African country")

  all_results <- tibble()
  country_names <- unname(WB_AFRICA_NAMES)

  total_combos <- length(WB_DOC_TYPES) * length(country_names)
  combo_count <- 0

  for (docty in WB_DOC_TYPES) {
    for (country in country_names) {
      combo_count <- combo_count + 1

      if (combo_count %% 20 == 0) {
        cli::cli_alert_info("Progress: {combo_count}/{total_combos} combinations...")
      }

      offset <- 0
      page_size <- 50

      repeat {
        url <- build_wb_url(docty = docty, country = country,
                            rows = page_size, offset = offset)
        json <- safe_get_json(url)

        if (is.null(json)) break

        total <- as.numeric(json$total %||% 0)
        if (total == 0) break

        records <- parse_wb_response(json)
        if (nrow(records) == 0) break

        all_results <- bind_rows(all_results, records)
        offset <- offset + page_size

        if (offset >= total) break
      }
    }
  }

  cli::cli_alert_success("Strategy 1 found {nrow(all_results)} raw results")
  all_results
}

# ── Strategy 2: Keyword searches ──────────────────────────────────────────
search_by_keywords <- function() {
  cli::cli_h2("Strategy 2: Keyword-based searches")

  all_results <- tibble()

  for (query in WB_KEYWORD_QUERIES) {
    cli::cli_alert_info("Searching: {query}")

    offset <- 0
    page_size <- 50
    max_results <- 500  # cap per query

    repeat {
      url <- build_wb_url(qterm = query, rows = page_size, offset = offset)
      json <- safe_get_json(url)

      if (is.null(json)) break

      total <- as.numeric(json$total %||% 0)
      if (total == 0) break

      records <- parse_wb_response(json)
      if (nrow(records) == 0) break

      all_results <- bind_rows(all_results, records)
      offset <- offset + page_size

      if (offset >= total || offset >= max_results) break
    }
  }

  cli::cli_alert_success("Strategy 2 found {nrow(all_results)} raw results")
  all_results
}

# ── Relevance filtering ───────────────────────────────────────────────────
filter_relevant <- function(results) {
  cli::cli_h2("Filtering for relevance")

  if (nrow(results) == 0) return(results)

  # Deduplicate by document ID
  results <- results %>% distinct(id, .keep_all = TRUE)
  cli::cli_alert_info("After dedup: {nrow(results)} documents")

  # Build combined text for filtering
  results <- results %>%
    mutate(
      combined_text = paste(
        coalesce(title, ""),
        coalesce(country, ""),
        coalesce(abstract, ""),
        coalesce(report_name, ""),
        sep = " "
      )
    )

  # Apply relevance filter: Africa + (agriculture OR adaptation)
  results <- results %>%
    rowwise() %>%
    mutate(relevant = passes_relevance_fast(combined_text,
                                             require_africa = TRUE,
                                             require_sector = TRUE)) %>%
    ungroup() %>%
    filter(relevant) %>%
    select(-combined_text, -relevant)

  cli::cli_alert_success("After relevance filter: {nrow(results)} documents")
  results
}

# ── Download PDFs ─────────────────────────────────────────────────────────
download_results <- function(results) {
  cli::cli_h2("Downloading PDFs")

  if (nrow(results) == 0) {
    cli::cli_alert_warning("No documents to download.")
    return(invisible(NULL))
  }

  # Filter to those with PDF URLs
  with_pdf <- results %>% filter(!is.na(pdf_url) & nchar(pdf_url) > 0)
  cli::cli_alert_info("{nrow(with_pdf)} documents have PDF URLs")

  success <- 0
  failed <- 0
  skipped <- 0

  for (i in seq_len(nrow(with_pdf))) {
    row <- with_pdf[i, ]

    # Build filename
    year <- tryCatch(
      format(as.Date(row$doc_date), "%Y"),
      error = function(e) "XXXX"
    )
    proj <- safe_filename(coalesce(row$project_id, "noproj"))
    dtype <- safe_filename(coalesce(row$doc_type, "doc"))
    fname <- glue("{SOURCE_NAME}_{proj}_{dtype}_{year}.pdf")
    dest <- file.path(DOWNLOAD_DIR, fname)

    # Check if already exists
    if (file.exists(dest)) {
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "skipped",
                   "File already exists")
      skipped <- skipped + 1
      next
    }

    # Download
    result <- download_pdf(row$pdf_url, dest)

    if (identical(result, TRUE)) {
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "success")
      success <- success + 1
    } else if (identical(result, "skipped")) {
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "skipped")
      skipped <- skipped + 1
    } else {
      log_download(SOURCE_NAME, row$project_id, row$doc_type,
                   row$title, row$pdf_url, dest, "failed",
                   "Download or validation failed")
      failed <- failed + 1
    }

    # Progress
    if (i %% 10 == 0) {
      cli::cli_alert_info("Progress: {i}/{nrow(with_pdf)} " %>%
        paste0("(OK: {success}, fail: {failed}, skip: {skipped})"))
    }
  }

  cli::cli_h3("Download Summary")
  cli::cli_alert_success("Success: {success}")
  cli::cli_alert_danger("Failed: {failed}")
  cli::cli_alert_info("Skipped: {skipped}")
  cli::cli_alert_info("No PDF URL: {nrow(results) - nrow(with_pdf)}")
}

# ── Save metadata ─────────────────────────────────────────────────────────
save_metadata <- function(results) {
  meta_path <- file.path(PATHS$data, "worldbank_metadata.csv")
  readr::write_csv(results, meta_path)
  cli::cli_alert_success("Metadata saved to: {meta_path}")
}

# ── Main execution ────────────────────────────────────────────────────────
run_worldbank_scraper <- function() {
  cli::cli_h1("World Bank Grey Literature Scraper")
  cli::cli_alert_info("Download directory: {DOWNLOAD_DIR}")

  # Collect results from both strategies
  results1 <- search_by_doctype_country()
  results2 <- search_by_keywords()

  # Combine and deduplicate
  all_results <- bind_rows(results1, results2)
  cli::cli_alert_info("Total raw results: {nrow(all_results)}")

  # Filter for relevance
  relevant <- filter_relevant(all_results)

  # Save metadata before downloading
  save_metadata(relevant)

  # Download PDFs
  download_results(relevant)

  # Print summary
  print_source_summary(SOURCE_NAME)

  cli::cli_h2("Done!")
  invisible(relevant)
}

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  run_worldbank_scraper()
}
