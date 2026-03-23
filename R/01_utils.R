##############################################################################
# 01_utils.R — Shared utility functions for all scrapers
# HTTP helpers, PDF validation, relevance filtering, logging
##############################################################################

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(readr)
  library(jsonlite)
  library(xml2)
  library(glue)
  library(fs)
  library(cli)
  library(lubridate)
  library(tibble)
  library(curl)
  library(purrr)
})

# Source config if not already loaded
if (!exists("PATHS")) {
  # Try multiple methods to find 00_config.R
  config_found <- FALSE

  # Method 1: relative to this script (works with source())
  tryCatch({
    config_path <- file.path(dirname(sys.frame(1)$ofile), "00_config.R")
    if (file.exists(config_path)) { source(config_path); config_found <- TRUE }
  }, error = function(e) NULL)

  # Method 2: from working directory
  if (!config_found) {
    candidates <- c(
      file.path(getwd(), "R", "00_config.R"),
      file.path(getwd(), "00_config.R"),
      file.path(getwd(), "..", "00_config.R")
    )
    for (p in candidates) {
      if (file.exists(p)) { source(p); config_found <- TRUE; break }
    }
  }

  if (!config_found) {
    stop("Cannot find 00_config.R. Please source it first or cd into the project root.")
  }
}

# ── Polite HTTP GET ────────────────────────────────────────────────────────
#' Make an HTTP GET request with polite delays, retries, and backoff.
#'
#' @param url Character. The URL to fetch.
#' @param max_retries Integer. Maximum number of retry attempts.
#' @param delay_range Numeric vector of length 2. Min and max delay in seconds.
#' @param ... Additional arguments passed to httr::GET.
#' @return An httr response object, or NULL on failure.
polite_get <- function(url,
                       max_retries = HTTP_CONFIG$max_retries,
                       delay_range = c(HTTP_CONFIG$delay_min, HTTP_CONFIG$delay_max),
                       ...) {
  # Random delay before request
  delay <- runif(1, delay_range[1], delay_range[2])
  Sys.sleep(delay)

  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch(
      {
        httr::GET(
          url,
          httr::user_agent(HTTP_CONFIG$user_agent),
          httr::timeout(HTTP_CONFIG$timeout_sec),
          ...
        )
      },
      error = function(e) {
        cli::cli_alert_warning("HTTP error on attempt {attempt}/{max_retries}: {e$message}")
        NULL
      }
    )

    if (is.null(resp)) {
      if (attempt < max_retries) {
        backoff <- delay * (2^(attempt - 1))
        cli::cli_alert_info("Retrying in {round(backoff, 1)}s...")
        Sys.sleep(backoff)
      }
      next
    }

    status <- httr::status_code(resp)

    # Success
    if (status >= 200 && status < 300) {
      return(resp)
    }

    # Rate limited — backoff and retry
    if (status == 429) {
      retry_after <- as.numeric(httr::headers(resp)[["retry-after"]])
      if (is.na(retry_after)) retry_after <- 30
      backoff <- max(retry_after, delay * (2^attempt))
      cli::cli_alert_warning("Rate limited (429). Waiting {round(backoff, 1)}s...")
      Sys.sleep(backoff)
      next
    }

    # Server error — retry
    if (status >= 500) {
      cli::cli_alert_warning("Server error ({status}) on attempt {attempt}/{max_retries}")
      if (attempt < max_retries) {
        backoff <- delay * (2^(attempt - 1))
        Sys.sleep(backoff)
      }
      next
    }

    # Client error (4xx other than 429) — don't retry
    cli::cli_alert_danger("HTTP {status} for: {url}")
    return(resp)
  }

  cli::cli_alert_danger("All {max_retries} attempts failed for: {url}")
  return(NULL)
}

# ── Safe HTML reader ───────────────────────────────────────────────────────
#' Read an HTML page safely, returning NULL on failure.
#'
#' @param url Character. URL to read.
#' @return An xml_document or NULL.
safe_read_html <- function(url) {
  resp <- polite_get(url)
  if (is.null(resp)) return(NULL)
  if (httr::status_code(resp) != 200) return(NULL)

  tryCatch(
    {
      content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
      # Check for JS-only pages (empty or very short body)
      if (nchar(content_text) < 500 || !grepl("<body", content_text, ignore.case = TRUE)) {
        cli::cli_alert_warning("Page appears to be JS-only (empty body): {url}")
        return(NULL)
      }
      xml2::read_html(content_text)
    },
    error = function(e) {
      cli::cli_alert_warning("HTML parse error: {e$message}")
      NULL
    }
  )
}

# ── Safe JSON fetch ────────────────────────────────────────────────────────
#' Fetch a URL and parse the response as JSON. Returns NULL on failure.
#'
#' @param url Character. URL returning JSON.
#' @return A list (parsed JSON) or NULL.
safe_get_json <- function(url) {
  resp <- polite_get(url)
  if (is.null(resp)) return(NULL)
  if (httr::status_code(resp) != 200) {
    cli::cli_alert_danger("JSON request returned HTTP {httr::status_code(resp)}: {url}")
    return(NULL)
  }

  tryCatch(
    {
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)
    },
    error = function(e) {
      cli::cli_alert_warning("JSON parse error: {e$message}")
      NULL
    }
  )
}

# ── PDF Download & Validation ──────────────────────────────────────────────
#' Download a PDF file with validation.
#'
#' Checks: file size > min_pdf_bytes and first 5 bytes are %PDF-.
#' Deletes invalid files and returns FALSE.
#'
#' @param url Character. PDF URL.
#' @param dest Character. Destination file path.
#' @param min_bytes Integer. Minimum file size in bytes.
#' @return TRUE if download succeeded and PDF is valid, FALSE otherwise.
download_pdf <- function(url, dest,
                         min_bytes = HTTP_CONFIG$min_pdf_bytes) {
  # Skip if already exists

  if (file.exists(dest)) {
    cli::cli_alert_info("Already exists, skipping: {basename(dest)}")
    return("skipped")
  }

  # Ensure directory exists
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  # Download with polite delay
  resp <- polite_get(url)
  if (is.null(resp)) return(FALSE)
  if (httr::status_code(resp) != 200) {
    cli::cli_alert_danger("Download failed (HTTP {httr::status_code(resp)}): {basename(dest)}")
    return(FALSE)
  }

  # Write content
  tryCatch(
    {
      content_raw <- httr::content(resp, as = "raw")
      writeBin(content_raw, dest)
    },
    error = function(e) {
      cli::cli_alert_danger("Write error: {e$message}")
      if (file.exists(dest)) file.remove(dest)
      return(FALSE)
    }
  )

  # Validate: file size
  fsize <- file.info(dest)$size
  if (is.na(fsize) || fsize < min_bytes) {
    cli::cli_alert_warning("PDF too small ({fsize} bytes): {basename(dest)}")
    file.remove(dest)
    return(FALSE)
  }

  # Validate: magic bytes (%PDF-)
  tryCatch(
    {
      con <- file(dest, "rb")
      magic <- readBin(con, "raw", n = 5)
      close(con)
      magic_str <- rawToChar(magic)
      if (magic_str != "%PDF-") {
        cli::cli_alert_warning("Not a valid PDF (magic: {magic_str}): {basename(dest)}")
        file.remove(dest)
        return(FALSE)
      }
    },
    error = function(e) {
      cli::cli_alert_warning("Magic byte check error: {e$message}")
      if (file.exists(dest)) file.remove(dest)
      return(FALSE)
    }
  )

  cli::cli_alert_success("Downloaded: {basename(dest)} ({round(fsize/1024)}KB)")
  return(TRUE)
}

# ── Safe Filename ──────────────────────────────────────────────────────────
#' Sanitize a string for use as a filename.
#'
#' @param x Character. Input string.
#' @param max_len Integer. Maximum filename length.
#' @return A safe filename string.
safe_filename <- function(x, max_len = 120) {
  x <- as.character(x)
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- str_replace_all(x, "[^A-Za-z0-9_\\-]", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x <- str_sub(x, 1, max_len)
  if (nchar(x) == 0) x <- "unnamed"
  x
}

# ── Relevance Filter ──────────────────────────────────────────────────────
#' Check if text matches Africa AND (agriculture OR adaptation) keywords.
#'
#' @param text Character. Text to check (title, abstract, description, etc.)
#' @param require_africa Logical. Whether to require an Africa match.
#' @param require_sector Logical. Whether to require agriculture OR adaptation match.
#' @return TRUE if the text passes the filter.
passes_relevance_filter <- function(text,
                                    require_africa = TRUE,
                                    require_sector = TRUE) {
  if (is.null(text) || is.na(text) || nchar(text) == 0) return(FALSE)
  text_lower <- tolower(text)

  has_africa <- !require_africa
  has_sector <- !require_sector

  if (require_africa) {
    has_africa <- any(sapply(AFRICA_ALL, function(kw) {
      grepl(kw, text_lower, fixed = TRUE)
    }))
  }

  if (require_sector) {
    has_agri <- any(sapply(AGRICULTURE_ALL, function(kw) {
      grepl(kw, text_lower, fixed = TRUE)
    }))
    has_adapt <- any(sapply(ADAPTATION_ALL, function(kw) {
      grepl(kw, text_lower, fixed = TRUE)
    }))
    has_sector <- has_agri || has_adapt
  }

  has_africa && has_sector
}

# ── Faster relevance filter using single regex ─────────────────────────────
#' Build a single regex from a keyword vector for fast matching.
build_regex <- function(keywords) {
  # Escape special regex chars and join with |
  escaped <- str_replace_all(keywords, "([\\[\\](){}\\\\.*+?^$|])", "\\\\\\1")
  paste0("(?:", paste(escaped, collapse = "|"), ")")
}

# Pre-build regex patterns for speed
AFRICA_REGEX    <- build_regex(AFRICA_ALL)
AGRI_REGEX      <- build_regex(AGRICULTURE_ALL)
ADAPT_REGEX     <- build_regex(ADAPTATION_ALL)

#' Fast relevance filter using pre-compiled regex.
passes_relevance_fast <- function(text, require_africa = TRUE, require_sector = TRUE) {
  if (is.null(text) || is.na(text) || nchar(text) == 0) return(FALSE)
  text_lower <- tolower(text)

  if (require_africa && !grepl(AFRICA_REGEX, text_lower, perl = TRUE)) return(FALSE)
  if (require_sector) {
    has_agri  <- grepl(AGRI_REGEX, text_lower, perl = TRUE)
    has_adapt <- grepl(ADAPT_REGEX, text_lower, perl = TRUE)
    if (!has_agri && !has_adapt) return(FALSE)
  }
  TRUE
}

# ── Download Logger ────────────────────────────────────────────────────────
#' Append a log entry to the download log CSV.
#'
#' @param source Character. Source name (e.g., "worldbank", "gcf").
#' @param project_code Character. Project identifier.
#' @param doc_type Character. Document type.
#' @param title Character. Document title.
#' @param url Character. Download URL.
#' @param filepath Character. Local file path (or NA).
#' @param status Character. One of: success, failed, skipped, filtered, blocked, requires_browser.
#' @param notes Character. Additional notes.
log_download <- function(source, project_code = NA, doc_type = NA,
                         title = NA, url = NA, filepath = NA,
                         status = "success", notes = NA) {
  entry <- tibble(
    timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    source       = source,
    project_code = as.character(project_code),
    doc_type     = as.character(doc_type),
    title        = as.character(title),
    url          = as.character(url),
    filepath     = as.character(filepath),
    status       = status,
    notes        = as.character(notes)
  )

  log_path <- PATHS$log_file

  if (file.exists(log_path)) {
    readr::write_csv(entry, log_path, append = TRUE)
  } else {
    readr::write_csv(entry, log_path)
  }
}

# ── Summary Reporter ───────────────────────────────────────────────────────
#' Print a summary of the download log for a given source.
print_source_summary <- function(source_name) {
  log_path <- PATHS$log_file
  if (!file.exists(log_path)) {
    cli::cli_alert_warning("No log file found.")
    return(invisible(NULL))
  }

  log <- readr::read_csv(log_path, show_col_types = FALSE) %>%
    filter(source == source_name)

  if (nrow(log) == 0) {
    cli::cli_alert_warning("No entries for source: {source_name}")
    return(invisible(NULL))
  }

  counts <- log %>% count(status) %>% arrange(desc(n))

  cli::cli_h2("Summary for: {source_name}")
  cli::cli_text("Total entries: {nrow(log)}")
  for (i in seq_len(nrow(counts))) {
    cli::cli_text("  {counts$status[i]}: {counts$n[i]}")
  }

  return(invisible(log))
}

# ── Check required packages ────────────────────────────────────────────────
check_packages <- function() {
  required <- c("httr", "rvest", "dplyr", "stringr", "readr", "jsonlite",
                 "xml2", "glue", "fs", "cli", "lubridate", "tibble",
                 "curl", "purrr")
  missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    cli::cli_alert_danger("Missing packages: {paste(missing, collapse = ', ')}")
    cli::cli_alert_info("Install with: install.packages(c({paste0('\"', missing, '\"', collapse = ', ')}))")
    return(FALSE)
  }
  cli::cli_alert_success("All required packages are installed.")
  return(TRUE)
}

cat("✓ Utils loaded.\n")
