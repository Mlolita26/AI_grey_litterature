##############################################################################
# gef.R — Global Environment Facility (GEF) Project Document Scraper
#
# Targets: Project documents (evaluations, CEO endorsements, proposals)
#          for Climate Change projects in Africa, Approved/Completed since 2000
#
# Website: https://www.thegef.org/projects-operations/database
# The GEF site is Drupal with server-rendered HTML tables.
# Documents are hosted on Azure CDN (publicpartnershipdata.azureedge.net).
#
# Strategy:
#   1. Scrape project metadata from HTML tables (per African country)
#   2. Filter for approval year >= 2000
#   3. Visit each project page to collect document links
#   4. Filter for target document types
#   5. Download PDFs
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

DOWNLOAD_DIR <- file.path(PATHS$downloads, "gef")
dir.create(DOWNLOAD_DIR, recursive = TRUE, showWarnings = FALSE)

SOURCE_NAME <- "gef"

# ── GEF Configuration ─────────────────────────────────────────────────────
GEF_BASE <- "https://www.thegef.org"
GEF_DB_URL <- paste0(GEF_BASE, "/projects-operations/database")
GEF_EXPORT_URL <- paste0(GEF_DB_URL, "/export")

# Focal area IDs
# GEF has no "Agriculture" focal area; the five available are:
#   Biodiversity (2205), Chemicals & Waste (2206), Climate Change (2207),
#   International Waters (2209), Land Degradation (2210).
# Land Degradation (2210) often covers dryland/soil agriculture adaptation.
# To also scrape Land Degradation projects, add it to the focal_area loop below.
GEF_FOCAL_CLIMATE_CHANGE  <- "2207"
GEF_FOCAL_LAND_DEGRADATION <- "2210"  # optional: uncomment in run_gef_scraper() if needed

# Status IDs
GEF_STATUS_COMPLETED <- "396"
GEF_STATUS_APPROVED  <- "412"

# African country IDs for project_country_national facet
GEF_AFRICA_COUNTRIES <- c(
  Algeria = "15", Angola = "16", Benin = "27", Botswana = "31",
  `Burkina Faso` = "34", Burundi = "35", `Cabo Verde` = "36",
  Cameroon = "38", `Central African Republic` = "39", Chad = "40",
  Comoros = "44", Congo = "45", `Congo DR` = "46",
  `Cote d'Ivoire` = "49", Djibouti = "53", Egypt = "57",
  `Equatorial Guinea` = "59", Eritrea = "60", Eswatini = "155",
  Ethiopia = "62", Gabon = "64", Gambia = "65", Ghana = "67",
  Guinea = "71", `Guinea-Bissau` = "72", Kenya = "84",
  Lesotho = "93", Liberia = "94", Libya = "95", Madagascar = "98",
  Malawi = "99", Mali = "102", Mauritania = "105", Mauritius = "106",
  Morocco = "815147", Mozambique = "113", Namibia = "115",
  Nigeria = "120", Rwanda = "135", Senegal = "139",
  Seychelles = "141", `Sierra Leone` = "142", Somalia = "146",
  `South Africa` = "147", `South Sudan` = "148", Sudan = "153",
  Tanzania = "158", Togo = "161", Tunisia = "164",
  Uganda = "168", Zambia = "176", Zimbabwe = "177",
  # Two countries added — missing from original list:
  Niger = "119", `Sao Tome and Principe` = "137"
)

# Minimum approval year
GEF_MIN_YEAR <- 2000

# Document type patterns to target
GEF_DOC_TYPE_PATTERNS <- c(
  "terminal.evaluation",
  "mid.?term.review",
  "CEO.endorsement",
  "project.document",
  "project.identification",
  "PIF",
  "evaluation",
  "completion",
  "PIR",     # Project Implementation Report
  "MTR",     # Mid-Term Review
  "TE"       # Terminal Evaluation
)

# ── Helper: Build GEF filter URL ──────────────────────────────────────────
build_gef_url <- function(country_id = NULL,
                          focal_area = GEF_FOCAL_CLIMATE_CHANGE,
                          statuses = c(GEF_STATUS_COMPLETED, GEF_STATUS_APPROVED),
                          page = NULL) {
  filters <- c()
  idx <- 0

  # Focal area
  filters <- c(filters, paste0("f[", idx, "]=focal_areas:", focal_area))
  idx <- idx + 1

  # Statuses
  for (s in statuses) {
    filters <- c(filters, paste0("f[", idx, "]=latest_timeline_status:", s))
    idx <- idx + 1
  }

  # Country (optional — omit to get all)
  if (!is.null(country_id)) {
    filters <- c(filters, paste0("f[", idx, "]=project_country_national:", country_id))
    idx <- idx + 1
  }

  url <- paste0(GEF_DB_URL, "?", paste(filters, collapse = "&"))

  if (!is.null(page) && page > 0) {
    url <- paste0(url, "&page=", page)
  }

  url
}

# ── Step 1: Scrape project listing from HTML table ────────────────────────
collect_project_metadata <- function() {
  cli::cli_h2("Step 1: Scraping project metadata from HTML table")

  all_projects <- list()
  n_countries <- length(GEF_AFRICA_COUNTRIES)

  for (i in seq_along(GEF_AFRICA_COUNTRIES)) {
    country_name <- names(GEF_AFRICA_COUNTRIES)[i]
    country_id <- GEF_AFRICA_COUNTRIES[i]

    cli::cli_alert_info("[{i}/{n_countries}] Scraping: {country_name}")

    page <- 0
    country_count <- 0

    repeat {
      url <- build_gef_url(country_id = country_id, page = page)
      html <- safe_read_html(url)
      if (is.null(html)) break

      # Parse all table rows
      rows <- html %>% rvest::html_nodes("table tbody tr")
      if (length(rows) == 0) break

      for (tr in rows) {
        cells <- tr %>% rvest::html_nodes("td")
        if (length(cells) < 3) next

        # Find the project link
        link <- tr %>% rvest::html_node("a[href*='/projects-operations/projects/']")
        if (is.null(link)) next

        href <- rvest::html_attr(link, "href")
        if (is.na(href)) next

        title <- trimws(rvest::html_text(link))
        cell_texts <- trimws(rvest::html_text(cells))

        gef_id <- sub(".*/projects/", "", href)
        project_url <- if (grepl("^https?://", href)) href else paste0(GEF_BASE, href)

        # Columns: Title, ID, Countries, Focal Areas, Type, Agencies, GEF Grant, Cofinancing, Status
        all_projects[[length(all_projects) + 1]] <- list(
          gef_id        = as.character(gef_id),
          project_title = as.character(title),
          project_url   = as.character(project_url),
          gef_project_id = if (length(cell_texts) >= 2) cell_texts[2] else NA_character_,
          country       = if (length(cell_texts) >= 3) cell_texts[3] else country_name,
          focal_areas   = if (length(cell_texts) >= 4) cell_texts[4] else NA_character_,
          project_type  = if (length(cell_texts) >= 5) cell_texts[5] else NA_character_,
          agencies      = if (length(cell_texts) >= 6) cell_texts[6] else NA_character_,
          gef_grant     = if (length(cell_texts) >= 7) cell_texts[7] else NA_character_,
          cofinancing   = if (length(cell_texts) >= 8) cell_texts[8] else NA_character_,
          status        = if (length(cell_texts) >= 9) cell_texts[9] else NA_character_,
          query_country = country_name
        )
        country_count <- country_count + 1
      }

      # Check for next page
      next_link <- html %>% rvest::html_node("li.pager-next a, a[rel='next']")
      if (is.null(next_link)) break

      page <- page + 1
      if (page > 100) break  # safety
    }

    if (country_count > 0) {
      cli::cli_alert_success("  {country_name}: {country_count} projects")
    } else {
      cli::cli_alert_info("  {country_name}: 0 projects")
    }

    if (i %% 10 == 0) {
      cli::cli_alert_info("Progress: {i}/{n_countries} countries, {length(all_projects)} total projects")
    }
  }

  if (length(all_projects) == 0) {
    cli::cli_alert_danger("No projects found.")
    return(tibble(
      gef_id = character(), project_title = character(),
      project_url = character(), gef_project_id = character(),
      country = character(), focal_areas = character(),
      project_type = character(), agencies = character(),
      gef_grant = character(), cofinancing = character(),
      status = character(), query_country = character()
    ))
  }

  result <- bind_rows(lapply(all_projects, as_tibble))
  cli::cli_alert_success("Total raw projects collected: {nrow(result)}")
  result
}

# ── Step 2: Clean and filter project metadata ─────────────────────────────
clean_project_metadata <- function(projects) {
  cli::cli_h2("Step 2: Cleaning and filtering project metadata")

  if (nrow(projects) == 0) return(projects)

  # Standardize column names (CSV export may have varied names)
  names(projects) <- tolower(gsub("[^a-z0-9]+", "_", tolower(names(projects))))
  names(projects) <- gsub("_+", "_", names(projects))
  names(projects) <- gsub("^_|_$", "", names(projects))

  cli::cli_alert_info("Columns: {paste(names(projects), collapse = ', ')}")

  # Try to find the GEF ID column
  id_col <- grep("gef_id|gef.id|project_id|id", names(projects), value = TRUE)[1]
  title_col <- grep("title|name|project_name", names(projects), value = TRUE)[1]
  country_col <- grep("countr|nation", names(projects), value = TRUE)[1]
  status_col <- grep("status|timeline", names(projects), value = TRUE)[1]
  year_col <- grep("approval|fiscal.year|fy|year", names(projects), value = TRUE)[1]

  # Rename to standard names
  if (!is.na(id_col)) projects$gef_id <- as.character(projects[[id_col]])
  if (!is.na(title_col)) projects$project_title <- as.character(projects[[title_col]])
  if (!is.na(country_col)) projects$country <- as.character(projects[[country_col]])
  if (!is.na(status_col)) projects$status <- as.character(projects[[status_col]])

  # Extract year for filtering
  if (!is.na(year_col)) {
    projects$approval_year <- tryCatch({
      yr <- as.numeric(gsub("[^0-9]", "", as.character(projects[[year_col]])))
      # Handle 2-digit years
      ifelse(!is.na(yr) & yr < 100, yr + 2000, yr)
    }, error = function(e) NA_real_)
  } else {
    projects$approval_year <- NA_real_
  }

  # Deduplicate by GEF ID
  if ("gef_id" %in% names(projects)) {
    before <- nrow(projects)
    projects <- projects[!duplicated(projects$gef_id), ]
    cli::cli_alert_info("Deduped: {before} -> {nrow(projects)} projects")
  }

  # Filter by year
  if ("approval_year" %in% names(projects) && !all(is.na(projects$approval_year))) {
    before <- nrow(projects)
    projects <- projects[is.na(projects$approval_year) | projects$approval_year >= GEF_MIN_YEAR, ]
    cli::cli_alert_info("Year filter (>= {GEF_MIN_YEAR}): {before} -> {nrow(projects)} projects")
  }

  # Build project URLs if not already present
  if (!"project_url" %in% names(projects) && "gef_id" %in% names(projects)) {
    projects$project_url <- paste0(GEF_BASE, "/projects-operations/projects/", projects$gef_id)
  }

  cli::cli_alert_success("After cleaning: {nrow(projects)} projects")
  projects
}

# ── Step 3: Scrape documents from each project page ───────────────────────
scrape_project_documents <- function(projects) {
  cli::cli_h2("Step 3: Scraping document links from project pages")

  all_docs <- tibble(
    gef_id = character(), project_title = character(),
    project_url = character(), doc_title = character(),
    doc_url = character(), doc_type = character(),
    country = character()
  )

  n <- nrow(projects)

  for (i in seq_len(n)) {
    gef_id <- as.character(projects$gef_id[i])
    proj_url <- as.character(projects$project_url[i])
    proj_title <- as.character(projects$project_title[i] %||% NA_character_)
    proj_country <- as.character(projects$country[i] %||% NA_character_)

    cli::cli_alert_info("[{i}/{n}] Scraping: GEF {gef_id}")

    html <- safe_read_html(proj_url)
    if (is.null(html)) {
      cli::cli_alert_warning("  Failed to load project page")
      next
    }

    # Get better title from page if needed
    if (is.na(proj_title)) {
      proj_title <- tryCatch({
        t <- html %>% rvest::html_node("h1") %>% rvest::html_text() %>% trimws()
        if (!is.na(t) && nchar(t) > 0) t else NA_character_
      }, error = function(e) NA_character_)
    }

    # Find all document links on the project page
    new_docs <- extract_documents_from_page(html, gef_id, proj_title, proj_url, proj_country)

    if (nrow(new_docs) > 0) {
      all_docs <- bind_rows(all_docs, new_docs)
      cli::cli_alert_success("  Found {nrow(new_docs)} documents")
    } else {
      cli::cli_alert_info("  No documents found")
    }

    if (i %% 20 == 0) {
      cli::cli_alert_info("Progress: {i}/{n} projects, {nrow(all_docs)} total docs")
    }
  }

  # Deduplicate
  if (nrow(all_docs) > 0) {
    all_docs <- all_docs[!duplicated(all_docs$doc_url), ]
  }

  cli::cli_alert_success("Total unique documents: {nrow(all_docs)}")
  all_docs
}

# ── Helper: Extract documents from a project page ─────────────────────────
extract_documents_from_page <- function(html, gef_id, proj_title, proj_url, country) {
  results <- list()

  tryCatch({
    # Strategy 1: Azure CDN links (primary document host)
    azure_links <- html %>%
      rvest::html_nodes("a[href*='azureedge.net']") %>%
      rvest::html_attr("href")

    # Strategy 2: Direct PDF links
    pdf_links <- html %>%
      rvest::html_nodes("a[href$='.pdf']") %>%
      rvest::html_attr("href")

    # Strategy 3: Any document links with common file extensions
    doc_links <- html %>%
      rvest::html_nodes("a[href$='.doc'], a[href$='.docx'], a[href$='.pdf']") %>%
      rvest::html_attr("href")

    # Combine all unique document URLs
    all_links <- unique(c(azure_links, pdf_links, doc_links))
    all_links <- all_links[!is.na(all_links) & nchar(all_links) > 0]

    if (length(all_links) == 0) {
      # Strategy 4: Look for links in document sections/tables
      doc_sections <- html %>%
        rvest::html_nodes(".field--name-field-project-documents a, .document-list a, table a[href*='pdf']")
      extra_links <- doc_sections %>% rvest::html_attr("href")
      all_links <- unique(extra_links[!is.na(extra_links)])
    }

    # Get the anchor text for each link for title/type inference
    all_anchors <- html %>% rvest::html_nodes("a")
    all_anchor_hrefs <- all_anchors %>% rvest::html_attr("href")

    for (link in all_links) {
      # Get anchor text
      matching_idx <- which(all_anchor_hrefs == link)
      doc_title <- if (length(matching_idx) > 0) {
        trimws(rvest::html_text(all_anchors[matching_idx[1]]))
      } else {
        basename(link)
      }

      # Infer document type from URL and title
      doc_type <- infer_gef_doc_type(link, doc_title)

      # Ensure absolute URL
      full_url <- if (grepl("^https?://", link)) link else paste0(GEF_BASE, link)

      results[[length(results) + 1]] <- list(
        gef_id        = as.character(gef_id),
        project_title = as.character(proj_title %||% NA_character_),
        project_url   = as.character(proj_url),
        doc_title     = as.character(doc_title),
        doc_url       = as.character(full_url),
        doc_type      = as.character(doc_type),
        country       = as.character(country %||% NA_character_)
      )
    }
  }, error = function(e) {
    cli::cli_alert_warning("  Error extracting documents: {e$message}")
  })

  if (length(results) == 0) {
    return(tibble(
      gef_id = character(), project_title = character(),
      project_url = character(), doc_title = character(),
      doc_url = character(), doc_type = character(),
      country = character()
    ))
  }

  bind_rows(lapply(results, as_tibble))
}

# ── Helper: Infer document type from URL and title ────────────────────────
infer_gef_doc_type <- function(url, title) {
  combined <- tolower(paste(url, title))

  if (grepl("terminal.evaluation|/te/|_te[_.]", combined)) return("Terminal Evaluation")
  if (grepl("mid.?term.review|/mtr/|_mtr[_.]", combined)) return("Mid-Term Review")
  if (grepl("pir|implementation.report", combined))        return("Project Implementation Report")
  if (grepl("ceo.endorsement|ceoendorsement", combined))   return("CEO Endorsement")
  if (grepl("pif|project.identification", combined))        return("Project Identification Form")
  if (grepl("project.document|prodoc", combined))           return("Project Document")
  if (grepl("review.sheet", combined))                      return("Review Sheet")
  if (grepl("completion", combined))                        return("Completion Report")
  if (grepl("evaluation", combined))                        return("Evaluation")
  if (grepl("tracking.tool|tt[_.]", combined))              return("Tracking Tool")
  if (grepl("agency.project", combined))                    return("Agency Project Document")

  NA_character_
}

# ── Step 4: Filter documents by type ──────────────────────────────────────
filter_target_documents <- function(docs) {
  cli::cli_h2("Step 4: Filtering for target document types")

  if (nrow(docs) == 0) return(docs)

  type_pattern <- paste(GEF_DOC_TYPE_PATTERNS, collapse = "|")

  docs$is_target <- grepl(type_pattern, docs$doc_type, ignore.case = TRUE) |
                    grepl(type_pattern, docs$doc_title, ignore.case = TRUE) |
                    grepl(type_pattern, docs$doc_url, ignore.case = TRUE)

  target_docs <- docs[docs$is_target, ]
  target_docs$is_target <- NULL
  excluded_n <- sum(!docs$is_target)

  cli::cli_alert_success("Target documents: {nrow(target_docs)} (filtered out {excluded_n})")

  if (nrow(target_docs) > 0) {
    type_counts <- target_docs %>% count(doc_type) %>% arrange(desc(n))
    for (j in seq_len(min(nrow(type_counts), 15))) {
      cli::cli_alert_info("  {coalesce(type_counts$doc_type[j], '(unknown)')}: {type_counts$n[j]}")
    }
  }

  target_docs
}

# ── Step 5: Download documents ────────────────────────────────────────────
download_documents <- function(docs) {
  cli::cli_h2("Step 5: Downloading documents")

  if (nrow(docs) == 0) {
    cli::cli_alert_warning("No documents to download.")
    return(invisible(NULL))
  }

  cli::cli_alert_info("{nrow(docs)} documents to download")

  success <- 0
  failed  <- 0
  skipped <- 0

  for (i in seq_len(nrow(docs))) {
    gef_id <- as.character(docs$gef_id[i])
    dtype  <- as.character(docs$doc_type[i] %||% "doc")
    doc_url <- as.character(docs$doc_url[i])

    # Determine file extension from URL
    ext <- tools::file_ext(doc_url)
    if (nchar(ext) == 0 || nchar(ext) > 5) ext <- "pdf"
    ext <- tolower(ext)

    # Build filename
    proj <- safe_filename(gef_id)
    dtype_safe <- safe_filename(dtype)
    fname <- glue("{SOURCE_NAME}_{proj}_{dtype_safe}.{ext}")
    dest <- file.path(DOWNLOAD_DIR, fname)

    # Handle duplicate filenames
    if (file.exists(dest)) {
      base_name <- tools::file_path_sans_ext(fname)
      counter <- 1
      while (file.exists(dest)) {
        dest <- file.path(DOWNLOAD_DIR, glue("{base_name}_{counter}.{ext}"))
        counter <- counter + 1
        if (counter > 10) break
      }
      if (counter > 10) {
        log_download(SOURCE_NAME, gef_id, dtype,
                     docs$doc_title[i], doc_url, dest, "skipped",
                     "Too many duplicates")
        skipped <- skipped + 1
        next
      }
    }

    # Download (use download_pdf for PDFs, polite_get + writeBin for others)
    if (ext == "pdf") {
      result <- download_pdf(doc_url, dest)
    } else {
      result <- tryCatch({
        Sys.sleep(runif(1, HTTP_CONFIG$delay_min, HTTP_CONFIG$delay_max))
        resp <- httr::GET(
          doc_url,
          httr::user_agent(HTTP_CONFIG$user_agent),
          httr::timeout(HTTP_CONFIG$timeout_sec)
        )
        if (httr::status_code(resp) == 200) {
          writeBin(httr::content(resp, as = "raw"), dest)
          if (file.info(dest)$size > HTTP_CONFIG$min_pdf_bytes) TRUE else {
            file.remove(dest)
            FALSE
          }
        } else FALSE
      }, error = function(e) FALSE)
    }

    if (identical(result, TRUE)) {
      log_download(SOURCE_NAME, gef_id, dtype,
                   docs$doc_title[i], doc_url, dest, "success")
      success <- success + 1
    } else if (identical(result, "skipped")) {
      log_download(SOURCE_NAME, gef_id, dtype,
                   docs$doc_title[i], doc_url, dest, "skipped")
      skipped <- skipped + 1
    } else {
      log_download(SOURCE_NAME, gef_id, dtype,
                   docs$doc_title[i], doc_url, dest, "failed",
                   "Download or validation failed")
      failed <- failed + 1
    }

    if (i %% 20 == 0) {
      cli::cli_alert_info("Progress: {i}/{nrow(docs)} (OK: {success}, fail: {failed}, skip: {skipped})")
    }
  }

  cli::cli_h3("Download Summary")
  cli::cli_alert_success("Success: {success}")
  cli::cli_alert_danger("Failed: {failed}")
  cli::cli_alert_info("Skipped: {skipped}")
}

# ── Save metadata ─────────────────────────────────────────────────────────
save_metadata <- function(data, filename) {
  meta_path <- file.path(PATHS$data, filename)
  # Flatten any list-columns
  for (col in names(data)) {
    if (is.list(data[[col]])) {
      data[[col]] <- vapply(data[[col]], function(x) {
        if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])
      }, character(1))
    }
  }
  readr::write_csv(as.data.frame(data), meta_path)
  cli::cli_alert_success("Saved to: {meta_path}")
}

# ── Main execution ────────────────────────────────────────────────────────
run_gef_scraper <- function() {
  cli::cli_h1("GEF Grey Literature Scraper")
  cli::cli_alert_info("Download directory: {DOWNLOAD_DIR}")
  cli::cli_alert_info("Filters: Climate Change, Africa, Approved + Completed, >= {GEF_MIN_YEAR}")

  # Step 1: Collect project metadata by scraping HTML tables
  raw_projects <- collect_project_metadata()

  if (nrow(raw_projects) == 0) {
    cli::cli_alert_danger("No projects found.")
    return(invisible(NULL))
  }

  # Step 2: Clean, deduplicate, and filter by year
  projects <- clean_project_metadata(raw_projects)

  if (nrow(projects) == 0) {
    cli::cli_alert_danger("No projects after filtering.")
    return(invisible(NULL))
  }

  # Save project metadata
  save_metadata(projects, "gef_projects_metadata.csv")

  # Step 3: Scrape documents from each project page
  all_docs <- scrape_project_documents(projects)

  if (nrow(all_docs) == 0) {
    cli::cli_alert_danger("No documents found on any project page.")
    return(invisible(projects))
  }

  # Save all documents metadata
  save_metadata(all_docs, "gef_all_documents.csv")

  # Step 4: Filter for target document types
  target_docs <- filter_target_documents(all_docs)

  if (nrow(target_docs) == 0) {
    cli::cli_alert_warning("No target documents after filtering. All docs metadata saved.")
    return(invisible(all_docs))
  }

  # Save target documents metadata
  save_metadata(target_docs, "gef_target_documents.csv")

  # Step 5: Download
  download_documents(target_docs)

  # Summary
  print_source_summary(SOURCE_NAME)

  cli::cli_h2("Done!")
  invisible(target_docs)
}

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  run_gef_scraper()
}
