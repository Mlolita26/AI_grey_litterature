##############################################################################
# gcf.R — Green Climate Fund (GCF) Project Document Scraper
#
# Targets: Funding Proposals and Evaluation Reports for Africa adaptation
#          projects from https://www.greenclimate.fund/projects
#
# The GCF website is Drupal 7 with Views AJAX — project tiles and document
# tables are NOT server-rendered. We must POST to /views/ajax to get content.
#
# Status IDs: 231=Approved, 232=Under implementation, 234=Lapsed,
#             445=Completed, 452=Terminated, 453=Under cancellation, 461=Cancelled
# Theme IDs:  235=Adaptation
# Region IDs: 318=Africa
#
# Strategy:
#   1. POST to /views/ajax to get project listing (Approved + Completed,
#      Adaptation, Africa)
#   2. Visit each project page, extract node ID, POST to /views/ajax for docs
#   3. Filter for target document types (funding proposals, evaluations)
#   4. Visit document pages to resolve PDF download URLs
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

DOWNLOAD_DIR <- file.path(PATHS$downloads, "gcf")
dir.create(DOWNLOAD_DIR, recursive = TRUE, showWarnings = FALSE)

SOURCE_NAME <- "gcf"

# ── GCF Configuration ─────────────────────────────────────────────────────
GCF_BASE <- "https://www.greenclimate.fund"
GCF_AJAX_URL <- paste0(GCF_BASE, "/views/ajax")

# Drupal Views AJAX parameters for project listing
GCF_VIEW_NAME       <- "search"
GCF_VIEW_DISPLAY    <- "project_tiles"
GCF_VIEW_DOM_ID     <- "96ddb1cb7b8effbf1798921f9b25e262"
GCF_VIEW_PATH       <- "node/39"
GCF_ALL_STATUSES    <- "231+232+234+445+452+453+461"
GCF_ITEMS_PER_PAGE  <- 30

# Filters: Approved (231) + Completed (445), Adaptation (235), Africa (318)
GCF_STATUS_FILTERS  <- c("field_status:231", "field_status:445")
GCF_THEME_FILTERS   <- c("field_theme:235")
GCF_REGION_FILTERS  <- c("field_region:318")

# Document types to target
GCF_DOC_TYPE_PATTERNS <- c(
  "funding.proposal",
  "evaluation",
  "completion"
)

# Drupal Views AJAX parameters for document listing on project pages
GCF_DOC_VIEW_DISPLAY <- "title_type_date"

# ── Helper: POST to Drupal Views AJAX ─────────────────────────────────────
#' Send a POST request to /views/ajax and return parsed HTML fragment.
#'
#' @param page Integer. Zero-indexed page number.
#' @param view_display Character. View display ID.
#' @param view_args Character. View arguments (slash-separated).
#' @param facet_filters Character vector. Facet filter values like "field_status:231".
#' @param view_dom_id Character. The DOM ID for the view.
#' @return An xml_document (HTML fragment) or NULL on failure.
gcf_views_ajax <- function(page = 0,
                           view_display = GCF_VIEW_DISPLAY,
                           view_args = paste0("project/all/all/all/", GCF_ALL_STATUSES),
                           facet_filters = c(GCF_STATUS_FILTERS, GCF_THEME_FILTERS, GCF_REGION_FILTERS),
                           view_dom_id = GCF_VIEW_DOM_ID) {

  # Build form body
  body <- list(
    view_name       = GCF_VIEW_NAME,
    view_display_id = view_display,
    view_args       = view_args,
    view_path       = GCF_VIEW_PATH,
    view_dom_id     = view_dom_id,
    pager_element   = "0",
    page            = as.character(page)
  )

  # Add facet filters as f[] parameters
  # httr encodes repeated parameter names via named list entries
  filter_body <- setNames(
    as.list(facet_filters),
    rep("f[]", length(facet_filters))
  )

  full_body <- c(body, filter_body)

  # Polite delay
  Sys.sleep(runif(1, HTTP_CONFIG$delay_min, HTTP_CONFIG$delay_max))

  resp <- tryCatch(
    httr::POST(
      GCF_AJAX_URL,
      httr::user_agent(HTTP_CONFIG$user_agent),
      httr::timeout(HTTP_CONFIG$timeout_sec),
      body = full_body,
      encode = "form"
    ),
    error = function(e) {
      cli::cli_alert_danger("AJAX POST error: {e$message}")
      NULL
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    cli::cli_alert_danger("AJAX request failed (status: {httr::status_code(resp) %||% 'NULL'})")
    return(NULL)
  }

  # Parse JSON response — it's an array of commands
  json <- tryCatch({
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    jsonlite::fromJSON(txt, simplifyDataFrame = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("JSON parse error: {e$message}")
    NULL
  })

  if (is.null(json)) return(NULL)

  # Find the "insert" command that contains rendered HTML
  html_content <- NULL
  for (cmd in json) {
    if (!is.null(cmd$command) && cmd$command == "insert" && !is.null(cmd$data)) {
      html_content <- cmd$data
      break
    }
  }

  if (is.null(html_content) || nchar(html_content) < 50) {
    cli::cli_alert_warning("No HTML content in AJAX response")
    return(NULL)
  }

  # Parse the HTML fragment
  tryCatch(
    xml2::read_html(html_content),
    error = function(e) {
      cli::cli_alert_warning("HTML fragment parse error: {e$message}")
      NULL
    }
  )
}

# ── Step 1: Scrape project listing via AJAX ───────────────────────────────
scrape_project_listing <- function() {
  cli::cli_h2("Step 1: Scraping GCF project listing via AJAX")
  cli::cli_alert_info("Filters: Approved + Completed, Adaptation, Africa")

  all_projects <- tibble()
  page <- 0

  repeat {
    cli::cli_alert_info("Fetching listing page {page + 1} (offset {page * GCF_ITEMS_PER_PAGE})...")

    html <- gcf_views_ajax(page = page)

    if (is.null(html)) {
      cli::cli_alert_warning("Failed to fetch page {page + 1}, stopping.")
      break
    }

    # Extract project links: a.stretched-link with href containing /project/
    project_nodes <- html %>%
      rvest::html_nodes("a[href*='/project/']")

    if (length(project_nodes) == 0) {
      # Also try without the stretched-link class
      project_nodes <- html %>%
        rvest::html_nodes("a[href*='greenclimate.fund/project/']")
    }

    project_hrefs <- project_nodes %>%
      rvest::html_attr("href") %>%
      unique()

    # Filter to actual project links (absolute or relative)
    project_hrefs <- project_hrefs[grepl("/project/[a-zA-Z0-9-]+$", project_hrefs)]

    if (length(project_hrefs) == 0) {
      cli::cli_alert_info("No projects found on page {page + 1}. Done paginating.")
      break
    }

    # Extract card info for each project
    cards <- html %>% rvest::html_nodes(".card-project, .col-card")

    for (href in project_hrefs) {
      # Extract project code from URL
      code <- toupper(sub(".*/project/", "", href))

      # Ensure full URL
      full_url <- if (grepl("^https?://", href)) href else paste0(GCF_BASE, href)

      # Try to get title and country from the card
      title <- NA_character_
      country <- NA_character_
      theme <- NA_character_

      # Find the card containing this link
      for (card in cards) {
        card_html <- as.character(card)
        if (grepl(href, card_html, fixed = TRUE)) {
          title <- tryCatch({
            t <- card %>% rvest::html_node(".card-project__title, .card-title") %>%
              rvest::html_text() %>% trimws()
            if (!is.na(t) && nchar(t) > 0) t else NA_character_
          }, error = function(e) NA_character_)

          country <- tryCatch({
            c <- card %>% rvest::html_node(".card-project__countries") %>%
              rvest::html_text() %>% trimws()
            if (!is.na(c) && nchar(c) > 0) c else NA_character_
          }, error = function(e) NA_character_)

          theme <- tryCatch({
            th <- card %>% rvest::html_node(".badge") %>%
              rvest::html_text() %>% trimws()
            if (!is.na(th) && nchar(th) > 0) th else NA_character_
          }, error = function(e) NA_character_)

          break
        }
      }

      all_projects <- bind_rows(all_projects, tibble(
        project_code = code,
        project_url  = full_url,
        title        = title,
        country      = country,
        theme        = theme
      ))
    }

    cli::cli_alert_success("Page {page + 1}: found {length(project_hrefs)} projects")
    page <- page + 1

    # Safety cap
    if (page > 50) {
      cli::cli_alert_warning("Reached page limit (50). Stopping.")
      break
    }
  }

  if (nrow(all_projects) == 0) {
    cli::cli_alert_danger("No projects found via AJAX.")
    return(tibble(
      project_code = character(),
      project_url  = character(),
      title        = character(),
      country      = character(),
      theme        = character()
    ))
  }

  # Deduplicate
  all_projects <- all_projects %>% distinct(project_code, .keep_all = TRUE)
  cli::cli_alert_success("Total unique projects found: {nrow(all_projects)}")
  all_projects
}

# ── Step 2: Scrape documents from each project page ───────────────────────
scrape_project_documents <- function(projects) {
  cli::cli_h2("Step 2: Scraping document links from project pages")

  all_docs <- tibble(
    project_code = character(), project_title = character(),
    project_url = character(), doc_title = character(),
    doc_type = character(), doc_date = character(),
    doc_page_url = character(), country = character(),
    entity = character()
  )

  for (i in seq_len(nrow(projects))) {
    row <- projects[i, ]
    cli::cli_alert_info("[{i}/{nrow(projects)}] Scraping: {row$project_code}")

    # Fetch the project page to get the Drupal node ID
    html <- safe_read_html(row$project_url)
    if (is.null(html)) {
      cli::cli_alert_warning("  Failed to load project page for {row$project_code}")
      next
    }

    # Extract node ID from Drupal.settings in the page scripts
    node_id <- extract_node_id(html)

    # Get better title from page
    page_title <- tryCatch({
      t <- html %>% rvest::html_node("h1") %>% rvest::html_text() %>% trimws()
      if (!is.na(t) && nchar(t) > 0) t else row$title
    }, error = function(e) row$title)

    # Extract project metadata from server-rendered parts
    project_meta <- extract_project_metadata(html)

    # Get documents via AJAX if we have a node ID
    doc_html <- NULL
    if (!is.na(node_id)) {
      doc_view_args <- paste0("document+board_document/all/all/all/", node_id)

      # Try to also get the document view's DOM ID from Drupal.settings
      doc_dom_id <- extract_doc_view_dom_id(html)

      # Fetch all document pages
      doc_page <- 0
      repeat {
        ajax_html <- gcf_views_ajax(
          page = doc_page,
          view_display = GCF_DOC_VIEW_DISPLAY,
          view_args = doc_view_args,
          facet_filters = character(0),  # no facet filters for document listing
          view_dom_id = doc_dom_id
        )

        if (is.null(ajax_html)) break

        # Parse document rows from the AJAX response
        new_docs <- parse_document_table(ajax_html, row, page_title, project_meta)

        if (nrow(new_docs) == 0) break

        all_docs <- bind_rows(all_docs, new_docs)

        # Check if there are more pages
        pager <- ajax_html %>% rvest::html_nodes(".pager-next, .pager-item")
        if (length(pager) == 0) break

        doc_page <- doc_page + 1
        if (doc_page > 20) break  # safety
      }
    } else {
      cli::cli_alert_warning("  Could not find node ID, trying direct HTML scrape")
      # Fallback: try to find document links in the server-rendered HTML
      new_docs <- parse_document_links_from_html(html, row, page_title, project_meta)
      if (nrow(new_docs) > 0) {
        all_docs <- bind_rows(all_docs, new_docs)
      }
    }

    doc_count <- sum(all_docs$project_code == row$project_code)
    if (doc_count > 0) {
      cli::cli_alert_success("  Found {doc_count} documents for {row$project_code}")
    } else {
      cli::cli_alert_info("  No documents found for {row$project_code}")
    }

    if (i %% 10 == 0) {
      cli::cli_alert_info("Progress: {i}/{nrow(projects)} projects, {nrow(all_docs)} total docs")
    }
  }

  cli::cli_alert_success("Total documents collected: {nrow(all_docs)}")
  all_docs
}

# ── Helper: Extract Drupal node ID from page ──────────────────────────────
extract_node_id <- function(html) {
  tryCatch({
    # Method 1: Look in Drupal.settings script for view_args containing node ID
    scripts <- html %>% rvest::html_nodes("script") %>% rvest::html_text()
    for (script in scripts) {
      # Match pattern: "view_args":"document+board_document\/all\/all\/all\/NNN"
      m <- regmatches(script, regexpr(
        "document\\+board_document[/\\\\]+all[/\\\\]+all[/\\\\]+all[/\\\\]+([0-9]+)",
        script
      ))
      if (length(m) > 0) {
        nid <- sub(".*[/\\\\]", "", m[1])
        return(nid)
      }
    }

    # Method 2: Look for node ID in body class or data attribute
    body <- html %>% rvest::html_node("body")
    body_class <- rvest::html_attr(body, "class")
    if (!is.na(body_class)) {
      m <- regmatches(body_class, regexpr("page-node-([0-9]+)", body_class))
      if (length(m) > 0) {
        return(sub("page-node-", "", m[1]))
      }
    }

    # Method 3: Look for nid in any data attribute
    nid_nodes <- html %>% rvest::html_nodes("[data-nid]")
    if (length(nid_nodes) > 0) {
      return(rvest::html_attr(nid_nodes[1], "data-nid"))
    }

    NA_character_
  }, error = function(e) NA_character_)
}

# ── Helper: Extract document view DOM ID from page ────────────────────────
extract_doc_view_dom_id <- function(html) {
  tryCatch({
    scripts <- html %>% rvest::html_nodes("script") %>% rvest::html_text()
    for (script in scripts) {
      # Look for the views config with title_type_date display
      if (grepl("title_type_date", script)) {
        # Extract the DOM ID: "views_dom_id:HEXSTRING"
        m <- regmatches(script, regexpr(
          'views_dom_id:([a-f0-9]{20,})',
          script
        ))
        if (length(m) > 0) {
          # Return just the hex part after the colon
          return(sub("views_dom_id:", "", m[1]))
        }

        # Alternative pattern with quotes
        m <- regmatches(script, regexpr(
          '"view_dom_id"\\s*:\\s*"([a-f0-9]{20,})"',
          script
        ))
        if (length(m) > 0) {
          return(gsub('"view_dom_id"\\s*:\\s*"|"', "", m[1]))
        }
      }
    }
    # Fallback: use a generic DOM ID (Drupal will still process the request)
    "gcf_doc_view"
  }, error = function(e) "gcf_doc_view")
}

# ── Helper: Extract project metadata from page ────────────────────────────
extract_project_metadata <- function(html) {
  safe_text <- function(selector) {
    tryCatch({
      txt <- html %>% rvest::html_node(selector) %>% rvest::html_text() %>% trimws()
      if (is.na(txt) || nchar(txt) == 0) NA_character_ else txt
    }, error = function(e) NA_character_)
  }

  # Try to extract country from the page
  country <- tryCatch({
    # Look for labeled fields
    page_text <- html %>% rvest::html_text()
    found <- sapply(AFRICA_COUNTRIES_EN, function(c) {
      if (grepl(c, page_text, fixed = TRUE)) c else NA_character_
    })
    found <- found[!is.na(found)]
    if (length(found) > 0) paste(found, collapse = "; ") else NA_character_
  }, error = function(e) NA_character_)

  list(
    country = country,
    entity  = safe_text(".field--name-field-entity .field-item"),
    amount  = safe_text(".field--name-field-amount .field-item")
  )
}

# ── Helper: Parse document table from AJAX response ───────────────────────
parse_document_table <- function(html, project_row, page_title, meta) {
  # Collect results as lists of character vectors, then build tibble once
  results <- list()

  tryCatch({
    country_val <- as.character(coalesce(project_row$country, meta$country) %||% NA_character_)
    entity_val  <- as.character(meta$entity %||% NA_character_)
    proj_code   <- as.character(project_row$project_code)
    proj_url    <- as.character(project_row$project_url)
    p_title     <- as.character(page_title %||% NA_character_)

    # Look for table rows with document links
    rows <- html %>% rvest::html_nodes("tr")

    for (tr in rows) {
      cells <- tr %>% rvest::html_nodes("td")
      if (length(cells) < 2) next

      link_node <- tr %>% rvest::html_node("a[href*='/document/']")
      if (is.null(link_node)) next

      href <- rvest::html_attr(link_node, "href")
      if (is.na(href)) next

      doc_title <- trimws(rvest::html_text(link_node))
      cell_texts <- trimws(rvest::html_text(cells))

      doc_type <- if (length(cell_texts) >= 2) cell_texts[2] else NA_character_
      doc_date <- if (length(cell_texts) >= 3) cell_texts[3] else NA_character_
      doc_url  <- if (grepl("^https?://", href)) href else paste0(GCF_BASE, href)

      results[[length(results) + 1]] <- list(
        project_code  = proj_code,
        project_title = p_title,
        project_url   = proj_url,
        doc_title     = as.character(doc_title),
        doc_type      = as.character(doc_type),
        doc_date      = as.character(doc_date),
        doc_page_url  = as.character(doc_url),
        country       = country_val,
        entity        = entity_val
      )
    }

    # If no table rows, try generic link approach
    if (length(results) == 0) {
      links <- html %>% rvest::html_nodes("a[href*='/document/']")
      for (link in links) {
        href <- rvest::html_attr(link, "href")
        if (is.na(href)) next

        doc_url <- if (grepl("^https?://", href)) href else paste0(GCF_BASE, href)

        results[[length(results) + 1]] <- list(
          project_code  = proj_code,
          project_title = p_title,
          project_url   = proj_url,
          doc_title     = as.character(trimws(rvest::html_text(link))),
          doc_type      = as.character(infer_doc_type_from_slug(href)),
          doc_date      = NA_character_,
          doc_page_url  = as.character(doc_url),
          country       = country_val,
          entity        = entity_val
        )
      }
    }
  }, error = function(e) {
    cli::cli_alert_warning("  Error parsing document table: {e$message}")
  })

  if (length(results) == 0) return(tibble(
    project_code = character(), project_title = character(),
    project_url = character(), doc_title = character(),
    doc_type = character(), doc_date = character(),
    doc_page_url = character(), country = character(),
    entity = character()
  ))

  bind_rows(lapply(results, as_tibble))
}

# ── Helper: Fallback — parse document links from server HTML ──────────────
parse_document_links_from_html <- function(html, project_row, page_title, meta) {
  empty <- tibble(
    project_code = character(), project_title = character(),
    project_url = character(), doc_title = character(),
    doc_type = character(), doc_date = character(),
    doc_page_url = character(), country = character(),
    entity = character()
  )

  tryCatch({
    links <- html %>% rvest::html_nodes("a[href*='/document/']")
    hrefs <- links %>% rvest::html_attr("href") %>% unique()
    hrefs <- hrefs[grepl("^/document/", hrefs)]

    if (length(hrefs) == 0) return(empty)

    country_val <- as.character(coalesce(project_row$country, meta$country) %||% NA_character_)

    results <- lapply(hrefs, function(href) {
      doc_url <- paste0(GCF_BASE, href)
      matching <- links[rvest::html_attr(links, "href") == href]
      doc_title <- if (length(matching) > 0) trimws(rvest::html_text(matching[1])) else NA_character_

      tibble(
        project_code  = as.character(project_row$project_code),
        project_title = as.character(page_title %||% NA_character_),
        project_url   = as.character(project_row$project_url),
        doc_title     = as.character(doc_title),
        doc_type      = as.character(infer_doc_type_from_slug(href)),
        doc_date      = NA_character_,
        doc_page_url  = as.character(doc_url),
        country       = country_val,
        entity        = as.character(meta$entity %||% NA_character_)
      )
    })

    bind_rows(results)
  }, error = function(e) empty)
}

# ── Helper: Infer document type from URL slug ─────────────────────────────
infer_doc_type_from_slug <- function(href) {
  slug <- tolower(href)
  if (grepl("evaluation", slug))                      return("Evaluation report")
  if (grepl("funding.proposal|approved.*proposal", slug)) return("Funding proposal")
  if (grepl("completion", slug))                      return("Completion report")
  if (grepl("performance.report|apr-", slug))         return("Annual Performance Report")
  NA_character_
}

# ── Step 3: Filter documents by type ──────────────────────────────────────
filter_target_documents <- function(docs) {
  cli::cli_h2("Step 3: Filtering for target document types")

  if (nrow(docs) == 0) return(docs)

  # Deduplicate by document page URL
  docs <- docs %>% distinct(doc_page_url, .keep_all = TRUE)
  cli::cli_alert_info("After dedup: {nrow(docs)} documents")

  type_pattern <- paste(GCF_DOC_TYPE_PATTERNS, collapse = "|")

  docs <- docs %>%
    mutate(
      is_target = grepl(type_pattern, doc_type, ignore.case = TRUE) |
                  grepl(type_pattern, doc_title, ignore.case = TRUE) |
                  grepl(type_pattern, doc_page_url, ignore.case = TRUE)
    )

  target_docs <- docs %>% filter(is_target) %>% select(-is_target)
  excluded_n <- sum(!docs$is_target)

  cli::cli_alert_success("Target documents: {nrow(target_docs)} (filtered out {excluded_n})")

  if (nrow(target_docs) > 0) {
    type_counts <- target_docs %>% count(doc_type) %>% arrange(desc(n))
    for (j in seq_len(nrow(type_counts))) {
      cli::cli_alert_info("  {coalesce(type_counts$doc_type[j], '(unknown)')}: {type_counts$n[j]}")
    }
  }

  target_docs
}

# ── Helper: Fetch and parse a GCF document page (not safe_read_html) ──────
#' GCF document detail pages are small. safe_read_html rejects pages <500 chars.
#' This function fetches without that restriction.
fetch_document_page <- function(url) {
  resp <- polite_get(url)
  if (is.null(resp)) return(NULL)
  if (httr::status_code(resp) != 200) return(NULL)

  tryCatch({
    content_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    xml2::read_html(content_text)
  }, error = function(e) {
    cli::cli_alert_warning("  HTML parse error: {e$message}")
    NULL
  })
}

# ── Step 4: Resolve PDF URLs from document pages ──────────────────────────
resolve_pdf_urls <- function(docs) {
  cli::cli_h2("Step 4: Resolving PDF download URLs")

  if (nrow(docs) == 0) return(docs)

  # Flatten any list-columns to plain character
  for (col in names(docs)) {
    if (is.list(docs[[col]])) {
      docs[[col]] <- vapply(docs[[col]], function(x) {
        if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])
      }, character(1))
    }
  }

  docs$pdf_url <- NA_character_

  for (i in seq_len(nrow(docs))) {
    doc_title <- as.character(docs$doc_title[i])
    doc_url   <- as.character(docs$doc_page_url[i])

    cli::cli_alert_info("[{i}/{nrow(docs)}] Resolving: {doc_title}")

    pdf_url <- tryCatch({
      found <- NA_character_
      html <- fetch_document_page(doc_url)

      if (!is.null(html)) {
        # Strategy 1: Link to /sites/default/files/ (primary GCF pattern)
        pdf_links <- html %>%
          rvest::html_nodes("a[href*='/sites/default/files/']") %>%
          rvest::html_attr("href")
        pdf_links <- pdf_links[grepl("\\.pdf", pdf_links, ignore.case = TRUE)]

        if (length(pdf_links) > 0) {
          found <- pdf_links[1]
        } else {
          # Strategy 2: Any .pdf link on the page
          all_links <- html %>%
            rvest::html_nodes("a") %>%
            rvest::html_attr("href")
          all_links <- all_links[!is.na(all_links)]
          pdf_links <- all_links[grepl("\\.pdf", all_links, ignore.case = TRUE)]

          if (length(pdf_links) > 0) {
            found <- pdf_links[1]
          }
        }

        # Ensure absolute URL
        if (!is.na(found) && !grepl("^https?://", found)) {
          found <- paste0(GCF_BASE, found)
        }
      } else {
        cli::cli_alert_warning("  Failed to load document page")
      }

      found
    }, error = function(e) {
      cli::cli_alert_warning("  Error resolving PDF: {e$message}")
      NA_character_
    })

    docs$pdf_url[i] <- as.character(pdf_url)

    if (!is.na(pdf_url)) {
      cli::cli_alert_success("  PDF: {basename(pdf_url)}")
    } else {
      cli::cli_alert_warning("  No PDF link found")
    }

    if (i %% 10 == 0) {
      resolved <- sum(!is.na(docs$pdf_url[1:i]))
      cli::cli_alert_info("Progress: {i}/{nrow(docs)} ({resolved} PDFs found)")
    }
  }

  resolved_count <- sum(!is.na(docs$pdf_url))
  cli::cli_alert_success("Resolved {resolved_count}/{nrow(docs)} PDF URLs")
  docs
}

# ── Step 5: Download PDFs ─────────────────────────────────────────────────
download_documents <- function(docs) {
  cli::cli_h2("Step 5: Downloading PDFs")

  with_pdf <- docs %>% filter(!is.na(pdf_url) & nchar(pdf_url) > 0)

  if (nrow(with_pdf) == 0) {
    cli::cli_alert_warning("No documents with PDF URLs to download.")
    return(invisible(NULL))
  }

  cli::cli_alert_info("{nrow(with_pdf)} documents have PDF URLs")

  success <- 0
  failed  <- 0
  skipped <- 0

  for (i in seq_len(nrow(with_pdf))) {
    row <- with_pdf[i, ]

    # Build filename: gcf_{project_code}_{doc_type}.pdf
    proj <- safe_filename(coalesce(row$project_code, "noproj"))
    dtype <- safe_filename(coalesce(row$doc_type, "doc"))
    fname <- glue("{SOURCE_NAME}_{proj}_{dtype}.pdf")
    dest <- file.path(DOWNLOAD_DIR, fname)

    # Handle duplicate filenames
    if (file.exists(dest)) {
      base_name <- tools::file_path_sans_ext(fname)
      ext <- tools::file_ext(fname)
      counter <- 1
      while (file.exists(dest)) {
        dest <- file.path(DOWNLOAD_DIR, glue("{base_name}_{counter}.{ext}"))
        counter <- counter + 1
        if (counter > 10) break
      }
      if (counter > 10) {
        log_download(SOURCE_NAME, row$project_code, row$doc_type,
                     row$doc_title, row$pdf_url, dest, "skipped",
                     "Too many duplicates")
        skipped <- skipped + 1
        next
      }
    }

    result <- download_pdf(row$pdf_url, dest)

    if (identical(result, TRUE)) {
      log_download(SOURCE_NAME, row$project_code, row$doc_type,
                   row$doc_title, row$pdf_url, dest, "success")
      success <- success + 1
    } else if (identical(result, "skipped")) {
      log_download(SOURCE_NAME, row$project_code, row$doc_type,
                   row$doc_title, row$pdf_url, dest, "skipped")
      skipped <- skipped + 1
    } else {
      log_download(SOURCE_NAME, row$project_code, row$doc_type,
                   row$doc_title, row$pdf_url, dest, "failed",
                   "Download or validation failed")
      failed <- failed + 1
    }

    if (i %% 10 == 0) {
      cli::cli_alert_info("Progress: {i}/{nrow(with_pdf)} (OK: {success}, fail: {failed}, skip: {skipped})")
    }
  }

  cli::cli_h3("Download Summary")
  cli::cli_alert_success("Success: {success}")
  cli::cli_alert_danger("Failed: {failed}")
  cli::cli_alert_info("Skipped: {skipped}")
}

# ── Save metadata ─────────────────────────────────────────────────────────
save_metadata <- function(docs) {
  meta_path <- file.path(PATHS$data, "gcf_metadata.csv")
  # Flatten any list-columns to plain character
  for (col in names(docs)) {
    if (is.list(docs[[col]])) {
      docs[[col]] <- vapply(docs[[col]], function(x) {
        if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])
      }, character(1))
    }
  }
  readr::write_csv(as.data.frame(docs), meta_path)
  cli::cli_alert_success("Metadata saved to: {meta_path}")
}

# ── Main execution ────────────────────────────────────────────────────────
run_gcf_scraper <- function() {
  cli::cli_h1("Green Climate Fund Grey Literature Scraper")
  cli::cli_alert_info("Download directory: {DOWNLOAD_DIR}")
  cli::cli_alert_info("Filters: Approved + Completed, Adaptation, Africa")

  # Step 1: Get project list via AJAX
  projects <- scrape_project_listing()

  if (nrow(projects) == 0) {
    cli::cli_alert_danger("No projects found. Check filters or website availability.")
    return(invisible(NULL))
  }

  # Step 2: Scrape document links from each project page
  all_docs <- scrape_project_documents(projects)

  if (nrow(all_docs) == 0) {
    cli::cli_alert_danger("No documents found on any project page.")
    return(invisible(NULL))
  }

  # Step 3: Filter for funding proposals and evaluation reports
  target_docs <- filter_target_documents(all_docs)

  if (nrow(target_docs) == 0) {
    cli::cli_alert_warning("No target documents after filtering. Saving all docs metadata.")
    save_metadata(all_docs)
    return(invisible(all_docs))
  }

  # Step 4: Resolve actual PDF download URLs
  target_docs <- resolve_pdf_urls(target_docs)

  # Save metadata
  save_metadata(target_docs)

  # Step 5: Download PDFs
  download_documents(target_docs)

  # Print summary
  print_source_summary(SOURCE_NAME)

  cli::cli_h2("Done!")
  invisible(target_docs)
}

# Run if called directly
if (sys.nframe() == 0 || !interactive()) {
  run_gcf_scraper()
}
