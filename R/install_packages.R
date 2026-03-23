##############################################################################
# install_packages.R — Package Installer for Grey Literature Downloader
#
# Run this once before using any scraper script.
# Usage:  source("install_packages.R")  or  Rscript install_packages.R
##############################################################################

required_packages <- c(
  # HTTP & web scraping
  "httr",         # HTTP requests (GET, headers, authentication)
  "rvest",        # HTML parsing and CSS/XPath selectors
  "jsonlite",     # JSON parsing (APIs, dFlip JS configs)
  "xml2",         # Underlying XML/HTML parser for rvest

  # Data wrangling
  "dplyr",        # Data frame manipulation
  "purrr",        # Functional programming (map, walk)
  "tibble",       # Modern data frames
  "tidyr",        # Data reshaping (pivot_wider)
  "stringr",      # String manipulation and regex
  "readr",        # CSV reading/writing

  # Utilities
  "glue",         # String interpolation
  "cli",          # Coloured console output and progress
  "tools"         # File path utilities (built-in, listed for clarity)
)

cli_available <- requireNamespace("cli", quietly = TRUE)

msg <- function(txt) {
  if (cli_available) cli::cli_alert_info(txt) else message(txt)
}
ok  <- function(txt) {
  if (cli_available) cli::cli_alert_success(txt) else message("OK: ", txt)
}
err <- function(txt) {
  if (cli_available) cli::cli_alert_danger(txt) else message("FAIL: ", txt)
}

if (cli_available) cli::cli_h1("Installing required packages")

to_install <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

# "tools" is a base package — always available, no need to install
to_install <- setdiff(to_install, c("tools"))

if (length(to_install) == 0) {
  ok("All packages already installed.")
} else {
  msg("Installing: {paste(to_install, collapse = ', ')}")
  install.packages(to_install, repos = "https://cloud.r-project.org")

  # Verify installation
  failed <- to_install[
    !vapply(to_install, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(failed) == 0) {
    ok("All packages installed successfully.")
  } else {
    err("Failed to install: {paste(failed, collapse = ', ')}")
    err("Try installing manually: install.packages(c({paste(sprintf('\\'{x}\\'', failed), collapse = ', ')}))")
  }
}

if (cli_available) {
  cli::cli_h2("Package versions")
  installed_info <- vapply(
    setdiff(required_packages, "tools"),
    function(p) {
      if (requireNamespace(p, quietly = TRUE))
        as.character(utils::packageVersion(p))
      else
        "NOT INSTALLED"
    },
    character(1)
  )
  print(data.frame(package = names(installed_info), version = installed_info,
                   row.names = NULL))
}
