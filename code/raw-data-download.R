###############################################################################
# Title: UN Comtrade bulk data download
# Maintainer: Hyoungchul Kim
# Initial date: 2025-07-16
# Modified date: 2025-07-29
#
# Description:
#   Bulk downloads UN Comtrade bilateral trade data using the comtradr API.
#   Downloads HS0-HS5 revision data for 15 countries across 1990-2019.
#
# Input:
#   - UN Comtrade bulk API (requires premium subscription key)
#   - See: https://uncomtrade.org/docs/api-subscription-keys/
#
# Output:
#   - data/comtrade/comtrade_{country}_{hs_code}.csv
###############################################################################

# load packages
pacman::p_load(here, data.table, purrr, glue, comtradr)

options(timeout = 600)

# Define HS revision codes and their corresponding year ranges
hs_revisions <- list(
  H0 = 1990:1995,
  H1 = 1996:2001,
  H2 = 2002:2006,
  H3 = 2010:2011,
  # H4 = 2012:2016,
  H5 = 2019:2019
)

# List of countries (reporters)
country_list <- c(
  "KOR", "JPN", "AUS", "DNK", "FIN", "DEU", "NZL", "ESP",
  "CHE", "USA", "ITA", "GBR", "CAN", "NOR", "BEL"
)

# Helper: Retry logic for safe network download
retry_request <- function(expr, max_attempts = 3, wait_sec = 5) {
  for (i in seq_len(max_attempts)) {
    result <- try(expr, silent = TRUE)
    if (!inherits(result, "try-error")) {
      return(result)
    }
    message(glue("⚠️ Attempt {i} failed. Retrying in {wait_sec} seconds..."))
    Sys.sleep(wait_sec)
    wait_sec <- wait_sec * 2
  }
  stop("❌ All retry attempts failed.")
}

# Main download function for a given (country, HS revision)
download_comtrade <- function(cntry, hs_code, years) {
  safely_download <- safely(function() {
    dat <- retry_request({
      comtradr::ct_get_bulk(
        reporter = cntry,
        commodity_classification = hs_code,
        frequency = "A",
        verbose = TRUE,
        start_date = min(years),
        end_date = max(years)
      )
    })

    out_dir <- here("data", "comtrade")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    out_path <- glue("{out_dir}/comtrade_{cntry}_{tolower(hs_code)}.csv")
    fwrite(dat, out_path)
    message(glue("✅ Success: {cntry} [{hs_code}] ({min(years)}–{max(years)})"))
  })

  result <- safely_download()
  if (!is.null(result$error)) {
    message(glue("❌ Failed: {cntry} [{hs_code}] — {result$error$message}"))
  }
}

# Loop over HS revisions and countries
walk(names(hs_revisions), function(hs_code) {
  years <- hs_revisions[[hs_code]]
  message(glue("📦 Starting downloads for {hs_code} ({min(years)}–{max(years)})"))
  walk(country_list, function(cntry) {
    download_comtrade(cntry, hs_code, years)
    Sys.sleep(2) # polite delay
  })
})
