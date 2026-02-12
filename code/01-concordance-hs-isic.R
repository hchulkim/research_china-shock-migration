###############################################################################
# Title: HS-to-ISIC concordance
# Maintainer: Hyoungchul Kim
# Initial date: 2025-07-21
# Modified date: 2025-08-07
#
# Description:
#   Crosswalks HS 6-digit codes to ISIC Rev.4 codes using the concordance
#   package. Processes UN Comtrade data in HS1, HS3, HS5 revisions for
#   10 developed economies and produces a consolidated trade dataset.
#
# Input:
#   - data/comtrade/comtrade_{country}_h{1,3,5}.csv
#
# Output:
#   - data/temp/comtrade_isic4.csv
###############################################################################

# load packages
pacman::p_load(fs, here, data.table, concordance, stringr, glue, purrr, dplyr, tidyr, comtradr)


# Define countries for analysis - developed economies for comparison with Korea
# This list represents major developed economies used in the China shock literature
country_list = c("AUS", "CHE", "DEU", "DNK", "ESP", "FIN", "GBR", "JPN", "KOR", "NZL")

hs_versions <- c(1, 3, 5)
input_files <- as.vector(outer(country_list, hs_versions, function(cntry, hs) {
  here::here("data", "comtrade", glue::glue("comtrade_{cntry}_h{hs}.csv"))
}))
input_files <- input_files[fs::file_exists(input_files)]

# Crosswalk a single country-year file from HS to ISIC Rev.4
crosswalk_file <- function(file_path) {
  message("Processing: ", file_path)
  start_time <- Sys.time()

  # Filter to China trade (partner_code 156) and 6-digit HS codes
  dt <- data.table::fread(file_path) %>%
    as_tibble() %>%
    filter(str_length(cmd_code) == 6, partner_code == 156)

  hs_version <- stringr::str_match(file_path, "_h([0-5])\\.csv$")[,2]
  hs_year <- switch(hs_version,
                    "1" = 2001,  # HS1996 revision used in 2001
                    "3" = 2010,  # HS2007 revision used in 2010
                    "5" = 2019)  # HS2017 revision used in 2019
  dt <- dt %>% filter(period == hs_year) 

  hs_col <- switch(hs_version,
                   "0" = "hs0",
                   "1" = "hs1", 
                   "2" = "hs2",
                   "3" = "hs3",
                   "4" = "hs4",
                   "5" = "hs5")
  if (is.null(hs_col)) {
    stop("HS code not found in the file path name")
  }

  if (!"cmd_code" %in% names(dt)) {
    stop("cmd_code column not found in the data. Available columns: ", paste(names(dt), collapse = ", "))
  }

  dt_clean <- dt %>%
    filter(str_detect(cmd_code, "^\\d+$")) %>%
    # Standardize trade values and handle missing data
    mutate(
      fobvalue = as.numeric(fobvalue),  # Export values (Free On Board)
      cifvalue = as.numeric(cifvalue),  # Import values (Cost, Insurance, Freight)
      fobvalue = if_else(is.na(fobvalue), 0, fobvalue),
      cifvalue = if_else(is.na(cifvalue), 0, cifvalue)
    )

  # Convert individual HS code to ISIC Rev.4 using concordance package
  get_isic4_crosswalk <- function(hs_code, origin) {
    res <- concordance::concord(sourcevar = hs_code, origin = origin, destination = "ISIC4", all = TRUE)
    print(paste0("Processing HS code: ", hs_code))

    if (is.null(res[[1]]) ||
        length(res[[1]]$match) == 0 || 
        length(res[[1]]$weight) == 0 || 
        any(is.na(res[[1]]$match)) || 
        any(is.na(res[[1]]$weight))) {
      return(tibble(
        isic4 = NA,
        weight = NA
      ))
    }
    
    codes <- res[[1]]$match
    weights <- as.numeric(res[[1]]$weight)
    
    tibble(isic4 = codes, weight = weights)
  }

  origin <- paste0("HS", hs_version)

  # Apply concordance to each HS code and expand (one HS may map to multiple ISIC)
  result <- dt_clean %>%
    mutate(row_id = row_number()) %>%
    mutate(
      crosswalk_data = map(cmd_code, ~get_isic4_crosswalk(.x, origin))
    ) %>%
    unnest(crosswalk_data, keep_empty = TRUE)
  
  total_rows <- nrow(dt_clean)
  message(sprintf("Processed %d rows. Time elapsed: %.1fs", 
                 total_rows, 
                 as.numeric(difftime(Sys.time(), start_time, units="secs"))))

  out_dir <- here::here("data", "temp")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  out_file <- file.path(out_dir, basename(stringr::str_replace(file_path, "\\.csv$", "_isic4.csv")))
  
  result %>%
    mutate(cmd_code = as.character(cmd_code)) %>%
    as.data.table() %>%
    data.table::fwrite(out_file)
  
  message("Saved: ", out_file)
}

walk(input_files, crosswalk_file)

# Load country reference tables for ISO codes
reporter <- comtradr::ct_get_ref_table("reporter") |>
  dplyr::transmute(
    reporter_code = as.character(id),
    reporter_iso = iso_3,
    reporter_desc = country
  ) 

partner <- comtradr::ct_get_ref_table("partner") |>
  dplyr::transmute(
    partner_code = as.character(id),
    partner_iso = iso_3,
    partner_desc = country
  )

# Load and aggregate a single crosswalked file, applying concordance weights
load_comtrade_isic4 = function(file_path) {
    data <- fread(file_path,
        select = c("period", "reporter_code", "partner_code", "flow_code", "row_id", "cmd_code", "isic4", "weight", "fobvalue", "cifvalue"),
        colClasses = list(
            character = c("period", "flow_code", "cmd_code", "isic4", "reporter_code", "partner_code"),
            integer = c("row_id"),
            numeric = c("weight", "fobvalue", "cifvalue")
        )
    ) 

    data = data[!is.na(weight)]

    # Apply concordance weights to distribute trade values across ISIC industries
    data[, `:=`(
        fobvalue_weighted = fobvalue * weight,
        cifvalue_weighted = cifvalue * weight
    )]

    # Aggregate to final ISIC-level trade flows
    # Sum across all HS codes that map to the same ISIC industry
    data = data[, .(
        fobvalue = sum(fobvalue_weighted, na.rm = TRUE),
        cifvalue = sum(cifvalue_weighted, na.rm = TRUE)
    ), by = .(period, reporter_code, partner_code, flow_code, isic4)]

    # Focus on export (X) and import (M) flows only
    data = data[flow_code %in% c("X", "M")]
    # Create unified value variable: FOB for exports, CIF for imports
    data[, value := fifelse(flow_code == "X", fobvalue, cifvalue)]

    # Add country names and ISO codes for analysis
    data = merge(data, reporter, by = "reporter_code", all.x = TRUE)
    data = merge(data, partner, by = "partner_code", all.x = TRUE)

    return(data)
}

## Combine all processed files into single ISIC-classified trade dataset
# This creates the final dataset with all countries and years in ISIC Rev.4 classification

# Identify all processed crosswalk files
temp_dir <- here::here("data", "temp")
input_files <- list.files(temp_dir, pattern = "_isic4\\.csv$", full.names = TRUE)

# Load and combine all country-year files into single dataset
comtrade_isic4 = map(input_files, load_comtrade_isic4) |> 
    rbindlist(use.names = TRUE, fill = TRUE)

# Save the final consolidated trade dataset in ISIC Rev.4 classification
# This file serves as input for the next step: ISIC-to-KSIC conversion
fwrite(comtrade_isic4, here::here("data", "temp", "comtrade_isic4.csv"))


