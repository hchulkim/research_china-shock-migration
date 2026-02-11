###############################################################################
# Title: Baseline analysis dataset construction
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-12
# Modified date: 2026-02-01
#
# Description:
#   Assembles the main analysis dataset by combining bilateral migration
#   flows, trade exposure shocks, shift-share IVs, control variables, and
#   population weights. Creates two datasets: (1) baseline with ADH-style
#   multi-country IV, and (2) alternative with Japan-only export IV.
#
# Input:
#   - data/region/final_region_code.rds
#   - data/temp/migration_data_reg_226.csv
#   - data/temp/ssiv_shock_2001_2010_2019.csv
#   - data/temp/ssiv_share.csv
#   - data/temp/controls.csv
#   - data/temp/pop_2001.csv
#   - data/cz/cz_data.xlsx
#
# Output:
#   - data/proc/baseline_data.csv
#   - data/proc/baseline_data_jpn.csv
#   - data/proc/exposure_descriptive.csv
###############################################################################

# Load required packages for data manipulation and Excel file reading
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, readr, data.table, tidyfast, stringr)

# load controls for weights, local share, etc
controls <- fread(here("data", "temp", "controls.csv"))

# download the final matched region data
region <- readr::read_rds(here("data", "region", "final_region_code.rds")) |>
    as.data.table()

# exclude small island region
region <- region[iso != "37430", ]

# create expanded grid main data
# every codes will probably be in two folds: one for origin and one for destination
main_data <- expand.grid(
    iso_d = region$iso,
    iso_o = region$iso,
    period = c(1, 2)
) |>
    as.data.table()

# exclude same region migration
main_data <- main_data[iso_d != iso_o, ]


# load the cz
cz_data <- readxl::read_excel(here("data", "cz", "cz_data.xlsx")) |>
    dplyr::mutate(
        cz_code = as.character(cz_code)
    ) |>
    as.data.table()
cz_data <- cz_data[, .(cz_code, cz_id)]

# merge cz data to the main data and also manually take care of some cases following cz_data
main_data <- merge(main_data, cz_data, by.x = "iso_d", by.y = "cz_code", all.x = TRUE)
main_data[substr(iso_d, 1, 2) == "11", cz_id := 1]
main_data[substr(iso_d, 1, 2) == "21", cz_id := 2]
main_data[substr(iso_d, 1, 2) == "22", cz_id := 3]
main_data[substr(iso_d, 1, 2) == "23", cz_id := 4]
main_data[substr(iso_d, 1, 2) == "25", cz_id := 5]
main_data[substr(iso_d, 1, 2) == "24", cz_id := 6]
main_data[substr(iso_d, 1, 2) == "26", cz_id := 7]
main_data[substr(iso_d, 1, 2) == "39", cz_id := 33]

main_data <- merge(main_data, cz_data, by.x = "iso_o", by.y = "cz_code", all.x = TRUE, suffixes = c("_d", "_o"))
main_data[substr(iso_o, 1, 2) == "11", cz_id_o := 1]
main_data[substr(iso_o, 1, 2) == "21", cz_id_o := 2]
main_data[substr(iso_o, 1, 2) == "22", cz_id_o := 3]
main_data[substr(iso_o, 1, 2) == "23", cz_id_o := 4]
main_data[substr(iso_o, 1, 2) == "25", cz_id_o := 5]
main_data[substr(iso_o, 1, 2) == "24", cz_id_o := 6]
main_data[substr(iso_o, 1, 2) == "26", cz_id_o := 7]
main_data[substr(iso_o, 1, 2) == "39", cz_id_o := 33]

# take out case where cz_id_d == cz_id_o
main_data <- main_data[cz_id_d != cz_id_o, ]




# first read in the migration data
migration_data <- fread(here("data", "temp", "migration_data_reg_226.csv"),
    colClasses = list(character = c("iso_d", "iso_o"), integer = c("year"), numeric = c("migration"))
)

# read in the exposure shock data
shock <- fread(here("data", "temp", "ssiv_shock_2001_2010_2019.csv"),
    colClasses = list(character = c("ksic10"))
)

# change wording for period into 1 and 2 for matching
shock <- shock[, period := fifelse(period == 2010, 1, 2)]

# read in the share data
share <- fread(here("data", "temp", "ssiv_share.csv"),
    colClasses = list(character = c("ksic10"))
)

# create the migration data for the main data
migration_data_reg <- migration_data[year > 2000, .(year, iso_d, iso_o, migration)]


# another migration data
migration_data_reg_2 <- migration_data[year == 2001 | year == 2010 | year == 2019, .(year, iso_d, iso_o, migration)]

# lag across year for migration data
migration_data_reg_2[, migration_lag := shift(migration, type = "lag"), by = .(iso_d, iso_o)]
migration_data_reg_2 <- migration_data_reg_2[year != 2001, ]

migration_data_reg_2[, migration_change := log(migration) - log(migration_lag)]

migration_data_reg_2[, period := fifelse(year == 2010, 1, 2)]
migration_data_reg_2[, migration := NULL]

# use cumulative migration data
migration_data_reg[year <= 2010, period := 1]
migration_data_reg[year > 2010, period := 2]

# create final migration data from iso_o to iso_d
migration_data_reg <- migration_data_reg[, .(migration = sum(migration, na.rm = T)), by = .(period, iso_d, iso_o)]

# merge with main data
main_data <- merge(main_data, migration_data_reg, by = c("iso_d", "iso_o", "period"), all.x = TRUE)

main_data <- merge(main_data, migration_data_reg_2, by = c("iso_d", "iso_o", "period"), all.x = TRUE)

# we assume there was no migration if migration value is NA
main_data <- main_data[is.na(migration), migration := 0]



# create treatment variable using KOR first
treat_create <- function(country) {
    treat <- shock[reporter_iso == country, .(period, flow_code, ksic10, shock, emp_2001, emp_2010)]
    treat[period == 1, shock := shock / emp_2001]
    treat[period == 2, shock := shock / emp_2010]
    treat[is.na(shock) | is.infinite(shock), shock := 0]
    treat <- treat[, .(treatment = sum(shock, na.rm = T)), by = .(period, flow_code, ksic10)]
    treat <- treat[, .(period, flow_code, ksic10, treatment)]
}
treat_var <- treat_create("KOR")

# ADH country list for SSIV
country_list <- c("AUS", "DNK", "FIN", "DEU", "NZL", "ESP", "CHE")

ssiv_create <- function(year) {
    # Create the column name dynamically
    emp_col <- paste0("emp_", year)

    # Check if the column exists
    if (!emp_col %in% names(shock)) {
        stop(paste("Column", emp_col, "not found in shock data"))
    }
    ssiv <- shock[reporter_iso %in% country_list, ]
    ssiv[, shock := shock / get(emp_col)]
    ssiv[is.na(shock) | is.infinite(shock), shock := 0]
    ssiv <- ssiv[, .(period, flow_code, ksic10, shock)]
    ssiv <- ssiv[, .(iv_var = sum(shock, na.rm = T)), by = .(period, flow_code, ksic10)]
    year <- as.character(year)
    ssiv |> fwrite(here("data", "temp", glue::glue("ssiv_var_{year}.csv")))
    return(ssiv)
}
ssiv_var <- ssiv_create(1999)



# combine treatment and ssiv
treat_var <- merge(treat_var, ssiv_var, by = c("period", "flow_code", "ksic10"), all.x = TRUE)
treat_var <- treat_var |> dt_pivot_wider(,
    names_from = flow_code,
    values_from = c(treatment, iv_var)
)
treat_var <- treat_var[!is.na(treatment_M) & !is.na(iv_var_M) & !is.na(treatment_X) & !is.na(iv_var_X), ]

# save local share to add it later
local_share <- share[, .(cz_id, ksic10, emp_share_1999)]

# finally create the treatment variable and add it to main data

# add ssiv var first
share_ssiv <- share[, .(cz_id, ksic10, emp_share_1999)]
share_ssiv <- share_ssiv[is.na(emp_share_1999), emp_share_1999 := 0]


share_ssiv <- share_ssiv[rep(1:.N, each = 2)]
share_ssiv[, period := rep(c(1, 2), .N / 2)]

ssiv <- merge(share_ssiv, treat_var[, .(period, ksic10, iv_var_M, iv_var_X)], by = c("period", "ksic10"), all.x = TRUE)

ssiv[, `:=`(iv_var_M = iv_var_M * emp_share_1999, iv_var_X = iv_var_X * emp_share_1999)]

ssiv <- ssiv[, .(z_import = sum(iv_var_M, na.rm = T), z_export = sum(iv_var_X, na.rm = T)), by = .(period, cz_id)][, .(period, cz_id, z_import, z_export)][!is.na(period)]

main_data <- merge(main_data, ssiv, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, ssiv, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))

# now finally add the treatment variable
share <- share[, .(cz_id, ksic10, emp_share_2001, emp_share_2010)]
share[is.na(emp_share_2001), emp_share_2001 := 0]
share[is.na(emp_share_2010), emp_share_2010 := 0]

share_long <- share |> dt_pivot_longer(
    cols = -c(cz_id, ksic10),
    names_to = "period",
    values_to = "emp_share",
)

share_long[period == "emp_share_2001", period := 1]
share_long[period == "emp_share_2010", period := 2]
share_long[, period := as.integer(period)]


treat <- merge(share_long, treat_var[, .(period, ksic10, treatment_M, treatment_X)], by = c("period", "ksic10"), all.x = TRUE)

treat[, treatment_M := treatment_M * emp_share]
treat[, treatment_X := treatment_X * emp_share]

treat <- treat[, .(x_import = sum(treatment_M, na.rm = T), x_export = sum(treatment_X, na.rm = T)), by = .(period, cz_id)]

main_data <- merge(main_data, treat, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, treat, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))

# save data for descriptive analysis of exposure by regions in Korea
main_data |> fwrite(here("data", "proc", "exposure_descriptive.csv"))

# add controls
main_data <- merge(main_data, controls, by.x = c("cz_id_d"), by.y = c("cz_id"), all.x = TRUE)
main_data <- merge(main_data, controls, by.x = c("cz_id_o"), by.y = c("cz_id"), all.x = TRUE, suffixes = c("_d", "_o"))



# add the local share to the main data
local_share[is.na(emp_share_1999), emp_share_1999 := 0]

# Expand by repeating each row for both periods
local_share <- local_share[rep(1:.N, each = 2)]
local_share[, period := rep(c(1, 2), .N / 2)]

local_share <- merge(local_share, treat_var, by = c("period", "ksic10"), all.x = TRUE)



# keep shares with positive absolute value industry shock
local_share_m <- local_share[abs(treatment_M) > 0, ][, .(period, ksic10, cz_id, emp_share_1999)]
local_share_x <- local_share[abs(treatment_X) > 0, ][, .(period, ksic10, cz_id, emp_share_1999)]

local_share_m <- local_share_m[, .(local_share_m = sum(emp_share_1999, na.rm = T)), by = .(period, cz_id)][, .(period, cz_id, local_share_m)]
local_share_x <- local_share_x[, .(local_share_x = sum(emp_share_1999, na.rm = T)), by = .(period, cz_id)][, .(period, cz_id, local_share_x)]


main_data <- merge(main_data, local_share_m, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, local_share_m, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))


main_data <- merge(main_data, local_share_x, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, local_share_x, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))


# load population data
pop2001 <- fread(here("data", "temp", "pop_2001.csv"), colClasses = list(character = c("iso"), numeric = c("pop")))

# merge pop data to the main data
main_data <- merge(main_data, pop2001[, .(iso, pop)], by.x = c("iso_d"), by.y = c("iso"), all.x = TRUE)
main_data <- merge(main_data, pop2001[, .(iso, pop)], by.x = c("iso_o"), by.y = c("iso"), all.x = TRUE, suffixes = c("_d", "_o"))



# download the final data
main_data |> fwrite(here("data", "proc", "baseline_data.csv"))



############################################
# Create another dataset using Japan as Iv
############################################

# load controls
controls <- fread(here("data", "temp", "controls.csv"))

# download the final matched region data
region <- readr::read_rds(here("data", "region", "final_region_code.rds")) |>
    as.data.table()

# exclude small island region
region <- region[iso != "37430", ]

# create expanded grid main data
# every codes will probably be in two folds: one for origin and one for destination
main_data <- expand.grid(
    iso_d = region$iso,
    iso_o = region$iso,
    period = c(1, 2)
) |>
    as.data.table()

# exclude same region migration
main_data <- main_data[iso_d != iso_o, ]


# load the cz
cz_data <- readxl::read_excel(here("data", "cz", "cz_data.xlsx")) |>
    dplyr::mutate(
        cz_code = as.character(cz_code)
    ) |>
    as.data.table()
cz_data <- cz_data[, .(cz_code, cz_id)]

# merge cz data to the main data and also manually take care of some cases following cz_data
main_data <- merge(main_data, cz_data, by.x = "iso_d", by.y = "cz_code", all.x = TRUE)
main_data[substr(iso_d, 1, 2) == "11", cz_id := 1]
main_data[substr(iso_d, 1, 2) == "21", cz_id := 2]
main_data[substr(iso_d, 1, 2) == "22", cz_id := 3]
main_data[substr(iso_d, 1, 2) == "23", cz_id := 4]
main_data[substr(iso_d, 1, 2) == "25", cz_id := 5]
main_data[substr(iso_d, 1, 2) == "24", cz_id := 6]
main_data[substr(iso_d, 1, 2) == "26", cz_id := 7]
main_data[substr(iso_d, 1, 2) == "39", cz_id := 33]

main_data <- merge(main_data, cz_data, by.x = "iso_o", by.y = "cz_code", all.x = TRUE, suffixes = c("_d", "_o"))
main_data[substr(iso_o, 1, 2) == "11", cz_id_o := 1]
main_data[substr(iso_o, 1, 2) == "21", cz_id_o := 2]
main_data[substr(iso_o, 1, 2) == "22", cz_id_o := 3]
main_data[substr(iso_o, 1, 2) == "23", cz_id_o := 4]
main_data[substr(iso_o, 1, 2) == "25", cz_id_o := 5]
main_data[substr(iso_o, 1, 2) == "24", cz_id_o := 6]
main_data[substr(iso_o, 1, 2) == "26", cz_id_o := 7]
main_data[substr(iso_o, 1, 2) == "39", cz_id_o := 33]

# take out case where cz_id_d == cz_id_o
main_data <- main_data[cz_id_d != cz_id_o, ]




# first read in the migration data
migration_data <- fread(here("data", "temp", "migration_data_reg_226.csv"),
    colClasses = list(character = c("iso_d", "iso_o"), integer = c("year"), numeric = c("migration"))
)

# read in the exposure shock data
shock <- fread(here("data", "temp", "ssiv_shock_2001_2010_2019.csv"),
    colClasses = list(character = c("ksic10"))
)

# change wording for period into 1 and 2 for matching
shock <- shock[, period := fifelse(period == 2010, 1, 2)]

# read in the share data
share <- fread(here("data", "temp", "ssiv_share.csv"),
    colClasses = list(character = c("ksic10"))
)

# create the migration data for the main data
migration_data_reg <- migration_data[year > 2000, .(year, iso_d, iso_o, migration)]


# another migration data
migration_data_reg_2 <- migration_data[year == 2001 | year == 2010 | year == 2020, .(year, iso_d, iso_o, migration)]

# lag across year for migration data
migration_data_reg_2[, migration_lag := shift(migration, type = "lag"), by = .(iso_d, iso_o)]
migration_data_reg_2 <- migration_data_reg_2[year != 2001, ]

migration_data_reg_2[, migration_change := log(migration) - log(migration_lag)]

migration_data_reg_2[, period := fifelse(year == 2010, 1, 2)]
migration_data_reg_2[, migration := NULL]

# use cumulative migration data
migration_data_reg[year <= 2010, period := 1]
migration_data_reg[year > 2010, period := 2]

# create final migration data from iso_o to iso_d
migration_data_reg <- migration_data_reg[, .(migration = sum(migration, na.rm = T)), by = .(period, iso_d, iso_o)]

# merge with main data
main_data <- merge(main_data, migration_data_reg, by = c("iso_d", "iso_o", "period"), all.x = TRUE)

main_data <- merge(main_data, migration_data_reg_2, by = c("iso_d", "iso_o", "period"), all.x = TRUE)

# we assume there was no migration if migration value is NA
main_data <- main_data[is.na(migration), migration := 0]



# create treatment variable using KOR first
treat_create <- function(country) {
    treat <- shock[reporter_iso == country, .(period, flow_code, ksic10, shock, emp_2001, emp_2010)]
    treat[period == 1, shock := shock / emp_2001]
    treat[period == 2, shock := shock / emp_2010]
    treat[is.na(shock) | is.infinite(shock), shock := 0]
    treat <- treat[, .(treatment = sum(shock, na.rm = T)), by = .(period, flow_code, ksic10)]
    treat <- treat[, .(period, flow_code, ksic10, treatment)]
}
treat_var <- treat_create("KOR")

# Japan-only country list for SSIV
country_list <- c("JPN")

ssiv_create <- function(year) {
    # Create the column name dynamically
    emp_col <- paste0("emp_", year)

    # Check if the column exists
    if (!emp_col %in% names(shock)) {
        stop(paste("Column", emp_col, "not found in shock data"))
    }
    ssiv <- shock[reporter_iso %in% country_list, ]
    ssiv[, shock := shock / get(emp_col)]
    ssiv[is.na(shock) | is.infinite(shock), shock := 0]
    ssiv <- ssiv[, .(period, flow_code, ksic10, shock)]
    ssiv <- ssiv[, .(iv_var = sum(shock, na.rm = T)), by = .(period, flow_code, ksic10)]
    year <- as.character(year)
    ssiv |> fwrite(here("data", "temp", glue::glue("ssiv_var_jpn_{year}.csv")))
    return(ssiv)
}
ssiv_var <- ssiv_create(1999)



# combine treatment and ssiv

treat_var <- merge(treat_var, ssiv_var, by = c("period", "flow_code", "ksic10"), all.x = TRUE)
treat_var <- treat_var |> dt_pivot_wider(,
    names_from = flow_code,
    values_from = c(treatment, iv_var)
)
treat_var <- treat_var[!is.na(treatment_M) & !is.na(iv_var_M) & !is.na(treatment_X) & !is.na(iv_var_X), ]

# save local share to add it later
local_share <- share[, .(cz_id, ksic10, emp_share_1999)]

# finally create the treatment variable and add it to main data

# add ssiv var first
share_ssiv <- share[, .(cz_id, ksic10, emp_share_1999)]
share_ssiv <- share_ssiv[is.na(emp_share_1999), emp_share_1999 := 0]


share_ssiv <- share_ssiv[rep(1:.N, each = 2)]
share_ssiv[, period := rep(c(1, 2), .N / 2)]

ssiv <- merge(share_ssiv, treat_var[, .(period, ksic10, iv_var_M, iv_var_X)], by = c("period", "ksic10"), all.x = TRUE)

ssiv[, `:=`(iv_var_M = iv_var_M * emp_share_1999, iv_var_X = iv_var_X * emp_share_1999)]

ssiv <- ssiv[, .(z_import = sum(iv_var_M, na.rm = T), z_export = sum(iv_var_X, na.rm = T)), by = .(period, cz_id)][, .(period, cz_id, z_import, z_export)][!is.na(period)]

main_data <- merge(main_data, ssiv, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, ssiv, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))

# now finally add the treatment variable
share <- share[, .(cz_id, ksic10, emp_share_2001, emp_share_2010)]
share[is.na(emp_share_2001), emp_share_2001 := 0]
share[is.na(emp_share_2010), emp_share_2010 := 0]

share_long <- share |> dt_pivot_longer(
    cols = -c(cz_id, ksic10),
    names_to = "period",
    values_to = "emp_share",
)

share_long[period == "emp_share_2001", period := 1]
share_long[period == "emp_share_2010", period := 2]
share_long[, period := as.integer(period)]


treat <- merge(share_long, treat_var[, .(period, ksic10, treatment_M, treatment_X)], by = c("period", "ksic10"), all.x = TRUE)

treat[, treatment_M := treatment_M * emp_share]
treat[, treatment_X := treatment_X * emp_share]

treat <- treat[, .(x_import = sum(treatment_M, na.rm = T), x_export = sum(treatment_X, na.rm = T)), by = .(period, cz_id)]

main_data <- merge(main_data, treat, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, treat, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))

# save data for descriptive analysis of exposure by regions in Korea
# main_data |> fwrite(here("data", "proc", "exposure_descriptive.csv"))

# add controls
main_data <- merge(main_data, controls, by.x = c("cz_id_d"), by.y = c("cz_id"), all.x = TRUE)
main_data <- merge(main_data, controls, by.x = c("cz_id_o"), by.y = c("cz_id"), all.x = TRUE, suffixes = c("_d", "_o"))



# add the local share to the main data
local_share[is.na(emp_share_1999), emp_share_1999 := 0]

# Expand by repeating each row for both periods
local_share <- local_share[rep(1:.N, each = 2)]
local_share[, period := rep(c(1, 2), .N / 2)]

local_share <- merge(local_share, treat_var, by = c("period", "ksic10"), all.x = TRUE)



# keep shares with positive absolute value industry shock
local_share_m <- local_share[abs(treatment_M) > 0, ][, .(period, ksic10, cz_id, emp_share_1999)]
local_share_x <- local_share[abs(treatment_X) > 0, ][, .(period, ksic10, cz_id, emp_share_1999)]

local_share_m <- local_share_m[, .(local_share_m = sum(emp_share_1999, na.rm = T)), by = .(period, cz_id)][, .(period, cz_id, local_share_m)]
local_share_x <- local_share_x[, .(local_share_x = sum(emp_share_1999, na.rm = T)), by = .(period, cz_id)][, .(period, cz_id, local_share_x)]


main_data <- merge(main_data, local_share_m, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, local_share_m, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))


main_data <- merge(main_data, local_share_x, by.x = c("cz_id_d", "period"), by.y = c("cz_id", "period"), all.x = TRUE)
main_data <- merge(main_data, local_share_x, by.x = c("cz_id_o", "period"), by.y = c("cz_id", "period"), all.x = TRUE, suffixes = c("_d", "_o"))


# load population data
pop2001 <- fread(here("data", "temp", "pop_2001.csv"), colClasses = list(character = c("iso"), numeric = c("pop")))

# merge pop data to the main data
main_data <- merge(main_data, pop2001[, .(iso, pop)], by.x = c("iso_d"), by.y = c("iso"), all.x = TRUE)
main_data <- merge(main_data, pop2001[, .(iso, pop)], by.x = c("iso_o"), by.y = c("iso"), all.x = TRUE, suffixes = c("_d", "_o"))



# download the final data
main_data |> fwrite(here("data", "proc", "baseline_data_jpn.csv"))
