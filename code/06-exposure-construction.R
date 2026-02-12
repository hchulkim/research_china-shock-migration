###############################################################################
# Title: Shift-share exposure construction
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-08
# Modified date: 2025-10-18
#
# Description:
#   Constructs shift-share IV components: industry-level trade shocks and
#   local employment shares. Follows Borusyak et al. (2022). Deflates
#   trade values to 2019 USD using the US GDP deflator.
#
# Input:
#   - data/temp/comtrade_ksic10.csv
#   - data/temp/est_region_industry_matched.csv
#   - data/cz/cz_data.xlsx
#
# Output:
#   - data/temp/ssiv_shock_2001_2019.csv
#   - data/temp/ssiv_shock_2001_2010_2019.csv
#   - data/temp/ssiv_share.csv
###############################################################################

# load packages
pacman::p_load(here, data.table, tidyfast, tradestatistics)

## Creating the exposure shock

# construct GDP deflator using World Bank data

# Assume your data.table is named ots_gdp_deflator
usa_deflators_2010_to_2019 <- ots_gdp_deflator[country_iso == "usa" & year_from >= 2010 & year_from < 2019]
usa_deflators_2001_to_2019 <- ots_gdp_deflator[country_iso == "usa" & year_from >= 2001 & year_from < 2019]
usa_deflators_1990_to_2019 <- ots_gdp_deflator[country_iso == "usa" & year_from >= 1990 & year_from < 2019]

# Compute cumulative deflator from 2010 to 2019
deflator_2010_to_2019 <- prod(usa_deflators_2010_to_2019$gdp_deflator)
deflator_2001_to_2019 <- prod(usa_deflators_2001_to_2019$gdp_deflator)
deflator_1990_to_2019 <- prod(usa_deflators_1990_to_2019$gdp_deflator)

# load the est data
est <- fread(here("data", "temp", "est_region_industry_matched.csv"), 
             colClasses = list(character = c("iso", "ksic10")))

# industry employment for year 1994 to 2010
est_industry <- est[year %in% c(1994, 1996, 1999, 2000, 2001, 2010), .(iso, year, ksic10, emp_all)]

est_industry = est_industry[, .(ind_emp = sum(emp_all, na.rm = T)), by = .(year, ksic10)]

est_industry[, year := as.character(year)]
est_industry[, year := paste0("emp_", year)]

# make yearly employment value in wide format
est_industry_wide = est_industry |> 
    dt_pivot_wider(
        names_from = year,
        values_from = ind_emp
    )

# set NA to 0
est_industry_wide = est_industry_wide[, lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = patterns("emp_"), by = ksic10]

# load the comtrade data
comtrade_ksic10 <- fread(here("data", "temp", "comtrade_ksic10.csv"), 
                        colClasses = list(character = "ksic10"))

# adjust by 2019 value
comtrade_ksic10[period == 2001, value := value * deflator_2001_to_2019]
comtrade_ksic10[period == 2010, value := value * deflator_2010_to_2019]
comtrade_ksic10[period == 2019, value := value]

## lag the trade value column to get the change in trade

# 2019 to 2001
comtrade1 = comtrade_ksic10[period != 2010,]

comtrade1 <- comtrade1[order(reporter_iso, flow_code, ksic10, period)]
comtrade1[, value_lag := shift(value, type = "lag"), by = .(reporter_iso, flow_code, ksic10)]

# filter only year 2019 and set NA to 0, then get change
comtrade1 = comtrade1[period == 2019,]
comtrade1[is.na(value_lag), value_lag := 0]

# create shock for per 1000 USD
comtrade1[, shock := (value - value_lag) / 1000]

comtrade1 = merge(comtrade1, est_industry_wide, by = "ksic10", all.x = T)

# delete empty ksic10
comtrade1 = comtrade1[!is.na(ksic10),]
comtrade1 = comtrade1[ksic10 != ""]

comtrade1[, .(period, reporter_code, reporter_iso, partner_code, partner_iso, flow_code, ksic10, shock, emp_1994, emp_1996, emp_1999, emp_2000, emp_2001, emp_2010)] |> 
    fwrite(here("data", "temp", "ssiv_shock_2001_2019.csv"))

# 2019 to 2010 and 2010 to 2001
comtrade2 <- comtrade_ksic10[order(reporter_iso, flow_code, ksic10, period)]
comtrade2[, value_lag := shift(value, type = "lag"), by = .(reporter_iso, flow_code, ksic10)]

#  and set NA to 0, then get change
comtrade2 = comtrade2[period != 2001,]
comtrade2[is.na(value_lag), value_lag := 0]

# create shock for per 1000 USD
comtrade2[, shock := (value - value_lag) / 1000]

comtrade2 = merge(comtrade2, est_industry_wide, by = "ksic10", all.x = T)

# delete empty ksic10
comtrade2 = comtrade2[!is.na(ksic10),]
comtrade2 = comtrade2[ksic10 != ""]

comtrade2[, .(period, reporter_code, reporter_iso, partner_code, partner_iso, flow_code, ksic10, shock, emp_1994, emp_1996, emp_1999, emp_2000, emp_2001, emp_2010)] |> 
    fwrite(here("data", "temp", "ssiv_shock_2001_2010_2019.csv"))


## creating the share

# load the cz
cz_data = readxl::read_excel(here("data", "cz", "cz_data.xlsx")) |> 
    dplyr::mutate(
        cz_code = as.character(cz_code)
    ) |> as.data.table()

est_industry <- est[year %in% c(1994, 1996, 1999, 2000, 2001, 2010), .(iso, year, ksic10, emp_all)]

# set up cz ids for the est data iso regions
est_industry = merge(est_industry, cz_data, by.x = "iso", by.y = "cz_code", all.x = TRUE)
# some values has to be done manually following cz_data
est_industry[substr(iso, 1, 2) == "11", cz_id := 1]
est_industry[substr(iso, 1, 2) == "21", cz_id := 2]
est_industry[substr(iso, 1, 2) == "22", cz_id := 3]
est_industry[substr(iso, 1, 2) == "23", cz_id := 4]
est_industry[substr(iso, 1, 2) == "25", cz_id := 5]
est_industry[substr(iso, 1, 2) == "24", cz_id := 6]
est_industry[substr(iso, 1, 2) == "26", cz_id := 7]
est_industry[substr(iso, 1, 2) == "39", cz_id := 33]

# create local industry employment share by cz
est_industry = est_industry[, .(ind_emp = sum(emp_all, na.rm = T)), by = .(year, cz_id, ksic10)]

est_industry[, ind_emp_total := sum(ind_emp, na.rm = T), by = .(year, cz_id)]
est_industry[, ind_emp_share := ind_emp / ind_emp_total]
est_industry = est_industry[, .(cz_id, year, ksic10, ind_emp_share)]

est_industry[, year := as.character(year)]
est_industry[, year := paste0("emp_share_", year)]

est_industry_wide = est_industry |> 
    dt_pivot_wider(
        names_from = year,
        values_from = ind_emp_share
    )

# set NA to 0
est_industry_wide = est_industry_wide[, lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

est_industry_wide |> 
    fwrite(here("data", "temp", "ssiv_share.csv"))