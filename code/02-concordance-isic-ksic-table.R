###############################################################################
# Title: ISIC-to-KSIC concordance
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-05
# Modified date: 2025-08-07
#
# Description:
#   Creates concordance table from ISIC Rev.4 to KSIC10 and applies it
#   to convert Comtrade trade data from ISIC4 to KSIC10 classification.
#
# Input:
#   - data/concordance/isic/isic4_ksic10.xlsx
#   - data/temp/comtrade_isic4.csv
#
# Output:
#   - data/temp/isic4_ksic10_table.csv
#   - data/temp/comtrade_ksic10.csv
###############################################################################

# load packages
pacman::p_load(here, data.table, readxl, dplyr)

## Create crosswalk table

df <- read_excel(here("data", "concordance", "isic", "isic4_ksic10.xlsx"), sheet = 1, skip = 1)
dt <- as.data.table(df)

setnames(dt, old = names(dt), new = c("isic4", "isic_eng", "ksic10", "ksic_kor"))

# Fill down merged Excel cells
dt[, isic4 := zoo::na.locf(isic4)]
dt[, isic_eng := zoo::na.locf(isic_eng)]

dt = dt[!is.na(ksic10), ]
dt <- dt[!duplicated(dt[, .(isic4, ksic10)]),]
dt <- dt[order(isic4)]

# Proportional weight when one ISIC maps to multiple KSIC codes
dt[, weight := 1 / .N, by = isic4]

fwrite(dt, file = here("data", "temp", "isic4_ksic10_table.csv"))

## Apply concordance to trade data

comtrade_isic4 = fread(here("data", "temp", "comtrade_isic4.csv"),
                       colClasses = list(character = "isic4"))

# Merge with concordance and apply proportional weights
comtrade_ksic10 = comtrade_isic4 |>
  mutate(row_id = row_number()) |>
  left_join(dt, by = "isic4") |> as.data.table()

comtrade_ksic10[, value := value * weight]

comtrade_ksic10[, `:=`(isic4 = NULL, isic_eng = NULL, fobvalue = NULL, cifvalue = NULL, row_id = NULL, weight = NULL)]

# Aggregate to KSIC10 industry level
comtrade_ksic10 <- comtrade_ksic10[, .(value = sum(value, na.rm = TRUE)),
                                   by = .(period, reporter_code, reporter_iso, partner_code, partner_iso, flow_code, ksic10)]

fwrite(comtrade_ksic10, file = here("data", "temp", "comtrade_ksic10.csv"))




