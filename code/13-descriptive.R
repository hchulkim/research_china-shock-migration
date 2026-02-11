###############################################################################
# Title: Descriptive statistics and figures
# Maintainer: Hyoungchul Kim
# Initial date: 2025-07-16
# Modified date: 2026-02-01
#
# Description:
#   Produces descriptive tables and maps: Korea-China trade values (Table 1),
#   district and commuting zone maps (Figure 1), trade exposure maps
#   (Figure 2), and summary statistics for shocks and instruments.
#
# Input:
#   - data/comtrade/comtrade_KOR_h{0,1,3,5}.csv
#   - data/region/bnd_sigungu_00_2019_4Q.shp
#   - data/cz/cz_data.xlsx
#   - data/proc/exposure_descriptive.csv
#   - data/proc/baseline_data.csv
#   - data/proc/baseline_data_jpn.csv
#   - data/temp/ssiv_share.csv
#   - data/temp/ssiv_shock_2001_2010_2019.csv
#
# Output:
#   - output/tables/tab1.tex
#   - output/figures/fig1_1.png
#   - output/figures/fig1_2.png
#   - output/figures/fig2_1.png
#   - output/figures/fig2_2.png
###############################################################################

p_load(here, data.table, purrr, glue, stringr, kableExtra, comtradr, sf, ggplot2, readxl, dplyr, rmapshaper, tradestatistics)


### Table 1: Value of trades for Korea (1990, 2001, 2010, 2019)

# Load data
file_list <- list.files(here("data", "comtrade"), pattern = "comtrade_KOR_h[0,1,3,5].csv", full.names = TRUE)

# Read and filter data in one step, selecting only needed columns
trade_kor_china <- rbindlist(
  lapply(file_list, function(file) {
    fread(file, select = c("period", "flow_code", "cmd_code", "fobvalue", "cifvalue", "partner_code"))
  }),
  use.names = TRUE, fill = TRUE
)

# Apply all filters at once and calculate trade values in one step
trade_table <- trade_kor_china[
  partner_code == 156 &
    str_length(cmd_code) == 6 &
    period %in% c(1990, 2001, 2010, 2019)
][, .(
  export = sum(fobvalue[flow_code == "X"], na.rm = TRUE),
  import = sum(cifvalue[flow_code == "M"], na.rm = TRUE)
), by = period]


trade_table[, `:=`(Balance = export - import)]


# construct GDP deflator using World Bank data

# Assume your data.table is named ots_gdp_deflator
usa_deflators_2010_to_2019 <- ots_gdp_deflator[country_iso == "usa" & year_from >= 2010 & year_from < 2019]
usa_deflators_2001_to_2019 <- ots_gdp_deflator[country_iso == "usa" & year_from >= 2001 & year_from < 2019]
usa_deflators_1990_to_2019 <- ots_gdp_deflator[country_iso == "usa" & year_from >= 1990 & year_from < 2019]

# Compute cumulative deflator from 2010 to 2019
deflator_2010_to_2019 <- prod(usa_deflators_2010_to_2019$gdp_deflator)
deflator_2001_to_2019 <- prod(usa_deflators_2001_to_2019$gdp_deflator)
deflator_1990_to_2019 <- prod(usa_deflators_1990_to_2019$gdp_deflator)

# Create deflation factors lookup
deflation_factors <- c(
  "1990" = deflator_1990_to_2019,
  "2001" = deflator_2001_to_2019,
  "2010" = deflator_2010_to_2019,
  "2019" = 1
)

# Apply deflation and convert to billions in one step
trade_table[, `:=`(
  export = round(export * deflation_factors[as.character(period)] / 1e9, 1),
  import = round(import * deflation_factors[as.character(period)] / 1e9, 1),
  Balance = round(Balance * deflation_factors[as.character(period)] / 1e9, 1)
)]
setnames(trade_table, old = c("period", "export", "import", "Balance"), new = c("Year", "Export", "Import", "Balance"))

tab1 <- trade_table |>
  kbl(format = "latex", booktabs = TRUE) |>
  add_header_above(c("Trade with China (in billion USD)" = 4), bold = TRUE)
writeLines(tab1, here("output", "tables", "tab1.tex"))

### Figure 1: Maps of South Korea (Districts and commuting zones)
# This section creates spatial visualizations showing administrative boundaries

# Load Korean administrative boundary shapefile (시군구 level, 2019 4th quarter)
# Simplify polygons to reduce file size and improve rendering speed
data <- st_read(here("data", "region", "bnd_sigungu_00_2019_4Q.shp")) |>
  ms_simplify()

# Load commuting zone classification data
cz <- read_excel(here("data", "cz", "cz_data.xlsx"))

## clean the district numeric code

# 1. change the last digit to 0
data <- data |> mutate(SIGUNGU_CD = ifelse(str_sub(SIGUNGU_CD, -1, -1) != "0", paste0(str_sub(SIGUNGU_CD, 1, -2), "0"), SIGUNGU_CD))

# do the first merge
data <- data |> left_join(cz |> mutate(cz_code = as.character(cz_code)), by = c("SIGUNGU_CD" = "cz_code"))

# take out NAs
data_na <- data |>
  filter(is.na(cz_label)) |>
  mutate(city_code = str_sub(SIGUNGU_CD, 1, 2)) |>
  select(-c("cz_label", "cz_id", "region_label"))

data <- data |> filter(!is.na(cz_label))

# 2. For metro regions, only use first two digits
cz_city <- cz |>
  filter(str_length(cz_code) == 2) |>
  mutate(cz_code = as.character(cz_code))

data_na <- data_na |> left_join(cz_city, by = c("city_code" = "cz_code"))

data <- data |> bind_rows(data_na |> filter(!is.na(cz_label)))

data_na <- data_na |> filter(is.na(cz_label))

# Generate district-level map (take out small island region)
# Group by district code and union geometries to handle any duplicate polygons
ggplot() +
  geom_sf(
    data = data |> filter(SIGUNGU_CD != "37430") |> group_by(SIGUNGU_CD) |> summarise(geometry = st_union(geometry)),
    fill = "gray", color = "black"
  ) +
  theme_void()
ggsave(here("output", "figures", "fig1_1.png"), width = 10, height = 10)

# Generate commuting zone map (take out small island region)
# Group by commuting zone ID to show functional economic regions
ggplot() +
  geom_sf(
    data = data |> filter(cz_id != 34) |> group_by(cz_id) |> summarise(geometry = st_union(geometry)),
    fill = "gray", color = "black"
  ) +
  theme_void()
ggsave(here("output", "figures", "fig1_2.png"), width = 10, height = 10)

### Figure 2: Map of trade shocks (2001-2010)

cz_map <- data |>
  group_by(cz_id) |>
  summarise(geometry = st_union(geometry))

exposure_descriptive <- fread(here("data", "proc", "exposure_descriptive.csv"))

exposure_descriptive <- exposure_descriptive[period == 1, .(cz_id = cz_id_o, x_import_o, x_export_o)]

# get unique cz_id row only
exposure_descriptive <- exposure_descriptive[!duplicated(cz_id), ]

cz_map <- cz_map |> left_join(exposure_descriptive, by = c("cz_id"))

# Create quantile columns for both import and export exposure
cz_map <- cz_map |>
  mutate(
    # Create quantile groups: 0-20%, 20-40%, 80-100%
    import_quantile = case_when(
      x_import_o <= quantile(x_import_o, 0.2, na.rm = TRUE) ~ "0-20%",
      x_import_o <= quantile(x_import_o, 0.4, na.rm = TRUE) ~ "20-40%",
      x_import_o >= quantile(x_import_o, 0.8, na.rm = TRUE) ~ "80-100%",
      TRUE ~ "40-80%" # middle range
    ),
    export_quantile = case_when(
      x_export_o <= quantile(x_export_o, 0.2, na.rm = TRUE) ~ "0-20%",
      x_export_o <= quantile(x_export_o, 0.4, na.rm = TRUE) ~ "20-40%",
      x_export_o >= quantile(x_export_o, 0.8, na.rm = TRUE) ~ "80-100%",
      TRUE ~ "40-80%" # middle range
    )
  ) |>
  # Convert to ordered factor for proper color mapping
  mutate(
    import_quantile = factor(import_quantile, levels = c("0-20%", "20-40%", "40-80%", "80-100%"), ordered = TRUE),
    export_quantile = factor(export_quantile, levels = c("0-20%", "20-40%", "40-80%", "80-100%"), ordered = TRUE)
  )

# Create green color palette: lighter green for lower exposure, darker green for higher exposure
green_palette <- c("#E8F5E8", "#C8E6C9", "#A5D6A7", "#4CAF50") # Light to dark green

ggplot() +
  geom_sf(data = cz_map |> filter(cz_id != 34), aes(fill = import_quantile)) +
  scale_fill_manual(values = green_palette, name = "Import Exposure\nQuantiles") +
  theme_void()
ggsave(here("output", "figures", "fig2_1.png"), width = 10, height = 10)

ggplot() +
  geom_sf(data = cz_map |> filter(cz_id != 34), aes(fill = export_quantile)) +
  scale_fill_manual(values = green_palette, name = "Export Exposure\nQuantiles") +
  theme_void()
ggsave(here("output", "figures", "fig2_2.png"), width = 10, height = 10)


### Table A.1: Summary statistics

cat("=== Loading data ===\n")

# load main data
dt_adh <- fread(here("data", "proc", "baseline_data.csv"))
dt_jpn <- fread(here("data", "proc", "baseline_data_jpn.csv"))

# create CZ_pair
dt_adh[, cz_pair := paste(cz_id_d, cz_id_o, sep = "_")]
dt_jpn[, cz_pair := paste(cz_id_d, cz_id_o, sep = "_")]

# Mixed IV dataset (import ADH, export JPN)
dt_mix <- copy(dt_adh)
dt_mix[, z_export_d := NULL]
dt_mix[, z_export_o := NULL]
dt_mix <- merge(dt_mix, dt_jpn[, .(iso_o, iso_d, period, z_export_d, z_export_o)],
  by = c("iso_o", "iso_d", "period"), all.x = TRUE
)

# keeping finite values
dt_chg_adh <- dt_adh[is.finite(migration_change)]
dt_chg_jpn <- dt_jpn[is.finite(migration_change)]
dt_chg_mix <- dt_mix[is.finite(migration_change)]


## import shock summary statistics
import <- unique(dt_adh[, .(period, cz_id_o, x_import_o)], by = c("period", "cz_id_o"))

# summary statistics of x_import_o
summary(import[, .(x_import_o)])

sd(import[, x_import_o])

## export shock summary statistics
export <- unique(dt_adh[, .(period, cz_id_d, x_export_d)], by = c("period", "cz_id_d"))

# summary statistics of x_export_d
summary(export[, .(x_export_d)])

sd(export[, x_export_d])


## dependent variable summary statistics
summary(dt_chg_adh[, .(migration_change)])
sd(dt_chg_adh[, migration_change])


## shift IV statistics

# read in the share data
share <- fread(here("data", "temp", "ssiv_share.csv"),
  colClasses = list(character = c("ksic10"))
)

# get average
share <- share[, .(share = mean(emp_share_1999, na.rm = TRUE)), by = "ksic10"]

# read in the exposure shock data
shock <- fread(here("data", "temp", "ssiv_shock_2001_2010_2019.csv"),
  colClasses = list(character = c("ksic10"))
)

# export JPN
shock_jpn <- shock[reporter_iso == "JPN" & flow_code == "X", ]

shock_jpn <- shock_jpn[, .(shock_jp = sum(shock, na.rm = TRUE)), by = .(ksic10, period)]

# export ADH
shock_adh_x <- shock[reporter_iso %in% c("AUS", "DNK", "FIN", "DEU", "NZL", "ESP", "CHE") & flow_code == "X", ]

shock_adh_x <- shock_adh_x[, .(shock_ad = sum(shock, na.rm = TRUE)), by = .(ksic10, period)]

# import ADH
shock_adh_m <- shock[reporter_iso %in% c("AUS", "DNK", "FIN", "DEU", "NZL", "ESP", "CHE") & flow_code == "M", ]

shock_adh_m <- shock_adh_m[, .(shock_ad = sum(shock, na.rm = TRUE)), by = .(ksic10, period)]

# merge the data
shock_jpn <- merge(shock_jpn, share, by = "ksic10", all.x = TRUE)
shock_adh_x <- merge(shock_adh_x, share, by = "ksic10", all.x = TRUE)
shock_adh_m <- merge(shock_adh_m, share, by = "ksic10", all.x = TRUE)

# calculate the instrumental variable
shock_jpn[!is.na(share) & !is.na(shock_jp), z_jp := shock_jp * share]
shock_adh_x[!is.na(share) & !is.na(shock_ad), z_ad_x := shock_ad * share]
shock_adh_m[!is.na(share) & !is.na(shock_ad), z_ad_m := shock_ad * share]

# z-score normalize the instrumental variable
shock_jpn[, z_jp := (z_jp - mean(z_jp, na.rm = TRUE)) / sd(z_jp, na.rm = TRUE)]
shock_adh_x[, z_ad_x := (z_ad_x - mean(z_ad_x, na.rm = TRUE)) / sd(z_ad_x, na.rm = TRUE)]
shock_adh_m[, z_ad_m := (z_ad_m - mean(z_ad_m, na.rm = TRUE)) / sd(z_ad_m, na.rm = TRUE)]

# summary statistics of z_jp
summary(shock_jpn[, .(z_jp)], na.rm = TRUE)
sd(shock_jpn[, z_jp], na.rm = TRUE)

# summary statistics of z_ad_x
summary(shock_adh_x[, .(z_ad_x)], na.rm = TRUE)
sd(shock_adh_x[, z_ad_x], na.rm = TRUE)

# summary statistics of z_ad_m
summary(shock_adh_m[, .(z_ad_m)], na.rm = TRUE)
sd(shock_adh_m[, z_ad_m], na.rm = TRUE)
