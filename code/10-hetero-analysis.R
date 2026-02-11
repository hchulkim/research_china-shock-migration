###############################################################################
# Title: Heterogeneity analysis
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-12
# Modified date: 2026-02-01
#
# Description:
#   Runs heterogeneous IV regressions by gender, age group, household size,
#   and their interactions. Constructs demographic-specific migration DVs
#   from microdata and uses the mixed IV strategy.
#
# Input:
#   - data/proc/baseline_data.csv
#   - data/proc/baseline_data_jpn.csv
#   - data/temp/migration2001_2014_refine.rds
#   - data/temp/migration2015_2019_refine.rds
#
# Output:
#   - output/tables/tab_age.tex
#   - output/tables/tab_age_single.tex
#   - output/tables/tab_age_multi.tex
###############################################################################

# Load packages
pacman::p_load(here, readr, data.table, dplyr, tidyr, fixest, texreg, ggplot2)


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

# Create additional DVs
# Migration rate (per 1000 origin pop)
dt_adh[, mig_rate := migration / pop_o * 1000]
dt_jpn[, mig_rate := migration / pop_o * 1000]
dt_mix[, mig_rate := migration / pop_o * 1000]

# Asinh migration
dt_adh[, asinh_mig := asinh(migration)]
dt_jpn[, asinh_mig := asinh(migration)]
dt_mix[, asinh_mig := asinh(migration)]

# Log migration rate
dt_adh[, log_mig_rate := log(mig_rate)]
dt_jpn[, log_mig_rate := log(mig_rate)]
dt_mix[, log_mig_rate := log(mig_rate)]

# create safe feols
safe_feols <- function(...) {
    tryCatch(feols(...), error = function(e) {
        cat("  [ERROR]", conditionMessage(e), "\n")
        NULL
    })
}


# keeping finite values
dt_chg_adh <- dt_adh[is.finite(migration_change)]
dt_chg_jpn <- dt_jpn[is.finite(migration_change)]
dt_chg_mix <- dt_mix[is.finite(migration_change)]


# read in migration data
migration2001_2010 <- read_rds(here("data", "temp", "migration2001_2014_refine.rds")) |> filter(year == 2001 | year == 2010)
migration2019 <- read_rds(here("data", "temp", "migration2015_2019_refine.rds")) |> filter(year == 2019)



# only keep necessary variable
migration2001_2010 <- migration2001_2010 |> select(year, iso_d, iso_o, age = `세대주_만나이`, gender = `세대주_성별`, reason = `전입사유`, migration = `이동_총인구`, hhsize = `이동_총인구`)
migration2019 <- migration2019 |> select(year, iso_d, iso_o, age = `세대주만연령`, gender = `세대주성별코드`, reason = `전입사유코드`, migration = `이동_총인구수`, hhsize = `이동_총인구수`)


# aggregate by iso_o, iso_d with columns for male and age groups
aggregate_migration <- function(df) {
    df |>
        group_by(year, iso_o, iso_d) |>
        summarise(
            migration_age_25_34 = sum(migration[age >= 25 & age < 35], na.rm = TRUE),
            migration_age_35_44 = sum(migration[age >= 35 & age < 45], na.rm = TRUE),
            migration_age_45_54 = sum(migration[age >= 45 & age < 55], na.rm = TRUE),
            migration_age_55_64 = sum(migration[age >= 55 & age < 65], na.rm = TRUE),
            migration_age_65_plus = sum(migration[age >= 65], na.rm = TRUE),
            migration_age_25_34_single = sum(migration[age >= 25 & age < 35 & hhsize == 1], na.rm = TRUE),
            migration_age_25_34_multi = sum(migration[age >= 25 & age < 35 & hhsize > 1], na.rm = TRUE),
            migration_age_35_44_single = sum(migration[age >= 35 & age < 45 & hhsize == 1], na.rm = TRUE),
            migration_age_35_44_multi = sum(migration[age >= 35 & age < 45 & hhsize > 1], na.rm = TRUE),
            migration_age_45_54_single = sum(migration[age >= 45 & age < 55 & hhsize == 1], na.rm = TRUE),
            migration_age_45_54_multi = sum(migration[age >= 45 & age < 55 & hhsize > 1], na.rm = TRUE),
            migration_age_55_64_single = sum(migration[age >= 55 & age < 65 & hhsize == 1], na.rm = TRUE),
            migration_age_55_64_multi = sum(migration[age >= 55 & age < 65 & hhsize > 1], na.rm = TRUE),
            .groups = "drop"
        )
}

migration2001_2010_agg <- aggregate_migration(migration2001_2010)
migration2019_agg <- aggregate_migration(migration2019)

# garbage collect for memory space
rm(migration2001_2010, migration2019)

# row bind the two datasets
migration_all <- bind_rows(migration2001_2010_agg, migration2019_agg)

# compute log migration change between periods
# migration columns to compute changes for
mig_cols <- c(
    "migration_age_25_34", "migration_age_35_44", "migration_age_45_54", "migration_age_55_64", "migration_age_65_plus",
    "migration_age_25_34_male", "migration_age_25_34_female",
    "migration_age_35_44_male", "migration_age_35_44_female",
    "migration_age_45_54_male", "migration_age_45_54_female",
    "migration_age_55_64_male", "migration_age_55_64_female",
    "migration_age_65_plus_male", "migration_age_65_plus_female",
    "migration_age_35_54_male", "migration_age_35_54_female",
    "migration_age_25_34_single", "migration_age_25_34_multi",
    "migration_age_35_44_single", "migration_age_35_44_multi",
    "migration_age_45_54_single", "migration_age_45_54_multi",
    "migration_age_55_64_single", "migration_age_55_64_multi"
)

# reshape to wide format by year, then compute changes
migration_wide <- migration_all |>
    pivot_wider(
        id_cols = c(iso_o, iso_d),
        names_from = year,
        values_from = all_of(mig_cols),
        names_sep = "_"
    )

# compute log changes: 2010-2001 (period 1) and 2019-2010 (period 2)
migration_change_p1 <- migration_wide |>
    transmute(
        iso_o, iso_d,
        period = 1,
        d_log_migration_age_25_34 = log(migration_age_25_34_2010) - log(migration_age_25_34_2001),
        d_log_migration_age_35_44 = log(migration_age_35_44_2010) - log(migration_age_35_44_2001),
        d_log_migration_age_45_54 = log(migration_age_45_54_2010) - log(migration_age_45_54_2001),
        d_log_migration_age_55_64 = log(migration_age_55_64_2010) - log(migration_age_55_64_2001),
        d_log_migration_age_65_plus = log(migration_age_65_plus_2010) - log(migration_age_65_plus_2001),
        d_log_migration_age_25_34_single = log(migration_age_25_34_single_2010) - log(migration_age_25_34_single_2001),
        d_log_migration_age_25_34_multi = log(migration_age_25_34_multi_2010) - log(migration_age_25_34_multi_2001),
        d_log_migration_age_35_44_single = log(migration_age_35_44_single_2010) - log(migration_age_35_44_single_2001),
        d_log_migration_age_35_44_multi = log(migration_age_35_44_multi_2010) - log(migration_age_35_44_multi_2001),
        d_log_migration_age_45_54_single = log(migration_age_45_54_single_2010) - log(migration_age_45_54_single_2001),
        d_log_migration_age_45_54_multi = log(migration_age_45_54_multi_2010) - log(migration_age_45_54_multi_2001),
        d_log_migration_age_55_64_single = log(migration_age_55_64_single_2010) - log(migration_age_55_64_single_2001),
        d_log_migration_age_55_64_multi = log(migration_age_55_64_multi_2010) - log(migration_age_55_64_multi_2001)
    )

migration_change_p2 <- migration_wide |>
    transmute(
        iso_o, iso_d,
        period = 2,
        d_log_migration_age_25_34 = log(migration_age_25_34_2019) - log(migration_age_25_34_2010),
        d_log_migration_age_35_44 = log(migration_age_35_44_2019) - log(migration_age_35_44_2010),
        d_log_migration_age_45_54 = log(migration_age_45_54_2019) - log(migration_age_45_54_2010),
        d_log_migration_age_55_64 = log(migration_age_55_64_2019) - log(migration_age_55_64_2010),
        d_log_migration_age_65_plus = log(migration_age_65_plus_2019) - log(migration_age_65_plus_2010),
        d_log_migration_age_25_34_single = log(migration_age_25_34_single_2019) - log(migration_age_25_34_single_2010),
        d_log_migration_age_25_34_multi = log(migration_age_25_34_multi_2019) - log(migration_age_25_34_multi_2010),
        d_log_migration_age_35_44_single = log(migration_age_35_44_single_2019) - log(migration_age_35_44_single_2010),
        d_log_migration_age_35_44_multi = log(migration_age_35_44_multi_2019) - log(migration_age_35_44_multi_2010),
        d_log_migration_age_45_54_single = log(migration_age_45_54_single_2019) - log(migration_age_45_54_single_2010),
        d_log_migration_age_45_54_multi = log(migration_age_45_54_multi_2019) - log(migration_age_45_54_multi_2010),
        d_log_migration_age_55_64_single = log(migration_age_55_64_single_2019) - log(migration_age_55_64_single_2010),
        d_log_migration_age_55_64_multi = log(migration_age_55_64_multi_2019) - log(migration_age_55_64_multi_2010),
    )

# combine both periods
migration_hetero <- bind_rows(migration_change_p1, migration_change_p2)

# merge data into dt_chg_adh, dt_chg_mix
dt_chg_adh <- dt_chg_adh |> left_join(migration_hetero |> mutate(iso_d = as.numeric(iso_d), iso_o = as.numeric(iso_o)), by = c("iso_d", "iso_o", "period"))
dt_chg_mix <- dt_chg_mix |> left_join(migration_hetero |> mutate(iso_d = as.numeric(iso_d), iso_o = as.numeric(iso_o)), by = c("iso_d", "iso_o", "period"))

## gender-specific migration
setFixest_dict(c(
    "x_import_d" = "Import (d)",
    "x_import_o" = "Import (o)",
    "x_export_d" = "Export (d)",
    "x_export_o" = "Export (o)",
    "z_import_d" = "IV for Import (d)",
    "z_import_o" = "IV for Import (o)",
    "z_export_d" = "IV for Export (d)",
    "z_export_o" = "IV for Export (o)",
    "cz_id_d" = "CZ (destination)",
    "cz_id_o" = "CZ (origin)",
    "period" = "Period",
    "d_log_migration_age_25_34" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_35_44" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_45_54" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_55_64" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_65_plus" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_25_34_single" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_25_34_multi" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_35_44_single" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_35_44_multi" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_45_54_single" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_45_54_multi" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_55_64_single" = "$\\Delta \\log(\\text{migration})$",
    "d_log_migration_age_55_64_multi" = "$\\Delta \\log(\\text{migration})$"
))



## age

# 25-34
s1a <- safe_feols(
    d_log_migration_age_25_34 ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 35-44
s1b <- safe_feols(
    d_log_migration_age_35_44 ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 45-54
s1c <- safe_feols(
    d_log_migration_age_45_54 ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 55-64
s1d <- safe_feols(
    d_log_migration_age_55_64 ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 65 plus
s1e <- safe_feols(
    d_log_migration_age_65_plus ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

## save the main table tex file
etable(s1a, s1b, s1c, s1d, s1e,
    tex = TRUE,
    se.below = TRUE,
    headers = list("Age" = list("25-34" = 1, "35-44" = 1, "45-54" = 1, "55-64" = 1, "65+" = 1)),
    fitstat = c("n", "ivf"),
    drop = "local",
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab_age.tex")
)


## age x household size: single-person HH by age (4 columns)

# 25-34 single
s1a <- safe_feols(
    d_log_migration_age_25_34_single ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 35-44 single
s1b <- safe_feols(
    d_log_migration_age_35_44_single ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 45-54 single
s1c <- safe_feols(
    d_log_migration_age_45_54_single ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 55-64 single
s1d <- safe_feols(
    d_log_migration_age_55_64_single ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

## save the single-person HH age table tex file
etable(s1a, s1b, s1c, s1d,
    tex = TRUE,
    se.below = TRUE,
    headers = list("Age" = list("25-34" = 1, "35-44" = 1, "45-54" = 1, "55-64" = 1)),
    fitstat = c("n", "ivf"),
    drop = "local",
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab_age_single.tex")
)

## age x household size: multi-person HH by age (4 columns)

# 25-34 multi
s1a <- safe_feols(
    d_log_migration_age_25_34_multi ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 35-44 multi
s1b <- safe_feols(
    d_log_migration_age_35_44_multi ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 45-54 multi
s1c <- safe_feols(
    d_log_migration_age_45_54_multi ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

# 55-64 multi
s1d <- safe_feols(
    d_log_migration_age_55_64_multi ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

## save the multi-person HH age table tex file
etable(s1a, s1b, s1c, s1d,
    tex = TRUE,
    se.below = TRUE,
    headers = list("Age" = list("25-34" = 1, "35-44" = 1, "45-54" = 1, "55-64" = 1)),
    fitstat = c("n", "ivf"),
    drop = "local",
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab_age_multi.tex")
)
