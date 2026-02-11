###############################################################################
# Title: Robustness and appendix analysis
# Maintainer: Hyoungchul Kim
# Initial date: 2025-07-16
# Modified date: 2026-02-01
#
# Description:
#   Robustness checks and appendix tables: regressions with additional
#   controls, balance tests at regional and industry levels, per-period
#   analysis without CZ FEs, and long-period (2001-2019) analysis.
#
# Input:
#   - data/proc/baseline_data.csv
#   - data/proc/baseline_data_jpn.csv
#   - data/covariates/{college,foreign_born,female_emp}_share_by_cz_1990.csv
#   - data/temp/migration_change.csv
#   - data/temp/ssiv_share.csv
#   - data/temp/ssiv_shock_2001_2010_2019.csv
#   - data/covariates/wage_by_industry_1995.csv
#   - data/covariates/computer_use_by_industry_2000.csv
#   - data/temp/migration2001_2014_refine.rds
#   - data/temp/migration2015_2019_refine.rds
#
# Output:
#   - output/tables/tab_appendix_control.tex
#   - output/tables/tab_appendix_balance_region.tex
#   - output/tables/tab_appendix_balance_industry.tex
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

# take out outdated foreign born share
dt_chg_adh[, foreign_born_share_d := NULL]
dt_chg_adh[, foreign_born_share_o := NULL]
dt_chg_jpn[, foreign_born_share_d := NULL]
dt_chg_jpn[, foreign_born_share_o := NULL]
dt_chg_mix[, foreign_born_share_d := NULL]
dt_chg_mix[, foreign_born_share_o := NULL]

# add the 1990 controls
college_1990 <- fread(here("data", "covariates", "college_share_by_cz_1990.csv"))
foreign_1990 <- fread(here("data", "covariates", "foreign_born_share_by_cz_1990.csv"))
female_1990 <- fread(here("data", "covariates", "female_emp_share_by_cz_1990.csv"))
migration_1990 <- fread(here("data", "temp", "migration_change.csv"))

# merge the 1990 controls
# destination
dt_chg_adh <- merge(dt_chg_adh, college_1990[, .(cz_id, college_share_d = college_share)], by.x = "cz_id_d", by.y = "cz_id", all.x = TRUE)
dt_chg_adh <- merge(dt_chg_adh, foreign_1990[, .(cz_id, foreign_born_share_d = foreign_born_share)], by.x = "cz_id_d", by.y = "cz_id", all.x = TRUE)
dt_chg_adh <- merge(dt_chg_adh, female_1990[, .(cz_id, female_emp_share_d = female_emp_share)], by.x = "cz_id_d", by.y = "cz_id", all.x = TRUE)
dt_chg_mix <- merge(dt_chg_mix, college_1990[, .(cz_id, college_share_d = college_share)], by.x = "cz_id_d", by.y = "cz_id", all.x = TRUE)
dt_chg_mix <- merge(dt_chg_mix, foreign_1990[, .(cz_id, foreign_born_share_d = foreign_born_share)], by.x = "cz_id_d", by.y = "cz_id", all.x = TRUE)
dt_chg_mix <- merge(dt_chg_mix, female_1990[, .(cz_id, female_emp_share_d = female_emp_share)], by.x = "cz_id_d", by.y = "cz_id", all.x = TRUE)

# origin
dt_chg_adh <- merge(dt_chg_adh, college_1990[, .(cz_id, college_share_o = college_share)], by.x = "cz_id_o", by.y = "cz_id", all.x = TRUE)
dt_chg_adh <- merge(dt_chg_adh, foreign_1990[, .(cz_id, foreign_born_share_o = foreign_born_share)], by.x = "cz_id_o", by.y = "cz_id", all.x = TRUE)
dt_chg_adh <- merge(dt_chg_adh, female_1990[, .(cz_id, female_emp_share_o = female_emp_share)], by.x = "cz_id_o", by.y = "cz_id", all.x = TRUE)
dt_chg_mix <- merge(dt_chg_mix, college_1990[, .(cz_id, college_share_o = college_share)], by.x = "cz_id_o", by.y = "cz_id", all.x = TRUE)
dt_chg_mix <- merge(dt_chg_mix, foreign_1990[, .(cz_id, foreign_born_share_o = foreign_born_share)], by.x = "cz_id_o", by.y = "cz_id", all.x = TRUE)
dt_chg_mix <- merge(dt_chg_mix, female_1990[, .(cz_id, female_emp_share_o = female_emp_share)], by.x = "cz_id_o", by.y = "cz_id", all.x = TRUE)

# 1A: All 4 shocks, ADH IV
cat("--- 1A: All 4 shocks | ADH IV | migration_change ---\n")
s1a <- safe_feols(
    migration_change ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o + college_share_d:period + college_share_o:period + foreign_born_share_d:period + foreign_born_share_o:period |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_adh, cluster = ~cz_pair, weights = ~pop_o
)

# 1B: All 4 shocks, Mixed IV
cat("\n--- 1B: All 4 shocks | Mixed IV | migration_change ---\n")
s1b <- safe_feols(
    migration_change ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o + college_share_d:factor(period) + college_share_o:factor(period) |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_mix, cluster = ~cz_pair, weights = ~pop_o
)

setFixest_dict(c(
    "migration_change" = "$\\Delta \\log(\\text{migration})$",
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
    "female_emp_share" = "Female employment share",
    "college_share" = "College share",
    "foreign_born_share" = "Foreign-born share",
    # "migration_change" = "Migration change (2000 - 1996)",
    "z_import" = "Import IV",
    "z_export" = "Export IV",
    "z_ad_x" = "Export IV",
    "z_ad_m" = "Import IV",
    "z_jp" = "Export IV",
    "computer_share" = "Computer share (2000)",
    "log_wage" = "Log wage (1995)"
))

## save the main table 2 tex file
etable(s1a, s1b,
    tex = TRUE,
    se.below = TRUE,
    fitstat = c("n", "ivf"),
    drop = c("local", "college", "foreign"),
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab_appendix_control.tex")
)

# balance test

dt_chg_adh_imp <- unique(dt_chg_adh[, .(period, cz_id = cz_id_o, z_import = z_import_o, college_share = college_share_o, foreign_born_share = foreign_born_share_o, local_share_m = local_share_m_o, female_emp_share = female_emp_share_o)], by = c("period", "cz_id"))

dt_chg_adh_exp <- unique(dt_chg_adh[, .(period, cz_id = cz_id_d, z_export = z_export_d, college_share = college_share_d, foreign_born_share = foreign_born_share_d, local_share_x = local_share_x_d, female_emp_share = female_emp_share_d)], by = c("period", "cz_id"))

dt_chg_mix_imp <- unique(dt_chg_mix[, .(period, cz_id = cz_id_o, z_import = z_import_o, college_share = college_share_o, foreign_born_share = foreign_born_share_o, local_share_m = local_share_m_o, female_emp_share = female_emp_share_o)], by = c("period", "cz_id"))

dt_chg_mix_exp <- unique(dt_chg_mix[, .(period, cz_id = cz_id_d, z_export = z_export_d, college_share = college_share_d, foreign_born_share = foreign_born_share_d, local_share_x = local_share_x_d, female_emp_share = female_emp_share_d)], by = c("period", "cz_id"))

# Rescale IVs by 1/1000 for readable coefficients
dt_chg_adh_imp[, z_import := z_import / 1000]
dt_chg_adh_exp[, z_export := z_export / 1000]
dt_chg_mix_imp[, z_import := z_import / 1000]
dt_chg_mix_exp[, z_export := z_export / 1000]

# college share

# import shock
s1a <- safe_feols(
    college_share ~ local_share_m + z_import |
        period,
    data = dt_chg_adh_imp
)

# export shock
s1b <- safe_feols(
    college_share ~ local_share_x + z_export |
        period,
    data = dt_chg_adh_exp
)

s1c <- safe_feols(
    college_share ~ local_share_x + z_export |
        period,
    data = dt_chg_mix_exp
)

# foreign born share

# import shock
s2a <- safe_feols(
    foreign_born_share ~ local_share_m + z_import |
        period,
    data = dt_chg_adh_imp
)

# export shock
s2b <- safe_feols(
    foreign_born_share ~ local_share_x + z_export |
        period,
    data = dt_chg_adh_exp
)

s2c <- safe_feols(
    foreign_born_share ~ local_share_x + z_export |
        period,
    data = dt_chg_mix_exp
)


# female employment share

# import shock
s3a <- safe_feols(
    female_emp_share ~ local_share_m + z_import |
        period,
    data = dt_chg_adh_imp
)

# export shock
s3b <- safe_feols(
    female_emp_share ~ local_share_x + z_export |
        period,
    data = dt_chg_adh_exp
)

s3c <- safe_feols(
    female_emp_share ~ local_share_x + z_export |
        period,
    data = dt_chg_mix_exp
)

# migration change

migration_1990_adh_imp <- merge(migration_1990, dt_chg_adh_imp[period == 1, ], by = "cz_id")

migration_1990_adh_exp <- merge(migration_1990, dt_chg_adh_exp[period == 1, ], by = "cz_id")

migration_1990_mix_exp <- merge(migration_1990, dt_chg_mix_exp[period == 1, ], by = "cz_id")


# import shock
s4a <- safe_feols(
    migration_change ~ local_share_m + z_import,
    data = migration_1990_adh_imp
)

# export shock
s4b <- safe_feols(
    migration_change ~ local_share_x + z_export,
    data = migration_1990_adh_exp
)

s4c <- safe_feols(
    migration_change ~ local_share_x + z_export,
    data = migration_1990_mix_exp
)

setFixest_dict(c(
    # "migration_change" = "$\\Delta \\log(migration)$",
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
    "female_emp_share" = "Female employment share",
    "college_share" = "College share",
    "foreign_born_share" = "Foreign-born share",
    "migration_change" = "Migration change (2000 - 1996)",
    "z_import" = "Import IV",
    "z_export" = "Export IV",
    "z_ad_x" = "Export IV",
    "z_ad_m" = "Import IV",
    "z_jp" = "Export IV",
    "computer_share" = "Computer share (2000)",
    "log_wage" = "Log wage (1995)"
))

# save balance test
etable(s1a, s1b, s1c, s2a, s2b, s2c, s3a, s3b, s3c, s4a, s4b, s4c,
    tex = TRUE,
    se.below = TRUE,
    drop.section = "fixef",
    fitstat = c("n"),
    drop = c("local", "Constant"),
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab_appendix_balance_region.tex")
)

# industry balance test

# read in the share data
share <- fread(here("data", "temp", "ssiv_share.csv"),
    colClasses = list(character = c("ksic10"))
)

# get average by ksic 2 digit
share[, ksic2 := substr(ksic10, 1, 2)]
share_2digit <- share[, .(share = mean(emp_share_1999, na.rm = TRUE)), by = "ksic2"]

# read in the exposure shock data
shock <- fread(here("data", "temp", "ssiv_shock_2001_2010_2019.csv"),
    colClasses = list(character = c("ksic10"))
)

# export jpn
shock_jpn <- shock[reporter_iso == "JPN" & flow_code == "X", ]

shock_jpn[, ksic2 := substr(ksic10, 1, 2)]

shock_jpn_2digit <- shock_jpn[, .(shock_jp = sum(shock, na.rm = TRUE)), by = .(ksic2, period)]

# export ADH
shock_adh_x <- shock[reporter_iso %in% c("AUS", "DNK", "FIN", "DEU", "NZL", "ESP", "CHE") & flow_code == "X", ]

shock_adh_x[, ksic2 := substr(ksic10, 1, 2)]

shock_adh_x_2digit <- shock_adh_x[, .(shock_ad = sum(shock, na.rm = TRUE)), by = .(ksic2, period)]

# import ADH
shock_adh_m <- shock[reporter_iso %in% c("AUS", "DNK", "FIN", "DEU", "NZL", "ESP", "CHE") & flow_code == "M", ]

shock_adh_m[, ksic2 := substr(ksic10, 1, 2)]

shock_adh_m_2digit <- shock_adh_m[, .(shock_ad = sum(shock, na.rm = TRUE)), by = .(ksic2, period)]

# merge the data
shock_jpn_2digit <- merge(shock_jpn_2digit, share_2digit, by = "ksic2", all.x = TRUE)
shock_adh_x_2digit <- merge(shock_adh_x_2digit, share_2digit, by = "ksic2", all.x = TRUE)
shock_adh_m_2digit <- merge(shock_adh_m_2digit, share_2digit, by = "ksic2", all.x = TRUE)

# calculate the instrumental variable
shock_jpn_2digit[, z_jp := shock_jp * share]
shock_adh_x_2digit[, z_ad_x := shock_ad * share]
shock_adh_m_2digit[, z_ad_m := shock_ad * share]

# Rescale IVs by 1/1000 for readable coefficients
shock_jpn_2digit[, z_jp := z_jp / 1000]
shock_adh_x_2digit[, z_ad_x := z_ad_x / 1000]
shock_adh_m_2digit[, z_ad_m := z_ad_m / 1000]

# read in industry level data
wage <- fread(here("data", "covariates", "wage_by_industry_1995.csv"))

wage <- wage[, .(ksic2 = as.character(industry_code), log_wage = log(hourly_wage))]

# merge it into the data
shock_jpn_2digit <- merge(shock_jpn_2digit, wage, by = "ksic2", all.x = TRUE)
shock_adh_x_2digit <- merge(shock_adh_x_2digit, wage, by = "ksic2", all.x = TRUE)
shock_adh_m_2digit <- merge(shock_adh_m_2digit, wage, by = "ksic2", all.x = TRUE)

# run regression

# export jpn
s5a <- feols(log_wage ~ z_jp | period, data = shock_jpn_2digit)

# export ADH
s5b <- feols(log_wage ~ z_ad_x | period, data = shock_adh_x_2digit)

# import ADH
s5c <- feols(log_wage ~ z_ad_m | period, data = shock_adh_m_2digit)

# read the computer use data
computer <- fread(here("data", "covariates", "computer_use_by_industry_2000.csv"))[, .(ksic2 = as.character(industry_code_2d), computer_share = daily_computer_share)]

computer[nchar(ksic2) == 1, ksic2 := paste0("0", ksic2)]

# merge it into the data
shock_jpn_2digit <- merge(shock_jpn_2digit, computer, by = "ksic2", all.x = TRUE)
shock_adh_x_2digit <- merge(shock_adh_x_2digit, computer, by = "ksic2", all.x = TRUE)
shock_adh_m_2digit <- merge(shock_adh_m_2digit, computer, by = "ksic2", all.x = TRUE)

# run balance test

# export jpn
s6a <- feols(computer_share ~ z_jp | period, data = shock_jpn_2digit)

# export ADH
s6b <- feols(computer_share ~ z_ad_x | period, data = shock_adh_x_2digit)

# import ADH
s6c <- feols(computer_share ~ z_ad_m | period, data = shock_adh_m_2digit)





# save balance test
etable(s5a, s5b, s5c, s6a, s6b, s6c,
    tex = TRUE,
    se.below = TRUE,
    drop.section = "fixef",
    fitstat = c("n"),
    drop = c("local"),
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab_appendix_balance_industry.tex")
)
