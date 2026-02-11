###############################################################################
# Title: Main regression analysis
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-12
# Modified date: 2026-02-01
#
# Description:
#   Runs the main IV regressions (Table 2) estimating the effect of trade
#   shocks on bilateral migration. Uses ADH-style and mixed IV strategies
#   with CZ fixed effects and population weights. Produces first-stage
#   results.
#
# Input:
#   - data/proc/baseline_data.csv
#   - data/proc/baseline_data_jpn.csv
#
# Output:
#   - output/tables/tab2.tex
#   - output/tables/table2_firststage_s1a.tex
#   - output/tables/table2_firststage_s1b.tex
###############################################################################

# Load packages
pacman::p_load(here, readr, data.table, fixest, texreg, ggplot2)


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

###############################################################################
# Table 2: Change in log migration DV (main analysis)
# DV = migration_change = log(mig_t) - log(mig_{t-1})
###############################################################################

cat("\n========================================\n")
cat("TABLE 2: DV = Change in log(migration)\n")
cat("========================================\n\n")

# keeping finite values
dt_chg_adh <- dt_adh[is.finite(migration_change)]
dt_chg_jpn <- dt_jpn[is.finite(migration_change)]
dt_chg_mix <- dt_mix[is.finite(migration_change)]

# 1A: All 4 shocks, ADH IV
cat("--- 1A: All 4 shocks | ADH IV | migration_change ---\n")
s1a <- safe_feols(
    migration_change ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
        cz_id_d + cz_id_o + period |
        x_import_d + x_import_o + x_export_d + x_export_o ~ z_import_d + z_import_o + z_export_d + z_export_o,
    data = dt_chg_adh, cluster = ~cz_pair, weights = ~pop_o
)

# 1B: All 4 shocks, Mixed IV
cat("\n--- 1B: All 4 shocks | Mixed IV | migration_change ---\n")
s1b <- safe_feols(
    migration_change ~ local_share_m_d + local_share_m_o + local_share_x_d + local_share_x_o |
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
    "period" = "Period"
))

## save the main table 2 tex file
etable(s1a, s1b,
    tex = TRUE,
    se.below = TRUE,
    fitstat = c("n", "ivf"),
    drop = "local",
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "tab2.tex")
)


###############################################################################
# First-stage result
###############################################################################

etable(summary(s1a, stage = 1),
    tex = TRUE,
    se.below = TRUE,
    fitstat = c("n", "ivf"),
    drop = "local",
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "table2_firststage_s1a.tex")
)

etable(summary(s1b, stage = 1),
    tex = TRUE,
    se.below = TRUE,
    fitstat = c("n", "ivf"),
    drop = "local",
    digits = 3,
    signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    depvar = TRUE,
    style.tex = style.tex("aer"),
    file = here("output", "tables", "table2_firststage_s1b.tex")
)
