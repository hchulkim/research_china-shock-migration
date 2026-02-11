###############################################################################
# Title: Control variables construction
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-13
# Modified date: 2025-10-18
#
# Description:
#   Constructs baseline control variables at the commuting zone level:
#   manufacturing employment share, college-educated population share,
#   foreign-born population share, population, and pre-treatment migration
#   trends (1996-2000).
#
# Input:
#   - data/temp/est_region_industry_matched.csv
#   - data/covariates/college_educated2000.csv
#   - data/covariates/foreign_born2000.csv
#   - data/population/pop2001.xlsx
#   - data/temp/migration1996_2000_refine.rds
#   - data/cz/cz_data.xlsx
#   - data/concordance/region/region_stat.xlsx
#   - data/concordance/region/region_kosis.xlsx
#
# Output:
#   - data/temp/controls.csv
#   - data/temp/manu_share.csv
#   - data/temp/college_share2000.csv
#   - data/temp/foreign_share2000.csv
#   - data/temp/pop_2001.csv
#   - data/temp/migration_change.csv
###############################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, data.table, stringr, tidyfast, readr)

here::i_am("code/07-controls-construction.R")


# load cz data
cz_data <- readxl::read_excel(here("data", "cz", "cz_data.xlsx")) |>
  dplyr::mutate(
    cz_code = as.character(cz_code)
  ) |>
  as.data.table()
cz_data <- cz_data[, .(cz_code, cz_id)]


# load necessary data for creating controls
# percentage of employment in manufacturing in 2000
manu <- fread(here("data", "temp", "est_region_industry_matched.csv"),
  colClasses = list(character = c("iso", "ksic10"))
)[year < 2002, ]

# manu ksic10 code is 10~34
manu[as.numeric(substr(ksic10, 1, 2)) >= 10 & as.numeric(substr(ksic10, 1, 2)) < 35, manufacturing := 1]
manu[is.na(manufacturing), manufacturing := 0]


# merge cz data to the main data and also manually take care of some cases following cz_data
manu <- merge(manu, cz_data, by.x = "iso", by.y = "cz_code", all.x = TRUE)
manu[substr(iso, 1, 2) == "11", cz_id := 1]
manu[substr(iso, 1, 2) == "21", cz_id := 2]
manu[substr(iso, 1, 2) == "22", cz_id := 3]
manu[substr(iso, 1, 2) == "23", cz_id := 4]
manu[substr(iso, 1, 2) == "25", cz_id := 5]
manu[substr(iso, 1, 2) == "24", cz_id := 6]
manu[substr(iso, 1, 2) == "26", cz_id := 7]
manu[substr(iso, 1, 2) == "39", cz_id := 33]

# share by employment in manufacturing
manu_share <- manu[, .(employment = sum(emp_all, na.rm = TRUE)), by = .(year, cz_id, manufacturing)]

manu_share[, emp_all := sum(employment, na.rm = TRUE), by = .(year, cz_id)]
manu_share[, share := employment / emp_all]
manu_share <- manu_share[manufacturing == 1, ]
manu_share <- manu_share[, .(year, cz_id, share)]

# save the data
fwrite(manu_share, here("data", "temp", "manu_share.csv"))

# load percentage of college-educated population
college <- fread(here("data", "covariates", "college_educated2000.csv"), skip = 2)
setnames(college, c("iso", "age", "college_educated", "college_educated_pop"))
college <- college[str_detect(iso, "^[0-9]{5}"), ]
college[, `:=`(iso = str_sub(iso, 1, 5), college_educated_pop = as.numeric(college_educated_pop))]
college <- college[str_sub(iso, 5, 5) == "0", ]

# manually correct some region codes
college[iso == "33310", iso := "33010"]
college[iso == "34400", iso := "34060"]
college[iso == "38010", iso := "38110"]
college[iso == "38020", iso := "38110"]
college[iso == "38040", iso := "38110"]


# merge cz data to the main data and also manually take care of some cases following cz_data
college <- merge(college, cz_data, by.x = "iso", by.y = "cz_code", all.x = TRUE)
college[substr(iso, 1, 2) == "11", cz_id := 1]
college[substr(iso, 1, 2) == "21", cz_id := 2]
college[substr(iso, 1, 2) == "22", cz_id := 3]
college[substr(iso, 1, 2) == "23", cz_id := 4]
college[substr(iso, 1, 2) == "25", cz_id := 5]
college[substr(iso, 1, 2) == "24", cz_id := 6]
college[substr(iso, 1, 2) == "26", cz_id := 7]
college[substr(iso, 1, 2) == "39", cz_id := 33]

college[college_educated == "00 계", college_educated := 0]
college[college_educated != 0, college_educated := 1]

college <- college[, .(college_educated_pop = sum(college_educated_pop, na.rm = TRUE)), by = .(college_educated, cz_id)]

college_wide <- college |>
  dt_pivot_wider(
    names_from = college_educated,
    values_from = college_educated_pop
  )
pop_all <- college_wide[, .(cz_id, pop = `0`)]

college_wide[, college_educated_share := `1` / (`0` + `1`)]
college_wide <- college_wide[, .(cz_id, college_educated_share)]
college_wide[, year := 2000]

# save the data
fwrite(college, here("data", "temp", "college_share2000.csv"))


# load the foreign born data
foreign <- fread(here("data", "covariates", "foreign_born2000.csv"), skip = 2)
setnames(foreign, c("iso", "foreign_born_pop"))


foreign <- foreign[str_detect(iso, "^[0-9]{5}"), ]
foreign[, `:=`(iso = str_sub(iso, 1, 5), foreign_born_pop = as.numeric(foreign_born_pop))]
foreign <- foreign[str_sub(iso, 5, 5) == "0", ]

# manually correct some region codes
foreign[iso == "33310", iso := "33010"]
foreign[iso == "34400", iso := "34060"]
foreign[iso == "38010", iso := "38110"]
foreign[iso == "38020", iso := "38110"]
foreign[iso == "38040", iso := "38110"]


# merge cz data to the main data and also manually take care of some cases following cz_data
foreign <- merge(foreign, cz_data, by.x = "iso", by.y = "cz_code", all.x = TRUE)
foreign[substr(iso, 1, 2) == "11", cz_id := 1]
foreign[substr(iso, 1, 2) == "21", cz_id := 2]
foreign[substr(iso, 1, 2) == "22", cz_id := 3]
foreign[substr(iso, 1, 2) == "23", cz_id := 4]
foreign[substr(iso, 1, 2) == "25", cz_id := 5]
foreign[substr(iso, 1, 2) == "24", cz_id := 6]
foreign[substr(iso, 1, 2) == "26", cz_id := 7]
foreign[substr(iso, 1, 2) == "39", cz_id := 33]

foreign <- foreign[, .(foreign_born_pop = sum(foreign_born_pop, na.rm = TRUE)), by = .(cz_id)]


# add pop and get share
foreign <- merge(foreign, pop_all, by = "cz_id", all.x = TRUE)
foreign[, foreign_born_share := foreign_born_pop / pop]
foreign <- foreign[, .(cz_id, foreign_born_share)]
foreign[, year := 2000]

# save the data
fwrite(foreign, here("data", "temp", "foreign_share2000.csv"))






# save the whole controls data
controls <- merge(manu_share[year == 2000, .(cz_id, manu_share = share)], college_wide[, .(cz_id, college_educated_share)], by = "cz_id", all.x = TRUE)

controls <- merge(controls, foreign[, .(cz_id, foreign_born_share)], by = "cz_id", all.x = TRUE)

fwrite(controls, here("data", "temp", "controls.csv"))


# load the population data

# add population as weight
pop2001 <- readxl::read_excel(here("data", "population", "pop2001.xlsx")) |> as.data.table()
setnames(pop2001, new = c("iso", "pop2001"))

# 2001
pop2001 <- pop2001[str_detect(iso, "[0-9]{5}"), ]
pop2001[, iso := str_extract(iso, "[0-9]{5}")]

pop2001[, pop := as.numeric(pop2001)]


# read in the concordance for region stat----
region_stat <- readxl::read_excel(here("data", "concordance", "region", "region_stat.xlsx"))

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel(here("data", "concordance", "region", "region_kosis.xlsx"))

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat = str_sub(city_stat, 1, 5))


## important: due to change in code of certain province, I manually change such case.
population <- pop2001 %>%
  mutate(iso = ifelse(str_sub(iso, 1, 2) == "51", paste0("42", str_sub(iso, 3, 5)), iso))

population <- population %>%
  mutate(iso = ifelse(str_sub(iso, 1, 2) == "52", paste0("45", str_sub(iso, 3, 5)), iso))

# now use region_stat to crosswalk change in regions (mostly to 2001 regions) -------
population <- population %>%
  mutate(iso = ifelse(iso == "44825", "70000", iso))

population <- population %>%
  mutate(iso = ifelse(iso == "43745", "80000", iso))


population <- population %>% left_join(region_kosis, by = c("iso" = "city_kosis"))

population <- population %>%
  select(-iso) %>%
  rename(iso = city_stat)

population <- population %>% select(-city_label)

# filter out na
population <- population %>% filter(!is.na(iso))



# now use stat region case

population <- population %>% left_join(region_stat, by = c("iso" = "city_stat_raw"))

population <- population %>%
  mutate(iso = ifelse(!is.na(city_stat), city_stat, iso))
population <- population %>% select(-city_stat)



population <- population %>%
  group_by(iso) %>%
  summarise(pop = sum(pop, na.rm = T)) %>%
  ungroup()

# also get the net pop growth

population %>%
  fwrite(here("data", "temp", "pop_2001.csv"))



# log migration change for 1996-2000
migration1996_2000 <- read_rds(here("data", "temp", "migration1996_2000_refine.rds")) |> as.data.table()

# only use 1996 and 2000
migration_old <- migration1996_2000[year %in% c(1996, 2000), ]

migration_old[, migration := `연령별남자(합계)` + `연령별여자(합계)`]

migration_old <- migration_old[, .(year, iso_o, migration)]

# sum by iso_o and year
migration_old <- migration_old[, .(migration = sum(migration, na.rm = T)), by = .(iso_o, year)]

# make it into log migration
migration_old[, log_migration := log(migration)]

# create migration change using shift lag
migration_old[, log_migration_lag := shift(log_migration), by = .(iso_o)]
migration_old[, migration_change := log_migration - log_migration_lag]

migration_old <- migration_old[year == 2000, ]

# drop na
migration_old <- migration_old[!is.na(migration_change), ]

# save the data
migration_old <- migration_old[, .(iso_o, year, migration_change)]

# save the data
migration_old |> fwrite(here("data", "temp", "migration_change.csv"))

# add cz id
migration_old <- merge(migration_old, cz_data, by.x = "iso_o", by.y = "cz_code", all.x = TRUE)

# manually correct some region codes
migration_old[substr(iso_o, 1, 2) == "11", cz_id := 1]
migration_old[substr(iso_o, 1, 2) == "21", cz_id := 2]
migration_old[substr(iso_o, 1, 2) == "22", cz_id := 3]
migration_old[substr(iso_o, 1, 2) == "23", cz_id := 4]
migration_old[substr(iso_o, 1, 2) == "25", cz_id := 5]
migration_old[substr(iso_o, 1, 2) == "24", cz_id := 6]
migration_old[substr(iso_o, 1, 2) == "26", cz_id := 7]
migration_old[substr(iso_o, 1, 2) == "39", cz_id := 33]

# save the data
migration_old |> fwrite(here("data", "temp", "migration_change.csv"))
