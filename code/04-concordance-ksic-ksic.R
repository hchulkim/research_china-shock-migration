###############################################################################
# Title: KSIC cross-revision concordance
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-05
# Modified date: 2025-08-08
#
# Description:
#   Creates concordance tables between KSIC revisions (8->9 and 9->10) and
#   applies them to convert establishment employment data to a unified
#   KSIC10 classification across all years (1994-2019).
#
# Input:
#   - data/concordance/ksic/ksic9_8.xls
#   - data/concordance/ksic/ksic9_10.xlsx
#   - data/temp/est{1994,...,2019}_region_matched.rds
#
# Output:
#   - data/temp/ksic9_10.csv
#   - data/temp/ksic8_9.csv
#   - data/temp/est_region_industry_matched.csv
###############################################################################

if (!require(pacman)) install.packages("pacman")
p_load(here, data.table, readxl, zoo, dplyr, purrr, readr)

# Declare this file location
here::i_am("code/04-concordance-ksic-ksic.R")

### Create crosswalk tables

## KSIC 9 to 10 table

# load the data
ksic910 = readxl::read_excel(here("data", "concordance", "ksic", "ksic9_10.xlsx"), skip = 2, sheet = 2, col_names = F, col_types = c("text", "text", "text", "text", "numeric", "text")) |>
	as.data.table()

setnames(ksic910, old = c("...1", "...2", "...3", "...4", "...5", "...6"), new = c("ksic9", "ksic9_des", "ksic10", "ksic10_des", "weight", "misc"))

# Fill down the KSIC9 column
ksic910[, ksic9 := zoo::na.locf(ksic9)]

# Sort the data by ksic9 to make sure weight is correctly specified
ksic910 = ksic910[order(ksic9)]

# Take out duplicates 
ksic910 <- ksic910[!duplicated(ksic910[, .(ksic9, ksic10)])]

# Create weights
ksic910[, weight := 1 / .N, by = ksic9]

# make sure codes are in character
ksic910[, `:=` (ksic10 = as.character(ksic10), ksic9 = as.character(ksic9))]

fwrite(ksic910, here("data", "temp", "ksic9_10.csv"))

## KSIC 8 to 9 table

# load the data
ksic89 = readxl::read_excel(here("data", "concordance", "ksic", "ksic9_8.xls"), skip = 2, sheet = 2, col_names = F, col_types = c("text", "text", "text", "text", "numeric", "text")) |>
	as.data.table()

setnames(ksic89, old = c("...1", "...2", "...3", "...4", "...5", "...6"), new = c("ksic8", "ksic8_des", "ksic9", "ksic9_des", "weight", "misc"))

# Fill down the KSIC8 column
ksic89[, ksic8 := zoo::na.locf(ksic8)]

# Sort the data by ksic8 to make sure weight is correctly specified
ksic89 = ksic89[order(ksic8)]

# Take out duplicates 
ksic89 <- ksic89[!duplicated(ksic89[, .(ksic8, ksic9)])]

# Create weights
ksic89[, weight := 1 / .N, by = ksic8]

# make sure codes are in character
ksic89[, `:=` (ksic9 = as.character(ksic9), ksic8 = as.character(ksic8))]

fwrite(ksic89, here("data", "temp", "ksic8_9.csv"))



#--------------------
# read in the est data
#--------------------
est1994 <-read_rds(here("data", "temp", "est1994_region_matched.rds"))
est1996 <-read_rds(here("data", "temp", "est1996_region_matched.rds"))
est1999 <-read_rds(here("data", "temp", "est1999_region_matched.rds"))
est2000 <-read_rds(here("data", "temp", "est2000_region_matched.rds"))
est2001 <-read_rds(here("data", "temp", "est2001_region_matched.rds"))
est2010 <-read_rds(here("data", "temp", "est2010_region_matched.rds"))
est2019 <-read_rds(here("data", "temp", "est2019_region_matched.rds"))

ksic89 <- as_tibble(ksic89)
ksic910 <- as_tibble(ksic910)

# paste industry code into 5 digit
est1994 <- est1994 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 1994)
est1996 <- est1996 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 1996)
est1999 <- est1999 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 1999)
est2000 <- est2000 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 2000)
  


est2001 <- est2001 %>% mutate(ksic8=paste0(`산업분류(중)`, `산업분류(소)`, `산업분류(세)`, `산업분류(세세)`), emp_female=`8-⑥종사자_합계_여(①+…+⑤)`, emp_male=`8-⑥종사자_합계_남(①+…+⑤)`, emp_all=`8-⑥종사자_합계_계(①+…+⑤)`) %>% select(iso, ksic8, emp_female, emp_male, emp_all) %>% 
  mutate(year = 2001)


est2010 <- est2010 %>% mutate(ksic9=paste0(`산업분류_중`, `산업분류_소`, `산업분류_세`, `산업분류_세세`), emp_female=`종사자수_합계_여`, emp_male=`종사자수_합계_남`, emp_all=`종사자수_합계_계`) %>% select(iso, ksic9, emp_female, emp_male, emp_all) %>% 
  mutate(year = 2010)



est2019 <- est2019 %>% mutate(ksic10=paste0(`주사업_산업중분류코드`, `주사업_산업소분류코드`, `주사업_산업세분류코드`, `주사업_산업세세분류코드`)) %>% 
  select(iso, ksic10, emp_female=`여자종사자수`, emp_male=`남자종사자수`, emp_all=`합계종사자수`) %>% 
  mutate(year = 2019)

# save 2019 as it is already ksic10

est2019 <- est2019 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup() %>% 
  mutate(year = 2019)

fwrite(est2019, here("data", "temp", "est2019_region_industry_matched.csv"))

# convert ksic 8 to 9: 1994 to 2001
ksic_1994_1999 <- list(est1994, est1996, est1999)

ksic_1994_1999_matched <- map(ksic_1994_1999, function(x) {
  x |> left_join(ksic89, by = "ksic8") |> 
    mutate(emp_all = emp_all * weight) |> 
    select(iso, year, ksic9, emp_all) |> 
    group_by(iso, year, ksic9) |> 
    summarise(emp_all = sum(emp_all, na.rm = T)) |> 
    ungroup()
}) |> list_rbind()

ksic_2000_2001 <- list(est2000, est2001)

ksic_2000_2001_matched <- map(ksic_2000_2001, function(x) {
  x |> left_join(ksic89, by = "ksic8") |> 
    mutate(emp_all = emp_all * weight,
           emp_female = emp_female * weight,
           emp_male = emp_male * weight) |> 
    select(iso, year, ksic9, emp_all, emp_female, emp_male) |> 
    group_by(iso, year, ksic9) |> 
    summarise(emp_all = sum(emp_all, na.rm = T), emp_female = sum(emp_female, na.rm = T), emp_male = sum(emp_male, na.rm = T)) |> 
    ungroup()
}) |> list_rbind()

# combine all
ksic_matched <- bind_rows(ksic_1994_1999_matched, ksic_2000_2001_matched)

# convert ksic 9 to 10: 1994 to 2001, 2010
ksic_1994_2001_2010 <- list(ksic_matched, est2010)

ksic_1994_2001_2010_matched <- map(ksic_1994_2001_2010, function(x) {
  x |> left_join(ksic910, by = "ksic9") |> 
    mutate(emp_all = emp_all * weight,
           emp_female = emp_female * weight,
           emp_male = emp_male * weight) |> 
    select(iso, year, ksic10, emp_all, emp_female, emp_male) |> 
    group_by(iso, year, ksic10) |> 
    summarise(emp_all = sum(emp_all, na.rm = T), emp_female = sum(emp_female, na.rm = T), emp_male = sum(emp_male, na.rm = T)) |> 
    ungroup()
}) |> list_rbind()

# for year 1994 to 1999, we again set emp_female and emp_male to NA
ksic_1994_2001_2010_matched <- ksic_1994_2001_2010_matched |> 
  mutate(emp_female = if_else(year %in% c(1994, 1996, 1999), NA, emp_female),
         emp_male = if_else(year %in% c(1994, 1996, 1999), NA, emp_male))


# combine all and save
bind_rows(ksic_1994_2001_2010_matched, est2019) |> fwrite(here("data", "temp", "est_region_industry_matched.csv"))