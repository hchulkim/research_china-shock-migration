###############################################################################
# Title: Establishment-to-district concordance
# Maintainer: Hyoungchul Kim
# Initial date: 2025-08-07
# Modified date: 2025-08-07
#
# Description:
#   Creates crosswalk between establishment survey regional codes and
#   standardized district codes used in migration analysis. Loads
#   establishment data from 1994-2019 and harmonizes regional identifiers.
#
# Input:
#   - data/est/est{1994,1996,1999,2000,2001,2010,2019}.csv
#   - data/concordance/region/region_stat.xlsx
#
# Output:
#   - data/temp/est{1994,...,2019}_region_matched.rds
###############################################################################


# Load required packages for data manipulation and file I/O
pacman::p_load(here, dplyr, readxl, stringr, readr)

#-------------------- 
# Read in establishment data and region concordance table
# This section loads industry establishment data from multiple years
# and the concordance table for matching regional codes
#-------------------- 

# Load region concordance table for matching establishment locations to districts
# This table maps raw regional codes from establishment data to standardized district codes
region_stat <- readxl::read_excel(here("data","concordance", "region", "region_stat.xlsx"))

# Convert to data.table and ensure all columns are character for consistent string matching
region_stat <- region_stat %>% mutate(across(everything(), as.character))


# Load establishment survey data from multiple years (1994-2019)
# Each dataset contains establishment-level employment data by region and industry
# Note: Missing values are coded as various asterisk patterns in the original data

# Load 1994 establishment data
# Contains regional codes (province, city/county) and 5-digit industry codes plus total employment
est1994 <- read.table(here("data", "est", "est1994.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                         , "character", "character", "character", "character", "character", "character", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

# Assign meaningful column names for 1994 data structure
colnames(est1994) = c("행정구역(시도)"
                   , "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_all")


# Load 1996 establishment data (same structure as 1994)
est1996 <- read.table(here("data", "est", "est1996.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est1996) = c("행정구역(시도)"
                      , "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_all")

# Load 1999 establishment data (same structure as 1994-1996)
est1999 <- read.table(here("data", "est", "est1999.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est1999) = c("행정구역(시도)"
                      , "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_all")


# Load 2000 establishment data 
# Note: Starting from 2000, data includes gender breakdown of employment
est2000 <- read.table(here("data", "est", "est2000.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                            , "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

# Column structure includes male/female employment breakdowns in addition to total
colnames(est2000) = c("행정구역(시도)", "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_male", "emp_female", "emp_all")


# Load 2001 establishment data
# Note: 2001 data has much more detailed structure with organizational info and detailed employment breakdowns
est2001 <- read.table(here("data", "est", "est2001.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                        , "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                        , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

# 2001 structure includes: regional codes, detailed industry classification, 
# organizational form, establishment type, founding year/month, owner gender,
# and detailed employment breakdown by type (self-employed, unpaid family, regular, temporary, etc.)
colnames(est2001) = c("행정구역(시도)"
                   , "행정구역(구시군)", "산업분류(대)", "산업분류(중)", "산업분류(소)", "산업분류(세)", "산업분류(세세)", "4.조직형태", "5.사업체구분", "6-1.창설년", "6-2.창설월", "2.대표자성별", "8-①종사자_자영업주_남", "8-①종사자_자영업주_여", "8-①종사자_자영업주_계", "8-②종사자_무급가족_남", "8-②종사자_무급가족_여", "8-②종사자_무급가족_계", "8-③종사자_상용_남", "8-③종사자_상용_여", "8-③종사자_상용_계"
                   , "8-④종사자_임시및일일_남", "8-④종사자_임시및일일_여", "8-④종사자_임시및일일_계", "8-⑤종사자_무급_남", "8-⑤종사자_무급_여", "8-⑤종사자_무급_계", "8-⑥종사자_합계_남(①+…+⑤)", "8-⑥종사자_합계_여(①+…+⑤)", "8-⑥종사자_합계_계(①+…+⑤)")


# Load 2010 establishment data
# Note: Similar detailed structure to 2001 but with updated column naming conventions
est2010 <- read.table(here("data", "est", "est2010.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                    , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

# 2010 structure: regional codes, detailed industry classification, organizational form,
# establishment type, founding year/month, owner gender, and employment breakdown by type and gender
colnames(est2010) = c("행정구역_시도"
                   , "행정구역_구시군", "산업분류_대", "산업분류_중", "산업분류_소", "산업분류_세", "산업분류_세세", "조직형태", "사업체구분", "창업년도", "창업월", "대표자성별", "종사자수_상용종사자_남", "종사자수_상용종사자_여", "종사자수_상용종사자_계", "종사자수_임시 및 일일종사자_남", "종사자수_임시 및 일일종사자_여", "종사자수_임시 및 일일종사자_계", "종사자수_자영업주_남", "종사자수_자영업주_여", "종사자수_자영업주_계"
                   , "종사자수_무급가족종사자_남", "종사자수_무급가족종사자_여", "종사자수_무급가족종사자_계", "종사자수_무급종사자_남", "종사자수_무급종사자_여", "종사자수_무급종사자_계", "종사자수_합계_남", "종사자수_합계_여", "종사자수_합계_계")


# Load 2019 establishment data 
# Note: 2019 has a modernized structure with coded variables and includes survey reference year
est2019 <- read.table(here("data", "est", "est2019.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                         , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                         , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

# 2019 structure: survey year, regional codes (as codes rather than text), owner gender code,
# founding year/month, organizational/establishment type codes, industry codes, 
# employment breakdown by type and gender, plus additional detailed industry subcodes
colnames(est2019) = c("조사기준연도"
                      , "행정구역시도코드", "행정구역시군구코드", "대표자성별코드", "창업연도", "창업월", "조직형태코드", "사업체구분코드", "산업대분류코드", "주사업_산업중분류코드", "주사업_산업소분류코드", "상용근로_합계종사자수", "합계종사자수", "상용근로_남자종사자수", "남자종사자수", "상용근로_여자종사자수", "여자종사자수", "임시일용근로_합계종사자수", "임시일용근로_남자종사자수", "임시일용근로_여자종사자수", "자영업_합계종사자수"
                      , "자영업_남자종사자수", "자영업_여자종사자수", "무급가족_합계종사자수", "무급가족_남자종사자수", "무급가족_여자종사자수", "기타_합계종사자수", "기타_남자종사자수", "기타_여자종사자수", "주사업_산업세분류코드", "주사업_산업세세분류코드")


#-------------------- 
# Region code harmonization and matching
# This section creates standardized regional identifiers and matches them
# to the district codes used in migration analysis
#-------------------- 

# Step 1: Create composite regional identifiers by concatenating province and city/county codes
# Note: Different years use different column naming conventions
est1994 <- est1994 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est1996 <- est1996 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est1999 <- est1999 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est2000 <- est2000 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est2001 <- est2001 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est2010 <- est2010 %>% mutate(iso=paste0(`행정구역_시도`, `행정구역_구시군`))  # Different naming convention
est2019 <- est2019 %>% mutate(iso=paste0(`행정구역시도코드`, `행정구역시군구코드`))  # Coded format


# Step 2: Standardize regional codes to 5-digit format with trailing zero
# This ensures consistent formatting across years (XXXX0 format)
est1994 <- est1994 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est1996 <- est1996 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est1999 <- est1999 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2000 <- est2000 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2001 <- est2001 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2010 <- est2010 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2019 <- est2019 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))


# Step 3: Match establishment regional codes to standardized district codes
# Left join with region concordance table to get standardized district identifiers
est1994 <- est1994 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est1996 <- est1996 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est1999 <- est1999 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2000 <- est2000 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2001 <- est2001 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2010 <- est2010 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2019 <- est2019 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))


# Step 4: Apply matched district codes where available, keeping original codes as fallback
# This creates the final standardized regional identifier for matching with migration data
est1994 <- est1994 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est1996 <- est1996 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est1999 <- est1999 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2000 <- est2000 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2001 <- est2001 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2010 <- est2010 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2019 <- est2019 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


#-------------------- 
# Save processed establishment data with standardized regional codes
# These files will be used in subsequent analysis steps
#-------------------- 

est1994 %>% write_rds(here("data", "temp", "est1994_region_matched.rds"))
est1996 %>% write_rds(here("data", "temp", "est1996_region_matched.rds"))
est1999 %>% write_rds(here("data", "temp", "est1999_region_matched.rds"))
est2000 %>% write_rds(here("data", "temp", "est2000_region_matched.rds"))
est2001 %>% write_rds(here("data", "temp", "est2001_region_matched.rds"))
est2010 %>% write_rds(here("data", "temp", "est2010_region_matched.rds"))
est2019 %>% write_rds(here("data", "temp", "est2019_region_matched.rds"))


