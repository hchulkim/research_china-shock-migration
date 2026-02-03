#####################################################################
# Title: Make-file for analysis report
# Maintainer: Hyoungchul
# Initial date: 2025-07-17
# Modified date: 2025-08-08
#####################################################################

## Directory vars (usually only these need changing)
tempdir = data/temp/
procdir = data/proc/
outputdir = output/
figdir = output/figures/
tabdir = output/tables/
slidedir = output/slides/
papdir = output/paper/

# country for IV construction
country_list := AUS CHE DEU DNK ESP FIN GBR JPN KOR NZL

country := $(country_list:%=data/comtrade/comtrade_%_h1.csv) $(country_list:%=data/comtrade/comtrade_%_h3.csv) $(country_list:%=data/comtrade/comtrade_%_h5.csv)

all: crosswalk

## Draw the Makefile DAG
## Requires: https://github.com/lindenb/makefile2graph
dag: makefile-dag.png
makefile-dag.png: Makefile
	make -Bnd all | make2graph | dot -Tpng -Gdpi=300 -o makefile-dag.png

copy_paste:
	rm -rf ../Apps/Overleaf/china-shock-internal-migration/output
	cp -r output/ ../Apps/Overleaf/china-shock-internal-migration/

# Download raw data from the sources (if possible). Be aware that significant structure changes in the data sources may require significant changes in the code.
raw: raw_comtrade raw_crosswalk_ksic_ksic raw_district

raw_comtrade:
	Rscript code/raw-data-download.R

raw_crosswalk_ksic_ksic:
	wget -nc -P data/concordance/ksic \
	 "https://kssc.kostat.go.kr:8443/ksscNew_web/upload/KSIC%EC%97%B0%EA%B3%84%ED%91%9C(9%EC%B0%A8_10%EC%B0%A8).xlsx" \
	 "https://kssc.kostat.go.kr:8443/ksscNew_web/upload/9%EC%B0%A8%EA%B0%9C%EC%A0%95%20%EC%97%B0%EA%B3%84%ED%91%9C.xls" \
	 "https://kssc.kostat.go.kr:8443/ksscNew_web/upload/%EC%8B%A0%EA%B5%AC%20%EC%97%B0%EA%B3%84%ED%91%9C(2000-1998).xls"

raw_district:
	cd data/region && unzip bnd_sigungu_00_2019_4Q.zip

## crosswalk data
crosswalk: comtrade_isic4.csv isic4_ksic10_table.csv comtrade_ksic10.csv ksic8_9.csv ksic9_10.csv

# shock: hs to isic4 to ksic10
comtrade_isic4.csv: $(country)
	Rscript code/01-concordance-hs-isic.R

isic4_ksic10_table.csv comtrade_ksic10.csv: data/concordance/isic/isic4_ksic10.xlsx data/temp/comtrade_isic4.csv
	Rscript code/02-concordance-isic-ksic-table.R

# crosswalk regions in est data
est1994_region_matched.rds est1996_region_matched.rds est1999_region_matched.rds est2000_region_matched.rds est2001_region_matched.rds est2010_region_matched.rds est2019_region_matched.rds &: data/est/est1994.csv data/est/est1996.csv data/est/est1999.csv data/est/est2000.csv data/est/est2001.csv data/est/est2010.csv data/est/est2019.csv data/concordance/region/region_stat.xlsx
	Rscript code/03-concordance-est-district.R

# share: ksic 8 and 9 to 10 
ksic8_9.csv ksic9_10.csv &: data/concordance/ksic/ksic9_8.xls data/concordance/ksic/ksic9_10.xlsx
	Rscript code/04-concordance-ksic-ksic.R

# clean up the output directory
clean:
	rm -f $(figdir)* $(tabdir)* $(tempdir)* $(procdir)*

## Helpers
.PHONY: all clean dag copy_paste raw raw_comtrade raw_crosswalk_ksic_ksic raw_district crosswalk
