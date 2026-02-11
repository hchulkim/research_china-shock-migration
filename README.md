# The China Shock and Internal Migration: Evidence from Bilateral Migration Flows

[![R](https://img.shields.io/badge/R-4.5.0-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Platform](https://img.shields.io/badge/Platform-Linux%20%7C%20macOS-lightgrey.svg)]()

> Replication package for "The China Shock and Internal Migration: Evidence from Bilateral Migration Flows"

---

## 📋 Table of Contents

- [Overview](#overview)
- [Citation](#citation)
- [Computational Requirements](#computational-requirements)
- [Data Availability](#data-availability)
- [Replication Instructions](#replication-instructions)
- [Code Structure](#code-structure)
- [Output Files](#output-files)
- [References](#references)
- [Acknowledgements](#acknowledgements)
- [Contact](#contact)

---

## Overview

This replication package contains all code necessary to reproduce the analysis in "The China Shock and Internal Migration: Evidence from Bilateral Migration Flows." The analysis examines how trade shocks affect internal migration patterns using bilateral migration flow data from South Korea.

**Technical Details:**
- All analysis code is written in **R** (except for `Makefile`).
- Reproducibility tools: `Make` and `LaTeX`.
- Minimum system requirement: **64GB RAM**.
- Supported platforms: Linux, macOS.

---

## Citation

```bibtex
@unpublished{choikimlee2026chinashock,
  author = {Choi, Jaerim and Kim, Hyoungchul and Lee, Seung Hoon},
  title = {The China Shock and Internal Migration: Evidence from Bilateral Migration Flows},
  year = {2026},
  note = {Working paper},
  url = {https://github.com/hchulkim/china-shock-internal-migration}
}
```

---

## Computational Requirements

### Software Dependencies

| Component | Version/Details |
|-----------|-----------------|
| R | 4.5.0 (tested) |
| Package Manager | [`rix`](https://docs.ropensci.org/rix/articles/getting-started.html) |
| Build System | GNU Make |
| Shell | Bash (Linux/Unix) |

### Hardware Requirements

The code was mainly tested on the following local system:

| Specification | Details |
|---------------|---------|
| **OS** | Ubuntu 22.04.5 LTS (Jammy Jellyfish) |
| **RAM** | 64GB |
| **Storage** | 1TB (project uses ~143GB) |
| **CPU** | Intel, 22 threads (16 cores × 2 threads) |

> Subset of the code was also tested on MacOS and other Linux distro (Arch Linux).

### Platform Support

| Platform | Status |
|----------|--------|
| Linux (Ubuntu, Fedora, Arch) | ✅ Fully supported |
| macOS | ✅ Supported |
| Windows | ⚠️ Not officially supported |

> **Note for Windows users:** You may encounter issues with file path formats and parallel processing. Consider using WSL (Windows Subsystem for Linux).

---

## Data Availability

> ⚠️ **Important:** The `data/` folder is not included in this repository due to file size. Download it from **[HERE]**.

| Data.Name  | Data.Files | Location | Provided | Availability statement |
| -- | -- | -- | -- | -- | 
| “UN Comtrade data” | `comtrade_{COUNTRY}_h{0-5}.csv` | `data/comtrade/` | TRUE | Data on international trade flows were downloaded from the United Nations Comtrade Database (UN Comtrade, 2025) using a premium API subscription. We use annual bilateral trade data at the HS 6-digit level from the "Comtrade Bulk Data" service. Data can be accessed through https://comtradeplus.un.org/ (under "Bulk Data Download") or retrieved programmatically using the Comtrade API ([https://comtradeplus.un.org/API](https://comtradeplus.un.org/API)). A copy of the data used in this study is provided as part of this archive. The data are publicly available but require a registered UN Comtrade account for bulk download access. | 
| "GDP deflator values" | [`tradstatistics` R package](https://docs.ropensci.org/tradestatistics/index.html) | No physical files | TRUE (provided as an installed package from the GitHub repository.) | For adjusting UN Comtrade values between different years, we use dataframe `ots_gdp_deflator` from a public R package named `tradestatistics` provided at [https://github.com/ropensci/tradestatistics](https://github.com/ropensci/tradestatistics). In order to preserve the R package dependency, we also compress the GitHub repository in `data/concordance/tradestatistics-master.zip`. |
| "Consumer Price Index for Korea" | `cpi_korea.xlsx` | `data/cpi/` | TRUE | Data on Consumer Price Index (CPI) for Korea was obtained from "CPI survey" by Statistics of Korea which can retrived from [https://www.index.go.kr/unify/idx-info.do?idxCd=4226](https://www.index.go.kr/unify/idx-info.do?idxCd=4226). It was used as a reference for adjusting past nominal wage to 2019 adjusted real wage. A copy of the data used in this study is provided as part of this archive. |
| “Internal migration data of South Korea” | `migration2001.csv` ~ `migration2019.csv` | `data/migration/` | TRUE | Data on internal migration within South Korea were obtained from the Microdata Integrated Service (MDIS) of Statistics Korea (KOSTAT, 2025). We use individual-level data from the Population and Housing Census: Internal Migration Survey. Data can be accessed at [https://mdis.kostat.go.kr](https://mdis.kostat.go.kr) under “마이크로데이터 > 주택/인구총조사 > 인구이동조사” (Microdata > Population/Housing Census > Internal Migration Survey). Although the data are publicly available, access requires creating an account and obtaining approval through the MDIS system. A copy of the data used in this study is included as part of this archive. |
| “Administrative districts (shapefile) of South Korea” | `bnd_sigungu_00_2019_4Q.shp`; `bnd_sigungu_00_2019_4Q.dbf`; `bnd_sigungu_00_2019_4Q.prj`; `bnd_sigungu_00_2019_4Q.shp.xml`; `bnd_sigungu_00_2019_4Q.shx` | `data/region/` | TRUE | Data on administrative districts of South Korea were obtained from [SGIS](https://sgis.kostat.go.kr/view/index) of Statistics Korea (KOSTAT, 2025). The shapefile we use are boundaries set in December 2019. While the data are publicly available from SGIS, access requires creating an account and obtaining approval. A copy of the data used in this study is included in this archive. |
| "Commuting zone of South Korea" | `cz_data.xlsx` | `data/cz/` | TRUE | Data on commuting zones of South Korea were constructed following the methodology of [Lee and Lee (2015), “Delimitation of City-Regions Based on the Method of Travel-to-Working Area and Analyzing Spatial Structure of City-Regions”, The Korea Spatial Planning Review, 84, 165–189.](https://doi.org/10.15793/kspr.2015.84..010) Using commuting flow information and the procedure described in their study, we group 226 administrative districts (si/gun/gu) into 33 commuting zones that approximate local labor markets. While the original commuting flow data are not publicly available, our crosswalk between districts and commuting zones—constructed based on the figures and descriptions in Lee and Lee (2015)—is provided as part of this archive. |
| "Korea’s Census on Establishments" |  `est1999.csv`; `est2000.csv`; `est2001.csv`; `est2010.csv`; `est2019.csv` | `data/est/` | TRUE | Data on business establishments in South Korea were obtained from the Census on Establishments via the Microdata Integrated Service (MDIS) of Statistics Korea (KOSTAT, 2025). This annual census covers all establishments with one or more employees (excluding individual agricultural, forestry, and fisheries, and certain public organizations) as of December 31 each year, providing detailed information on establishment name, location, industry classification, employees, annual sales, founding date, and more. Data can be accessed through the MDIS portal at [https://mdis.kostat.go.kr](https://mdis.kostat.go.kr). While the data are publicly available, access requires user registration and login via the MDIS system. A copy of the data used in this study is included in this archive. |
| "Crosswalk for HS codes to ISIC Rev.4 codes" | [concordance R package](https://github.com/insongkim/concordance?tab=readme-ov-file#installation-instructions) | No physical files | TRUE (provided as an installed package from the GitHub repository.) | For crosswalking HS codes in UN Comtrade to ISIC Rev.4 codes, we use public R package named **concordance** provided at [https://github.com/insongkim/concordance?tab=readme-ov-file#installation-instructions](https://github.com/insongkim/concordance?tab=readme-ov-file#installation-instructions). For a detailed crosswalk procedure, check the GitHub repository. In order to preserve the R package dependency, we also compress the GitHub repository in `data/concordance/cconcordance/concordance-master.zip`. **IMPORTANT**: Make sure you install the developer version as conversion from HS to ISIC is provided through the developer version. |
| "Crosswalk for ISIC Rev.4 codes to KSIC10 codes" | `isic4_ksic10.xlsx` | `data/concordance/isic` | TRUE | Industry classification crosswalks between ISIC to KSIC were obtained from the official Korean Standard Industrial Classification (KSIC) system, maintained by Statistics Korea. Specifically, we used the conversion tables available at the KSIC website ([https://kosis.kr/eng/bulletinBoard/qnaView.do?boardIdx=335027](https://kosis.kr/eng/bulletinBoard/qnaView.do?boardIdx=335027)). These files provide official mappings between the ISIC and the KSIC system and are publicly available without login. For computational purposes, we only modified the original file name to an English name after downloading; the content of the data remains unchanged. A copy of the conversion file used in this study is included as part of this archive. | 
| "Crosswalk between KSIC 8, 9, 10 codes" | `ksic9_10.xlsx`; `ksic8_9.xls`; `ksic_old.xls`  | `data/concordance/ksic` | TRUE | Industry classification crosswalks were obtained from the official Korean Standard Industrial Classification (KSIC) system, maintained by Statistics Korea. Specifically, we used the KSIC version conversion tables available at the KSIC website ([https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#](https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#)). These files provide official mappings between different versions of the KSIC system and are publicly available without login. For computational purposes, we only modified the original file name to an English name after downloading; the content of the data remains unchanged. A copy of the KSIC conversion file used in this study is included as part of this archive. |
| "Crosswalk between *change in administrative districts of South Korea over the years* and between *institutional differences in region codes*" | `region_stat.xlsx`; `region_kosis.xlsx` | `data/concordance/region` | TRUE | Due to (1) changes in administrative district boundaries and (2) differing numeric codes assigned by two institutions in South Korea—Statistics Korea and the Ministry of the Interior and Safety—it is necessary to establish a consistent concordance of districts in the data. We also need to crosswalk region codes provided by different institutions. In order to accomplish this, the authors have created their own crosswalk tables which is based on [https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#](https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#). `region_stat.xlsx` table crosswalks the changes in administrative district and `region_kosis.xlsx` crosswalks two different region codes provided by two institutions. In the end, an unique code was assigned to each unit of analysis to ensure consistency across the timeline. For more detail on this matter, you can check out the Appendix in the paper. We have also included files related to this crosswalk in this archive. |
| "Crosswalk between ISCO and KSCO" | `isco_ksco.xls`  | `data/concordance/ksco` | TRUE | Occupation classification crosswalk between ISCO and KSCO was obtained from Statistics Korea. Specifically, we used the ISCO-KSCO conversion tables available at [https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#](https://kssc.kostat.go.kr:8443/ksscNew_web/index.jsp#). This data is publicly available without login. A copy of the conversion file used in this study is included as part of this archive. |

---

## Replication Instructions

### Quick Start

```bash
# 1. Clone the repository
git clone https://github.com/hchulkim/china-shock-internal-migration.git
cd china-shock-internal-migration

# 2. Download and set-up data folder

# 3. Build the Nix environment
nix-build

# 4. Enter the Nix shell
nix-shell

# 5. Run the full pipeline
make
```

### Manual Execution

If you prefer to run scripts individually, execute them in numerical order from the `code/` directory:

```bash
Rscript code/01-concordance-hs-isic.R
Rscript code/02-concordance-isic-ksic-table.R
# ... continue in sequence
```

> ⚠️ **Warning:** Do not run `raw-data-download.R` unless absolutely necessary. This script re-downloads raw data and may break if source formats have changed.

---

## Code Structure

```
project/
├── Makefile              # Master build script
├── code/
│   ├── raw-data-download.R          # ⚠️ DO NOT RUN (API data download)
│   ├── 01-concordance-hs-isic.R     # HS → ISIC crosswalk
│   ├── 02-concordance-isic-ksic-table.R # ISIC → KSIC crosswalk
│   ├── 03-concordance-est-district.R    # District codes (establishments)
│   ├── 04-concordance-ksic-ksic.R       # KSIC version crosswalk
│   ├── 05-concordance-migration-district.R  # District codes (migration)
│   ├── 06-exposure-construction.R   # Trade shock exposure
│   ├── 07-controls-construction.R   # Control variables
│   ├── 08-baseline-data-build.R     # Main analysis dataset
│   ├── 09-main-analysis.R           # Baseline results
│   ├── 10-hetero-analysis.R         # Heterogeneous effects
│   ├── 11-robustness-appendix-analysis.R  # Robustness checks
│   └── 13-descriptive.R             # Descriptive statistics & maps
├── data/                 # [Download separately]
└── output/
    ├── tables/
    └── figures/
```

---

## Output Files

### Main Tables

| Table | Description | Output File | Script |
|-------|-------------|-------------|--------|
| Table 1 | Korea-China trade descriptive statistics | `tab1.tex` | `13-descriptive.R` |
| Table 2 | Main IV regression results | `tab2.tex` | `09-main-analysis.R` |
| Table 2 (First stage) | First-stage results (ADH IV) | `table2_firststage_s1a.tex` | `09-main-analysis.R` |
| Table 2 (First stage) | First-stage results (Mixed IV) | `table2_firststage_s1b.tex` | `09-main-analysis.R` |

### Heterogeneous Effects Tables

| Table | Description | Output File | Script |
|-------|-------------|-------------|--------|
| Age heterogeneity | Effects by age group | `tab_age.tex` | `10-hetero-analysis.R` |
| Single-person HH | Effects for single-person households | `tab_age_single.tex` | `10-hetero-analysis.R` |
| Multi-person HH | Effects for multi-person households | `tab_age_multi.tex` | `10-hetero-analysis.R` |

### Main Figures

| Figure | Description | Output File | Script |
|--------|-------------|-------------|--------|
| Figure 1 | District and commuting zone maps | `fig1_1.png`, `fig1_2.png` | `13-descriptive.R` |
| Figure 2 | Trade exposure maps | `fig2_1.png`, `fig2_2.png` | `13-descriptive.R` |

### Appendix Tables

| Table | Description | Output File | Script |
|-------|-------------|-------------|--------|
| Robustness (controls) | Additional control variables | `tab_appendix_control.tex` | `11-robustness-appendix-analysis.R` |
| Balance (regional) | Regional balance test | `tab_appendix_balance_region.tex` | `11-robustness-appendix-analysis.R` |
| Balance (industry) | Industry balance test | `tab_appendix_balance_industry.tex` | `11-robustness-appendix-analysis.R` |

---

## References

### Data Sources

- **Im, Z. J. & Kaihovaara, A.** (2020). Cleaned Blinder Offshorability Index for ISCO-08 and RTI Index. [DOI: 10.13140/RG.2.2.14045.95206/1](https://doi.org/10.13140/RG.2.2.14045.95206/1)

- **Korea Labor Institute.** (2025). Korean Labor and Income Panel Study (KLIPS), Waves 1–4. https://www.kli.re.kr

- **Lee, S. & Lee, H.** (2015). Delimitation of City-Regions Based on the Method of Travel-to-Working Area. *The Korea Spatial Planning Review*, 84, 165–189. [DOI: 10.15793/kspr.2015.84..010](https://doi.org/10.15793/kspr.2015.84..010)

- **Statistics Korea (KOSTAT).** (2025). Census on Establishments, Consumer Price Index, District Shapefiles, Population and Housing Census, and Classification Conversion Tables. Retrieved from [MDIS](https://mdis.kostat.go.kr), [KOSIS](https://kosis.kr), and [SGIS](https://sgis.kostat.go.kr).

- **United Nations.** (2025). UN Comtrade Database. https://comtradeplus.un.org/

### Software

- **Liao, S., Kim, I. S., Miyano, S., & Zhang, H.** (2020). *concordance*: Product Concordance. R package version 2.0.0. https://CRAN.R-project.org/package=concordance

- **Vargas, M.** (2025). *tradestatistics*: Open Trade Statistics API Wrapper. R package version 5.0.0. https://docs.ropensci.org/tradestatistics/

---

## Acknowledgements

This README template was adapted from the [Social Science Data Editors' template](https://github.com/social-science-data-editors/template_README).

---

## Contact

| Name | Role | Email |
|------|------|-------|
| **Hyoungchul Kim** | Main Maintainer | [hkim.inbox@gmail.com](mailto:hkim.inbox@gmail.com) |
| **Jaerim Choi** | Author | [jaerimchoi@yonsei.ac.kr](mailto:jaerimchoi@yonsei.ac.kr) |
| **Seung Hoon Lee** | Author | [seunghoonlee@yonsei.ac.kr](mailto:seunghoonlee@yonsei.ac.kr) |

---

<p align="center">
  <i>Last updated: 2026</i>
</p>
