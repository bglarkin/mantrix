Mantid Abundance and Interaction Strength Estimates
================
Beau Larkin
Last updated: 12 May, 2025

- [Description](#description)
- [Packages](#packages)
- [Mantid Abundance Data](#mantid-abundance-data)
  - [Published Densities from
    Delaware](#published-densities-from-delaware)
- [Per Capita Interaction Strengths
  (PCIS)](#per-capita-interaction-strengths-pcis)
  - [Sources and Calculations](#sources-and-calculations)
  - [Summarized PCIS by Guild](#summarized-pcis-by-guild)

# Description

This script summarizes published data on the abundance and per capita
interaction strengths (PCIS) of mantids in order to set parameter values
in the fuzzy interaction web (FIW). We extracted mantid density
estimates from long-established populations, calculated scaled
densities, and used percentile values to inform experimental scenarios.
We also calculated PCIS values from field experiments, assigning
interaction strengths to functional guilds represented in the FIW.

# Packages

``` r
packages_needed <- c("tidyverse", "knitr", "rprojroot")
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(!packages_installed)) install.packages(packages_needed[!packages_installed])
for (pkg in packages_needed) library(pkg, character.only = TRUE)
```

``` r
root_path <- function(...) rprojroot::find_rstudio_root_file(...)
```

# Mantid Abundance Data

## Published Densities from Delaware

We used count data from Rathet and Hurd
([1983](https://doi.org/10.2307/2425265)) and Hurd et
al. ([2020](https://doi.org/10.1093/AESA/SAZ070)) to establish a
realistic range for mantid densities. Counts were averaged and scaled to
reflect individuals per 1000 m².

``` r
abun1983 <- data.frame(
  quadrat = 1:9,
  obs1 = c(21, 11, 6, 10, 6, 5, 9, 6, 7),
  obs2 = c(0, 5, 2, 2, 4, 3, 1, 5, 5)
) %>%
  mutate(
    avg = (obs1 + obs2) / 2,
    density = avg * 10,
    source = "RathetHurd1983"
  )
abun2020 <- data.frame(
  year = c(1999, 2004, 2012, 2016, 2018),
  density = c(0.09, 0.13, 0.36, 0.17, 0.04) * 1000
) %>%
  mutate(source = "HurdEtAl2020")
```

``` r
bind_rows(abun1983 %>% select(source, density),
          abun2020 %>% select(source, density)) %>%
  kable(format = "pandoc")
```

| source         | density |
|:---------------|--------:|
| RathetHurd1983 |     105 |
| RathetHurd1983 |      80 |
| RathetHurd1983 |      40 |
| RathetHurd1983 |      60 |
| RathetHurd1983 |      50 |
| RathetHurd1983 |      40 |
| RathetHurd1983 |      50 |
| RathetHurd1983 |      55 |
| RathetHurd1983 |      60 |
| HurdEtAl2020   |      90 |
| HurdEtAl2020   |     130 |
| HurdEtAl2020   |     360 |
| HurdEtAl2020   |     170 |
| HurdEtAl2020   |      40 |

``` r
abun_params <- 
  c(
    avg_1983 = mean(abun1983$density),
    avg_2020 = mean(abun2020$density),
    quan = quantile(c(abun1983$density, abun2020$density), 0.15),
    quan = quantile(c(abun1983$density, abun2020$density), 0.85)
  ) %>% as.data.frame()
colnames(abun_params) <- c("value")
kable(abun_params, format = "pandoc", caption = "Field-estimated abundance of mantids in Delaware;\naverages and quantiles")
```

|          | value |
|----------|------:|
| avg_1983 |    60 |
| avg_2020 |   158 |
| quan.15% |    40 |
| quan.85% |   132 |

Field-estimated abundance of mantids in Delaware; averages and quantiles

# Per Capita Interaction Strengths (PCIS)

## Sources and Calculations

PCIS values were derived from field experiments using control and mantid
treatment plots. From Table 2 in Fagan and Hurd
([1991](https://doi.org/10.2307/2426113)), we extracted mean insect
abundances per 1 m³ plot and applied the Paine
([1992](https://doi.org/10.1038/355073a0)) method to estimate PCIS. Four
mantids were used per treatment plot. From Fagan and Hurd (1994), we
extracted counts from treatment plots with a moderate density of mantids
(2.4 mantids per m<sup>2</sup>). PCIS from these studies, where predator
densities were augmented, were halved to reduce possible upward bias in
this predator augmentation study.

Moran and Hurd ([1997](https://doi.org/10.1007/s004420050360)) used
field-relevant densities of mantids, and PCIS estimates were visually
interpreted from figures.

``` r
pcis_fh91 <- read_csv(root_path("data", "pcis", "pcis_fh91.csv"), show_col_types = FALSE) %>%
  mutate(pcis = ((treatment - control) / (control * 4)) * 0.5)

pcis_fh94 <- read_csv(root_path("data", "pcis", "pcis_fh94.csv"), show_col_types = FALSE) %>%
  mutate(pcis = ((treatment - control) / (control * 9.5)) * 0.5)

pcis_mh97 <- read_csv(root_path("data", "pcis", "pcis_mh97.csv"), show_col_types = FALSE)
```

``` r
pcis <- bind_rows(pcis_fh91, pcis_fh94, pcis_mh97)
```

## Summarized PCIS by Guild

``` r
pcis %>%
  group_by(guild, source) %>%
  summarize(pcis_sum = sum(pcis), .groups = "drop_last") %>%
  summarize(pcis_avg = round(mean(pcis_sum), 2), .groups = "drop") %>% 
  kable(format = "pandoc")
```

| guild      | pcis_avg |
|:-----------|---------:|
| herbivore  |    -0.10 |
| pollinator |    -0.08 |
| predator   |    -0.08 |
