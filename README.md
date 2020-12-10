
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whozr

[![R build
status](https://github.com/simisc/whozr/workflows/R-CMD-check/badge.svg)](https://github.com/simisc/whozr/actions)
[![DOI](https://zenodo.org/badge/113995327.svg)](https://zenodo.org/badge/latestdoi/113995327)
[![Licence](https://img.shields.io/github/license/simisc/whozr)](https://github.com/simisc/whozr/blob/master/LICENSE)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/simisc/whozr/branch/master/graph/badge.svg?token=OTFYOOXEVC)](https://codecov.io/gh/simisc/whozr)

Convert raw anthropometric measurement (weight, height, length, BMI,
MUAC, head circumeference, tricep skinfold and subscapular skinfold) to
z-scores, using the WHO growth standards (0-5 years) and/or references
(5-19 years).

Also allows the ‘reverse’ calculation of original anthropometry measures
from z-scores (requires further testing).

## Installation

You can install whozr from github with:

``` r
# install.packages("devtools")
devtools::install_github("simisc/whozr")
```

## Example

``` r
library(dplyr) # tibble(), %>%, mutate()
library(knitr) # kable()
library(whozr) # zwa(), zla(), zwl()
```

``` r
tibble(sex = c("F", "M", "M", "F"),
       age_days = c(120, 310, 370, 460),
       weight_kg = c(5.98, 8.09, 8.40, 8.46),
       length_cm = c(60.2, 70.7, 72.6, 74.9)) %>%
    mutate(waz = zwa(weight_kg, age_days, sex),
           laz = zla(length_cm, age_days, sex),
           wlz = zwl(weight_kg, length_cm, sex)) %>%
    kable(digits = 3)
```

| sex | age\_days | weight\_kg | length\_cm |     waz |     laz |     wlz |
| :-- | --------: | ---------: | ---------: | ------: | ------: | ------: |
| F   |       120 |       5.98 |       60.2 | \-0.539 | \-0.818 |   0.101 |
| M   |       310 |       8.09 |       70.7 | \-1.186 | \-1.229 | \-0.723 |
| M   |       370 |       8.40 |       72.6 | \-1.296 | \-1.398 | \-0.846 |
| F   |       460 |       8.46 |       74.9 | \-1.065 | \-0.996 | \-0.859 |
