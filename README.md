
<!-- README.md is generated from README.Rmd. Please edit that file -->
whozr
=====

Convert raw anthropometric measurement (weight, height, length, BMI, MUAC, head circumeference, tricep skinfold and subscapular skinfold) to z-scores, using the WHO growth standards (0-5 years) and/or references (5-19 years).

Installation
------------

You can install whozr from github with:

``` r
# install.packages("devtools")
devtools::install_github("simisc/whozr")
```

Example
-------

``` r
library(dplyr) # tibble(), %>%, mutate()
library(knitr) # kable()
library(whozr) # zwa(), zla(), zwl()
```

``` r
anthro <- tibble(
    sex = c("F", "M", "M", "F"),
    age_days = c(120, 310, 370, 460),
    weight = c(5.98, 8.09, 8.40, 8.46),
    length = c(60.2, 70.7, 72.6, 74.9)
    ) %>%
    mutate(
        waz = zwa(weight, age_days, sex),
        laz = zla(length, age_days, sex),
        wlz = zwl(weight, length, sex)
        )
kable(anthro, digits = 3)
```

| sex |  age\_days|  weight|  length|     waz|     laz|     wlz|
|:----|----------:|-------:|-------:|-------:|-------:|-------:|
| F   |        120|    5.98|    60.2|  -0.539|  -0.818|   0.101|
| M   |        310|    8.09|    70.7|  -1.186|  -1.229|  -0.723|
| M   |        370|    8.40|    72.6|  -1.296|  -1.398|  -0.846|
| F   |        460|    8.46|    74.9|  -1.065|  -0.996|  -0.859|
