# WHO 2007 (ages 5-19 years)

bfa <- readr::read_tsv("./data-raw/bfawho2007.txt")
hfa <- readr::read_tsv("./data-raw/hfawho2007.txt")
wfa <- readr::read_tsv("./data-raw/wfawho2007.txt")

# igrowup (ages 0-5 years)

ac <- readr::read_tsv("./data-raw/acanthro.txt")
bmi <- readr::read_tsv("./data-raw/bmianthro.txt") # loh
hc <- readr::read_tsv("./data-raw/hcanthro.txt")
len <- readr::read_tsv("./data-raw/lenanthro.txt") # loh
ss <- readr::read_tsv("./data-raw/ssanthro.txt")
ts <- readr::read_tsv("./data-raw/tsanthro.txt")
wei <- readr::read_tsv("./data-raw/weianthro.txt")
wfh <- readr::read_tsv("./data-raw/wfhanthro.txt") # lorh
wfl <- readr::read_tsv("./data-raw/wflanthro.txt") # lorh

# available to package functions but not to package users

devtools::use_data(
    bfa,
    hfa,
    wfa,
    ac,
    bmi,
    hc,
    len,
    ss,
    ts,
    wei,
    wfh,
    wfl,
    internal = TRUE
)
