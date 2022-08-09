# WHO 2007 (ages 5-19 years) standards
# igrowup (ages 0-5 years) reference

library(dplyr)
library(readr)
library(stringr)
library(purrr)

days_per_month <- 365.2425 / 12

rawdir <- "./data-raw/"
fnames <- list.files(path = rawdir, pattern = "*.txt")
names(fnames) <- str_remove(fnames, "\\.txt")

process_raw <- function(d) {
    d |>
        rename(x = 2) |>
        select(sex, x, l, m, s) |>
        mutate(sex = recode(sex, `1` = "M", `2` = "F")) |>
        group_by(sex)
}

dat <- fnames |>
    map(~ read_tsv(file.path(rawdir, .x), show_col_types = FALSE)) |>
    map(process_raw) |>
    map_at(
        .at = c("hfawho2007", "wfawho2007", "bfawho2007"),
        .f = ~ mutate(.x, x = x * days_per_month)
    )

rha <- bind_rows(dat$lenanthro, dat$hfawho2007) # Height for age
rwa <- bind_rows(dat$weianthro, dat$wfawho2007) # Weight for age
rba <- bind_rows(dat$bmianthro, dat$bfawho2007) # BMI age
raca <- dat$acanthro # MUAC for age
rhca <- dat$hcanthro # Head circ for age
rssa <- dat$ssanthro # Subscapular skinfold for age
rtsa <- dat$tsanthro # Triceps skinfold for age
rwh <- dat$wfhanthro # Weight for HEIGHT
rwl <- dat$wflanthro # Weight for LENGTH

usethis::use_data(raca,
    rba,
    rha,
    rhca,
    rssa,
    rtsa,
    rwa,
    rwh,
    rwl,
    internal = TRUE,
    overwrite = TRUE
)
