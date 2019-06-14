# WHO 2007 (ages 5-19 years) standards
# igrowup (ages 0-5 years) reference

library(tidyverse)

rawdir <- "./data-raw/"
files <- list.files(path = rawdir, pattern = "*.txt")

data = lapply(files, function(f) {
    d <- read_tsv(file.path(rawdir, f)) %>%
        mutate(sex = recode(sex, `1` = "M", `2` = "F"))
    names(d)[2] <- "x"
    if (ncol(d) == 6) {
        names(d)[6] <- "pos"
    }
    d
})

names(data) = str_replace(str_to_lower(files), ".txt", "")

# Can just ignore position: assume L up to 730 days, H after 731 days
lapply(data, function(d) {
    if (ncol(d) == 6) {
        return(tapply(d$x, d$pos, summary))
    }
})

names(data)
dpm <- 365.2425 / 12

# Height
rha <- bind_rows(data$lenanthro %>%
                     dplyr::select(-pos),
                 data$hfawho2007 %>%
                     mutate(x = x * dpm)) %>%
    arrange(sex, x)


# Weight
rwa <- bind_rows(data$weianthro,
                 data$wfawho2007 %>%
                     mutate(x = x * dpm)) %>%
    arrange(sex, x)

# BMI
rba <- bind_rows(data$bmianthro %>%
                     dplyr::select(-pos),
                 data$bfawho2007 %>%
                     mutate(x = x * dpm)) %>%
    arrange(sex, x)

raca <- data$acanthro # MUAC for age
rhca <- data$hcanthro # Head circ for age
rssa <- data$ssanthro # Subscapular skinfold for age
rtsa <- data$tsanthro # Triceps skinfold for age
rwh <- data$wfhanthro %>% # Weight for HEIGHT
    dplyr::select(-pos)
rwl <- data$wflanthro %>% # Weight for LENGTH
    dplyr::select(-pos)

usethis::use_data(raca,
                  rba,
                  rha,
                  rhca,
                  rssa,
                  rtsa,
                  rwa,
                  rwh,
                  rwl,
                  internal = TRUE)
