zha <- function(height, age, sex, trim_extreme_z = FALSE) {

    rha <- NA # How to access reference data from sysdat.rda?

    z <- whozr(y = height, x = age, sex = sex, ref = rha, adjust_large_z = FALSE)

    if (trim) {
        flag <- abs(z) - 0.5 > 5 # CHECK CUTOFFS
        z[flag] <- NA
    }

    return(z)
}

# How make ?whz and ?wlz point to same documentation

## Make separate script in data-raw directory to show how constructed own ref data
# WHO references coded 1 for males, 2 for females, mine "M", "F"
# Be careful with L or H choices. Are non-default versions included in main reference?

whozr <- function(y, x, sex, ref, adjust_large_z = FALSE) {

    # x: age in DAYS (or height for whz)
    # dpm <- 365.2425 * 12
    # y: outcome

    ## Test this before writing wrappers for each outcome
    ## Name wrapper functions zha, zwh, zaca, zba, etc.
    ## Name reference data rha, rwh, raca, rba, etc.

    dat <- ref %>%
        outer_join(
            tibble(sex = sex, x = x, y = y, out = 1)
        ) %>%
        group_by(sex) %>%
        mutate(l = approx(x, l, x)$y,
               m = approx(x, m, x)$y,
               s = approx(x, s, x)$y) %>%
        ungroup() %>%
        filter(!is.na(out)) %>%
        mutate(z = ifelse(abs(l) >= 0.01,
                          (((y / m) ^ l) - 1) / (l * s),
                          log(y / m) / s))

    if (adjust_large_z) {
        # WHO "macro" includes the following, but not Tim's papers. Where is this from?
        # Only for WAZ, BAZ, ACAZ, TCAZ, WHZ (soft). Do not use for HAZ, HCAZ (boney).
        dat <- dat %>%
            mutate(sd3 = m * ((1 + l * s * 3 * sign(z)) ** (1 / l)),
                   sd23 = sign(z) * (sd3 - m * ((1 + l * s * 2 * sign(z)) ** (1 / l))),
                   z = ifelse(abs(z > 3),
                              3 * sign(z) + ((y - sd3) / sd23),
                              z))
    }
    return(dat$z)
}

## Converting from z to raw
# y <- m * (z * l * s + 1) ^ (1 / l)
# y[small] <- m[small] * exp(z[small] * s[small])

rounde <- function(x, digits = 0) {
    # Rounds like people would, instead of like R does.

    expo <- 10 ^ digits
    res <- ifelse(
        abs(x * expo) - floor(abs(x * expo)) < 0.5,
        sign(x * expo) * floor(abs(x * expo)),
        sign(x * expo) * (floor(abs(x * expo)) + 1)
    ) / expo
    return(res)
}

format_ms <- function(var,
                      figs = 3,
                      sigfig = TRUE,
                      use_quantiles = FALSE) {

    # Format as mean (SD) by default, or as median (Q1, Q3).
    # Based on format_ms and format_iq from SMART paper.
    # Sig fig for SD, Q1, Q3 chosen as per Tim's sig fig paper.

    if (use_quantiles) {
        a <- median(var, na.rm = TRUE)
        s <- quantile(var, probs = c(0.25, 0.75), na.rm = TRUE)
    } else {
        a <- mean(var, na.rm = TRUE)
        s <- sd(var, na.rm = TRUE)
    }

    if (sigfig) {
        r <- figs - ceiling(log10(abs(a)))
        a <- rounde(a, r)
        s <- rounde(s, r)

        format <- ifelse(r < 1, "%.0f",
                         paste0("%.", as.character(r), "f"))
    } else {
        format <- paste0("%.", as.character(figs), "f")
    }

    if (use_quantiles) {
        format <- sprintf("%s (%s, %s)", format, format, format)
        out <- sprintf(format, a, s[1], s[2])
    } else {
        format <- sprintf("%s (%s)", format, format)
        out <- sprintf(format, a, s)
    }
    return(out)
}
