zwa <- function(weight, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = weight, x = age, sex = sex, ref = rwa, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- (z < -6) | (z > 5)
        z[flag] <- NA
    }
    return(z)
}

zha <- function(height, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = height, x = age, sex = sex, ref = rha, adjust_large_z = FALSE)
    if (trim_extreme_z) {
        flag <- abs(z) > 6
        z[flag] <- NA
    }
    return(z)
}

zwh <- function(weight, height, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = weight, x = height, sex = sex, ref = rwh, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

zwl <- function(weight, length, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = weight, x = length, sex = sex, ref = rwl, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

zba <- function(bmi, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = bmi, x = age, sex = sex, ref = rba, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

zhca <- function(headc, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = headc, x = age, sex = sex, ref = rhca, adjust_large_z = FALSE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

zaca <- function(muac, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = muac, x = age, sex = sex, ref = raca, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

ztsa <- function(tricep, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = tricep, x = age, sex = sex, ref = rtsa, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

zssa <- function(subscap, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = subscap, x = age, sex = sex, ref = rssa, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

whozr <- function(y, x, sex, ref, adjust_large_z = FALSE) {

    # x: age in DAYS (or height for whz)
    # y: outcome

    dat <- tibble::tibble(sex = sex, x = x, y = y)

    dat <- ref %>%
        dplyr::full_join(dat, by = c("sex", "x")) %>%
        dplyr::group_by(sex) %>%
        dplyr::mutate(l = approx(x, l, x)$y,
                      m = approx(x, m, x)$y,
                      s = approx(x, s, x)$y,
                      z = ifelse(abs(l) >= 0.01,
                                 (((y / m) ^ l) - 1) / (l * s),
                                 log(y / m) / s)) %>%
        dplyr::semi_join(dat, by = c("sex", "x", "y"))

    if (adjust_large_z) {
        # WHO "macro" includes the following, but not Tim's papers. Where is this from?
        # Only for WAZ, BAZ, ACAZ, TCAZ, WHZ, SSAZ (soft). Do not use for HAZ, HCAZ (boney).
        dat <- dat %>%
            dplyr::mutate(sd3 = m * ((1 + l * s * 3 * sign(z)) ** (1 / l)),
                          sd23 = sign(z) * (sd3 - m * ((1 + l * s * 2 * sign(z)) ** (1 / l))),
                          z = ifelse(abs(z > 3),
                                     3 * sign(z) + ((y - sd3) / sd23),
                                     z))
    }

    z <- dat$z[match(y, dat$y)] # to ensure output in same order as input
    return(z)
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
