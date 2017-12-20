whozr <- function(d_age,
                  d_sex,
                  d_observed,
                  ref,
                  d_sex_code = c("M", "F"),
                  ref_sex_code = c(1, 2),
                  extremes = FALSE,
                  reverse = FALSE) {

    print(
        sprintf(
            "IV range in data is %.2f to %.2f: have you used the correct units? Days or months, cm or m?",
            min(d_age),
            max(d_age)
        )
    )

    ## Lots to improve here.
    ## Unquoted variable names (arguments x, y) with data= argument
    ## Data (references) to be kept in package
    ## Defaults for 'extremes' should depend on which scores is being converted
    ## Based on version from SMART paper - check older versions for useful features too.
    ## Add documentation when have basic working version.

    # d_observed: if converting to z, this is the raw measurement
    #             if converting from z, this is the z-score

    # Version 03 April 2017 (based on ENN version)
    # WHO references coded 1 for males, 2 for females.
    # Includes correction for small L (unlike WHO version, but following Tim's).

    # correct_extreme_values
    # FALSE for haz, laz, hc (boney)
    # TRUE for waz, baz, whz, wlz, muac, tricep (soft)

    # HAZ: correct_extreme_values = FALSE
    # WAZ: correct_extreme_values = TRUE
    # BAZ: correct_extreme_values = TRUE
    # Infants:
        # LAZ: correct_extreme_values = FALSE
        # HCAZ: correct_extreme_values = FALSE
        # WAZ: correct_extreme_values = TRUE
        # ACAZ: correct_extreme_values = TRUE
        # TCAZ: correct_extreme_values = TRUE
        # WLZ: correct_extreme_values = TRUE
        # BAZ: correct_extreme_values = TRUE

    # Age in DAYS if working with 0-5 reference.
    # Age in MONTHS if working with 5-19 reference.
    # WHO references coded 1 for males, 2 for females.

    n <- length(d_age)

    # VERY UGLY FIDDLE:
    if ("length" %in% names(ref) | "height" %in% names(ref)) {
        col <- which(names(ref) %in% c("length", "height"))
        names(ref)[col] <-
            "age" # not really, but for ease of later calculations...
        # In future version, take ref indep var colnames as input!
    }

    # Rewrite the following conditions using any() and all():
    ## With short-circuit || or && instead of if statements!

    if (sum(as.numeric(c("age", "sex", "l", "m", "s") %in% names(ref))) != 5) {
        stop("Reference data does not contain required columns. Check names.")
    }
    if (n != length(d_sex) | n != length(d_observed)) {
        stop("Vectors d_age, d_sex and d_observed do not have equal length.")
    }
    if (sum(as.numeric(d_sex %in% d_sex_code)) != n) {
        stop("Incorrect levels: d_sex_code.")
    }
    if (sum(as.numeric(ref$sex %in% ref_sex_code)) != dim(ref)[1]) {
        stop("Incorrect levels: ref_sex_code.")
    }

    ## Find cleaner way to do this using dplyr...
    ## NB refer to functions with dplyr::fun() - DO NOT USE library() or require() in packages!

    l <- as.numeric(rep(NA, n))
    m <- as.numeric(rep(NA, n))
    s <- as.numeric(rep(NA, n))

    ## No longer include this option
    # old = which(d_age > max(ref$age))
    # if (include_adults) {
    #     d_age[old] = max(ref$age)
    # }

    ref_boys <- which(ref$sex == ref_sex_code[1])
    ref_grls <- which(ref$sex == ref_sex_code[2])

    boys <- which(d_sex == d_sex_code[1])
    l[boys] <- approx(
        x = ref$age[ref_boys],
        y = ref$l[ref_boys],
        xout = d_age[boys],
        rule = 1
    )$y
    m[boys] <- approx(
        x = ref$age[ref_boys],
        y = ref$m[ref_boys],
        xout = d_age[boys],
        rule = 1
    )$y
    s[boys] <- approx(
        x = ref$age[ref_boys],
        y = ref$s[ref_boys],
        xout = d_age[boys],
        rule = 1
    )$y

    grls <- which(d_sex == d_sex_code[2])
    l[grls] <- approx(
        x = ref$age[ref_grls],
        y = ref$l[ref_grls],
        xout = d_age[grls],
        rule = 1
    )$y
    m[grls] <- approx(
        x = ref$age[ref_grls],
        y = ref$m[ref_grls],
        xout = d_age[grls],
        rule = 1
    )$y
    s[grls] <- approx(
        x = ref$age[ref_grls],
        y = ref$s[ref_grls],
        xout = d_age[grls],
        rule = 1
    )$y

    small <- which(abs(l) < 0.01)

    if (!reverse) {
        print("Converting from raw to z.")
        z <- (((d_observed / m) ** l) - 1) / (l * s)

        z[small] <- log(d_observed[small] / m[small]) / s[small]

        # WHO code includes the following, but not Tim's papers. Where is this from?
        # This is not included in the "reverse" version... Drop it completely??
        if (extremes) {
            sd3 <- m * ((1 + l * s * 3 * sign(z)) ** (1 / l))
            sd23 <-
                sign(z) * (sd3 - m * ((1 + l * s * 2 * sign(z)) ** (1 / l)))
            rep <- which(abs(z) > 3)
            z[rep] <-
                3 * sign(z)[rep] + ((d_observed[rep] - sd3[rep]) / sd23[rep])
        }

        result <- z
    } else {
        print("Converting from z to raw.")

        y <- m * (d_observed * l * s + 1) ^ (1 / l)
        y[small] <- m[small] * exp(d_observed[small] * s[small])
        result <- y
    }
    return(result)
}

rounde <- function(x, digits = 0) {
    # Rounds like people would, instead of like R does.

    expo <- 10 ^ digits
    return(ifelse(
        abs(x * expo) - floor(abs(x * expo)) < 0.5,
        sign(x * expo) * floor(abs(x * expo)),
        sign(x * expo) * (floor(abs(x * expo)) + 1)
    ) / expo)
}

format_ms <- function(var,
                      figs = 3,
                      sigfig = TRUE,
                      median = FALSE) {

        # Format as mean (SD) by default, or as median (Q1, Q3).
        # Based on format_ms and format_iq from SMART paper.
        # Requires testing.

        if (median) {
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

        if (median) {
            format <- sprintf("%s (%s, %s)", format, format, format)
            out <- sprintf(format, a, s[1], s[2])
        } else {
            format <- sprintf("%s (%s)", format, format)
            out <- sprintf(format, a, s)
        }
        return(out)
    }

compare <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}
