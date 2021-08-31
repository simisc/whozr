#' Pipe
#'
#' Imported from \code{\link[magrittr]{pipe}}
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

#' Weight-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years) and WHO
#'   growth references (5-10 years).
#' @export
#' @param weight Weight (kg)
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZWA > 5 or ZWA < -6), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of weight-for-age z-scores
zwa <- function(weight, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = weight, x = age, sex = sex, ref = rwa, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- (z < -6) | (z > 5)
        z[flag] <- NA
    }
    return(z)
}

#' Height-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years) and WHO
#'   growth references (5-19 years). Like its alias \code{\link{zla}}:
#'   both assume that length was measured recumbent up to 2 years (day 730) and
#'   height was measured standing from day 731 onward.
#' @export
#' @param height Height (cm), measured recumbent up to 2 years (day 730) and
#'   standing thereafter, as recommended by WHO.
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZHA > 6 or ZHA < -6), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of height-for-age z-scores
zha <- function(height, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = height, x = age, sex = sex, ref = rha, adjust_large_z = FALSE)
    if (trim_extreme_z) {
        flag <- abs(z) > 6
        z[flag] <- NA
    }
    return(z)
}

#' Length-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years) and WHO
#'   growth references (5-19 years). This is just an alias for \code{\link{zha}}:
#'   both assume that length was measured recumbent up to 2 years (day 730) and
#'   height was measured standing from day 731 onward.
#' @export
#' @param length Length (cm), measured recumbent up to 2 years (day 730) and
#'   standing thereafter, as recommended by WHO.
#' @param ... Other arguments to \code{\link{zha}}.
#' @return A vector of height-for-age z-scores
zla <- function(length, ...) {
    zha(height = length, ...)
}

#' Weight-for-height z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years).
#' @export
#' @param weight Weight (kg)
#' @param height Height (cm), measured standing
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZWH > 5 or ZWH < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of weight-for-height z-scores
zwh <- function(weight, height, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = weight, x = height, sex = sex, ref = rwh, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' Weight-for-length z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years).
#' @export
#' @param weight Weight (kg)
#' @param length Length (cm), measured recumbent
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZWL > 5 or ZWL < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of weight-for-length z-scores
zwl <- function(weight, length, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = weight, x = length, sex = sex, ref = rwl, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' BMI-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years) and WHO
#'   growth references (5-19 years).
#' @export
#' @param bmi BMI
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZBA > 5 or ZBA < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of BMI-for-age z-scores
zba <- function(bmi, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = bmi, x = age, sex = sex, ref = rba, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' Head circumference-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years).
#' @export
#' @param headc Head circumference (cm)
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZHCA > 5 or ZHCA < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of head circumference-for-length z-scores
zhca <- function(headc, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = headc, x = age, sex = sex, ref = rhca, adjust_large_z = FALSE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' MUAC-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years).
#' @export
#' @param muac Mid-upper arm cicrumference (cm)
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZMA > 5 or ZMA < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of MUAC-for-length z-scores
zma <- function(muac, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = muac, x = age, sex = sex, ref = raca, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' Tricep skinfold thickness-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years).
#' @export
#' @param tricep Tricep skinfold thickness (mm)
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZTSA > 5 or ZTSA < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of tricep skinfold thickness-for-length z-scores
ztsa <- function(tricep, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = tricep, x = age, sex = sex, ref = rtsa, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' Subscapular skinfold thickness-for-age z-scores
#'
#' Calculate z-scores using with the WHO growth standards (0-5 years).
#' @export
#' @param subscap Subscapular skinfold thickness (mm)
#' @param age Age (days)
#' @param sex Sex, coded as "F" and "M"
#' @param trim_extreme_z Replace extreme scores (ZSSA > 5 or ZSSA < -5), described
#'   as biologically implausible by WHO, with \code{NA}.
#' @return A vector of subscapular skinfold thickness-for-length z-scores
zssa <- function(subscap, age, sex, trim_extreme_z = FALSE) {
    z <- whozr(y = subscap, x = age, sex = sex, ref = rssa, adjust_large_z = TRUE)
    if (trim_extreme_z) {
        flag <- abs(z) > 5
        z[flag] <- NA
    }
    return(z)
}

#' Calculate z-scores from LMS tables
#'
#' Calculate z-scores from LMS tables. Wrappers are provided for use with the WHO
#'   growth standards (0-5 years) and WHO growth references (5-19 years): see
#'   \code{\link{zwa}}, \code{\link{zha}}, \code{\link{zwh}}, \code{\link{zwl}},
#'   \code{\link{zba}}, \code{\link{zhca}}, \code{\link{zma}}, \code{\link{ztsa}},
#'   \code{\link{zssa}}.
#'
#' Cole, TJ (1990) The LMS method for constructing normalized growth standards.
#'   \emph{European Journal of Clinical Nutrition} 44(1), 45-60.
#'
#' WHO Multicentre Growth Reference Study Group (2006) WHO Child Growth Standards
#'   based on length/height, weight and age. \emph{Acta Paediatrica} 95(S450), 76-85.
#'
#' de Onis et al. (2007) Development of a WHO growth reference for school-aged
#'   children and adolescents. \emph{Bulletin of the World Health Organization}
#'   85(9), 660-667.
#' @export
#' @param y Outcome veariable
#' @param x Predictor, typically age
#' @param sex Sex, coded as "F" and "M"
#' @param ref Reference data. A tibble or data.frame with columns named \code{sex},
#'   \code{x},\code{l}, \code{m} and \code{s}
#' @param adjust_large_z Shrink large z-scores towards +/-3? This is done in the
#'   WHO's "R macro" but not mentioned in any of the references above. Should only
#'   be used for soft-tissue measures (WAZ, BAZ, ACAZ, TCAZ, WHZ, SSAZ), not for
#'   hard-tissue measures (HAZ, HCAZ).
#' @return A vector \code{z} of \code{y}-for-\code{x} z-scores
whozr <- function(y, x, sex, ref, adjust_large_z = FALSE) {

    indat <- tibble::tibble(sex = as.character(sex), x = x, y = y) %>%
        dplyr::mutate(index = dplyr::row_number())

    dat <- ref %>%
        dplyr::full_join(indat, by = c("sex", "x")) %>%
        dplyr::group_by_(~ sex) %>%
        dplyr::mutate_(l = ~ stats::approx(x, l, x)$y,
                       m = ~ stats::approx(x, m, x)$y,
                       s = ~ stats::approx(x, s, x)$y,
                       z = ~ ifelse(abs(l) >= 0.01,
                                    (((y / m) ^ l) - 1) / (l * s),
                                    log(y / m) / s)) %>%
        dplyr::semi_join(indat, by = c("sex", "x", "y", "index")) %>%
        dplyr::arrange_(~ index)

    if (adjust_large_z) {
        dat <- dat %>%
            dplyr::mutate_(sd3 = ~ m * ((1 + l * s * 3 * sign(z)) ^ (1 / l)),
                           sd23 = ~ sign(z) * (sd3 - m * ((1 + l * s * 2 * sign(z)) ^ (1 / l))),
                           z = ~ ifelse(abs(z) > 3,
                                        3 * sign(z) + ((y - sd3) / sd23),
                                        z))
    }

    dat$z
}

#' Reverse z-score calculation
#'
#' Calculate original anthropometry from z-scores and LMS tables.
#'
#' @export
#' @param z z-score
#' @param x Predictor, typically age
#' @param sex Sex, coded as "F" and "M"
#' @param ref Reference data. A tibble or data.frame with columns named \code{sex},
#'   \code{x},\code{l}, \code{m} and \code{s}
#' @param adjust_large_z Were large z-scores shrunk towards +/-3? This is done in the
#'   WHO's "R macro" but not mentioned in any of the references cited at
#'   \code{\link{whozr}}.Should only be used for soft-tissue measures (WAZ, BAZ,
#'   ACAZ, TCAZ, WHZ, SSAZ), not for hard-tissue measures (HAZ, HCAZ).
#' @return A vector \code{y} of anthropometry measures corresponding to \code{y}-for-\code{x} scores \code{z}.
reverse_whozr <- function(z, x, sex, ref, adjust_large_z = FALSE) {

    indat <- tibble::tibble(sex = as.character(sex), x = x, z = z) %>%
        dplyr::mutate(index = dplyr::row_number())

    dat <- ref %>%
        dplyr::full_join(indat, by = c("sex", "x")) %>%
        dplyr::group_by_(~ sex) %>%
        dplyr::mutate_(l = ~ stats::approx(x, l, x)$y,
                       m = ~ stats::approx(x, m, x)$y,
                       s = ~ stats::approx(x, s, x)$y,
                       y = ~ ifelse(abs(l) >= 0.01,
                                    m * (z * l * s + 1) ^ (1 / l),
                                    m * exp(z * s))) %>%
        dplyr::semi_join(indat, by = c("sex", "x", "z", "index")) %>%
        dplyr::arrange_(~ index)

    if (adjust_large_z) {
        dat <- dat %>%
            dplyr::mutate_(sd3 = ~ m * ((1 + l * s * 3 * sign(z)) ^ (1 / l)),
                           sd23 = ~ sign(z) * (sd3 - m * ((1 + l * s * 2 * sign(z)) ^ (1 / l))),
                           y = ~ ifelse(abs(z) > 3,
                                        sd23 * (z - 3 * sign(z)) + sd3,
                                        y))
    }

    dat$y
}

#' Format summaries for tables
#'
#' Cole, TJ (2015) Too many digits: presentation of numerical data.
#'   \emph{Archives of Disease in Childhood} 100(7), 608-609.
#'
#' @export
#' @param x Numeric vector to be summarised
#' @param figs Number of digits or significant figure to print
#' @param sigfig If \code{TRUE}, \code{figs} is interpreted as significant figures;
#'   if \code{FALSE}, \code{figs} is interpreted as number of digits. If significant
#'   figures are used, these are applied to the mean or median. Standard deviation or
#'   first/third quartiles are printed with the same number of digits as the mean
#'   or median.
#' @param use_quantiles If \code{TRUE}, output is median (IQR). If \code{FALSE},
#'   output is mean (SD).
#' @return A string, formatted as median (IQR) or mean (SD) with the chosen
#'   number of digits / significant figures, suitable for display in tables.
format_ms <- function(x,
                      figs = 3,
                      sigfig = TRUE,
                      use_quantiles = FALSE) {

    # Format as mean (SD) by default, or as median (Q1, Q3).
    # Based on format_ms and format_iq from SMART paper.
    # Sig fig for SD, Q1, Q3 chosen as per Tim's sig fig paper.

    if (use_quantiles) {
        a <- stats::median(x, na.rm = TRUE)
        s <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    } else {
        a <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
    }

    if (sigfig) {
        r <- figs - ceiling(log10(abs(a)))
        a <- round(a, r)
        s <- round(s, r)

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

    out
}
