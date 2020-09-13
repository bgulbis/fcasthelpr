# data_functions.R

#' Get today's value
#'
#' Get the value for today for a specified column
#'
#' @param .data A data frame
#' @param .col The column with the value
#' @param m A string with the name of the mask
#'
#' @return A numeric value
#'
#' @export
get_value <- function(.data, .col, m) {
    .col <- rlang::enquo(.col)
    mask <- rlang::sym("mask")
    date <- rlang::sym("date")

    .data %>%
        dplyr::filter(
            !!mask == m,
            !!date == lubridate::today()
        ) %>%
        dplyr::pull(!!.col)
}

#' Smooth the mean values
#'
#' Applies the \link[stats]{loess} function to time series values and adds the
#' fitted values to the data frame.
#'
#' @param .data A data frame from a \code{forecast} object
#' @param key Column to group by
#' @param .col String with the name of the column to smooth, default is ".mean"
#' @param span Parameter which controls the degree of smoothing
#' @param na.action Action to be taken with missing values in the response or
#'   predictors. The default is "na.exclude".
#'
#' @return Returns a \code{tibble} object.
#' @export
smooth_mean <- function(.data, key, .col = ".mean", span = 0.5, na.action = "na.exclude") {
    key <- rlang::enquo(key)

    .data %>%
        tibble::as_tibble() %>%
        dplyr::group_by(!!key) %>%
        dplyr::mutate(day = as.numeric(date - first(date)) + 1) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            smth = purrr::map(
                data,
                stats::loess,
                formula = stats::as.formula(paste(.col, "~ day")),
                span = span,
                na.action = na.action
            ),
            fit = purrr::map(smth, `[[`, "fitted")
        ) %>%
        dplyr::select(-dplyr::starts_with("smth")) %>%
        tidyr::unnest(cols = c(data, dplyr::starts_with("fit")))
}

#' Smooth the prediction interval values
#'
#' Applies the \link[stats]{loess} function to time series values and adds the
#' fitted values to the data frame.
#'
#' @param .data A data frame from a \code{forecast} object
#' @param key Column to group by
#' @param span Parameter which controls the degree of smoothing
#' @param na.action Action to be taken with missing values in the response or
#'   predictors. The default is "na.exclude".
#'
#' @return Returns a \code{tibble} object.
#' @export
smooth_pi <- function(.data, key, span = 0.75, na.action = "na.exclude") {
    key <- rlang::enquo(key)

    .data %>%
        tibble::as_tibble() %>%
        dplyr::group_by(!!key) %>%
        dplyr::mutate(day = as.numeric(date - first(date)) + 1) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            smth_lo = purrr::map(
                data,
                stats::loess,
                formula = stats::as.formula("lo_80 ~ day"),
                span = span,
                na.action = na.action
            ),
            fit_lo = purrr::map(smth_lo, `[[`, "fitted"),
            smth_hi = purrr::map(
                data,
                stats::loess,
                formula = stats::as.formula("hi_80 ~ day"),
                span = span,
                na.action = na.action
            ),
            fit_hi = purrr::map(smth_hi, `[[`, "fitted"),
        ) %>%
        dplyr::select(-dplyr::starts_with("smth")) %>%
        tidyr::unnest(cols = c(data, dplyr::starts_with("fit"))) %>%
        dplyr::ungroup()
}
