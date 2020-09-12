# data_functions.R

#' Calculate rolling mean
#'
#' @param x an object (representing a series of observations)
#' @param k integer width of the rolling window
#' @param fill a three-component vector or list (recycled otherwise) providing
#'   filling values at the left/within/to the right of the data range
#'
#' @return An object of the same class as x with the rolling mean.
#' @keywords internal
mean_7d <- function(x, k = 7, fill = "extend") {
    zoo::rollmean(x, k, fill)
}

#' Calculate the days-on-hand
#'
#' @param .data a data frame with daily values for inventory, shipments
#'   received, number reprocessed, number distributed, and number returned
#'
#' @return A data frame with days-on-hand calculation.
#' @export
calc_params <- function(.data) {
    .data %>%
        dplyr::mutate(
            days_on_hand = (mean_7d(new_inventory) + mean_7d(shipments) + mean_7d(reprocessed)) / (mean_7d(distributed) - mean_7d(returned)),
            doh_no_reprocess = (mean_7d(new_inventory) + mean_7d(shipments)) / mean_7d(distributed),
            dplyr::across(c(days_on_hand, doh_no_reprocess), dplyr::na_if, y = Inf),
            dplyr::across(c(days_on_hand, doh_no_reprocess), ~dplyr::if_else(. > 250 | . < 0, NA_real_, .))
        )
}


#' Create a subset of mask data
#'
#' @param .data data frame
#' @param masks vector of masks
#' @param ... grouping columns
#'
#' @return
#' @export
subset_masks <- function(.data, masks, ...) {
    .data %>%
        dplyr::filter(mask %in% masks) %>%
        dplyr::group_by(...) %>%
        dplyr::summarize(dplyr::across(where(is.numeric), sum, na.rm = TRUE)) %>%
        # calc_params() %>%
        dplyr::ungroup()
}

# if(getRversion() >= "2.15.1") utils::globalVariables("where")

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
