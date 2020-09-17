# data_functions.R

#' Get today's value
#'
#' Get the value for today for a specified column
#'
#' @param .data A data frame
#' @param .col The column with the value
#' @param m A string with the name of the mask
#' @param .date A POSIXct date
#'
#' @return A numeric value
#' @export
get_value <- function(.data, .col, m, .date = lubridate::today()) {
    .col <- rlang::enquo(.col)
    mask <- rlang::sym("mask")
    date <- rlang::sym("date")

    .data %>%
        dplyr::filter(
            !!mask == m,
            !!date == .date
        ) %>%
        dplyr::pull(!!.col)
}

#' Calculate inventory-related values
#'
#' Calculate the expected inventory, burn rate, days remaining, and
#' days-on-hand.
#'
#' @param .data A data frame
#' @param .col Column with the number of masks distributed
#'
#' @return Returns a \code{tibble} object.
#' @export
inventory_calcs <- function(.data, .col) {
    .col <- rlang::enquo(.col)
    shipments <- rlang::sym("shipments")
    lag_ship <- rlang::sym("lag_ship")
    lag_dist <- rlang::sym("lag_dist")
    stock_change <- rlang::sym("stock_change")
    .model <- rlang::sym(".model")
    new_inventory <- rlang::sym("new_inventory")
    tmp_inv <- rlang::sym("tmp_inv")
    cum_change <- rlang::sym("cum_change")
    available <- rlang::sym("available")
    burn_rate <- rlang::sym("burn_rate")
    days_remain <- rlang::sym("days_remain")

    .data %>%
        dplyr::mutate(
            dplyr::across(c(!!shipments, !!.col), round),
            !!"lag_dist" := dplyr::lag(!!.col),
            !!"stock_change" := !!lag_ship - !!lag_dist,
            dplyr::across(!!stock_change, ~if_else(!!.model == "Actual", 0, .)),
            !!"cum_change" := cumsum(!!stock_change),
            dplyr::across(!!new_inventory, ~dplyr::coalesce(., !!tmp_inv + !!cum_change)),
            dplyr::across(!!available, ~dplyr::coalesce(., !!new_inventory)),
            !!"burn_rate" := (dplyr::lag(!!available, 7) - !!available) / 7,
            # !!"avg_burn_rate" := zoo::rollmean(!!burn_rate, 7, fill = "extend"),
            !!"days_remain" := !!available / !!burn_rate,
            # !!"days_remain" := !!available / zoo::rollmean(!!burn_rate, 7, fill = "extend"),
            # dplyr::across(!!days_remain, ~dplyr::if_else(. < 0, NA_real_, .)),
            !!"days_on_hand" := !!available / zoo::rollmean(!!.col, 7, fill = "extend")
            # !!"days_on_hand" := zoo::rollmean(!!available, 7, fill = "extend") / zoo::rollmean(!!.col, 7, fill = "extend")
        )

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
    date <- rlang::sym("date")
    data <- rlang::sym("data")
    smth <- rlang::sym("smth")

    .data %>%
        tibble::as_tibble() %>%
        dplyr::group_by(!!key) %>%
        dplyr::mutate(!!"day" := as.numeric(!!date - dplyr::first(!!date)) + 1) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            !!"smth" := purrr::map(
                !!data,
                stats::loess,
                formula = stats::as.formula(paste(.col, "~ day")),
                span = span,
                na.action = na.action
            ),
            !!"fit" := purrr::map(!!smth, `[[`, "fitted")
        ) %>%
        dplyr::select(-dplyr::starts_with("smth")) %>%
        tidyr::unnest(cols = c(!!data, dplyr::starts_with("fit")))
}
