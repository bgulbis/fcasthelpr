# data_functions.R

#' Get today's value
#'
#' Get the value for today for a specified column
#'
#' @param .data A data frame
#' @param .col The column with the value
#' @param m A string with the name of the mask
#' @param model A string with the name of the forecast model to use, defaults
#'   to "Forecast" which is the ensemble model
#' @param .date A POSIXct date
#'
#' @return A numeric value
#' @export
get_value <- function(.data, .col, m, model = "Forecast", .date = lubridate::today()) {
    .col <- rlang::enquo(.col)
    mask <- rlang::sym("mask")
    date <- rlang::sym("date")
    .model <- rlang::sym(".model")

    .data %>%
        dplyr::filter(
            !!mask == m,
            !!.model == model,
            !!date == .date
        ) %>%
        dplyr::pull(!!.col)
}

#' Calculate inventory-related values
#'
#' Calculate the expected inventory, burn rate, days remaining, and
#' days-on-hand.
#'
#' @param .data A data frame with historical values
#' @param .xreg A data frame with forecast values
#' @param .col Column with the number of masks distributed
#'
#' @return Returns a \code{tibble} object.
#' @export
inventory_calcs <- function(.data, .xreg, .col) {
    .col <- rlang::enquo(.col)
    date <- rlang::sym("date")
    shipments <- rlang::sym("shipments")
    stock_change <- rlang::sym("stock_change")
    data_type <- rlang::sym("data_type")
    new_inventory <- rlang::sym("new_inventory")
    reprocessed <- rlang::sym("reprocessed")
    cum_change <- rlang::sym("cum_change")
    available <- rlang::sym("available")
    burn_rate <- rlang::sym("burn_rate")

    .data %>%
        dplyr::bind_rows(.xreg) %>%
        dplyr::arrange(!!date) %>%
        tidyr::fill(!!new_inventory, .direction = "down") %>%
        dplyr::mutate(
            dplyr::across(reprocessed, ~dplyr::if_else(date >= lubridate::mdy("9/5/2020"), 0, .)),
            dplyr::across(c(!!shipments, !!.col), dplyr::lag),
            !!"stock_change" := !!shipments - !!.col,
            dplyr::across(!!stock_change, ~if_else(!!data_type == "Actual", 0, .)),
            !!"cum_change" := cumsum(!!stock_change),
            !!"new_inventory" := !!new_inventory + !!cum_change,
            !!"available" := !!new_inventory + !!reprocessed,
            !!"burn_rate" := (dplyr::lag(!!available, 7) - !!available) / 7,
            # !!"days_remain" := !!available / !!burn_rate,
            !!"days_on_hand" := !!available / zoo::rollapplyr(!!.col, 7, mean, partial = TRUE)
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
