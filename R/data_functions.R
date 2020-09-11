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
        calc_params() %>%
        dplyr::ungroup()
}

#' Convert a data frame to a time series
#'
#' Converts the specified data frame column to a time series using \link[timetk]{tk_ts}.
#'
#' @param .data data frame
#' @param date date column
#' @param y column to convert to time series
#' @param .freq the number of observations per unit of time
#'
#' @return Returns a \code{ts} object.
#' @export
df_to_ts <- function(.data, date, y, .freq = 1) {
    y <- rlang::enquo(y)
    date <- rlang::enquo(date)

    .data %>%
        dplyr::select(!!date, y = !!y) %>%
        timetk::tk_ts(frequency = .freq, silent = TRUE)
}

#' Convert a time series to a data frame
#'
#' Converts a time series back into a date frame using \link[sweep]{sw_sweep}.
#'
#' @param .ts A time-series forecast of class \code{forecast}
#' @param timetk_idx If timetk index (non-regularized index) is present, uses it
#'   to develop forecast. Otherwise uses default index.
#' @param dates vector of dates to replace non-POSIXct indices
#'
#' @return Returns a \code{tibble} object.
#' @export
ts_to_df <- function(.ts, timetk_idx = TRUE, dates = NULL) {
    df <- .ts %>%
        sweep::sw_sweep(timetk_idx = timetk_idx, rename_index = "date") %>%
        dplyr::mutate(
            dplyr::across(where(is.POSIXct), lubridate::floor_date, unit = "day"),
            dplyr::across(where(is.numeric), as.integer)
        )

    if (!is.POSIXct(df$date) & !is.null(dates)) {
        df$orig_index <- df$date
        df$date <- dates
    }

    df
}

if(getRversion() >= "2.15.1") utils::globalVariables("where")

#' Get an individual model from a hybridModel object
#'
#' @param x The name of the individual model
#' @param .data A \code{hybridModel} object
#' @param .idx A vector of dates
#'
#' @return Returns a \code{tibble} object.
#' @keywords internal
get_mod <- function(x, .data, .idx) {
    tk_idx = timetk::has_timetk_idx(.data)

    df <- .data[[x]] %>%
        sweep::sw_sweep(timetk_idx = tk_idx, rename_index = "date") %>%
        dplyr::filter(key == "forecast")

    if (!is.POSIXct(df$date)) {
        df <- df %>%
            dplyr::select(-date) %>%
            tibble::add_column(date = .idx$date, .before = 1)
    }

    df
}

#' Converts a hybrid forecast to a data frame
#'
#' Converts a forecast object containing a ensemble (hybrid) forecast and all of
#' the individual forecasts, into a data frame with the ensemble forecast and
#' all individual forecasts.
#'
#' @param fcast An object of class \code{forecast}
#' @param mod An object of class \code{hybridModel}
#' @param y Name of the column that contains the time series values
#'
#' @return Returns a \code{tibble} object.
#' @export
hyb_to_df <- function(fcast, mod, y) {
    y <- rlang::enquo(y)

    df <- fcast %>%
        sweep::sw_sweep(timetk_idx = TRUE, rename_index = "date") %>%
        dplyr::mutate(
            dplyr::across(date, lubridate::floor_date, unit = "day"),
            dplyr::across(key, stringr::str_to_title)
        )

    idx <- df %>%
        dplyr::filter(key == "Forecast") %>%
        dplyr::select(date)

    mod_nm <- names(mod$weights)
    l <- purrr::map(mod_nm, get_mod, .data = fcast, .idx = idx)
    names(l) <- mod_nm

    df_mods <- dplyr::bind_rows(l, .id = "key") %>%
        dplyr::mutate(
            dplyr::across(
                key,
                stringr::str_replace_all,
                pattern = c(
                    "auto.arima" = "ARIMA",
                    "ets" = "ETS",
                    "nnetar" = "NNAR",
                    "stlm" = "STL",
                    "tbats" = "TBATS",
                    "thetam" = "Theta"
                )
            )
        ) %>%
        dplyr::arrange(date, key)

    if ("value" %in% names(df_mods)) {
        df_mods <- df_mods %>%
            dplyr::mutate(dplyr::across(!!y, ~dplyr::coalesce(., value))) %>%
            dplyr::select(-value)
    }

    df %>%
        dplyr::bind_rows(df_mods) %>%
        dplyr::arrange(date, key) %>%
        dplyr::rename(y = !!y) %>%
        dplyr::mutate(
            dplyr::across(key, factor),
            dplyr::across(key, ~fct_relevel(., c("Actual", "Forecast"))),
            day = as.numeric(difftime(date, first(date), units = "days"))
        )
}

#' Make a matrix with external regressors
#'
#' Creates a matrix with external regressors for if the date is a weekend, and a
#' normalized (using \link[timetk]{normalize_vec}) vector of values to be used
#' in a forecasting model.
#'
#' @param .data A data frame
#' @param date Name of the date column
#' @param y Name of the column with the external regressors
#' @param .min The population min value in the normalization process.
#' @param .max The population max value in the normalization process.
#'
#' @return Returns a matrix
#' @export
make_preds <- function(.data, date, y, .min = NULL, .max = NULL) {
    date <- rlang::enquo(date)
    y <- rlang::enquo(y)

    .data %>%
        dplyr::mutate(
            weekend = as.numeric(weekdays(!!date) %in% c("Saturday", "Sunday")),
            scaled = timetk::normalize_vec(!!y, min = .min, max = .max, silent = TRUE)
        ) %>%
        dplyr::select(scaled, weekend) %>%
        as.matrix()
}

#' Add smoothed values to data frame
#'
#' Applies the \link[stats]{loess} function to time series values and adds the
#' fitted values to the data frame.
#'
#' @param .data A data frame from a \code{forecast} object
#' @param y_span Parameter which controls the degree of smoothing for trend
#'   lines
#' @param pi_span Parameter which controls the degree of smoothing for
#'   prediction intervals
#' @param na Action to be taken with missing values in the response or
#'   predictors. The default is "na.exclude".
#'
#' @return Returns a \code{tibble} object.
#' @export
make_smooth <- function(.data, key, y_span = 0.75, pi_span = 0.75, na = "na.exclude") {
    key <- rlang::enquo(key)

    pi <- .data %>%
        dplyr::filter(stringr::str_to_lower(!!key) != "actual") %>%
        dplyr::group_by(!!key) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            smth_lo = purrr::map(
                data,
                stats::loess,
                formula = lo.80 ~ day,
                span = pi_span,
                na.action = na),
            fit_lo = purrr::map(smth_lo, `[[`, "fitted"),
            smth_hi = purrr::map(
                data,
                stats::loess,
                formula = hi.80 ~ day,
                span = pi_span,
                na.action = na),
            fit_hi = purrr::map(smth_hi, `[[`, "fitted")
        ) %>%
        tidyr::unnest(cols = c(data, fit_lo, fit_hi)) %>%
        dplyr::select(date, key, starts_with("fit"))

    .data %>%
        dplyr::group_by(!!key) %>%
        tidyr::nest() %>%
        dplyr::mutate(
            smth = purrr::map(
                data,
                stats::loess,
                formula = y ~ day,
                span = y_span,
                na.action = na),
            fit = purrr::map(smth, `[[`, "fitted")
        ) %>%
        dplyr::select(-smth) %>%
        tidyr::unnest(cols = c(data, fit)) %>%
        dplyr::left_join(pi, by = c("date", "key"))
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
                formula = as.formula(paste(.col, "~ day")),
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
                formula = as.formula("lo_80 ~ day"),
                span = span,
                na.action = na.action
            ),
            fit_lo = purrr::map(smth_lo, `[[`, "fitted"),
            smth_hi = purrr::map(
                data,
                stats::loess,
                formula = as.formula("hi_80 ~ day"),
                span = span,
                na.action = na.action
            ),
            fit_hi = purrr::map(smth_hi, `[[`, "fitted"),
        ) %>%
        dplyr::select(-dplyr::starts_with("smth")) %>%
        tidyr::unnest(cols = c(data, dplyr::starts_with("fit"))) %>%
        dplyr::ungroup()
}
