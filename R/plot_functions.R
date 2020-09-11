# plot_functions.R

#' Plot a forecast
#'
#' @param .data A data frame from a \code{forecast} object
#' @param .y A string with the column name containing the time series values.
#' @param actuals A data frame containing the actual (test) values.
#'
#' @return A \code{ggplot2} object
#' @export
ggmod_plot <- function(.data, .y = "y", actuals = NULL) {
    p <- ggplot2::ggplot(.data, ggplot2::aes_string(x = "date", y = .y))

    if ("lo.95" %in% names(.data)) {
        p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lo.95, ymax = hi.95), fill = "grey85", alpha = 0.5)
    }

    if ("lo.80" %in% names(.data)) {
        p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lo.80, ymax = hi.80), fill = "grey75", alpha = 0.5)
    }

    if (!is.null(actuals)) {
        p <- p + ggplot2::geom_line(data = actuals, color = "black")
    }
    p +
        ggplot2::geom_line(ggplot2::aes(color = key)) +
        ggplot2::scale_color_manual(NULL, values = c("black", "blue")) +
        themebg::theme_bg()
}

#' Add smoothed values to a plot
#'
#' Add the smoothed time series values to a \code{plotly} plot.
#'
#' @param p A \code{plotly} object
#' @param .data A data frame from a \code{forecast} object
#' @param .key Name of the model to add to the plot
#'
#' @return A \code{plotly} object
#' @export
add_to_plot <- function(p, .data, .key) {
    df_key <- dplyr::filter(.data, key == .key)

    if (.key %in% c("Actual", "Forecast")) {
        vis = TRUE
    } else {
        vis = "legendonly"
    }

    if (.key == "Actual") {
        p %>%
            plotly::add_markers(
                y = ~y,
                data = df_key,
                alpha = 0.5,
                marker = list(symbol = "circle-open"),
                legendgroup = .key,
                visible = vis
            ) %>%
            plotly::add_lines(
                y = ~fit,
                data = df_key,
                name = "Trend",
                legendgroup = .key,
                visible = vis
            )
    } else {
        p %>%
            plotly::add_ribbons(
                data = df_key,
                ymin = ~fit_lo,
                ymax = ~fit_hi,
                opacity = 0.5,
                line = list(width = 1),
                legendgroup = .key,
                visible = vis,
                showlegend = FALSE
            ) %>%
            plotly::add_lines(
                y = ~fit,
                data = df_key,
                line = list(
                    # dash = "dash",
                    width = 1.5
                ),
                name = .key,
                legendgroup = .key,
                visible = vis
            )
    }
}

#' Add actual values to plot
#'
#' @param p A \code{plotly} object
#' @param .data A data frame from a \code{forecast} object
#' @param .key Name of the model to add to the plot
#'
#' @return A \code{plotly} object
#' @export
actuals_to_plot <- function(p, .data, .key) {
    df_key <- dplyr::filter(.data, key == .key)

    if (.key %in% c("Actual", "Forecast")) {
        vis = TRUE
    } else {
        vis = "legendonly"
    }

    if (.key == "Actual") {
        p %>%
            plotly::add_lines(
                y = ~y,
                data = df_key,
                name = .key,
                legendgroup = .key,
                visible = vis
            )
    } else {
        p %>%
            plotly::add_ribbons(
                data = df_key,
                ymin = ~lo.80,
                ymax = ~hi.80,
                opacity = 0.5,
                line = list(width = 1),
                legendgroup = .key,
                visible = vis,
                showlegend = FALSE
            ) %>%
            plotly::add_lines(
                y = ~y,
                data = df_key,
                line = list(
                    # dash = "dash",
                    width = 1.5
                ),
                name = .key,
                legendgroup = .key,
                visible = vis
            )
    }
}

#' Plot a hybrid forecast
#'
#' Plot a hybrid forecast and all indivudal models using \code{plotly}.
#'
#' @param .data A data frame from a \code{forecast} object, with or without smoothed values.
#' @param .title A string with the plot title.
#' @param .xtitle A string with the x-axis title.
#' @param .ytitle A string with the y-axis title.
#' @param trend If True (default), then the smoothed values are plotted; otherwise plots the actual values.
#'
#' @return A \code{plotly} object
#' @export
plotly_hybrid <- function(.data,
                          .title = "Combined forecast model",
                          .xtitle = "Date",
                          .ytitle = "Number",
                          trend = TRUE) {

    keys <- unique(.data$key)

    pal <- RColorBrewer::brewer.pal(length(keys), "Set1")
    pal[1] <- "#000000"

    p <- plotly::plot_ly(x = ~date, color = ~key, colors = pal) %>%
        plotly::layout(
            title = list(text = .title, x = 0),
            xaxis = list(title = .xtitle, showgrid = FALSE),
            yaxis = list(title = .ytitle, showgrid = FALSE)
        )

    for (i in levels(keys)) {
        if (trend) {
            p <- add_to_plot(p, .data, i)
        } else {
            p <- actuals_to_plot(p, .data, i)
        }
    }

    p
}

#' Plot a hybrid forecast
#'
#' Plot a hybrid forecast and all indivudal models using \code{plotly}.
#'
#' @param actuals A tsibble object containing the known, historical, values
#' @param y column containing the historical values
#' @param combo A fable object with the forecast values
#' @param mods An optional fable object with values from individual forecasts
#' @param title A string with the plot title.
#' @param xtitle A string with the x-axis title.
#' @param ytitle A string with the y-axis title.
#' @param fc_line A string which Sets the dash style of the forecast lines.
#'   Valid options are: "solid", "dot", "dash", "longdash", "dashdot", or
#'   "longdashdot"
#'
#' @return A \code{plotly} object
#' @export
plotly_fable <- function(actuals,
                         y,
                         combo,
                         mods = NULL,
                         title = "Combined forecast model",
                         xtitle = "Date",
                         ytitle = "Number",
                         fc_line = "solid") {

    y <- rlang::enquo(y)
    lo_80 <- as.formula("~lo_80")
    hi_80 <- as.formula("~hi_80")
    .mean <- as.formula("~.mean")
    .model <- as.formula("~.model")

    p <- actuals %>%
        plotly::plot_ly(x = as.formula("~date"), colors = "Set1") %>%
        plotly::layout(
            title = list(text = title, x = 0),
            xaxis = list(title = xtitle, showgrid = FALSE),
            yaxis = list(title = ytitle, showgrid = FALSE)
        ) %>%
        plotly::add_lines(
            y = y,
            line = list(color = "#000000"),
            name = "Actual"
        ) %>%
        plotly::add_ribbons(
            ymin = lo_80,
            ymax = hi_80,
            data = combo,
            opacity = 0.4,
            fillcolor = "#BFBFBF",
            line = list(width = 1, color = "#000000"),
            legendgroup = .model,
            showlegend = FALSE
        ) %>%
        plotly::add_lines(
            y = .mean,
            data = combo,
            opacity = 0.8,
            line = list(
                color = "#000000",
                dash = fc_line,
                width = 1.5
            ),
            legendgroup = .model,
            name = "Forecast"
        )

    if (!is.null(mods)) {
        p <- p %>%
            plotly::add_ribbons(
                ymin = lo_80,
                ymax = hi_80,
                data = mods,
                color = .model,
                opacity = 0.4,
                line = list(width = 1),
                legendgroup = .model,
                visible = "legendonly",
                showlegend = FALSE
            ) %>%
            plotly::add_lines(
                y = .mean,
                data = mods,
                color = .model,
                opacity = 0.8,
                line = list(width = 1.5),
                legendgroup = .model,
                visible = "legendonly"
            )
    }

    p
}

#' Plot a smoothed forecast
#'
#' Plot a forecast with smoothed trend line using \code{plotly}.
#'
#' @param .data A data frame containing the historical and forecast values
#' @param title A string with the plot title.
#' @param xtitle A string with the x-axis title.
#' @param ytitle A string with the y-axis title.
#' @param fc_line A string which Sets the dash style of the forecast lines.
#'   Valid options are: "solid", "dot", "dash", "longdash", "dashdot", or
#'   "longdashdot"
#'
#' @return A \code{plotly} object
#' @export
plotly_smooth <- function(.data,
                         title = "Combined forecast model",
                         xtitle = "Date",
                         ytitle = "Number") {

    fit_lo <- as.formula("~fit_lo")
    fit_hi <- as.formula("~fit_hi")
    fit <- as.formula("~fit")
    .model <- as.formula("~.model")
    .mean <- as.formula("~.mean")

    .data %>%
        plotly::plot_ly(
            x = as.formula("~date"),
            color = .model,
            colors = c("#000000", "#377eb8")) %>%
        plotly::layout(
            title = list(text = title, x = 0),
            xaxis = list(title = xtitle, showgrid = FALSE),
            yaxis = list(title = ytitle, showgrid = FALSE)
        ) %>%
        plotly::add_markers(
            y = .mean,
            alpha = 0.5,
            marker = list(symbol = "circle-open"),
            legendgroup = .model,
            showlegend = FALSE
        ) %>%
        plotly::add_ribbons(
            ymin = fit_lo,
            ymax = fit_hi,
            opacity = 0.4,
            legendgroup = .model,
            showlegend = FALSE
        ) %>%
        plotly::add_lines(
            y = fit,
            name = .model,
            legendgroup = .model
        )
}
