# plot_functions.R

#' Plot forecasts
#'
#' Plots combined forecast model and all indivdual models using \code{plotly}.
#'
#' @param actuals A tsibble object containing the known, historical, values
#' @param y column containing the historical values
#' @param combo A fable object with the forecast values
#' @param mods An optional fable object with values from individual forecasts
#' @param title A string with the plot title.
#' @param xtitle A string with the x-axis title.
#' @param ytitle A string with the y-axis title.
#' @param pi If \code{TRUE} the prediction intervals for all individual models
#'   will be shown.
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
                         pi = TRUE,
                         fc_line = "solid") {

    y <- rlang::enquo(y)
    lo_80 <- stats::as.formula("~lo_80")
    hi_80 <- stats::as.formula("~hi_80")
    .mean <- stats::as.formula("~.mean")
    .model <- stats::as.formula("~.model")

    p <- actuals %>%
        plotly::plot_ly(x = stats::as.formula("~date"), colors = "Set1") %>%
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
        if (pi) {
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
                )
        }

        p <- p %>%
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
#'
#' @return A \code{plotly} object
#' @export
plotly_smooth <- function(.data,
                         title = "Combined forecast model",
                         xtitle = "Date",
                         ytitle = "Number") {

    fit_lo <- stats::as.formula("~fit_lo")
    fit_hi <- stats::as.formula("~fit_hi")
    fit <- stats::as.formula("~fit")
    .model <- stats::as.formula("~.model")
    .mean <- stats::as.formula("~.mean")

    .data %>%
        plotly::plot_ly(
            x = stats::as.formula("~date"),
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

#' Plot calculated future values
#'
#' Plots calculated future values \code{plotly}.
#'
#' @param .data A data frame containing values
#' @param y column containing the values
#' @param title A string with the plot title.
#' @param xtitle A string with the x-axis title.
#' @param ytitle A string with the y-axis title.
#' @param fc_line A string which Sets the dash style of the forecast lines.
#'   Valid options are: "solid", "dot", "dash", "longdash", "dashdot", or
#'   "longdashdot"
#'
#' @return A \code{plotly} object
#' @export
plotly_calc <- function(.data,
                         y,
                         title = "Calculated future values",
                         xtitle = "Date",
                         ytitle = "Number",
                         fc_line = "solid") {

    y <- rlang::enquo(y)
    .model <- stats::as.formula("~.model")

    .data %>%
        plotly::plot_ly(
            x = stats::as.formula("~date"),
            colors = c("#000000", "#377eb8")
        ) %>%
        plotly::layout(
            title = list(text = title, x = 0),
            xaxis = list(title = xtitle, showgrid = FALSE),
            yaxis = list(title = ytitle, showgrid = FALSE)
        ) %>%
        plotly::add_lines(
            y = y,
            color = .model,
            line = list(dash = fc_line),
            name = .model
        )
}
