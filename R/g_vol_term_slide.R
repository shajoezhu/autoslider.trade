#' Realized volatility measured over several windows at once
#'
#' Volatility depends on how long you look, so it is measured over several
#' trailing windows and drawn side by side: 20 days against 60 against 90 against
#' a year. Rising to the right means the recent past has been quieter than the
#' longer one; falling means the recent past has been the noisy one.
#'
#' This is the counterpart of `g_rolling_risk_slide()`, which follows one window
#' through time. Here the windows are the point, and they are the windows an
#' implied volatility term structure is quoted in, so the two can be compared
#' directly.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param windows `numeric` Window widths, in periods
#' @param ann_factor `numeric` Number of periods per year
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_vol_term_slide(eg_ohlc)
#'
#' # Two windows only
#' g_vol_term_slide(eg_ohlc, windows = c(20L, 60L))
#'
g_vol_term_slide <- function(prices,
                             symbol = code_col(),
                             date = "DATE",
                             close = "CLOSE",
                             windows = c(20L, 60L, 90L, 252L),
                             ann_factor = 252,
                             title = "Realized Volatility by Window") {
  assert_that(is.string(title), is.numeric(windows))
  assert_that(is.number(ann_factor))

  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  windows <- sort(unique(as.integer(windows)))
  # Trading days, the way realized volatility is quoted: 20d, 60d, 90d. A whole
  # number of years is the exception worth naming.
  horizon_label <- function(w) {
    if (w %% ann_factor == 0L) paste0(w / ann_factor, "Y") else paste0(w, "d")
  }

  d <- do.call(rbind, lapply(split(rets, rets$SYMBOL), function(x) {
    usable <- windows[windows <= length(x$RET)]
    if (!length(usable)) {
      message(
        "No window fits the ", length(x$RET), " returns available for ",
        x$SYMBOL[1L], "; leaving it out."
      )
      return(NULL)
    }
    if (length(usable) < length(windows)) {
      message(
        "Dropping windows longer than the ", length(x$RET), " returns available for ",
        x$SYMBOL[1L], ": ", paste(setdiff(windows, usable), collapse = ", ")
      )
    }
    data.frame(
      SYMBOL = x$SYMBOL[1L],
      WINDOW = vapply(usable, horizon_label, character(1)),
      VOL = 100 * vapply(usable, function(w) vol(tail(x$RET, w), ann_factor), numeric(1)),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(d)) {
    d <- data.frame(
      SYMBOL = character(0), WINDOW = character(0), VOL = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(d) <- NULL
  # Nothing to draw is an error, not an empty chart: a figure with no bars
  # invites the reader to believe volatility was measured and came to nothing.
  assert_that(
    nrow(d) > 0L,
    msg = "No window fits the history available, so there is no volatility to show."
  )
  d$WINDOW <- factor(d$WINDOW, levels = unique(d$WINDOW))

  ggplot(d, aes(x = WINDOW, y = VOL, fill = SYMBOL)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~SYMBOL, ncol = 1L) +
    labs(title = title, x = "Window", y = "Realized volatility (%, ann.)") +
    theme_minimal()
}
