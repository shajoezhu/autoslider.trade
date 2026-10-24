#' Price against its own trailing high-low band
#'
#' Where the price sits inside the range it has traded over the last `window`
#' periods: the band is the running high and low, and the position is how far up
#' that band the last price is, 0% at the floor and 100% at the ceiling. A price
#' pinned to the top of its 52-week range is in a different state from the same
#' price pinned to the bottom, and a level chart does not show which it is.
#'
#' The band is trailing, so it moves as the window moves; the position quoted in
#' the subtitle is always the latest one.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param window `integer` Width of the trailing band in periods; 252 is one
#'   year of daily prices
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_range_slide(eg_ohlc)
#'
#' # One quarter instead of one year
#' g_range_slide(eg_ohlc, window = 63L)
#'
g_range_slide <- function(prices,
                          symbol = code_col(),
                          date = "DATE",
                          close = "CLOSE",
                          window = 252L,
                          title = "Price Against Its 52-Week Range") {
  assert_that(is.string(title), is.count(window))

  d <- canonical_prices(prices, symbol, date, close)
  assert_that(nrow(d) > 0L, msg = "No prices to plot.")

  d <- do.call(rbind, lapply(split(d, d$SYMBOL), function(x) {
    data.frame(
      SYMBOL = x$SYMBOL,
      DATE = x$DATE,
      CLOSE = x$CLOSE,
      HIGH = roll_apply(x$CLOSE, window, max),
      LOW = roll_apply(x$CLOSE, window, min),
      stringsAsFactors = FALSE
    )
  }))
  rownames(d) <- NULL

  # Position in the band, 0 at the floor and 100 at the ceiling, one per code.
  pos <- vapply(split(d, d$SYMBOL), function(x) {
    last <- nrow(x)
    band <- x$HIGH[last] - x$LOW[last]
    if (is.na(band) || band <= 0) {
      return(NA_real_)
    }
    100 * (x$CLOSE[last] - x$LOW[last]) / band
  }, numeric(1))

  subtitle <- if (all(is.na(pos))) {
    "Position in range: not enough history for the band"
  } else {
    paste(
      "Position in range:",
      paste(names(pos), sprintf("%.0f%%", pos), collapse = ", ")
    )
  }

  ggplot(d, aes(x = DATE)) +
    geom_ribbon(aes(ymin = LOW, ymax = HIGH), fill = "steelblue", alpha = 0.15, na.rm = TRUE) +
    geom_line(aes(y = CLOSE), colour = "steelblue", na.rm = TRUE) +
    facet_wrap(~SYMBOL, ncol = 1L, scales = "free_y") +
    labs(title = title, subtitle = subtitle, x = "Date", y = close) +
    theme_minimal()
}
