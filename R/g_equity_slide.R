#' Equity curve figure
#'
#' Plot the price history of one or more trading codes. By default every
#' instrument is rescaled to start at 1 so that several instruments with very
#' different price levels can be compared on the same axes.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param normalize `logical` Should every instrument start at 1? Defaults to
#'   `TRUE`.
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_equity_slide(eg_prices)
#'
#' # Absolute prices instead of a common starting point
#' g_equity_slide(eg_prices, normalize = FALSE)
#'
g_equity_slide <- function(prices,
                           symbol = code_col(),
                           date = "DATE",
                           close = "CLOSE",
                           normalize = TRUE,
                           title = "Equity Curve") {
  assert_that(is.flag(normalize), is.string(title))
  assert_that(is.string(symbol), is.string(date), is.string(close))

  d <- canonical_prices(prices, symbol, date, close)
  d$VALUE <- d$CLOSE
  if (normalize) {
    d$VALUE <- d$VALUE / ave(d$VALUE, d$SYMBOL, FUN = function(x) x[1L])
  }

  ggplot(d, aes(x = DATE, y = VALUE, colour = SYMBOL)) +
    geom_line() +
    labs(
      title = title,
      x = "Date",
      y = if (normalize) "Growth of 1" else close,
      colour = symbol
    ) +
    theme_minimal()
}
