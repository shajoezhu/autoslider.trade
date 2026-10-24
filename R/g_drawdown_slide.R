#' Drawdown figure
#'
#' The underwater plot of each trading code: how far below its own running peak
#' the price sits, period by period. Below zero the instrument is recovering from
#' a high; the depth of a trough is the drawdown and its width is how long the
#' recovery took.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_drawdown_slide(eg_ohlc)
#'
g_drawdown_slide <- function(prices,
                             symbol = code_col(),
                             date = "DATE",
                             close = "CLOSE",
                             title = "Drawdown") {
  assert_that(is.string(title))

  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  d <- do.call(rbind, lapply(split(rets, rets$SYMBOL), function(x) {
    data.frame(
      SYMBOL = x$SYMBOL,
      DATE = x$DATE,
      DD = 100 * dd_series(x$RET),
      stringsAsFactors = FALSE
    )
  }))
  rownames(d) <- NULL

  ggplot(d, aes(x = DATE, y = DD)) +
    geom_area(alpha = 0.4, fill = "#d73027", na.rm = TRUE) +
    geom_line(colour = "#d73027", na.rm = TRUE) +
    geom_hline(yintercept = 0, colour = "grey30") +
    facet_wrap(~SYMBOL, ncol = 1L) +
    labs(title = title, x = "Date", y = "Drawdown (%)") +
    theme_minimal()
}
