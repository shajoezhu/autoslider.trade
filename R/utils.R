#' Name of the column holding the trading code
#'
#' The trading code is the instrument identifier shared with `filters.trade`.
#' The convention is repeated here through an option rather than a package
#' dependency, so the two packages stay independent. Override globally with
#' `options(autoslider.trade.code_col = "ticker")`.
#'
#' @return A `character` scalar
#' @noRd
code_col <- function() {
  getOption("autoslider.trade.code_col", "SYMBOL")
}

#' Reduce a price dataset to the canonical `SYMBOL`, `DATE` and `CLOSE` columns
#'
#' Every output function accepts configurable column names and works from here
#' on in one shape, so the plotting and tabulation code does not have to carry
#' column names through.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @return A `data.frame` sorted by trading code and date
#' @noRd
canonical_prices <- function(prices, symbol, date, close) {
  assert_that(has_name(prices, c(symbol, date, close)))
  out <- as.data.frame(prices[c(symbol, date, close)])
  names(out) <- c("SYMBOL", "DATE", "CLOSE")
  out[order(out$SYMBOL, out$DATE), , drop = FALSE]
}
