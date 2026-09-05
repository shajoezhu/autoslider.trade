#' Trade listing
#'
#' A listing of executed trades, keyed by trading code. Each trade is shown
#' with its date, side, quantity and price, plus the computed notional value.
#'
#' @param trades `data.frame` of trades
#' @param symbol `character` Name of the trading code column
#'
#' @return A `listing_df` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' l_trades_slide(eg_trades)
#'
l_trades_slide <- function(trades, symbol = code_col()) {
  required <- c(symbol, "DATE", "SIDE", "QTY", "PRICE")
  assert_that(has_name(trades, required))
  assert_that(is.string(symbol))

  d <- as.data.frame(trades[required])
  names(d)[names(d) == symbol] <- "SYMBOL"
  d$NOTIONAL <- d$QTY * d$PRICE

  display <- c("SYMBOL", "DATE", "SIDE", "QTY", "PRICE", "NOTIONAL")
  d <- d[order(d$SYMBOL, d$DATE), display]

  formatters::var_labels(d) <- c(
    SYMBOL = "Symbol",
    DATE = "Trade Date",
    SIDE = "Side",
    QTY = "Quantity",
    PRICE = "Price",
    NOTIONAL = "Notional"
  )

  as_listing(d, key_cols = "SYMBOL", disp_cols = display)
}
