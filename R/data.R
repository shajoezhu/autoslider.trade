#' Example daily closing prices
#'
#' A small set of daily closing prices for three trading codes, used in
#' examples and tests.
#'
#' @format A data frame with 15 rows and 3 columns:
#' \describe{
#'   \item{SYMBOL}{Trading code, one of `AAA`, `BBB`, `CCC`}
#'   \item{DATE}{Trade date, `Date`}
#'   \item{CLOSE}{Closing price, `numeric`}
#' }
#' @source Synthetic data created for this package.
"eg_prices"

#' Example trades
#'
#' A small set of trades used in examples and tests.
#'
#' @format A data frame with 5 rows and 5 columns:
#' \describe{
#'   \item{SYMBOL}{Trading code}
#'   \item{DATE}{Trade date, `Date`}
#'   \item{SIDE}{`BUY` or `SELL`}
#'   \item{QTY}{Quantity traded, `integer`}
#'   \item{PRICE}{Execution price, `numeric`}
#' }
#' @source Synthetic data created for this package.
"eg_trades"

#' NZ trading universe
#'
#' The full list of New Zealand instruments the user tracks on their
#' `homepage-stock` kanban. Copied verbatim from that repository (168
#' instruments); non-ASCII company names are preserved.
#'
#' @format A data frame with 168 rows and 2 columns:
#' \describe{
#'   \item{name}{Company or fund name, `character`}
#'   \item{symbol}{Trading code on the NZX, e.g. `AIA.NZ`, `character`}
#' }
#' @source Copied from `homepage-stock/data/nz_list.csv`.
"nz_tickers"

#' Example daily OHLCV prices
#'
#' A synthetic daily OHLCV series for three NZX instruments, used by the
#' candlestick figure and its tests. `homepage-stock` stores no price history
#' (only the ticker list) and fetching market data is out of scope for this
#' package, so the series is generated deterministically.
#'
#' @format A data frame with 360 rows and 7 columns:
#' \describe{
#'   \item{SYMBOL}{Trading code, one of `AIA.NZ`, `AIR.NZ`, `ANZ.NZ`}
#'   \item{DATE}{Trade date, `Date`}
#'   \item{OPEN}{Opening price, `numeric`}
#'   \item{HIGH}{Intraday high, `numeric`}
#'   \item{LOW}{Intraday low, `numeric`}
#'   \item{CLOSE}{Closing price, `numeric`}
#'   \item{VOLUME}{Shares traded, `numeric`}
#' }
#' @source Synthetic data created for this package.
"eg_ohlc"
