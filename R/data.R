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

#' Example option quotes
#'
#' A small, self-consistent set of European option quotes on `AIA.NZ`, used by
#' `t_option_slide()` and `g_vol_premium_slide()`. Option quotes cannot be
#' derived from a price series and this package does not fetch them, so without
#' an example the option outputs could never be tried.
#'
#' The prices are Black-Scholes at the realised volatility of the `AIA.NZ`
#' series in [eg_ohlc], so the implied volatilities the tables back-solve come
#' back to roughly that volatility rather than to arbitrary numbers.
#'
#' @format A data frame with 42 rows and 4 columns:
#' \describe{
#'   \item{STRIKE}{Strike price, `numeric`}
#'   \item{TYPE}{`call` or `put`, `character`}
#'   \item{PRICE}{Option price, `numeric`}
#'   \item{TTE}{Time to expiry in years, one of 0.25, 0.5, 1, `numeric`}
#' }
#' @source Synthetic data created for this package.
"eg_options"

#' Example company fundamentals
#'
#' A metrics-by-period trend table for the shape `t_fundamentals_slide()`
#' pivots. Fundamentals cannot be derived from prices and are not fetched, so
#' this exists for the same reason as [eg_options]: to make the fundamentals
#' output runnable.
#'
#' @format A data frame with 12 rows and 3 columns:
#' \describe{
#'   \item{METRIC}{Metric name, `character`}
#'   \item{PERIOD}{Reporting period, one of FY2023, FY2024, FY2025, `character`}
#'   \item{VALUE}{Metric value, `numeric`}
#' }
#' @source Synthetic data created for this package.
"eg_financials"
