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
