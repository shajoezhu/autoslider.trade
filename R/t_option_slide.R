#' Option pricing table with implied volatility and Greeks
#'
#' Prices a set of European option quotes and reports what the market is saying
#' through them: the implied volatility backed out of each price, and the Greeks
#' at that volatility. Reading a quote as a price alone tells you very little;
#' the implied volatility is what can be compared against the volatility the
#' underlying has actually realized.
#'
#' This table needs option quotes, which this package does not fetch. Supply
#' them as a `data.frame` with `STRIKE`, `TYPE` (`"call"` / `"put"`, or `"c"` /
#' `"p"`), `PRICE` and `TTE` (time to expiry in years), plus the spot price of
#' the underlying. Pricing is Black-Scholes on a non-dividend-paying underlying,
#' so a quote on something that pays a dividend is priced on that assumption.
#'
#' @param options `data.frame` of option quotes
#' @param spot `numeric` price of the underlying
#' @param r `numeric` continuously compounded risk-free rate
#' @param digits `integer` Number of decimal places in the output
#' @param title `character` Table title
#'
#' @return An `rtables` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' # A call and a put struck at the money a quarter out: both are quoted at about
#' # 30% volatility, which is what the table recovers.
#' quotes <- data.frame(
#'   STRIKE = c(3, 3),
#'   TYPE = c("call", "put"),
#'   TTE = c(0.25, 0.25),
#'   PRICE = c(0.186, 0.172)
#' )
#' t_option_slide(quotes, spot = 3)
#'
t_option_slide <- function(options,
                           spot,
                           r = 0.02,
                           digits = 4L,
                           title = "Option Pricing and Greeks") {
  assert_that(is.string(title), is.count(digits))
  assert_that(is.number(spot), spot > 0)
  assert_that(is.number(r))
  assert_that(is.data.frame(options), nrow(options) > 0L)

  stats <- option_rows(options, spot, r)

  labels <- c(
    Moneyness = "Moneyness (spot/strike - 1, %)",
    Price = "Price",
    ImpliedVol = "Implied Volatility (%)",
    Delta = "Delta",
    Gamma = "Gamma",
    Vega = "Vega (per 1 vol point)",
    Theta = "Theta (per day)",
    Rho = "Rho (per 1 rate point)"
  )
  names(stats)[match(names(labels), names(stats))] <- unname(labels)

  fmt_value <- function(x) rcell(round(mean(x), digits))

  lyt <- basic_table(title = title) |>
    split_cols_by("CONTRACT")

  for (m in unname(labels)) {
    lyt <- analyze(lyt, m, fmt_value)
  }

  build_table(lyt, stats)
}
