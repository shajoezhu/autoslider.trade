#' Performance summary table
#'
#' A table of per-instrument summary statistics: the number of observations,
#' the first and last price, the return over the period and the maximum
#' drawdown, expressed as percentages.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param digits `integer` Number of decimal places in the output
#'
#' @return An `rtables` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' t_performance_slide(eg_prices)
#'
t_performance_slide <- function(prices,
                                symbol = code_col(),
                                date = "DATE",
                                close = "CLOSE",
                                digits = 2L) {
  assert_that(is.count(digits))

  d <- canonical_prices(prices, symbol, date, close)

  stats <- do.call(rbind, lapply(split(d, d$SYMBOL), function(x) {
    first <- x$CLOSE[1L]
    last <- x$CLOSE[nrow(x)]
    data.frame(
      SYMBOL = x$SYMBOL[1L],
      Observations = nrow(x),
      First = first,
      Last = last,
      Return = 100 * (last / first - 1),
      Drawdown = 100 * (min(x$CLOSE) / max(x$CLOSE) - 1),
      stringsAsFactors = FALSE
    )
  }))
  rownames(stats) <- NULL

  fmt_count <- function(x) rcell(round(sum(x), digits))
  fmt_value <- function(x) rcell(round(mean(x), digits))

  lyt <- basic_table(title = "Performance Summary") |>
    split_cols_by("SYMBOL") |>
    analyze("Observations", fmt_count) |>
    analyze("First", fmt_value) |>
    analyze("Last", fmt_value) |>
    analyze("Return", fmt_value) |>
    analyze("Drawdown", fmt_value)

  build_table(lyt, stats)
}
