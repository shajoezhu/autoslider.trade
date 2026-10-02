#' Risk summary table
#'
#' A per-instrument summary of the risk and risk-adjusted return metrics used in
#' quantitative trading: total and annualized return, volatility, downside
#' deviation, Value at Risk (historical, with the Cornish-Fisher correction for
#' the fat tails returns actually have) and Conditional Value at Risk, the
#' maximum and average drawdown with its longest duration, and the Sharpe,
#' Sortino, Calmar and Omega ratios, plus skewness and excess kurtosis.
#'
#' Every figure is computed on simple returns derived from the prices, so no
#' separate return series has to be supplied. Supplying `benchmark` adds Beta,
#' tracking error and the information ratio against that trading code.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param benchmark `character` Trading code already present in `prices` to
#'   compare against, or `NULL` for no benchmark-relative rows
#' @param rf_rate `numeric` Annual risk-free rate used in the Sharpe and Sortino
#'   ratios
#' @param ann_factor `numeric` Number of periods per year, used to annualize
#'   volatility and returns
#' @param digits `integer` Number of decimal places in the output
#' @param title `character` Table title
#'
#' @return An `rtables` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' t_risk_slide(eg_ohlc)
#'
#' # Add benchmark-relative rows
#' t_risk_slide(eg_ohlc, benchmark = "ANZ.NZ")
#'
t_risk_slide <- function(prices,
                         symbol = code_col(),
                         date = "DATE",
                         close = "CLOSE",
                         benchmark = NULL,
                         rf_rate = 0.02,
                         ann_factor = 252,
                         digits = 2L,
                         title = "Risk Summary") {
  assert_that(is.count(digits), is.string(title))
  assert_that(is.number(rf_rate), is.number(ann_factor))

  d <- canonical_prices(prices, symbol, date, close)
  bm <- check_benchmark(d, benchmark)
  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  syms <- sort(unique(rets$SYMBOL))
  bm_map <- if (is.null(bm)) NULL else rets[rets$SYMBOL == bm, c("DATE", "RET")]

  res <- lapply(syms, function(s) {
    x <- rets[rets$SYMBOL == s, , drop = FALSE]
    if (is.null(bm_map)) {
      return(risk_summary(x$RET, rf_rate, ann_factor))
    }
    i <- match(as.character(x$DATE), as.character(bm_map$DATE))
    keep <- !is.na(i)
    risk_summary(x$RET[keep], rf_rate, ann_factor, bm_map$RET[i[keep]])
  })
  res <- do.call(rbind, res)
  stats <- as.data.frame(res)
  stats$SYMBOL <- syms
  rownames(stats) <- NULL

  # `analyze()` prints a variable's name as its row label, so the metric keys
  # become their display labels here, the same way `t_performance_slide()` does.
  labels <- risk_labels()
  metrics <- intersect(names(labels), names(stats))
  names(stats)[match(metrics, names(stats))] <- labels[metrics]

  fmt_value <- function(x) rcell(round(mean(x), digits))

  lyt <- basic_table(title = title) |>
    split_cols_by("SYMBOL")

  for (m in unname(labels[metrics])) {
    lyt <- analyze(lyt, m, fmt_value)
  }

  build_table(lyt, stats)
}
