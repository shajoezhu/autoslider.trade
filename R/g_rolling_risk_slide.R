#' Rolling risk figure
#'
#' Risk is not constant, so four rolling window metrics are stacked: annualized
#' volatility, the Sharpe ratio, historical Value at Risk and the maximum
#' drawdown inside the window, each computed over the trailing `window` periods.
#'
#' Each trading code is drawn in its own colour. When the data holds one code
#' only, the volatility panel instead colours the line by volatility regime
#' (`low`, `normal` or `high` against the `low` and `high` thresholds) so that
#' quiet and stressed stretches can be told apart at a glance.
#'
#' Short series degrade gracefully: while fewer than `window` periods are
#' available the panels are simply empty, and a `window` longer than the series
#' gives empty panels rather than an error.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param window `integer` Number of periods in the rolling window
#' @param rf_rate `numeric` Annual risk-free rate used in the Sharpe ratio
#' @param ann_factor `numeric` Number of periods per year
#' @param low `numeric` Upper bound of the low volatility regime
#' @param high `numeric` Lower bound of the high volatility regime
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_rolling_risk_slide(eg_ohlc)
#'
#' # About one quarter of daily prices
#' g_rolling_risk_slide(eg_ohlc, window = 63L)
#'
g_rolling_risk_slide <- function(prices,
                                 symbol = code_col(),
                                 date = "DATE",
                                 close = "CLOSE",
                                 window = 63L,
                                 rf_rate = 0.02,
                                 ann_factor = 252,
                                 low = 0.10,
                                 high = 0.20,
                                 title = "Rolling Risk") {
  assert_that(is.string(title), is.count(window))
  assert_that(is.number(rf_rate), is.number(ann_factor))
  assert_that(is.number(low), is.number(high), low < high)

  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  d <- do.call(rbind, lapply(split(rets, rets$SYMBOL), function(x) {
    data.frame(
      SYMBOL = x$SYMBOL,
      DATE = x$DATE,
      VOL = 100 * roll_vol(x$RET, window, ann_factor),
      SHARPE = roll_sharpe(x$RET, window, rf_rate, ann_factor),
      VAR = 100 * roll_var(x$RET, window, 0.95),
      MAXDD = 100 * roll_maxdd(x$RET, window),
      stringsAsFactors = FALSE
    )
  }))
  rownames(d) <- NULL
  d$REGIME <- vol_regime(d$VOL / 100, low, high)

  no_x <- list(
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  )
  by_code <- length(unique(d$SYMBOL)) > 1L

  p_vol <- ggplot(d, aes(x = DATE, y = VOL)) +
    theme_minimal() +
    no_x +
    labs(title = title, x = "Date", y = "Volatility (%, ann.)")
  if (by_code) {
    p_vol <- p_vol +
      geom_line(aes(colour = SYMBOL), na.rm = TRUE) +
      geom_hline(yintercept = 100 * c(low, high), linetype = "dashed", colour = "grey50")
  } else {
    regime_pal <- c(low = "#1a9850", normal = "grey30", high = "#d73027")
    p_vol <- p_vol +
      geom_line(aes(colour = REGIME), na.rm = TRUE) +
      scale_colour_manual(values = regime_pal, name = "Regime")
  }

  p_sharpe <- ggplot(d, aes(x = DATE, y = SHARPE, colour = SYMBOL)) +
    geom_line(na.rm = TRUE) +
    geom_hline(yintercept = 0, colour = "grey30") +
    labs(y = "Sharpe") +
    theme_minimal() +
    no_x

  p_var <- ggplot(d, aes(x = DATE, y = VAR, colour = SYMBOL)) +
    geom_line(na.rm = TRUE) +
    labs(y = "VaR 95 (%)") +
    theme_minimal() +
    no_x

  p_dd <- ggplot(d, aes(x = DATE, y = MAXDD, colour = SYMBOL)) +
    geom_line(na.rm = TRUE) +
    labs(x = "Date", y = "Max Drawdown (%)") +
    theme_minimal()

  cowplot::plot_grid(
    p_vol, p_sharpe, p_var, p_dd,
    ncol = 1L,
    rel_heights = c(1, 1, 1, 1),
    align = "v"
  )
}
