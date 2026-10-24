#' Return and risk by holding horizon
#'
#' The same instrument looks different depending on how long it is held, so the
#' metrics are recomputed over several trailing windows side by side: what the
#' last month did against what the last year did, and both against the whole
#' series. This is the table that separates a short-term view from a long-term
#' one, rather than one average that belongs to neither.
#'
#' A window longer than the history available is dropped, so the table never
#' compares a real window against the full series in disguise. The `Full` column
#' is always present.
#'
#' @param prices `data.frame` of prices for one trading code
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param windows `numeric` Trailing window widths, in periods
#' @param rf_rate `numeric` Annual risk-free rate used in the Sharpe ratio
#' @param ann_factor `numeric` Number of periods per year
#' @param digits `integer` Number of decimal places in the output
#' @param title `character` Table title
#'
#' @return An `rtables` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' air_nz <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
#' t_horizon_slide(air_nz)
#'
#' # One quarter against the full history
#' t_horizon_slide(air_nz, windows = 63L)
#'
t_horizon_slide <- function(prices,
                            symbol = code_col(),
                            date = "DATE",
                            close = "CLOSE",
                            windows = c(21L, 63L, 252L),
                            rf_rate = 0.02,
                            ann_factor = 252,
                            digits = 2L,
                            title = "Return and Risk by Horizon") {
  assert_that(is.string(title), is.count(digits))
  assert_that(is.number(rf_rate), is.number(ann_factor))
  assert_that(is.numeric(windows))

  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  # Horizons are compared for one instrument at a time; the report this feeds
  # covers one code per deck.
  codes <- sort(unique(rets$SYMBOL))
  assert_that(
    length(codes) == 1L,
    msg = sprintf(
      "t_horizon_slide() compares horizons for one trading code, but %d were given: %s.",
      length(codes), paste(codes, collapse = ", ")
    )
  )
  x <- rets$RET

  windows <- sort(unique(as.integer(windows)))
  usable <- windows[windows <= length(x)]
  if (length(usable) < length(windows)) {
    message(
      "Dropping windows longer than the ", length(x), " returns available: ",
      paste(setdiff(windows, usable), collapse = ", ")
    )
  }

  horizon_label <- function(w) {
    if (w %% ann_factor == 0L) paste0(w / ann_factor, "Y") else paste0(round(w / 21), "M")
  }

  stats <- as.data.frame(
    do.call(rbind, lapply(usable, function(w) risk_summary(tail(x, w), rf_rate, ann_factor)))
  )
  stats$HORIZON <- vapply(usable, horizon_label, character(1))
  stats$Observations <- usable
  # `rbind()` first: as.data.frame() on a bare named vector gives one tall
  # column, not one row per metric.
  full <- as.data.frame(rbind(risk_summary(x, rf_rate, ann_factor)))
  full$HORIZON <- "Full"
  full$Observations <- length(x)
  stats <- rbind(stats, full[names(stats)])
  rownames(stats) <- NULL

  # A slide holds a reading, not a data dump: the horizons are separated by
  # return, risk and what the return cost in drawdown.
  labels <- risk_labels()
  metrics <- intersect(
    c("Return", "AnnReturn", "Volatility", "MaxDrawdown", "VaR95", "Sharpe", "Calmar"),
    names(stats)
  )
  names(stats)[match(metrics, names(stats))] <- unname(labels[metrics])

  fmt_value <- function(x) rcell(round(mean(x), digits))

  lyt <- basic_table(title = title) |>
    split_cols_by("HORIZON")

  for (m in c("Observations", unname(labels[metrics]))) {
    lyt <- analyze(lyt, m, fmt_value)
  }

  build_table(lyt, stats)
}
