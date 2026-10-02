test_that("returns_long derives simple returns per trading code", {
  r <- returns_long(eg_prices, "SYMBOL", "DATE", "CLOSE")
  # The first observation of every instrument has no predecessor and is dropped.
  expect_equal(nrow(r), 12L)
  aaa <- r$RET[r$SYMBOL == "AAA"]
  expect_equal(aaa, c(102, 101, 105, 108) / c(100, 102, 101, 105) - 1)
  expect_equal(levels(factor(as.character(r$SYMBOL))), c("AAA", "BBB", "CCC"))
})

test_that("returns_long silently drops instruments with a single observation", {
  one <- eg_prices[eg_prices$SYMBOL == "AAA", ][1L, ]
  expect_equal(nrow(returns_long(one, "SYMBOL", "DATE", "CLOSE")), 0L)
})

test_that("returns_matrix keeps only the dates common to every trading code", {
  prices <- eg_ohlc
  prices <- prices[!(prices$SYMBOL == "ANZ.NZ" & prices$DATE > max(prices$DATE) - 9L), ]
  r <- returns_long(prices, "SYMBOL", "DATE", "CLOSE")
  m <- returns_matrix(r)
  expect_equal(colnames(m), c("AIA.NZ", "AIR.NZ", "ANZ.NZ"))
  expect_true(all(!is.na(m)))
  expect_equal(nrow(m), length(unique(prices$DATE[prices$SYMBOL == "ANZ.NZ"])) - 1L)
})

test_that("drawdown helpers agree with a hand-computed series", {
  x <- c(0.1, -0.2, 0.05)
  d <- dd_series(x)
  expect_true(all(d <= 0))
  expect_equal(d[2L], -0.2, tolerance = 1e-9)
  expect_equal(d[3L], 0.924 / 1.1 - 1, tolerance = 1e-9)
  expect_equal(max_dd(x), min(d))
  expect_equal(avg_dd(x), mean(d[d < 0]))
  # 1.1 -> 0.99 -> 0.891 -> 1.0692, still short of the peak: three periods down.
  expect_equal(dd_duration(c(0.1, -0.1, -0.1, 0.2)), 3L)
  expect_equal(dd_duration(c(0.1, 0.1, 0.1)), 0L)
})

test_that("Value at Risk and CVaR are reported as positive losses", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  expect_equal(var_hist(x, 0.95), -as.numeric(quantile(x, 0.05)), tolerance = 1e-9)
  expect_true(var_hist(x, 0.99) > var_hist(x, 0.95))
  expect_true(cvar(x, 0.95) > var_hist(x, 0.95))
  expect_true(var_param(x, 0.95) > 0)
  expect_true(is.na(var_cf(x[1:3], 0.95)))
})

test_that("risk-adjusted ratios stay finite on a flat series", {
  flat <- rep(0, 30L)
  expect_equal(sharpe(flat), 0)
  expect_equal(sortino(flat), 0)
  expect_equal(calmar(flat), 0)
  expect_equal(vol(flat), 0)
  expect_equal(downside_dev(flat), 0)
  expect_true(is.na(omega(flat)))
})

test_that("skewness and excess kurtosis vanish on a symmetric series", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  expect_equal(skew(x), 0, tolerance = 1e-9)
  # A uniform sample is platykurtic, so its excess kurtosis is below zero.
  expect_true(kurt(x) < 0)
  expect_true(is.na(skew(x[1:2])))
  expect_true(is.na(kurt(x[1:3])))
})

test_that("calmar relates the annualized return to the maximum drawdown", {
  x <- c(0.05, -0.10, 0.05, 0.05)
  expect_equal(calmar(x), ann_return(x) / abs(max_dd(x)), tolerance = 1e-9)
  expect_equal(calmar(rep(0.01, 20L)), 0)
})

test_that("beta against itself is one and information ratio zero", {
  x <- seq(-0.05, 0.05, length.out = 60L)
  expect_equal(beta(x, x), 1, tolerance = 1e-9)
  expect_equal(te(x, x), 0, tolerance = 1e-9)
  expect_equal(ir(x, x), 0)
  expect_true(is.na(beta(x[1L], x)))
})

test_that("roll_apply returns NA until the window fills and beyond the series", {
  out <- roll_apply(1:5, 3L, mean)
  expect_equal(out, c(NA, NA, 2, 3, 4))
  expect_true(all(is.na(roll_apply(1:5, 10L, mean))))
})

test_that("vol_regime classifies below, inside and above the thresholds", {
  r <- vol_regime(c(0.05, 0.15, 0.30, NA), low = 0.10, high = 0.20)
  expect_equal(as.character(r), c("low", "normal", "high", NA))
})

test_that("div_ratio is one for a single instrument", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  r <- returns_long(one, "SYMBOL", "DATE", "CLOSE")
  expect_equal(div_ratio(r), 1, tolerance = 1e-9)
})

# --- outputs ------------------------------------------------------------------

test_that("t_risk_slide renders every instrument", {
  out <- t_risk_slide(eg_ohlc)
  expect_true(inherits(out, "VTableTree"))

  rendered <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(rendered, "Risk Summary", fixed = TRUE)
  expect_match(rendered, "AIA.NZ")
  expect_match(rendered, "AIR.NZ")
  expect_match(rendered, "ANZ.NZ")
  expect_match(rendered, "Sharpe Ratio", fixed = TRUE)
  expect_match(rendered, "Max Drawdown", fixed = TRUE)
  expect_match(rendered, "VaR 95%", fixed = TRUE)
  expect_match(rendered, "CVaR 95%", fixed = TRUE)
})

test_that("t_risk_slide adds benchmark-relative rows only when asked", {
  plain <- paste(capture.output(print(t_risk_slide(eg_ohlc))), collapse = "\n")
  expect_no_match(plain, "Information Ratio", fixed = TRUE)

  with_bm <- paste(capture.output(print(t_risk_slide(eg_ohlc, benchmark = "ANZ.NZ"))), collapse = "\n")
  expect_match(with_bm, "Beta", fixed = TRUE)
  expect_match(with_bm, "Tracking Error", fixed = TRUE)
  expect_match(with_bm, "Information Ratio", fixed = TRUE)
})

test_that("t_risk_slide rejects an unknown benchmark and a missing column", {
  expect_error(t_risk_slide(eg_ohlc, benchmark = "ZZZ"), "benchmark")
  expect_error(t_risk_slide(eg_ohlc["CLOSE"]), "does not have")
})

test_that("t_risk_slide accepts renamed columns", {
  prices <- eg_ohlc
  names(prices) <- c("ticker", "day", "o", "h", "l", "px", "v")
  out <- t_risk_slide(prices, symbol = "ticker", date = "day", close = "px")
  expect_true(inherits(out, "VTableTree"))
})

test_that("t_risk_slide tolerates the short eg_prices series", {
  out <- t_risk_slide(eg_prices)
  rendered <- capture.output(print(out))
  expect_true(all(!grepl("NaN", rendered)))
})

test_that("g_drawdown_slide stays at or below zero", {
  p <- g_drawdown_slide(eg_ohlc)
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$DD <= 0))
  expect_equal(unique(p$data$SYMBOL), c("AIA.NZ", "AIR.NZ", "ANZ.NZ"))
})

# The marker layer is the one carrying TYPE, so it survives a change of layer order.
marker_data <- function(p) {
  layers <- Filter(function(l) !is.null(l$data) && "TYPE" %in% names(l$data), p$layers)
  expect_length(layers, 1L)
  layers[[1L]]$data
}

test_that("g_return_dist_slide marks mean, VaR and CVaR per instrument", {
  p <- g_return_dist_slide(eg_ohlc)
  expect_s3_class(p, "ggplot")

  marks <- marker_data(p)
  expect_equal(nrow(marks), 9L)
  expect_equal(unique(marks$SYMBOL), c("AIA.NZ", "AIR.NZ", "ANZ.NZ"))
  expect_equal(as.character(marks$TYPE[1:3]), c("Mean", "VaR 95%", "CVaR 95%"))
})

test_that("g_return_dist_slide relabels the markers with the confidence level", {
  p <- g_return_dist_slide(eg_ohlc, conf = 0.99)
  marks <- marker_data(p)
  expect_equal(as.character(marks$TYPE[1:3]), c("Mean", "VaR 99%", "CVaR 99%"))
})

test_that("g_rolling_risk_slide builds a four-panel figure", {
  p <- g_rolling_risk_slide(eg_ohlc)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_rolling_risk_slide accepts a window longer than the series", {
  p <- g_rolling_risk_slide(eg_ohlc, window = 500L)
  expect_s3_class(p, "ggplot")
})

test_that("g_rolling_risk_slide works on a single trading code", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  p <- g_rolling_risk_slide(one)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_portfolio_risk_slide draws the correlation heatmap without a benchmark", {
  expect_message(p <- g_portfolio_risk_slide(eg_ohlc), "correlation heatmap only")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 9L)
  expect_equal(p$data$VALUE[p$data$VAR1 == p$data$VAR2], c(1, 1, 1))
})

test_that("g_portfolio_risk_slide adds beta and information ratio panels", {
  p <- g_portfolio_risk_slide(eg_ohlc, benchmark = "ANZ.NZ")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("g_portfolio_risk_slide rejects an unknown benchmark and missing column", {
  expect_error(g_portfolio_risk_slide(eg_ohlc, benchmark = "ZZZ"), "benchmark")
  expect_error(g_portfolio_risk_slide(eg_ohlc["CLOSE"]), "does not have")
})

test_that("the risk figures reject a missing column", {
  expect_error(g_drawdown_slide(eg_ohlc["CLOSE"]), "does not have")
  expect_error(g_return_dist_slide(eg_ohlc["CLOSE"]), "does not have")
  expect_error(g_rolling_risk_slide(eg_ohlc["CLOSE"]), "does not have")
})

test_that("the risk figures accept renamed columns", {
  prices <- eg_ohlc
  names(prices) <- c("ticker", "day", "o", "h", "l", "px", "v")
  expect_s3_class(g_drawdown_slide(prices, "ticker", "day", "px"), "ggplot")
  expect_s3_class(g_return_dist_slide(prices, "ticker", "day", "px"), "ggplot")
  expect_s3_class(g_rolling_risk_slide(prices, "ticker", "day", "px"), "ggplot")
  expect_s3_class(g_portfolio_risk_slide(prices, "ticker", "day", "px"), "ggplot")
})

test_that("the risk figures tolerate the short eg_prices series", {
  expect_s3_class(g_drawdown_slide(eg_prices), "ggplot")
  expect_s3_class(g_return_dist_slide(eg_prices, bins = 3L), "ggplot")
  expect_s3_class(g_rolling_risk_slide(eg_prices), "ggplot")
  expect_s3_class(g_portfolio_risk_slide(eg_prices), "ggplot")
})
