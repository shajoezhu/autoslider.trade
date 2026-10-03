# The helpers these outputs are built on are covered in test-risk-helpers.R.

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
