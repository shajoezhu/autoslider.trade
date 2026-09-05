test_that("g_equity_slide returns a ggplot and normalizes to a common start", {
  p <- g_equity_slide(eg_prices)
  expect_s3_class(p, "ggplot")

  d <- p$data[order(p$data$SYMBOL, p$data$DATE), ]
  firsts <- d$VALUE[!duplicated(d$SYMBOL)]
  expect_equal(firsts, c(1, 1, 1))
})

test_that("g_equity_slide can plot absolute prices", {
  p <- g_equity_slide(eg_prices, normalize = FALSE)
  expect_s3_class(p, "ggplot")
  expect_true(max(p$data$VALUE) > 1)
})

test_that("g_equity_slide accepts renamed columns", {
  prices <- eg_prices
  names(prices) <- c("ticker", "day", "px")
  p <- g_equity_slide(prices, symbol = "ticker", date = "day", close = "px")
  expect_s3_class(p, "ggplot")
})

test_that("g_equity_slide rejects a missing column", {
  prices <- eg_prices["CLOSE"]
  expect_error(g_equity_slide(prices), "does not have")
})

test_that("l_trades_slide returns a listing with the computed notional", {
  out <- l_trades_slide(eg_trades)
  expect_s3_class(out, "listing_df")
  expect_true("NOTIONAL" %in% names(out))
  expect_equal(as.numeric(out$NOTIONAL), c(10200, 10800, 2400, 4200, 4200))
})

test_that("l_trades_slide rejects a missing column", {
  trades <- eg_trades[c("SYMBOL", "DATE")]
  expect_error(l_trades_slide(trades), "does not have")
})

test_that("t_performance_slide returns a table over the three instruments", {
  out <- t_performance_slide(eg_prices)
  expect_true(inherits(out, "VTableTree"))

  rendered <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(rendered, "AAA")
  expect_match(rendered, "BBB")
  expect_match(rendered, "CCC")
  expect_match(rendered, "Performance Summary", fixed = TRUE)
})

test_that("t_performance_slide rejects a missing column", {
  prices <- eg_prices[c("SYMBOL", "DATE")]
  expect_error(t_performance_slide(prices), "does not have")
})
