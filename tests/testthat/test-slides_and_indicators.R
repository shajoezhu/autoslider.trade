library(testthat)

test_that("g_equity_slide returns a ggplot and respects normalize", {
  p <- g_equity_slide(eg_prices)
  expect_s3_class(p, "ggplot")
  p_abs <- g_equity_slide(eg_prices, normalize = FALSE)
  expect_s3_class(p_abs, "ggplot")
})

test_that("t_performance_slide returns an rtables table with one column per symbol", {
  tab <- t_performance_slide(eg_prices)
  expect_true(inherits(tab, "TableTree"))
  expect_equal(length(rtables::col_paths(tab)), length(unique(eg_prices$SYMBOL)))
})

test_that("l_trades_slide returns a listing_df with a NOTIONAL column", {
  lst <- l_trades_slide(eg_trades)
  expect_s3_class(lst, "listing_df")
  expect_true("NOTIONAL" %in% names(lst))
})

test_that("indicators: sma is the rolling mean with leading NAs", {
  x <- c(1, 2, 3, 4)
  s <- sma(x, 2L)
  expect_length(s, 4L)
  expect_true(is.na(s[1L]))
  expect_equal(s[2:4], c(1.5, 2.5, 3.5))
})

test_that("indicators: bbands returns three equal-length vectors", {
  b <- bbands(1:30, 20L)
  expect_named(b, c("mid", "up", "dn"))
  expect_length(b$mid, 30L)
  expect_length(b$up, 30L)
  expect_length(b$dn, 30L)
})

test_that("indicators: rsi stays in [0, 100] where defined", {
  r <- rsi(1:30, 14L)
  expect_length(r, 30L)
  expect_true(all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100, na.rm = TRUE))
})

test_that("indicators: macd returns three equal-length vectors", {
  m <- macd(1:40)
  expect_named(m, c("dif", "dea", "hist"))
  expect_length(m$dif, 40L)
  expect_equal(m$hist, m$dif - m$dea)
})
