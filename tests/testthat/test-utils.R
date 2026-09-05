test_that("canonical_prices renames columns and sorts by symbol then date", {
  scrambled <- eg_prices[c(6:15, 1:5), ]
  out <- canonical_prices(scrambled, "SYMBOL", "DATE", "CLOSE")

  expect_named(out, c("SYMBOL", "DATE", "CLOSE"))
  expect_equal(out$SYMBOL, rep(c("AAA", "BBB", "CCC"), each = 5L))
  expect_equal(out$DATE, rep(as.Date("2026-01-01") + 0:4, times = 3L))
})

test_that("canonical_prices rejects a missing column", {
  expect_error(canonical_prices(eg_prices, "TICKER", "DATE", "CLOSE"), "does not have")
})

test_that("code_col defaults to SYMBOL and honours the option", {
  expect_equal(code_col(), "SYMBOL")

  old <- getOption("autoslider.trade.code_col")
  on.exit(options(autoslider.trade.code_col = old))
  options(autoslider.trade.code_col = "ticker")
  expect_equal(code_col(), "ticker")
})
