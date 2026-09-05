# Generate the example datasets shipped with the package.
# Run with: Rscript data-raw/data.R

eg_prices <- data.frame(
  SYMBOL = rep(c("AAA", "BBB", "CCC"), each = 5L),
  DATE = rep(as.Date("2026-01-01") + 0:4, times = 3L),
  CLOSE = c(
    100, 102, 101, 105, 108,
    50, 49, 48, 52, 55,
    20, 21, 22, 21, 25
  ),
  stringsAsFactors = FALSE
)

eg_trades <- data.frame(
  SYMBOL = c("AAA", "AAA", "BBB", "CCC", "CCC"),
  DATE = as.Date(c("2026-01-02", "2026-01-05", "2026-01-03", "2026-01-02", "2026-01-04")),
  SIDE = c("BUY", "SELL", "BUY", "BUY", "SELL"),
  QTY = c(100L, 100L, 50L, 200L, 200L),
  PRICE = c(102, 108, 48, 21, 21),
  stringsAsFactors = FALSE
)

usethis::use_data(eg_prices, eg_trades, overwrite = TRUE)
