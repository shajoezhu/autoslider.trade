test_that("g_candle_slide returns a ggplot with four stacked panels", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  p <- g_candle_slide(one)
  expect_s3_class(p, "ggplot")

  # cowplot draws each stacked sub-plot (price / volume / RSI / MACD) as a
  # `GeomDrawGrob` child of the combined panel gTree, so exactly four appear.
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gname <- function(g) if (is.null(g$name)) "" else g$name
  panel <- gt$grobs[[which(vapply(gt$grobs, function(g) grepl("^panel-1", gname(g)), logical(1L)))]]
  n_sub <- sum(vapply(panel$children, function(ch) grepl("^GeomDrawGrob", gname(ch)), logical(1L)))
  expect_equal(n_sub, 4L)
})

test_that("g_candle_slide accepts renamed columns", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  names(one) <- c("ticker", "day", "o", "h", "l", "c", "v")
  p <- g_candle_slide(
    one, symbol = "ticker", date = "day", open = "o", high = "h",
    low = "l", close = "c", volume = "v"
  )
  expect_s3_class(p, "ggplot")
})

test_that("g_candle_slide rejects a missing column", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "ANZ.NZ", c("SYMBOL", "DATE", "OPEN", "HIGH", "LOW", "CLOSE")]
  expect_error(g_candle_slide(one), "does not have")
})

test_that("g_candle_slide honours ma = NULL", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  p <- g_candle_slide(one, ma = NULL)
  expect_s3_class(p, "ggplot")
})

test_that("nz_tickers holds the 168-instrument NZX universe", {
  expect_s3_class(nz_tickers, "data.frame")
  expect_equal(nrow(nz_tickers), 168L)
  expect_true("symbol" %in% names(nz_tickers))
})

test_that("eg_ohlc has the OHLCV columns", {
  expect_true(all(c("SYMBOL", "DATE", "OPEN", "HIGH", "LOW", "CLOSE", "VOLUME") %in%
    names(eg_ohlc)))
  expect_equal(length(unique(eg_ohlc$SYMBOL)), 3L)
})
