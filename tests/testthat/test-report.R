test_that("text_rows splits a long paragraph and labels it once", {
  rows <- text_rows("Risk", paste(rep("word", 60L), collapse = " "), width = 40L)
  expect_true(nrow(rows) > 1L)
  expect_equal(rows$SECTION[1L], "Risk")
  expect_true(all(rows$SECTION[-1L] == ""))
  expect_true(all(nchar(rows$TEXT) <= 40L))
})

test_that("stock_commentary reads one instrument and one benchmark", {
  pair <- eg_ohlc[eg_ohlc$SYMBOL %in% c("AIR.NZ", "ANZ.NZ"), ]
  out <- stock_commentary(pair, "AIR.NZ", benchmark = "ANZ.NZ", short_window = 63L)

  expect_named(out, c("Long term", "Short term", "Risk", "Versus benchmark", "Watch"))
  expect_true(all(nzchar(out)))
  expect_match(out[["Long term"]], "AIR.NZ")
  expect_match(out[["Versus benchmark"]], "ANZ.NZ")
  expect_true(all(!grepl("  ", out, fixed = TRUE)))
  expect_true(all(!grepl("NaN|NA|Inf", out)))
})

test_that("stock_commentary drops the benchmark section when none is named", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  out <- stock_commentary(one, "AIR.NZ", short_window = 63L)
  expect_false("Versus benchmark" %in% names(out))
  expect_true("Watch" %in% names(out))
})

test_that("stock_commentary survives a series with a handful of prices", {
  out <- stock_commentary(eg_prices[eg_prices$SYMBOL == "BBB", ], "BBB", short_window = 63L)
  expect_true(all(nzchar(out)))
  expect_true(all(!grepl("NaN|Inf", out)))
})

test_that("t_horizon_slide puts the windows in the columns", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  expect_message(out <- t_horizon_slide(one), "Dropping windows longer than")
  expect_true(inherits(out, "VTableTree"))

  rendered <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(rendered, "1M", fixed = TRUE)
  expect_match(rendered, "3M", fixed = TRUE)
  expect_match(rendered, "Full", fixed = TRUE)
  expect_match(rendered, "Sharpe Ratio", fixed = TRUE)
  expect_no_match(rendered, "1Y", fixed = TRUE)
})

test_that("t_horizon_slide refuses several trading codes and a missing column", {
  expect_error(t_horizon_slide(eg_ohlc), "one trading code")
  expect_error(t_horizon_slide(eg_ohlc["CLOSE"]), "does not have")
})

test_that("t_horizon_slide accepts renamed columns", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  names(one) <- c("ticker", "day", "o", "h", "l", "px", "v")
  out <- t_horizon_slide(one, symbol = "ticker", date = "day", close = "px", windows = 21L)
  expect_true(inherits(out, "VTableTree"))
})

test_that("render_stock_report rejects a ticker or benchmark that is not there", {
  expect_error(
    render_stock_report(eg_ohlc, "ZZZ.NZ", outfile = tempfile(fileext = ".pptx")),
    "not among those in prices"
  )
  expect_error(
    render_stock_report(
      eg_ohlc, "AIR.NZ", benchmark = "ZZZ.NZ", outfile = tempfile(fileext = ".pptx")
    ),
    "not among those in prices"
  )
})

test_that("render_stock_report leaves out the candlestick slide without OHLCV", {
  skip_if_not_installed("rsvg")
  expect_message(
    path <- render_stock_report(eg_prices, "BBB", outfile = tempfile(fileext = ".pptx")),
    "leaving out the candlestick slide"
  )
  expect_true(file.exists(path))
})

test_that("render_stock_report writes one deck holding every slide", {
  skip_if_not_installed("rsvg")
  path <- render_stock_report(
    eg_ohlc, "AIR.NZ", benchmark = "ANZ.NZ", outfile = tempfile(fileext = ".pptx")
  )
  expect_true(file.exists(path))

  slides <- grep("^ppt/slides/slide[0-9]+\\.xml$", utils::unzip(path, list = TRUE)$Name)
  # The analysis, the horizon table, the risk summary and its continuations
  # alone are more than one slide, so a single-slide file means it broke.
  expect_true(length(slides) > 5L)
})

test_that("render_stock_report reads prices from an .rds path", {
  skip_if_not_installed("rsvg")
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  path <- tempfile(fileext = ".rds")
  saveRDS(one, path)
  out <- render_stock_report(path, "AIR.NZ", outfile = tempfile(fileext = ".pptx"))
  expect_true(file.exists(out))
})
