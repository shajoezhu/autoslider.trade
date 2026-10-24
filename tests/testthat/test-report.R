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

test_that("render_stock_report adds the option and fundamentals slides when given", {
  skip_if_not_installed("rsvg")
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  spot <- one$CLOSE[nrow(one)]
  quotes <- data.frame(
    STRIKE = c(spot, spot),
    TYPE = c("call", "put"),
    TTE = c(20 / 252, 20 / 252),
    PRICE = c(
      bs_price(spot, spot, 20 / 252, 0.35, 0.02, "call"),
      bs_price(spot, spot, 20 / 252, 0.35, 0.02, "put")
    )
  )
  fin <- data.frame(
    METRIC = rep(c("Revenue (m)", "Operating margin (%)"), each = 2L),
    PERIOD = rep(c("FY24", "FY25"), times = 2L),
    VALUE = c(120, 135, 11.2, 12.8)
  )

  plain <- render_stock_report(one, "AIR.NZ", outfile = tempfile(fileext = ".pptx"))
  full <- render_stock_report(
    one, "AIR.NZ", options = quotes, financials = fin, outfile = tempfile(fileext = ".pptx")
  )

  count <- function(path) {
    sum(grepl("^ppt/slides/slide[0-9]+\\.xml$", utils::unzip(path, list = TRUE)$Name))
  }
  # Two option slides and one fundamentals slide on top of the plain deck.
  expect_true(count(full) > count(plain))
})

# --- figures on the slide -----------------------------------------------------

png_dims <- function(path) {
  raw <- readBin(path, "raw", n = 33L)
  c(
    width = sum(as.integer(raw[17:20]) * 256^(3:0)),
    height = sum(as.integer(raw[21:24]) * 256^(3:0))
  )
}

# Slide files do not necessarily come out in display order, so the order is read
# from presentation.xml rather than from the file names.
display_titles <- function(path) {
  tmp <- file.path(tempdir(), basename(tempfile("deck")))
  dir.create(tmp, showWarnings = FALSE)
  utils::unzip(path, exdir = tmp)

  pres <- paste(readLines(file.path(tmp, "ppt/presentation.xml")), collapse = "")
  ids <- regmatches(pres, gregexpr('<p:sldId[^>]*r:id="[^"]+"', pres))[[1L]]
  ids <- vapply(
    regmatches(ids, gregexpr('r:id="[^"]+"', ids)),
    function(x) sub('r:id="', "", sub('"$', "", x)), character(1L)
  )
  rels <- paste(readLines(file.path(tmp, "ppt/_rels/presentation.xml.rels")), collapse = "")
  nodes <- regmatches(rels, gregexpr("<Relationship [^>]*/?>", rels))[[1L]]
  by_id <- character(0)
  for (n in nodes) {
    id <- sub('Id="', "", sub('"$', "", regmatches(n, regexpr('Id="[^"]+"', n))))
    target <- sub('Target="', "", sub('"$', "", regmatches(n, regexpr('Target="[^"]+"', n))))
    by_id[id] <- target
  }

  vapply(seq_along(ids), function(k) {
    x <- paste(readLines(file.path(tmp, "ppt", by_id[[ids[k]]])), collapse = "")
    tt <- regmatches(x, gregexpr("<a:t>[^<]*</a:t>", x))[[1L]]
    if (!length(tt)) "" else gsub("<a:t>|</a:t>", "", tt[1L])
  }, character(1L))
}

media_names <- function(path) {
  utils::unzip(path, list = TRUE)$Name
}

test_that("figure_png writes a raster of the size asked for", {
  file <- figure_png(g_equity_slide(eg_prices), tempfile(fileext = ".png"), dpi = 150L)
  expect_true(file.exists(file))
  expect_equal(unname(png_dims(file)), c(9 * 150L, 5 * 150L))
})

test_that("the report rasterizes its figures so their labels survive", {
  skip_if_not_installed("rsvg")
  raster <- render_stock_report(eg_ohlc, "AIR.NZ", outfile = tempfile(fileext = ".pptx"))

  media <- media_names(raster)
  expect_true(sum(grepl("\\.png$", media)) >= 5L)
  # No slide is left holding the vector figure PowerPoint cannot read.
  expect_length(svg_slide_positions(raster), 0L)

  expect_true(any(grepl("Price History", display_titles(raster), fixed = TRUE)))
})

test_that("the report keeps the figures where they belong and keeps the count", {
  skip_if_not_installed("rsvg")
  plain <- render_stock_report(
    eg_ohlc, "AIR.NZ", fig_dpi = NA, outfile = tempfile(fileext = ".pptx")
  )
  raster <- render_stock_report(eg_ohlc, "AIR.NZ", outfile = tempfile(fileext = ".pptx"))

  expect_equal(length(display_titles(plain)), length(display_titles(raster)))
  expect_equal(display_titles(plain), display_titles(raster))
  # fig_dpi = NA is the escape hatch: the vector figures stay.
  expect_true(length(svg_slide_positions(plain)) > 0L)
})

test_that("render_stock_report reads prices from an .rds path", {
  skip_if_not_installed("rsvg")
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  path <- tempfile(fileext = ".rds")
  saveRDS(one, path)
  out <- render_stock_report(path, "AIR.NZ", outfile = tempfile(fileext = ".pptx"))
  expect_true(file.exists(out))
})
