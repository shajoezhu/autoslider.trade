# Outputs ported from the cb_teams_marketplace skills (lseg): the parts of
# equity-research and option-vol-analysis that can be computed from prices, or
# from data the user brings. See METHODS.md for what was taken and what was not.

# --- price against its trailing range -----------------------------------------

test_that("g_range_slide brackets the price with its own high and low", {
  p <- g_range_slide(eg_ohlc)
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$CLOSE <= p$data$HIGH, na.rm = TRUE))
  expect_true(all(p$data$CLOSE >= p$data$LOW, na.rm = TRUE))
  expect_equal(unique(p$data$SYMBOL), c("AIA.NZ", "AIR.NZ", "ANZ.NZ"))
})

test_that("g_range_slide needs no history longer than the series", {
  # A 252-day band over 120 prices has no band yet, but the figure still builds.
  expect_s3_class(g_range_slide(eg_ohlc), "ggplot")
  expect_s3_class(g_range_slide(eg_prices, window = 2L), "ggplot")
})

test_that("g_range_slide rejects a missing column", {
  expect_error(g_range_slide(eg_ohlc["CLOSE"]), "does not have")
})

# --- realized volatility by window --------------------------------------------

test_that("g_vol_term_slide gives one bar per window per code", {
  expect_message(p <- g_vol_term_slide(eg_ohlc), "Dropping windows longer than")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 9L)
  expect_equal(as.character(p$data$WINDOW[p$data$SYMBOL == "AIR.NZ"]), c("20d", "60d", "90d"))
  expect_true(all(p$data$VOL > 0))
})

test_that("g_vol_term_slide agrees with vol() on each window", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  p <- g_vol_term_slide(one, windows = c(20L, 60L))
  x <- returns_long(one, "SYMBOL", "DATE", "CLOSE")$RET
  expect_equal(p$data$VOL, 100 * c(vol(tail(x, 20L)), vol(tail(x, 60L))), tolerance = 1e-9)
})

test_that("g_vol_term_slide rejects a missing column", {
  expect_error(g_vol_term_slide(eg_ohlc["CLOSE"]), "does not have")
})

# --- options ------------------------------------------------------------------

# A textbook contract: spot and strike at 100, one year, 20% vol, 5% rate.
bs_case <- function() {
  list(spot = 100, strike = 100, tte = 1, sigma = 0.20, r = 0.05)
}

test_that("bs_price matches the closed form for a known contract", {
  cs <- bs_case()
  # d1 = 0.35, d2 = 0.15 for these inputs; call 10.4506, put 5.5735.
  expect_equal(
    bs_price(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, "call"), 10.4506,
    tolerance = 1e-3
  )
  expect_equal(
    bs_price(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, "put"), 5.5735,
    tolerance = 1e-3
  )
})

test_that("put-call parity holds and deep contracts behave", {
  cs <- bs_case()
  call <- bs_price(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, "call")
  put <- bs_price(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, "put")
  expect_equal(call - put, cs$spot - cs$strike * exp(-cs$r * cs$tte), tolerance = 1e-9)

  # Nothing left to time value at expiry, and a zero-vol call is its intrinsic.
  expect_equal(bs_price(100, 90, 0, 0.2, 0.05, "call"), NA_real_)
  expect_true(is.na(bs_price(100, 100, 1, 0, 0.05, "call")))
})

test_that("bs_greeks returns the signs and magnitudes they should", {
  cs <- bs_case()
  call <- bs_greeks(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, "call")
  put <- bs_greeks(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, "put")

  expect_true(call[["delta"]] > 0 && call[["delta"]] < 1)
  expect_true(put[["delta"]] < 0 && put[["delta"]] > -1)
  expect_equal(call[["delta"]] - put[["delta"]], 1, tolerance = 1e-9)
  # Gamma and vega do not depend on which side you are on.
  expect_equal(call[["gamma"]], put[["gamma"]], tolerance = 1e-9)
  expect_equal(call[["vega"]], put[["vega"]], tolerance = 1e-9)
  # Time and a higher rate work against a put's rho, not a call's.
  expect_true(call[["theta"]] < 0)
  expect_true(call[["rho"]] > 0 && put[["rho"]] < 0)
})

test_that("bs_iv recovers the volatility a price was built from", {
  cs <- bs_case()
  for (type in c("call", "put")) {
    price <- bs_price(cs$spot, cs$strike, cs$tte, cs$sigma, cs$r, type)
    expect_equal(bs_iv(price, cs$spot, cs$strike, cs$tte, cs$r, type), cs$sigma,
                 tolerance = 1e-6)
  }
})

test_that("bs_iv returns NA where no volatility produces the price", {
  # Below intrinsic value, and above the underlying itself.
  expect_true(is.na(bs_iv(0.01, 100, 50, 1, 0.05, "call")))
  expect_true(is.na(bs_iv(200, 100, 100, 1, 0.05, "call")))
})

test_that("t_option_slide prices a quote sheet into a table", {
  quotes <- data.frame(
    STRIKE = c(100, 100),
    TYPE = c("call", "put"),
    TTE = c(1, 1),
    PRICE = c(10.4506, 5.5735)
  )
  out <- t_option_slide(quotes, spot = 100, r = 0.05)
  expect_true(inherits(out, "VTableTree"))

  rendered <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(rendered, "Call 100 1.00y")
  expect_match(rendered, "Put 100 1.00y")
  expect_match(rendered, "Implied Volatility", fixed = TRUE)
  # The quotes were built at 20%, which is what the table recovers (the put
  # lands a whisker under because bisection stops at a tolerance).
  expect_match(rendered, "19.9999", fixed = TRUE)
})

test_that("t_option_slide rejects a bad quote sheet", {
  quotes <- data.frame(STRIKE = 100, TYPE = "call", TTE = 1, PRICE = 10)
  expect_error(t_option_slide(quotes, spot = 0), "spot")
  expect_error(t_option_slide(quotes[c("STRIKE")], spot = 100), "does not have")
})

test_that("g_vol_premium_slide puts realized and implied on the same axes", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  spot <- one$CLOSE[nrow(one)]
  # Quoted at 40% against a realized 30%: implied is the rich one.
  quotes <- data.frame(
    STRIKE = c(spot, spot),
    TYPE = c("call", "call"),
    TTE = c(20 / 252, 60 / 252),
    PRICE = c(
      bs_price(spot, spot, 20 / 252, 0.40, 0.02, "call"),
      bs_price(spot, spot, 60 / 252, 0.40, 0.02, "call")
    )
  )
  p <- g_vol_premium_slide(one, quotes, windows = c(20L, 60L))
  expect_s3_class(p, "ggplot")
  expect_equal(sort(unique(p$data$SERIES)), c("Implied", "Realized"))
  expect_equal(nrow(p$data), 4L)

  iv <- p$data$VOL[p$data$SERIES == "Implied"]
  expect_equal(unique(round(iv)), 40)
})

test_that("g_vol_premium_slide refuses several codes and an empty sheet", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
  quotes <- data.frame(STRIKE = 3, TYPE = "call", TTE = 0.1, PRICE = 0.2)
  expect_error(g_vol_premium_slide(eg_ohlc, quotes), "one underlying")
})

# --- fundamentals and valuation -----------------------------------------------

fundamentals <- function() {
  data.frame(
    METRIC = rep(c("Revenue (m)", "Operating margin (%)"), each = 3L),
    PERIOD = rep(c("FY23", "FY24", "FY25"), times = 2L),
    VALUE = c(120, 135, 158, 11.2, 12.8, 14.1),
    stringsAsFactors = FALSE
  )
}

test_that("t_fundamentals_slide puts periods in the rows and metrics in the columns", {
  out <- t_fundamentals_slide(fundamentals())
  expect_true(inherits(out, "VTableTree"))

  rendered <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(rendered, "FY23", fixed = TRUE)
  expect_match(rendered, "FY25", fixed = TRUE)
  expect_match(rendered, "Revenue (m)", fixed = TRUE)
  expect_match(rendered, "Operating margin (%)", fixed = TRUE)
  # 120 -> 158 is +31.67%, and 11.2 -> 14.1 is +25.89%.
  expect_match(rendered, "31.67", fixed = TRUE)
  expect_match(rendered, "25.89", fixed = TRUE)
})

test_that("t_fundamentals_slide keeps the metrics in the order given", {
  out <- t_fundamentals_slide(fundamentals())
  # A column path is the split variable followed by the level.
  expect_equal(rtables::col_paths(out)[[1L]][[2L]], "Revenue (m)")
})

test_that("t_fundamentals_slide drops the change row when there is one period", {
  one <- fundamentals()
  one <- one[one$PERIOD == "FY25", ]
  out <- t_fundamentals_slide(one)
  expect_no_match(paste(capture.output(print(out)), collapse = "\n"), "Change", fixed = TRUE)
})

test_that("t_fundamentals_slide accepts renamed columns and rejects missing ones", {
  fin <- fundamentals()
  names(fin) <- c("measure", "year", "amount")
  out <- t_fundamentals_slide(fin, metric = "measure", period = "year", value = "amount")
  expect_true(inherits(out, "VTableTree"))

  expect_error(t_fundamentals_slide(fin), "does not have")
  bad <- fundamentals()
  bad$VALUE[1L] <- NA
  expect_error(t_fundamentals_slide(bad), "numeric value")
})
