# Unit tests for the internal risk layer in R/risk.R. The output functions built
# on top of it are covered in test-risk.R.

# --- prices to returns --------------------------------------------------------

test_that("returns_long derives simple returns within each trading code", {
  r <- returns_long(eg_prices, "SYMBOL", "DATE", "CLOSE")
  # The first observation of every instrument has no predecessor and is dropped.
  expect_equal(nrow(r), 12L)
  aaa <- r$RET[r$SYMBOL == "AAA"]
  expect_equal(aaa, c(102, 101, 105, 108) / c(100, 102, 101, 105) - 1)
  expect_equal(levels(factor(as.character(r$SYMBOL))), c("AAA", "BBB", "CCC"))
})

test_that("returns_long never crosses a trading code boundary", {
  # BBB closes at 55 and AAA then trades near 20: a return computed across that
  # boundary would be about -64%, and no such return appears.
  r <- returns_long(eg_prices, "SYMBOL", "DATE", "CLOSE")
  expect_true(all(abs(r$RET) < 0.5))
  expect_equal(sum(r$SYMBOL == "AAA"), 4L)
})

test_that("returns_long returns an empty frame for codes seen once", {
  one <- eg_prices[eg_prices$SYMBOL == "AAA", ][1L, ]
  empty <- returns_long(one, "SYMBOL", "DATE", "CLOSE")
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("SYMBOL", "DATE", "RET"))
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

test_that("returns_matrix names rows by date and works for one code", {
  one <- eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ]
  m <- returns_matrix(returns_long(one, "SYMBOL", "DATE", "CLOSE"), "AIA.NZ")
  expect_equal(dim(m), c(nrow(one) - 1L, 1L))
  expect_equal(rownames(m), as.character(one$DATE[-1L]))
  expect_equal(unname(m[, 1L]), one$CLOSE[-1L] / one$CLOSE[-nrow(one)] - 1)
})

test_that("check_benchmark accepts NULL or a code that is present", {
  d <- canonical_prices(eg_ohlc, "SYMBOL", "DATE", "CLOSE")
  expect_null(check_benchmark(d, NULL))
  expect_equal(check_benchmark(d, "ANZ.NZ"), "ANZ.NZ")
})

test_that("check_benchmark rejects a code that is not there and a non-string", {
  d <- canonical_prices(eg_ohlc, "SYMBOL", "DATE", "CLOSE")
  expect_error(check_benchmark(d, "ZZZ.NZ"), "not among the trading codes")
  expect_error(check_benchmark(d, 1))
})

# --- volatility and Value at Risk ---------------------------------------------

test_that("vol annualizes the standard deviation and needs two observations", {
  x <- c(0.01, -0.01, 0.02)
  expect_equal(vol(x), sd(x) * sqrt(252), tolerance = 1e-12)
  expect_equal(vol(x, ann_factor = 12), sd(x) * sqrt(12), tolerance = 1e-12)
  expect_true(is.na(vol(c(0.01))))
})

test_that("downside_dev only counts what falls below the threshold", {
  x <- c(0.02, -0.01, -0.03, 0.04)
  expect_equal(
    downside_dev(x), sqrt(mean(c(0.01, 0.03)^2)) * sqrt(252), tolerance = 1e-12
  )
  # Raising the threshold to 1% brings no extra observation in, but the distances
  # are then measured from it.
  expect_equal(
    downside_dev(x, threshold = 0.01), sqrt(mean(c(0.02, 0.04)^2)) * sqrt(252),
    tolerance = 1e-12
  )
  expect_equal(downside_dev(c(0.01, 0.02)), 0)
})

test_that("historical VaR is the loss at the confidence quantile", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  expect_equal(var_hist(x, 0.95), -as.numeric(quantile(x, 0.05)), tolerance = 1e-12)
  expect_true(var_hist(x, 0.99) > var_hist(x, 0.95))
})

test_that("parametric VaR matches the Gaussian formula", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  expect_equal(var_param(x, 0.95), qnorm(0.95) * sd(x) - mean(x), tolerance = 1e-12)
  expect_equal(var_param(x, 0.99), qnorm(0.99) * sd(x) - mean(x), tolerance = 1e-12)
})

test_that("Cornish-Fisher VaR inflates the Gaussian loss for a left tail", {
  # Nineteen quiet days and one crash: the Gaussian loss understates it.
  x <- c(rep(0.01, 19L), -0.5)
  expect_true(skew(x) < 0)
  expect_true(var_cf(x, 0.95) > var_param(x, 0.95))
  # And it needs four observations for the moments it uses.
  expect_true(is.na(var_cf(x[1:3], 0.95)))
})

test_that("CVaR averages only the losses beyond VaR", {
  x <- c(0.01, 0.02, -0.05, -0.10, 0.03)
  expect_equal(cvar(x, 0.95), -min(x), tolerance = 1e-12)
  expect_true(cvar(x, 0.95) > var_hist(x, 0.95))
})

# --- drawdown -----------------------------------------------------------------

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
})

test_that("drawdown helpers are zero when the price never falls", {
  x <- c(0.01, 0.02, 0.01)
  expect_equal(max_dd(x), 0)
  expect_equal(avg_dd(x), 0)
  expect_equal(dd_duration(x), 0)
})

test_that("max_dd of an empty series is zero rather than an error", {
  expect_equal(max_dd(numeric(0)), 0)
})

# --- levels of return ---------------------------------------------------------

test_that("total_return compounds and ann_return rescales it to a year", {
  x <- c(0.1, -0.1)
  expect_equal(total_return(x), 1.1 * 0.9 - 1, tolerance = 1e-12)
  expect_equal(ann_return(x), (1.1 * 0.9)^(252 / 2) - 1, tolerance = 1e-12)
  expect_equal(ann_return(x, ann_factor = 12), (1.1 * 0.9)^(12 / 2) - 1, tolerance = 1e-12)
})

test_that("excess_return is the annualized mean less the risk-free rate", {
  x <- c(0.01, -0.01, 0.02)
  expect_equal(excess_return(x), mean(x) * 252 - 0.02, tolerance = 1e-12)
  expect_equal(excess_return(x, rf_rate = 0), mean(x) * 252, tolerance = 1e-12)
})

# --- risk-adjusted ratios -----------------------------------------------------

test_that("sharpe and sortino divide the excess return by their own risk", {
  x <- c(0.02, -0.01, 0.03, -0.02)
  expect_equal(sharpe(x), excess_return(x) / vol(x), tolerance = 1e-12)
  expect_equal(
    sortino(x), excess_return(x) / (sqrt(mean(c(0.01, 0.02)^2)) * sqrt(252)),
    tolerance = 1e-12
  )
})

test_that("risk-adjusted ratios stay finite on a flat series", {
  flat <- rep(0, 30L)
  expect_equal(vol(flat), 0)
  expect_equal(downside_dev(flat), 0)
  expect_equal(sharpe(flat), 0)
  expect_equal(sortino(flat), 0)
  expect_equal(calmar(flat), 0)
})

test_that("calmar relates the annualized return to the maximum drawdown", {
  x <- c(0.05, -0.10, 0.05, 0.05)
  expect_equal(calmar(x), ann_return(x) / abs(max_dd(x)), tolerance = 1e-12)
  expect_equal(calmar(rep(0.01, 20L)), 0)
})

test_that("omega is the gains over the losses and undefined with no losses", {
  # Gains above 0 are 0.02 and losses 0.01; measured from 0.01 instead, the gain
  # shrinks to 0.01 while the loss grows to 0.02.
  expect_equal(omega(c(0.02, -0.01)), 2)
  expect_equal(omega(c(0.02, -0.01), threshold = 0.01), 0.5)
  expect_true(is.na(omega(rep(0.01, 5L))))
})

# --- shape of the distribution ------------------------------------------------

test_that("skewness and excess kurtosis vanish on a symmetric series", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  expect_equal(skew(x), 0, tolerance = 1e-9)
  # A uniform sample is platykurtic, so its excess kurtosis is below zero.
  expect_true(kurt(x) < 0)
})

test_that("skewness and kurtosis need three and four observations", {
  expect_true(is.na(skew(c(0.01, 0.02))))
  expect_true(is.na(kurt(c(0.01, 0.02, 0.03))))
  # A constant series has no spread, so both are zero rather than NaN.
  expect_equal(skew(rep(0.01, 10L)), 0)
  expect_equal(kurt(rep(0.01, 10L)), 0)
})

# --- against a benchmark ------------------------------------------------------

test_that("beta against itself is one and information ratio zero", {
  x <- seq(-0.05, 0.05, length.out = 60L)
  expect_equal(beta(x, x), 1, tolerance = 1e-9)
  expect_equal(te(x, x), 0, tolerance = 1e-9)
  expect_equal(ir(x, x), 0)
})

test_that("beta, tracking error and information ratio follow their formulas", {
  x <- c(0.02, -0.01, 0.03, -0.02, 0.01)
  mkt <- c(0.01, -0.005, 0.015, -0.01, 0.005)
  expect_equal(beta(x, mkt), cov(x, mkt) / var(mkt), tolerance = 1e-12)
  expect_equal(te(x, mkt), sd(x - mkt) * sqrt(252), tolerance = 1e-12)
  expect_equal(ir(x, mkt), mean(x - mkt) * 252 / te(x, mkt), tolerance = 1e-12)
})

test_that("benchmark-relative metrics degrade instead of dividing by zero", {
  # A single observation has no covariance; a benchmark that never moves has no
  # variance to divide by; a benchmark that never moves still leaves an active
  # return to report.
  expect_true(is.na(beta(c(0.01), c(0.01))))
  expect_true(is.na(beta(c(0.01, 0.02), rep(0.01, 2L))))
  expect_true(is.na(te(c(0.01), c(0.01))))
  expect_true(is.finite(ir(c(0.01, 0.02), rep(0.01, 2L))))
})

# --- rolling windows ----------------------------------------------------------

test_that("roll_apply returns NA until the window fills and beyond the series", {
  out <- roll_apply(1:5, 3L, mean)
  expect_equal(out, c(NA, NA, 2, 3, 4))
  expect_true(all(is.na(roll_apply(1:5, 10L, mean))))
})

test_that("every rolling helper equals its scalar form on the last window", {
  x <- c(0.01, 0.02, -0.01, 0.03)
  mkt <- c(0.005, 0.015, -0.005, 0.02)
  n <- 2L
  expect_equal(tail(roll_vol(x, n), 1L), vol(tail(x, n)), tolerance = 1e-12)
  expect_equal(tail(roll_sharpe(x, n), 1L), sharpe(tail(x, n)), tolerance = 1e-12)
  expect_equal(tail(roll_var(x, n), 1L), var_hist(tail(x, n)), tolerance = 1e-12)
  expect_equal(tail(roll_maxdd(x, n), 1L), max_dd(tail(x, n)), tolerance = 1e-12)
  expect_equal(
    tail(roll_beta(x, mkt, n), 1L), beta(tail(x, n), tail(mkt, n)), tolerance = 1e-12
  )
})

test_that("rolling series are NA while the window is incomplete", {
  x <- c(0.01, 0.02, -0.01, 0.03)
  expect_true(is.na(roll_vol(x, 2L)[1L]))
  expect_true(is.na(roll_sharpe(x, 2L)[1L]))
  expect_true(is.na(roll_var(x, 2L)[1L]))
  expect_true(is.na(roll_maxdd(x, 2L)[1L]))
  expect_true(is.na(roll_beta(x, x, 2L)[1L]))
})

test_that("vol_regime classifies below, inside and above the thresholds", {
  r <- vol_regime(c(0.05, 0.15, 0.30, NA), low = 0.10, high = 0.20)
  expect_equal(as.character(r), c("low", "normal", "high", NA))
  expect_equal(levels(r), c("low", "normal", "high"))
  expect_true(all(is.na(vol_regime(rep(NA_real_, 3L)))))
})

# --- portfolio ----------------------------------------------------------------

test_that("cor_matrix is symmetric with a unit diagonal", {
  cm <- cor_matrix(returns_long(eg_ohlc, "SYMBOL", "DATE", "CLOSE"))
  expect_equal(dim(cm), c(3L, 3L))
  expect_equal(unname(diag(cm)), c(1, 1, 1))
  expect_equal(cm, t(cm))
  expect_true(all(cm >= -1 & cm <= 1))
})

test_that("cor_matrix needs at least two trading codes", {
  one <- returns_long(eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ], "SYMBOL", "DATE", "CLOSE")
  expect_error(cor_matrix(one), "at least two")
})

two_codes <- function(a, b) {
  dates <- as.Date("2026-01-01") + seq_along(a) - 1L
  data.frame(
    SYMBOL = rep(c("A", "B"), each = length(a)),
    DATE = rep(dates, times = 2L),
    RET = c(a, b),
    stringsAsFactors = FALSE
  )
}

test_that("div_ratio is one alone and above one when the codes offset", {
  one <- returns_long(eg_ohlc[eg_ohlc$SYMBOL == "AIA.NZ", ], "SYMBOL", "DATE", "CLOSE")
  expect_equal(div_ratio(one), 1, tolerance = 1e-9)

  # Two codes moving against each other: the book is calmer than its parts.
  opposed <- two_codes(
    c(0.01, -0.01, 0.02, -0.02, 0.01, -0.01),
    c(-0.012, 0.011, -0.021, 0.019, -0.009, 0.012)
  )
  expect_true(div_ratio(opposed) > 1)
})

test_that("div_ratio is NA when the codes cancel out exactly", {
  # Equal weights in two mirror-image codes leave a portfolio with no volatility,
  # so the ratio of standalone risk to portfolio risk has no denominator.
  mirrored <- two_codes(
    c(0.01, -0.01, 0.02, -0.02, 0.01, -0.01),
    c(-0.01, 0.01, -0.02, 0.02, -0.01, 0.01)
  )
  expect_true(is.na(div_ratio(mirrored)))
})

# --- the shared summary -------------------------------------------------------

test_that("risk_summary reports the metrics in scale and in order", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  out <- risk_summary(x)
  expect_named(
    out,
    c(
      "Return", "AnnReturn", "Volatility", "DownsideDev", "VaR95", "VaR99", "VaR95CF",
      "CVaR95", "MaxDrawdown", "AvgDrawdown", "MaxDDDuration", "Sharpe", "Sortino",
      "Calmar", "Omega", "Skewness", "Kurtosis"
    )
  )
  expect_equal(unname(out[["Return"]]), 100 * total_return(x), tolerance = 1e-12)
  expect_equal(unname(out[["MaxDrawdown"]]), 100 * max_dd(x), tolerance = 1e-12)
  expect_equal(unname(out[["Sharpe"]]), sharpe(x), tolerance = 1e-12)
  expect_equal(unname(out[["Volatility"]]), 100 * vol(x), tolerance = 1e-12)
})

test_that("risk_summary adds the benchmark-relative metrics only with a benchmark", {
  x <- seq(-0.05, 0.05, length.out = 101L)
  mkt <- rev(x)
  with_bm <- risk_summary(x, mkt = mkt)
  expect_named(
    with_bm,
    c(names(risk_summary(x)), "Beta", "TrackingError", "InformationRatio")
  )
  expect_equal(unname(with_bm[["Beta"]]), beta(x, mkt), tolerance = 1e-12)
  expect_equal(unname(with_bm[["TrackingError"]]), 100 * te(x, mkt), tolerance = 1e-12)
})

test_that("risk_labels covers every metric the summary produces", {
  labels <- risk_labels()
  expect_true(all(names(risk_summary(c(0.01, -0.01, 0.02))) %in% names(labels)))
  expect_true(all(nzchar(labels)))
  expect_equal(labels[["MaxDrawdown"]], "Max Drawdown (%)")
})

test_that("risk_summary stays finite on a series too short for the moments", {
  out <- risk_summary(c(0.01, -0.01))
  expect_true(all(!is.nan(out)))
  expect_true(all(!is.infinite(out)))
})
