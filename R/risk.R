#' Convert prices to simple returns, per trading code
#'
#' The whole package is handed prices, not returns, so every risk output converts
#' here first: one place means the outputs cannot drift apart on how returns are
#' defined. Simple arithmetic returns are used, computed within each trading code
#' so that a code change never produces a spurious return.
#'
#' The first observation of every code has no predecessor and is dropped rather
#' than emitted as an `NA` row, so downstream series are `NA`-free.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @return A `data.frame` with `SYMBOL`, `DATE` and `RET`
#' @noRd
returns_long <- function(prices, symbol, date, close) {
  d <- canonical_prices(prices, symbol, date, close)
  out <- do.call(rbind, lapply(split(d, d$SYMBOL), function(x) {
    if (nrow(x) < 2L) {
      return(NULL)
    }
    px <- x$CLOSE
    data.frame(
      SYMBOL = x$SYMBOL[-1L],
      DATE = x$DATE[-1L],
      RET = px[-1L] / px[-length(px)] - 1,
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(out)) {
    out <- data.frame(
      SYMBOL = character(0), DATE = as.Date(character(0)), RET = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  out[order(out$SYMBOL, out$DATE), , drop = FALSE]
}

#' Widen returns to a matrix over dates common to every trading code
#'
#' Correlation and covariance are undefined on a ragged panel, so only the dates
#' present for every code survive. This keeps `cor()` and `cov()` away from
#' pairwise-deleted results that would depend on which pair is being asked about.
#'
#' @param x `data.frame` of `SYMBOL`, `DATE` and `RET`, as from `returns_long()`
#' @param symbols `character` Codes to include, in order. Defaults to all.
#' @return A `numeric` matrix of returns, codes in columns and dates in rows
#' @noRd
returns_matrix <- function(x, symbols = NULL) {
  if (is.null(symbols)) {
    symbols <- sort(unique(x$SYMBOL))
  }
  by_date <- split(seq_len(nrow(x)), x$DATE)
  keep <- vapply(by_date, function(i) length(unique(x$SYMBOL[i])), integer(1)) ==
    length(symbols)
  dates <- names(by_date)[keep]
  out <- vapply(symbols, function(s) {
    sub <- x[x$SYMBOL == s, , drop = FALSE]
    sub$RET[match(dates, as.character(sub$DATE))]
  }, numeric(length(dates)))
  dimnames(out) <- list(dates, symbols)
  out
}

#' Resolve and check a benchmark trading code
#'
#' @param prices `data.frame` already reduced by `canonical_prices()`
#' @param benchmark `character` or `NULL` Trading code to compare against
#' @return The benchmark code, or `NULL`
#' @noRd
check_benchmark <- function(prices, benchmark) {
  if (is.null(benchmark)) {
    return(NULL)
  }
  assert_that(is.string(benchmark))
  syms <- unique(prices$SYMBOL)
  if (!benchmark %in% syms) {
    stop(sprintf("benchmark '%s' is not among the trading codes present.", benchmark), call. = FALSE)
  }
  benchmark
}

#' Annualized volatility of returns
#' @param x `numeric` returns
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar
#' @noRd
vol <- function(x, ann_factor = 252) {
  if (length(x) < 2L) {
    return(NA_real_)
  }
  sd(x) * sqrt(ann_factor)
}

#' Annualized downside deviation of returns below a threshold
#' @param x `numeric` returns
#' @param threshold `numeric` return level to measure below
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar, 0 when nothing falls below the threshold
#' @noRd
downside_dev <- function(x, threshold = 0, ann_factor = 252) {
  d <- x[x < threshold]
  if (length(d) < 2L) {
    return(0)
  }
  sqrt(mean((d - threshold)^2)) * sqrt(ann_factor)
}

#' Historical Value at Risk
#'
#' The `conf` quantile of the loss distribution, returned as a positive loss.
#'
#' @param x `numeric` returns
#' @param conf `numeric` confidence level in (0, 1)
#' @return `numeric` scalar
#' @noRd
var_hist <- function(x, conf = 0.95) {
  -as.numeric(quantile(x, 1 - conf, names = FALSE))
}

#' Parametric (Gaussian) Value at Risk
#' @param x `numeric` returns
#' @param conf `numeric` confidence level in (0, 1)
#' @return `numeric` scalar
#' @noRd
var_param <- function(x, conf = 0.95) {
  -(mean(x) - qnorm(conf) * sd(x))
}

#' Cornish-Fisher Value at Risk
#'
#' Gaussian Value at Risk corrected for the skewness and excess kurtosis actually
#' present in the returns, since traded returns are fat-tailed. Reported the same
#' way round as `var_hist()`, as a positive loss.
#'
#' The expansion has to be applied to the *lower*-tail quantile: fed the upper
#' tail instead, a left-skewed series would come out with a *smaller* loss than
#' the Gaussian one it is meant to correct. With skewness and excess kurtosis of
#' zero it collapses to `var_param()`.
#'
#' @param x `numeric` returns
#' @param conf `numeric` confidence level in (0, 1)
#' @return `numeric` scalar
#' @noRd
var_cf <- function(x, conf = 0.95) {
  if (length(x) < 4L) {
    return(NA_real_)
  }
  z <- qnorm(1 - conf)
  s <- skew(x)
  k <- kurt(x)
  z_cf <- z + (z^2 - 1) * s / 6 + (z^3 - 3 * z) * k / 24 - (2 * z^3 - 5 * z) * s^2 / 36
  -(mean(x) + z_cf * sd(x))
}

#' Conditional Value at Risk (expected shortfall)
#'
#' The average loss on the days that are at least as bad as historical VaR.
#'
#' @param x `numeric` returns
#' @param conf `numeric` confidence level in (0, 1)
#' @return `numeric` scalar
#' @noRd
cvar <- function(x, conf = 0.95) {
  v <- var_hist(x, conf)
  tail_losses <- x[x <= -v]
  if (!length(tail_losses)) {
    return(v)
  }
  -mean(tail_losses)
}

#' Drawdown series of a return vector
#'
#' @param x `numeric` returns
#' @return `numeric` of the same length, in `[-1, 0]`
#' @noRd
dd_series <- function(x) {
  equity <- cumprod(1 + x)
  equity / cummax(equity) - 1
}

#' Maximum drawdown
#' @param x `numeric` returns
#' @return `numeric` scalar in `[-1, 0]`
#' @noRd
max_dd <- function(x) {
  if (!length(x)) {
    return(0)
  }
  min(dd_series(x), 0)
}

#' Average drawdown over the periods that are under water
#' @param x `numeric` returns
#' @return `numeric` scalar in `[-1, 0]`
#' @noRd
avg_dd <- function(x) {
  d <- dd_series(x)
  wet <- d[d < 0]
  if (!length(wet)) {
    return(0)
  }
  mean(wet)
}

#' Longest number of consecutive periods spent under water
#' @param x `numeric` returns
#' @return `numeric` scalar
#' @noRd
dd_duration <- function(x) {
  wet <- dd_series(x) < 0
  if (!any(wet)) {
    return(0)
  }
  with(rle(wet), max(lengths[values]))
}

#' Total return compounded over the series
#' @param x `numeric` returns
#' @return `numeric` scalar
#' @noRd
total_return <- function(x) {
  prod(1 + x) - 1
}

#' Annualized (geometric) return
#' @param x `numeric` returns
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar
#' @noRd
ann_return <- function(x, ann_factor = 252) {
  (1 + total_return(x))^(ann_factor / length(x)) - 1
}

#' Annualized excess return over the risk-free rate
#' @param x `numeric` returns
#' @param rf_rate `numeric` annual risk-free rate
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar
#' @noRd
excess_return <- function(x, rf_rate = 0.02, ann_factor = 252) {
  mean(x) * ann_factor - rf_rate
}

#' Sharpe ratio
#' @param x `numeric` returns
#' @param rf_rate `numeric` annual risk-free rate
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar, 0 when volatility is zero
#' @noRd
sharpe <- function(x, rf_rate = 0.02, ann_factor = 252) {
  v <- vol(x, ann_factor)
  if (is.na(v) || v <= 0) {
    return(0)
  }
  excess_return(x, rf_rate, ann_factor) / v
}

#' Sortino ratio
#' @param x `numeric` returns
#' @param rf_rate `numeric` annual risk-free rate
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar, 0 when downside deviation is zero
#' @noRd
sortino <- function(x, rf_rate = 0.02, ann_factor = 252) {
  d <- downside_dev(x, ann_factor = ann_factor)
  if (d <= 0) {
    return(0)
  }
  excess_return(x, rf_rate, ann_factor) / d
}

#' Calmar ratio (annualized return over maximum drawdown)
#' @param x `numeric` returns
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar, 0 when there is no drawdown
#' @noRd
calmar <- function(x, ann_factor = 252) {
  mdd <- abs(max_dd(x))
  if (mdd <= 0) {
    return(0)
  }
  ann_return(x, ann_factor) / mdd
}

#' Omega ratio (gains over losses relative to a threshold)
#' @param x `numeric` returns
#' @param threshold `numeric` return level
#' @return `numeric` scalar
#' @noRd
omega <- function(x, threshold = 0) {
  gain <- sum(pmax(x - threshold, 0))
  loss <- sum(pmax(threshold - x, 0))
  if (loss <= 0) {
    return(NA_real_)
  }
  gain / loss
}

#' Skewness of returns
#' @param x `numeric` returns
#' @return `numeric` scalar
#' @noRd
skew <- function(x) {
  n <- length(x)
  if (n < 3L) {
    return(NA_real_)
  }
  z <- x - mean(x)
  sd_x <- sqrt(sum(z^2) / n)
  if (sd_x <= 0) {
    return(0)
  }
  (sum(z^3) / n) / sd_x^3
}

#' Excess kurtosis of returns (0 for a Gaussian distribution)
#' @param x `numeric` returns
#' @return `numeric` scalar
#' @noRd
kurt <- function(x) {
  n <- length(x)
  if (n < 4L) {
    return(NA_real_)
  }
  z <- x - mean(x)
  sd_x <- sqrt(sum(z^2) / n)
  if (sd_x <= 0) {
    return(0)
  }
  (sum(z^4) / n) / sd_x^4 - 3
}

#' Beta against a benchmark return vector
#' @param x `numeric` returns
#' @param mkt `numeric` benchmark returns, aligned with `x`
#' @return `numeric` scalar, `NA_real_` when either series is too short
#' @noRd
beta <- function(x, mkt) {
  if (length(x) < 2L || length(mkt) < 2L) {
    return(NA_real_)
  }
  v <- var(mkt)
  if (!is.finite(v) || v <= 0) {
    return(NA_real_)
  }
  cov(x, mkt) / v
}

#' Tracking error against a benchmark
#' @param x `numeric` returns
#' @param mkt `numeric` benchmark returns, aligned with `x`
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar
#' @noRd
te <- function(x, mkt, ann_factor = 252) {
  if (length(x) < 2L) {
    return(NA_real_)
  }
  vol(x - mkt, ann_factor)
}

#' Information ratio against a benchmark
#' @param x `numeric` returns
#' @param mkt `numeric` benchmark returns, aligned with `x`
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar, 0 when tracking error is zero
#' @noRd
ir <- function(x, mkt, ann_factor = 252) {
  e <- te(x, mkt, ann_factor)
  if (is.na(e) || e <= 0) {
    return(0)
  }
  mean(x - mkt) * ann_factor / e
}

#' Per-period signal
#'
#' @param x `numeric` series to roll over
#' @param n `integer` window width
#' @param FUN `function` applied to each window, returning one value
#' @return `numeric` the same length as `x`, `NA` while the window is incomplete
#' @noRd
roll_apply <- function(x, n, FUN) {
  L <- length(x)
  if (n > L) {
    return(rep(NA_real_, L))
  }
  out <- rep(NA_real_, L)
  for (i in n:L) {
    out[i] <- FUN(x[(i - n + 1L):i])
  }
  out
}

#' Rolling annualized volatility
#' @param x `numeric` returns
#' @param n `integer` window
#' @param ann_factor `numeric` periods per year
#' @return `numeric` the same length as `x`
#' @noRd
roll_vol <- function(x, n, ann_factor = 252) {
  roll_apply(x, n, function(w) {
    if (length(w) < 2L) NA_real_ else sd(w) * sqrt(ann_factor)
  })
}

#' Rolling Sharpe ratio
#' @param x `numeric` returns
#' @param n `integer` window
#' @param rf_rate `numeric` annual risk-free rate
#' @param ann_factor `numeric` periods per year
#' @return `numeric` the same length as `x`
#' @noRd
roll_sharpe <- function(x, n, rf_rate = 0.02, ann_factor = 252) {
  roll_apply(x, n, function(w) sharpe(w, rf_rate, ann_factor))
}

#' Rolling historical Value at Risk
#' @param x `numeric` returns
#' @param n `integer` window
#' @param conf `numeric` confidence level in (0, 1)
#' @return `numeric` the same length as `x`
#' @noRd
roll_var <- function(x, n, conf = 0.95) {
  roll_apply(x, n, function(w) var_hist(w, conf))
}

#' Rolling maximum drawdown
#' @param x `numeric` returns
#' @param n `integer` window
#' @return `numeric` the same length as `x`
#' @noRd
roll_maxdd <- function(x, n) {
  roll_apply(x, n, max_dd)
}

#' Rolling beta against a benchmark
#' @param x `numeric` returns
#' @param mkt `numeric` benchmark returns, aligned with `x`
#' @param n `integer` window
#' @return `numeric` the same length as `x`
#' @noRd
roll_beta <- function(x, mkt, n) {
  roll_apply(seq_along(x), n, function(i) beta(x[i], mkt[i]))
}

#' Classify a volatility series into low, normal and high regimes
#' @param v `numeric` annualized volatilities
#' @param low `numeric` upper bound of the low regime
#' @param high `numeric` lower bound of the high regime
#' @return A `factor` with levels `low`, `normal` and `high`
#' @noRd
vol_regime <- function(v, low = 0.10, high = 0.20) {
  out <- ifelse(is.na(v), NA_character_, ifelse(v < low, "low", ifelse(v > high, "high", "normal")))
  factor(out, levels = c("low", "normal", "high"))
}

#' Correlation matrix of returns across trading codes
#' @param x `data.frame` of `SYMBOL`, `DATE` and `RET`
#' @return A `numeric` correlation matrix
#' @noRd
cor_matrix <- function(x) {
  m <- returns_matrix(x)
  if (ncol(m) < 2L) {
    stop("Correlation needs returns for at least two trading codes.", call. = FALSE)
  }
  cor(m)
}

#' Diversification ratio of an equally weighted book
#'
#' The weighted average standalone volatility over the volatility of the
#' portfolio: 1 for a single instrument, higher as diversification helps.
#'
#' @param x `data.frame` of `SYMBOL`, `DATE` and `RET`
#' @param ann_factor `numeric` periods per year
#' @return `numeric` scalar
#' @noRd
div_ratio <- function(x, ann_factor = 252) {
  m <- returns_matrix(x)
  n <- ncol(m)
  w <- rep(1 / n, n)
  asset_vol <- apply(m, 2L, sd) * sqrt(ann_factor)
  port_vol <- as.numeric(sqrt(t(w) %*% (cov(m) * ann_factor) %*% w))
  if (!is.finite(port_vol) || port_vol <= 0) {
    return(NA_real_)
  }
  sum(w * asset_vol) / port_vol
}

#' All-in-one risk summary for one return vector
#'
#' The single place the risk numbers are computed, so the summary table and the
#' risk figures cannot disagree. Returns are raw, not rescaled.
#'
#' @param x `numeric` returns
#' @param rf_rate `numeric` annual risk-free rate
#' @param ann_factor `numeric` periods per year
#' @param mkt `numeric` benchmark returns aligned with `x`, or `NULL`
#' @return A named `numeric` vector
#' @noRd
risk_summary <- function(x, rf_rate = 0.02, ann_factor = 252, mkt = NULL) {
  out <- c(
    Return = 100 * total_return(x),
    AnnReturn = 100 * ann_return(x, ann_factor),
    Volatility = 100 * vol(x, ann_factor),
    DownsideDev = 100 * downside_dev(x, ann_factor = ann_factor),
    VaR95 = 100 * var_hist(x, 0.95),
    VaR99 = 100 * var_hist(x, 0.99),
    VaR95CF = 100 * var_cf(x, 0.95),
    CVaR95 = 100 * cvar(x, 0.95),
    MaxDrawdown = 100 * max_dd(x),
    AvgDrawdown = 100 * avg_dd(x),
    MaxDDDuration = dd_duration(x),
    Sharpe = sharpe(x, rf_rate, ann_factor),
    Sortino = sortino(x, rf_rate, ann_factor),
    Calmar = calmar(x, ann_factor),
    Omega = omega(x),
    Skewness = skew(x),
    Kurtosis = kurt(x)
  )
  if (!is.null(mkt)) {
    out <- c(
      out,
      c(
        Beta = beta(x, mkt),
        TrackingError = 100 * te(x, mkt, ann_factor),
        InformationRatio = ir(x, mkt, ann_factor)
      )
    )
  }
  out
}

#' Labels used for the rows of the risk summary table
#' @return A named `character` vector mapping metric names to row labels
#' @noRd
risk_labels <- function() {
  c(
    Return = "Return (%)",
    AnnReturn = "Annualized Return (%)",
    Volatility = "Volatility (%, ann.)",
    DownsideDev = "Downside Deviation (%, ann.)",
    VaR95 = "VaR 95% (%, hist.)",
    VaR99 = "VaR 99% (%, hist.)",
    VaR95CF = "VaR 95% (%, Cornish-Fisher)",
    CVaR95 = "CVaR 95% (%)",
    MaxDrawdown = "Max Drawdown (%)",
    AvgDrawdown = "Avg Drawdown (%)",
    MaxDDDuration = "Max Drawdown Duration (days)",
    Sharpe = "Sharpe Ratio",
    Sortino = "Sortino Ratio",
    Calmar = "Calmar Ratio",
    Omega = "Omega Ratio",
    Skewness = "Skewness",
    Kurtosis = "Excess Kurtosis",
    Beta = "Beta",
    TrackingError = "Tracking Error (%)",
    InformationRatio = "Information Ratio"
  )
}
