#' Reduce a price dataset to canonical OHLCV columns
#'
#' Like `canonical_prices()` but for candlestick figures: every output function
#' that draws a candle chart works from one shape
#' (`SYMBOL` / `DATE` / `OPEN` / `HIGH` / `LOW` / `CLOSE` / `VOLUME`) so the
#' plotting code does not have to carry column names through.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param open `character` Name of the open column
#' @param high `character` Name of the high column
#' @param low `character` Name of the low column
#' @param close `character` Name of the close column
#' @param volume `character` Name of the volume column
#' @return A `data.frame` sorted by trading code and date
#' @noRd
canonical_ohlc <- function(prices, symbol, date, open, high, low, close, volume) {
  assert_that(has_name(prices, c(symbol, date, open, high, low, close, volume)))
  out <- as.data.frame(prices[c(symbol, date, open, high, low, close, volume)])
  names(out) <- c("SYMBOL", "DATE", "OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
  out[order(out$SYMBOL, out$DATE), , drop = FALSE]
}

#' Simple moving average
#' @param x `numeric` vector
#' @param n `integer` window
#' @return `numeric` same length as `x`, `NA` for the first `n-1` positions
#' @noRd
sma <- function(x, n) {
  n <- as.integer(n)
  cs <- c(0, cumsum(x))
  out <- rep(NA_real_, length(x))
  i <- seq.int(n, length(x))
  out[i] <- (cs[i + 1L] - cs[i - n + 1L]) / n
  out
}

#' Exponential moving average
#' @param x `numeric` vector
#' @param n `integer` window
#' @return `numeric` same length as `x`
#' @noRd
ema <- function(x, n) {
  n <- as.integer(n)
  k <- 2 / (n + 1L)
  out <- rep(NA_real_, length(x))
  out[1L] <- x[1L]
  for (i in seq.int(2L, length(x))) {
    out[i] <- k * x[i] + (1 - k) * out[i - 1L]
  }
  out
}

#' Bollinger Bands
#'
#' Middle band is an `n`-day SMA; the upper and lower bands sit `k` standard
#' deviations above and below it, using a sample standard deviation.
#'
#' @param x `numeric` vector
#' @param n `integer` window
#' @param k `numeric` number of standard deviations
#' @return A `list` with `mid`, `up` and `dn` `numeric` vectors
#' @noRd
bbands <- function(x, n = 20L, k = 2) {
  n <- as.integer(n)
  mid <- sma(x, n)
  cs <- c(0, cumsum(x))
  css <- c(0, cumsum(x * x))
  sd <- rep(NA_real_, length(x))
  i <- seq.int(n, length(x))
  mean_win <- (cs[i + 1L] - cs[i - n + 1L]) / n
  ss <- css[i + 1L] - css[i - n + 1L] - n * mean_win * mean_win
  sd[i] <- sqrt(ss / (n - 1L))
  list(mid = mid, up = mid + k * sd, dn = mid - k * sd)
}

#' Relative Strength Index
#'
#' Wilder-style RSI over a rolling `n`-day window of price changes.
#'
#' @param x `numeric` vector of prices
#' @param n `integer` window
#' @return `numeric` same length as `x`, `NA` for the first `n` positions
#' @noRd
rsi <- function(x, n = 14L) {
  n <- as.integer(n)
  L <- length(x)
  out <- rep(NA_real_, L)
  d <- diff(x)
  for (t in (n + 1L):L) {
    w <- d[(t - n):(t - 1L)]
    g <- mean(pmax(w, 0))
    l <- mean(pmax(-w, 0))
    rs <- if (l == 0) Inf else g / l
    out[t] <- if (is.infinite(rs)) 100 else 100 - 100 / (1 + rs)
  }
  out
}

#' Moving Average Convergence Divergence
#'
#' `dif` is the fast EMA minus the slow EMA, `dea` is the signal EMA of `dif`,
#' and `hist` is their difference.
#'
#' @param x `numeric` vector of prices
#' @param fast `integer` fast EMA window
#' @param slow `integer` slow EMA window
#' @param sig `integer` signal EMA window
#' @return A `list` with `dif`, `dea` and `hist` `numeric` vectors
#' @noRd
macd <- function(x, fast = 12L, slow = 26L, sig = 9L) {
  dif <- ema(x, fast) - ema(x, slow)
  dea <- ema(dif, sig)
  list(dif = dif, dea = dea, hist = dif - dea)
}
