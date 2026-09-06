# Generate the example datasets shipped with the package.
# Run with: Rscript data-raw/data.R

# --- NZ trading universe (real, copied from homepage-stock) -------------------
# The source of truth is `nz_list.csv`, the same 168 instruments the user tracks
# on their `homepage-stock` kanban. Kept verbatim, non-ASCII names and all.
nz_tickers <- utils::read.csv(
  "data-raw/nz_list.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)
names(nz_tickers) <- c("name", "symbol")
nz_tickers <- nz_tickers[order(nz_tickers$symbol), ]
rownames(nz_tickers) <- NULL

# --- Synthetic OHLCV example for candlestick figures --------------------------
# `homepage-stock` stores no price history (only the ticker list), and fetching
# market data is out of scope for this package. So the candlestick demo and its
# tests use a deterministic synthetic series built to look like daily OHLCV.
set.seed(20260101L)
eg_ohlc <- local({
  tickers <- c("AIA.NZ", "AIR.NZ", "ANZ.NZ")
  bases <- c(9, 3, 25)
  n_days <- 120L

  # 120 consecutive business days ending mid-2026.
  all_days <- seq(as.Date("2026-01-01"), by = 1L, length.out = 180L)
  wday <- as.integer(format(all_days, "%u"))
  days <- all_days[wday <= 5L][seq_len(n_days)]

  rows <- list()
  for (i in seq_along(tickers)) {
    sym <- tickers[i]
    px <- bases[i]
    series <- numeric(n_days)
    for (d in seq_len(n_days)) {
      px <- px * exp(rnorm(1L, mean = 0.0005, sd = 0.02))
      series[d] <- px
    }
    close <- round(series, 2L)
    prev <- c(close[1L], close[-n_days])
    open <- round(prev * (1 + rnorm(n_days, 0, 0.005)), 2L)
    hi <- pmax(open, close) * (1 + abs(rnorm(n_days, 0, 0.01)))
    lo <- pmin(open, close) * (1 - abs(rnorm(n_days, 0, 0.01)))
    hi <- round(hi, 2L)
    lo <- round(lo, 2L)
    vol <- round(runif(n_days, 1e5, 5e6))
    rows[[sym]] <- data.frame(
      SYMBOL = sym, DATE = days,
      OPEN = open, HIGH = hi, LOW = lo, CLOSE = close, VOLUME = vol,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
})

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

usethis::use_data(
  nz_tickers, eg_ohlc, eg_prices, eg_trades,
  overwrite = TRUE
)
