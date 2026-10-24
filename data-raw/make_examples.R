# Generate the two example datasets that cannot be derived from prices.
#
# eg_ohlc / eg_prices / eg_trades cover everything the price-driven outputs need,
# but option quotes and company fundamentals are data no price series can produce,
# and this package does not fetch either. Without an example, the two tools that
# need them (option_table, fundamentals_table) can never be tried from an agent:
# every call fails on the first argument.
#
# Both are priced/built deterministically so the example is self-consistent: the
# quotes are Black-Scholes prices at a known volatility, so the implied vol the
# table back-solves comes back to roughly that volatility instead of being noise.
#
#   Rscript data-raw/make_examples.R
#
# Writes data/eg_options.rda and data/eg_financials.rda.

suppressMessages(pkgload::load_all(".", quiet = TRUE))

set.seed(20261023)

# ---- option quotes ----------------------------------------------------------
# Spot and volatility taken from the AIA.NZ series in eg_ohlc, so the example
# hangs together with the prices the other tools use.

rets <- returns_long(eg_ohlc, "SYMBOL", "DATE", "CLOSE")
rets <- rets[rets$SYMBOL == "AIA.NZ", ]
spot <- tail(eg_ohlc$CLOSE[eg_ohlc$SYMBOL == "AIA.NZ"], 1)
sigma <- sd(rets$RET) * sqrt(252)
r <- 0.02

strikes <- round(seq(spot * 0.85, spot * 1.15, length.out = 7), 2)
ttes <- c(0.25, 0.5, 1)

eg_options <- do.call(rbind, lapply(ttes, function(t) {
  do.call(rbind, lapply(c("call", "put"), function(type) {
    # bs_price() asserts on scalars, so price one strike at a time.
    data.frame(
      STRIKE = strikes,
      TYPE = type,
      PRICE = round(
        vapply(strikes, function(k) bs_price(spot, k, t, sigma, r, type), numeric(1)),
        4
      ),
      TTE = t
    )
  }))
}))
rownames(eg_options) <- NULL

# ---- fundamentals -----------------------------------------------------------
# A metrics-by-period trend table: the shape t_fundamentals_slide() pivots.

metrics <- c("Revenue (NZ$m)", "Gross margin (%)", "Operating margin (%)", "Free cash flow (NZ$m)")
periods <- c("FY2023", "FY2024", "FY2025")

base <- list(
  "Revenue (NZ$m)" = c(812.4, 934.1, 1088.6),
  "Gross margin (%)" = c(38.2, 39.6, 41.1),
  "Operating margin (%)" = c(12.4, 14.0, 15.8),
  "Free cash flow (NZ$m)" = c(96.3, 121.7, 154.2)
)

eg_financials <- do.call(rbind, lapply(metrics, function(m) {
  data.frame(METRIC = m, PERIOD = periods, VALUE = base[[m]], stringsAsFactors = FALSE)
}))
rownames(eg_financials) <- NULL

# ---- write -----------------------------------------------------------------

usethis::use_data(eg_options, eg_financials, overwrite = TRUE)

cat(sprintf(
  "eg_options: %d rows, spot %.2f, sigma %.1f%%\n",
  nrow(eg_options), spot, 100 * sigma
))
cat(sprintf("eg_financials: %d rows, %d metrics x %d periods\n",
            nrow(eg_financials), length(metrics), length(periods)))
