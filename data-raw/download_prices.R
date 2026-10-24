# Download half a year of daily prices from Yahoo through quantmod.
#
# Fetching market data is out of scope for autoslider.trade, so this script lives
# in data-raw/, which .Rbuildignore keeps out of the tarball: it is a way to feed
# the package real prices, not part of the package.
#
#   Rscript data-raw/download_prices.R
#
# The output is a long data frame with SYMBOL / DATE / OPEN / HIGH / LOW / CLOSE
# / VOLUME, the shape eg_ohlc has and every output function accepts, so
# examples/report.R can be pointed straight at it:
#
#   DATA <- "data-raw/prices.rds"
#
# Not to data/: for an R package that directory holds lazy loaded datasets, and
# R CMD check rejects an .rds there ("Files not of a type allowed in a 'data'
# directory ... Please use e.g. 'inst/extdata'"). data-raw/ is listed in
# .Rbuildignore, so downloaded prices stay out of the tarball.

library(quantmod)

TICKERS <- c("AIA.NZ", "AIR.NZ", "ANZ.NZ") # Yahoo symbols for the NZX codes
DAYS <- 180L # roughly half a year of calendar days
OUT <- file.path("data-raw", "prices.rds") # run from the package root

from <- Sys.Date() - DAYS
to <- Sys.Date()

one <- function(ticker) {
  x <- try(
    getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE),
    silent = TRUE
  )
  if (inherits(x, "try-error") || NROW(x) == 0L) {
    warning("No data returned for ", ticker, "; leaving it out.", call. = FALSE)
    return(NULL)
  }
  d <- as.data.frame(x)
  d$DATE <- as.Date(rownames(d))
  names(d) <- sub(paste0("^", ticker, "."), "", names(d))
  data.frame(
    SYMBOL = ticker,
    DATE = d$DATE,
    OPEN = as.numeric(d$Open),
    HIGH = as.numeric(d$High),
    LOW = as.numeric(d$Low),
    CLOSE = as.numeric(d$Close),
    VOLUME = as.numeric(d$Volume),
    stringsAsFactors = FALSE
  )
}

prices <- do.call(rbind, lapply(TICKERS, one))

if (is.null(prices) || nrow(prices) == 0L) {
  stop("Yahoo returned nothing for any of: ", paste(TICKERS, collapse = ", "), call. = FALSE)
}

prices <- prices[order(prices$SYMBOL, prices$DATE), ]
rownames(prices) <- NULL
saveRDS(prices, OUT)

message(
  "Saved ", nrow(prices), " rows to ", normalizePath(OUT), "\n",
  paste(
    vapply(split(prices, prices$SYMBOL), function(d) {
      sprintf(
        "  %s: %d rows, %s to %s", d$SYMBOL[1L], nrow(d),
        min(d$DATE), max(d$DATE)
      )
    }, character(1L)),
    collapse = "\n"
  )
)
