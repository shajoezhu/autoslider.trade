#' Break a paragraph into rows a slide can hold
#'
#' A slide cell does not wrap, so a paragraph is split into chunks that fit and
#' only the first chunk carries the section label.
#'
#' @param section `character` label shown once, on the first row
#' @param text `character` paragraph
#' @param width `integer` characters per row
#' @return A `data.frame` with `SECTION` and `TEXT`
#' @noRd
text_rows <- function(section, text, width = 95L) {
  chunks <- strwrap(text, width = width)
  if (!length(chunks)) {
    chunks <- ""
  }
  data.frame(
    SECTION = c(section, rep("", length(chunks) - 1L)),
    TEXT = chunks,
    stringsAsFactors = FALSE
  )
}

#' Read one instrument's history and say what it shows
#'
#' The wording is derived from the same metrics the figures and tables show, so
#' the prose cannot contradict the numbers. Every sentence is mechanical: no
#' forecast is made, nothing is recommended.
#'
#' @param prices `data.frame` of prices for one trading code
#' @param ticker `character` trading code being reported on
#' @param benchmark `character` trading code compared against, or `NULL`
#' @param short_window `integer` periods treated as the short term
#' @param rf_rate `numeric` Annual risk-free rate
#' @param ann_factor `numeric` Number of periods per year
#' @param symbol,date,close `character` column names
#' @return A named `character` vector of paragraphs
#' @noRd
stock_commentary <- function(prices,
                             ticker,
                             benchmark = NULL,
                             short_window = 63L,
                             rf_rate = 0.02,
                             ann_factor = 252,
                             symbol = code_col(),
                             date = "DATE",
                             close = "CLOSE") {
  d <- canonical_prices(prices, symbol, date, close)
  # `prices` holds the instrument and, when one is named, its benchmark: both
  # are needed to align the two return series.
  px <- d$CLOSE[d$SYMBOL == ticker]
  rets <- returns_long(prices, symbol, date, close)
  x <- rets$RET[rets$SYMBOL == ticker]
  n <- length(x)
  assert_that(n > 0L, msg = "No returns to comment on: too few price observations.")

  short_n <- min(short_window, n)
  full <- risk_summary(x, rf_rate, ann_factor)
  short <- risk_summary(tail(x, short_n), rf_rate, ann_factor)

  ma_fast_n <- min(50L, length(px))
  ma_fast <- tail(sma(px, ma_fast_n), 1L)
  ma_slow <- if (length(px) >= 200L) tail(sma(px, 200L), 1L) else NA_real_
  rsi_now <- tail(rsi(px, 14L), 1L)
  below_peak <- tail(dd_series(x), 1L)
  vol_now <- tail(roll_vol(x, min(short_window, n), ann_factor), 1L)

  pct <- function(v, digits = 1L) paste0(formatC(v, format = "f", digits = digits), "%")
  num <- function(v, digits = 2L) formatC(v, format = "f", digits = digits)
  # Parts that do not apply are dropped rather than left as an empty sentence,
  # so the prose never carries a double space where one was omitted.
  say <- function(...) paste(c(...)[nzchar(c(...))], collapse = " ")

  # --- long term -------------------------------------------------------------
  trend <- if (is.na(ma_fast)) {
    ""
  } else {
    paste0(
      "The last price sits ", if (tail(px, 1L) >= ma_fast) "above" else "below",
      " its ", ma_fast_n, "-period average."
    )
  }
  cross <- if (is.na(ma_slow) || is.na(ma_fast)) {
    ""
  } else if (ma_fast > ma_slow) {
    "The faster average is above the slower one, the configuration usually read as an uptrend."
  } else {
    "The faster average is below the slower one, the configuration usually read as a downtrend."
  }
  lt_word <- if (full[["AnnReturn"]] >= 10 && full[["Sharpe"]] >= 0.5) {
    "constructive"
  } else if (full[["AnnReturn"]] > 0) {
    "modestly positive"
  } else if (full[["AnnReturn"]] > -5) {
    "flat to slightly negative"
  } else {
    "negative"
  }
  long_term <- say(
    "Over the", n, "periods on file,", ticker, "returned", pct(full[["Return"]]),
    paste0("(", pct(full[["AnnReturn"]]), " annualized) against"), pct(full[["Volatility"]]),
    "annualized volatility, a maximum drawdown of", pct(full[["MaxDrawdown"]]),
    "and a Sharpe ratio of", paste0(num(full[["Sharpe"]]), "."), trend, cross,
    "On those numbers the long-term picture is", paste0(lt_word, ".")
  )

  # --- short term ------------------------------------------------------------
  rsi_reading <- if (is.na(rsi_now)) {
    ""
  } else if (rsi_now > 70) {
    "RSI(14) above 70, which is stretched."
  } else if (rsi_now < 30) {
    "RSI(14) below 30, which is oversold."
  } else {
    paste0("RSI(14) at ", num(rsi_now, 0L), ", neither stretched nor oversold.")
  }
  st_word <- if (short[["Return"]] > 2 && (is.na(rsi_now) || rsi_now < 70)) {
    "constructive"
  } else if (short[["Return"]] < -2) {
    "under pressure"
  } else {
    "range-bound"
  }
  short_term <- say(
    "Over the last", short_n, "periods,", ticker, "returned", pct(short[["Return"]]),
    paste0("(", pct(short[["AnnReturn"]]), " annualized) with"), pct(short[["Volatility"]]),
    "volatility, and currently sits", pct(100 * below_peak), "below its own peak.",
    rsi_reading, "Near term the picture is", paste0(st_word, ".")
  )

  # --- risk ------------------------------------------------------------------
  risk <- say(
    "The worst peak-to-trough loss was", paste0(pct(full[["MaxDrawdown"]]), ","),
    "and the longest stretch spent under water was", num(full[["MaxDDDuration"]], 0L),
    "periods. A bad period costs about", pct(full[["VaR95"]]),
    "(historical VaR 95%) and the average of the periods beyond that",
    paste0(pct(full[["CVaR95"]]), " (CVaR 95%)."), "Volatility over the last", short_n,
    "periods is", paste0(pct(100 * vol_now), " annualized.")
  )

  # --- versus the benchmark -------------------------------------------------
  relative <- NULL
  if (!is.null(benchmark)) {
    own <- rets[rets$SYMBOL == ticker, c("DATE", "RET")]
    bm_map <- rets[rets$SYMBOL == benchmark, c("DATE", "RET")]
    i <- match(as.character(own$DATE), as.character(bm_map$DATE))
    keep <- !is.na(i)
    if (any(keep)) {
      mkt <- bm_map$RET[i[keep]]
      b <- beta(own$RET[keep], mkt)
      relative <- say(
        "Against", paste0(benchmark, ":"), "beta", paste0(num(b), ","),
        "tracking error", paste0(pct(100 * te(own$RET[keep], mkt, ann_factor)), ","),
        "information ratio", paste0(num(ir(own$RET[keep], mkt, ann_factor)), "."),
        "The move is", if (is.na(b) || abs(b) >= 0.8) "largely market-driven" else "largely its own",
        "over the overlapping periods."
      )
    }
  }

  # --- watch list ------------------------------------------------------------
  flags <- c(
    if (abs(full[["MaxDrawdown"]]) > 25) "drawdown deeper than 25% in the sample",
    if (!is.na(vol_now) && vol_now > 0.25) "volatility above 25% annualized over the short window",
    if (!is.na(rsi_now) && rsi_now > 70) "RSI stretched above 70",
    if (!is.na(rsi_now) && rsi_now < 30) "RSI oversold below 30",
    if (below_peak < -0.15) "still more than 15% below its peak",
    if (!is.na(ma_slow) && !is.na(ma_fast) && ma_fast < ma_slow) "faster average below the slower one",
    if (full[["Sharpe"]] < 0.2) "return barely compensated for its volatility"
  )
  watch <- if (length(flags)) {
    paste0(paste(flags, collapse = "; "), ".")
  } else {
    paste(
      "None of the usual flags (deep drawdown, high volatility, stretched RSI,",
      "broken trend) is raised."
    )
  }

  out <- c(
    `Long term` = long_term,
    `Short term` = short_term,
    Risk = risk
  )
  if (!is.null(relative)) {
    out <- c(out, c(`Versus benchmark` = relative))
  }
  c(out, c(Watch = watch))
}

#' Render a full analysis deck for one trading code
#'
#' Everything the package produces for one instrument, assembled into a single
#' deck: the written reading first, then the long-term evidence (price history,
#' drawdown, the horizon table and the risk summary), then the short-term
#' evidence (candlestick with indicators and rolling risk), and the benchmark
#' comparison last when one is named.
#'
#' Changing the trading code is the only thing needed to report on a different
#' company: pass the same price data and a different `ticker`. The wording on the
#' first slide is derived from the same metrics the rest of the deck shows, so it
#' cannot drift away from the numbers.
#'
#' This is a mechanical reading of historical prices. It is not investment
#' advice, and the deck says so on every slide.
#'
#' @param prices `data.frame` of prices, or the path to an `.rds` holding one
#' @param ticker `character` trading code to report on, present in `prices`
#' @param benchmark `character` trading code to compare against, or `NULL`
#' @param outfile `character` path of the `.pptx` to write
#' @param short_window `integer` periods treated as the short term
#' @param windows `numeric` trailing windows for the horizon table
#' @param rf_rate `numeric` Annual risk-free rate
#' @param ann_factor `numeric` Number of periods per year
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param footnote `character` footnote printed on every slide
#'
#' @return The normalized path of the deck written
#' @export
#'
#' @examples
#' \dontrun{
#' # Needs the `rsvg` package to rasterize the figures.
#' render_stock_report(eg_ohlc, "AIR.NZ", tempfile(fileext = ".pptx"))
#' }
#'
render_stock_report <- function(prices,
                                ticker,
                                benchmark = NULL,
                                outfile = "stock_report.pptx",
                                short_window = 63L,
                                windows = c(21L, 63L, 252L),
                                rf_rate = 0.02,
                                ann_factor = 252,
                                symbol = code_col(),
                                date = "DATE",
                                close = "CLOSE",
                                footnote = paste(
                                  "Mechanical reading of historical prices generated by",
                                  "autoslider.trade. Not investment advice."
                                )) {
  assert_that(is.string(ticker), is.string(outfile), is.string(footnote))
  assert_that(is.count(short_window), is.numeric(windows))
  assert_that(is.number(rf_rate), is.number(ann_factor))

  if (is.character(prices) && length(prices) == 1L) {
    prices <- readRDS(prices)
  }
  assert_that(is.data.frame(prices))
  assert_that(has_name(prices, c(symbol, date, close)))

  d <- canonical_prices(prices, symbol, date, close)
  assert_that(
    ticker %in% d$SYMBOL,
    msg = sprintf("trading code '%s' is not among those in prices.", ticker)
  )
  one <- d[d$SYMBOL == ticker, , drop = FALSE]

  pair <- one
  if (!is.null(benchmark)) {
    assert_that(is.string(benchmark))
    assert_that(
      benchmark %in% d$SYMBOL,
      msg = sprintf("benchmark '%s' is not among those in prices.", benchmark)
    )
    pair <- rbind(one, d[d$SYMBOL == benchmark, , drop = FALSE])
  }

  commentary <- stock_commentary(
    pair, ticker, benchmark, short_window, rf_rate, ann_factor, symbol, date, close
  )
  rows <- do.call(rbind, lapply(names(commentary), function(s) text_rows(s, commentary[[s]])))
  rows <- rbind(rows, text_rows("Note", "Figures and metrics below are the evidence for the reading above."))
  rownames(rows) <- NULL

  # Each slide is its own output, titled with the instrument it is about. A deck
  # is a list of these handed to `generate_slides()` in one call.
  slides <- list(
    list(
      out = as_listing(rows, key_cols = NULL, disp_cols = c("SECTION", "TEXT")),
      title = paste0(ticker, " - Analysis")
    ),
    list(
      out = t_horizon_slide(
        one, windows = windows, rf_rate = rf_rate, ann_factor = ann_factor,
        title = paste0(ticker, " - Return and Risk by Horizon")
      ),
      title = paste0(ticker, " - Return and Risk by Horizon")
    ),
    list(
      out = g_equity_slide(one, normalize = TRUE, title = paste0(ticker, " - Price History")),
      title = paste0(ticker, " - Price History")
    ),
    list(
      out = g_drawdown_slide(one, title = paste0(ticker, " - Drawdown")),
      title = paste0(ticker, " - Drawdown")
    ),
    list(
      out = t_risk_slide(
        pair, benchmark = benchmark, rf_rate = rf_rate, ann_factor = ann_factor,
        title = paste0(ticker, " - Risk Summary")
      ),
      title = paste0(ticker, " - Risk Summary")
    ),
    list(
      out = g_return_dist_slide(one, title = paste0(ticker, " - Return Distribution")),
      title = paste0(ticker, " - Return Distribution")
    ),
    list(
      out = g_rolling_risk_slide(
        one, window = short_window, rf_rate = rf_rate, ann_factor = ann_factor,
        title = paste0(ticker, " - Rolling Risk")
      ),
      title = paste0(ticker, " - Rolling Risk")
    )
  )

  # The candlestick figure needs the full OHLCV panel; a close-only series has
  # no candles to draw, so that slide is left out rather than faked.
  ohlc <- c("OPEN", "HIGH", "LOW", "CLOSE", "VOLUME")
  if (all(ohlc %in% names(prices))) {
    title <- paste0(ticker, " - Candlestick and Indicators")
    slides[[length(slides) + 1L]] <- list(
      out = g_candle_slide(prices[prices[[symbol]] == ticker, , drop = FALSE], title = title),
      title = title
    )
  } else {
    message("No OPEN/HIGH/LOW/VOLUME columns in prices; leaving out the candlestick slide.")
  }

  if (!is.null(benchmark) && length(unique(pair$SYMBOL)) > 1L) {
    title <- paste0(ticker, " - Versus ", benchmark)
    slides[[length(slides) + 1L]] <- list(
      out = g_portfolio_risk_slide(
        pair, benchmark = benchmark, window = short_window, title = title
      ),
      title = title
    )
  }

  # `generate_slides()` needs each output decorated with a title: an untitled
  # one fails while the title is split across lines.
  decorated <- lapply(slides, function(s) {
    if (inherits(s$out, "ggplot")) {
      autoslider.core::decorate(s$out, titles = s$title, metadata = list())
    } else {
      autoslider.core::decorate(
        s$out, title = s$title, footnotes = footnote, paper = "A4", metadata = list()
      )
    }
  })

  generate_slides(decorated, outfile)
  normalizePath(outfile)
}
