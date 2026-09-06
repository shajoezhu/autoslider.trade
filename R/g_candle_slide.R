#' Candlestick figure with technical indicators
#'
#' A richer take on the `quantmod::chartSeries` plot used in `homepage-stock`:
#' daily candlesticks with Bollinger Bands, moving averages and high/low
#' annotations in the top panel, then a volume panel with a volume moving
#' average, an RSI panel and a MACD panel. The four panels are stacked with
#' `cowplot`.
#'
#' Up days are drawn in green and down days in red. When more than one trading
#' code is supplied, only the first is plotted and the rest are ignored with a
#' message.
#'
#' @param prices `data.frame` of OHLCV prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param open `character` Name of the open column
#' @param high `character` Name of the high column
#' @param low `character` Name of the low column
#' @param close `character` Name of the close column
#' @param volume `character` Name of the volume column
#' @param ma `integer` Window(s) for the moving-average lines overlaid on the
#'   price panel. Set to `NULL` to omit them.
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_candle_slide(eg_ohlc)
#'
#' # Only the moving averages, no Bollinger Bands shown as separate colour
#' g_candle_slide(eg_ohlc, ma = 20L)
#'
g_candle_slide <- function(prices,
                           symbol = code_col(),
                           date = "DATE",
                           open = "OPEN",
                           high = "HIGH",
                           low = "LOW",
                           close = "CLOSE",
                           volume = "VOLUME",
                           ma = c(20L, 50L),
                           title = "Candlestick") {
  assert_that(
    is.string(symbol), is.string(date), is.string(open), is.string(high),
    is.string(low), is.string(close), is.string(volume), is.string(title)
  )
  assert_that(is.null(ma) || is.numeric(ma))

  d <- canonical_ohlc(prices, symbol, date, open, high, low, close, volume)

  syms <- unique(d$SYMBOL)
  if (length(syms) > 1L) {
    message("More than one trading code present; plotting only ", syms[1L])
  }
  d <- d[d$SYMBOL == syms[1L], , drop = FALSE]

  up_col <- "#1a9850"
  down_col <- "#d73027"

  d$DIR <- ifelse(d$CLOSE >= d$OPEN, "up", "down")
  for (m in ma) {
    d[[paste0("MA", m)]] <- sma(d$CLOSE, m)
  }
  bb <- bbands(d$CLOSE, 20L)
  d$UP <- bb$up
  d$MID <- bb$mid
  d$DN <- bb$dn
  d$VMA <- sma(d$VOLUME, 20L)
  d$RSI <- rsi(d$CLOSE, 14L)
  mc <- macd(d$CLOSE)
  d$DIF <- mc$dif
  d$DEA <- mc$dea
  d$HIST <- mc$hist

  hi_i <- which.max(d$CLOSE)
  lo_i <- which.min(d$CLOSE)

  ma_df <- if (is.null(ma)) NULL else do.call(
    rbind,
    lapply(seq_along(ma), function(i) {
      data.frame(
        DATE = d$DATE,
        y = d[[paste0("MA", ma[i])]],
        series = paste0("MA", ma[i]),
        stringsAsFactors = FALSE
      )
    })
  )
  ma_lty <- if (is.null(ma)) NULL else setNames(
    c("solid", "dashed", "dotted", "dotdash")[seq_along(ma)],
    paste0("MA", ma)
  )

  no_x <- list(
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  )

  p_price <- ggplot(d, aes(x = DATE)) +
    geom_segment(aes(y = LOW, yend = HIGH, colour = DIR)) +
    geom_rect(aes(
      xmin = DATE - 0.4, xmax = DATE + 0.4, ymin = OPEN, ymax = CLOSE, fill = DIR
    )) +
    geom_line(aes(y = UP), colour = "steelblue", linetype = "dotted", na.rm = TRUE) +
    geom_line(aes(y = MID), colour = "steelblue", linetype = "solid", na.rm = TRUE) +
    geom_line(aes(y = DN), colour = "steelblue", linetype = "dotted", na.rm = TRUE) +
    { if (!is.null(ma_df)) geom_line(
      aes(y = y, linetype = series), data = ma_df, colour = "grey30", na.rm = TRUE
    ) } +
    { if (!is.null(ma_df)) scale_linetype_manual(values = ma_lty) } +
    scale_colour_manual(values = c(up = up_col, down = down_col), guide = "none") +
    scale_fill_manual(values = c(up = up_col, down = down_col)) +
    annotate(
      "text", x = d$DATE[hi_i], y = d$CLOSE[hi_i],
      label = format(d$CLOSE[hi_i], digits = 3), vjust = -0.5, size = 3
    ) +
    annotate(
      "text", x = d$DATE[lo_i], y = d$CLOSE[lo_i],
      label = format(d$CLOSE[lo_i], digits = 3), vjust = 1.5, size = 3
    ) +
    labs(title = title, y = "Price") +
    theme_minimal() +
    no_x

  p_vol <- ggplot(d, aes(x = DATE)) +
    geom_col(aes(y = VOLUME, fill = DIR)) +
    geom_line(aes(y = VMA), colour = "grey30", na.rm = TRUE) +
    scale_fill_manual(values = c(up = up_col, down = down_col), guide = "none") +
    labs(y = "Volume") +
    theme_minimal() +
    no_x

  p_rsi <- ggplot(d, aes(x = DATE, y = RSI)) +
    geom_line(colour = "darkgreen", na.rm = TRUE) +
    geom_hline(yintercept = c(30, 70), linetype = "dashed", colour = "grey50") +
    labs(y = "RSI(14)") +
    theme_minimal() +
    no_x

  p_macd <- ggplot(d, aes(x = DATE)) +
    geom_col(aes(y = HIST), fill = "grey50", na.rm = TRUE) +
    geom_line(aes(y = DIF), colour = "steelblue", na.rm = TRUE) +
    geom_line(aes(y = DEA), colour = "darkorange", na.rm = TRUE) +
    labs(x = "Date", y = "MACD") +
    theme_minimal()

  cowplot::plot_grid(
    p_price, p_vol, p_rsi, p_macd,
    ncol = 1L,
    rel_heights = c(3, 1, 1, 1),
    align = "v"
  )
}
