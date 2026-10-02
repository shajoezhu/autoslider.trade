#' Implied against realized volatility
#'
#' The volatility premium: what the option market is charging for volatility at
#' each tenor against what the underlying actually did over a window of the same
#' length. Selling volatility when implied sits above realized and buying it when
#' the reverse holds is the basic trade, so the two are put on the same axes.
#'
#' The implied side is taken from the quote closest to the money at the tenor
#' closest to each window, so the comparison is like for like. Both need data
#' this package does not fetch: prices for the realized side, and option quotes
#' (`STRIKE`, `TYPE`, `PRICE`, `TTE`) for the implied side.
#'
#' A tenor with no matching contract, or a window longer than the history, is
#' simply absent rather than filled in.
#'
#' @param prices `data.frame` of prices for one trading code
#' @param options `data.frame` of option quotes
#' @param spot `numeric` price of the underlying. Defaults to the last close.
#' @param windows `numeric` Window widths for realized volatility, in periods
#' @param r `numeric` continuously compounded risk-free rate
#' @param ann_factor `numeric` Number of periods per year
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' # Prices at 30% volatility and options quoted at 38%: implied is rich.
#' one <- eg_ohlc[eg_ohlc$SYMBOL == "AIR.NZ", ]
#' spot <- one$CLOSE[nrow(one)]
#' quotes <- data.frame(
#'   STRIKE = c(spot, spot),
#'   TYPE = c("call", "call"),
#'   TTE = c(20 / 252, 60 / 252),
#'   PRICE = c(0.12, 0.20)
#' )
#' g_vol_premium_slide(one, quotes, windows = c(20L, 60L))
#'
g_vol_premium_slide <- function(prices,
                                options,
                                spot = NULL,
                                windows = c(20L, 60L, 90L),
                                r = 0.02,
                                ann_factor = 252,
                                title = "Implied Against Realized Volatility") {
  assert_that(is.string(title), is.numeric(windows))
  assert_that(is.number(r), is.number(ann_factor))
  assert_that(is.data.frame(options), nrow(options) > 0L)

  d <- canonical_prices(prices, code_col(), "DATE", "CLOSE")
  codes <- unique(d$SYMBOL)
  assert_that(
    length(codes) == 1L,
    msg = sprintf(
      "g_vol_premium_slide() compares one underlying against its options, but %d codes were given.",
      length(codes)
    )
  )
  if (is.null(spot)) {
    spot <- d$CLOSE[nrow(d)]
  }
  assert_that(is.number(spot), spot > 0)

  rets <- returns_long(prices, code_col(), "DATE", "CLOSE")
  x <- rets$RET
  windows <- sort(unique(as.integer(windows)))
  usable <- windows[windows <= length(x)]
  if (length(usable) < length(windows)) {
    message(
      "Dropping windows longer than the ", length(x), " returns available: ",
      paste(setdiff(windows, usable), collapse = ", ")
    )
  }

  horizon_label <- function(w) {
    if (w %% ann_factor == 0L) paste0(w / ann_factor, "Y") else paste0(w, "d")
  }
  implied <- atm_iv(options, spot, usable / ann_factor, r)

  d <- data.frame(
    TENOR = rep(vapply(usable, horizon_label, character(1)), times = 2L),
    SERIES = rep(c("Realized", "Implied"), each = length(usable)),
    VOL = c(100 * vapply(usable, function(w) vol(tail(x, w), ann_factor), numeric(1)), implied),
    stringsAsFactors = FALSE
  )
  d <- d[!is.na(d$VOL), , drop = FALSE]
  assert_that(nrow(d) > 0L, msg = "No realized or implied volatility to compare.")

  premium <- local({
    rv <- d$VOL[d$SERIES == "Realized"]
    iv <- d$VOL[d$SERIES == "Implied"]
    keep <- !is.na(iv)
    if (!any(keep)) {
      return("Premium: no implied volatility for these tenors")
    }
    paste(
      "Premium (implied - realized):",
      paste(d$TENOR[d$SERIES == "Realized"][keep], sprintf("%+.1f", iv[keep] - rv[keep]),
            collapse = ", ")
    )
  })

  d$TENOR <- factor(d$TENOR, levels = unique(d$TENOR))

  ggplot(d, aes(x = TENOR, y = VOL, fill = SERIES)) +
    geom_col(position = position_dodge()) +
    scale_fill_manual(values = c(Realized = "steelblue", Implied = "darkorange")) +
    labs(
      title = title, subtitle = premium,
      x = "Tenor", y = "Volatility (%, ann.)", fill = NULL
    ) +
    theme_minimal()
}
