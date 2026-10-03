#' Return distribution figure
#'
#' A histogram of the periodic returns of each trading code, with the mean and
#' the Value at Risk and Conditional Value at Risk at `conf` marked. Because
#' traded returns have fat tails, the bulk of the distribution sits in a narrow
#' band while the losses that matter sit out in a thin tail; pairing CVaR with
#' VaR shows how deep that tail goes, not merely where it starts.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param bins `integer` Number of histogram bins
#' @param conf `numeric` Confidence level for VaR and CVaR, in (0, 1)
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_return_dist_slide(eg_ohlc)
#'
#' # The 99% tail instead
#' g_return_dist_slide(eg_ohlc, conf = 0.99)
#'
g_return_dist_slide <- function(prices,
                                symbol = code_col(),
                                date = "DATE",
                                close = "CLOSE",
                                bins = 30L,
                                conf = 0.95,
                                title = "Return Distribution") {
  assert_that(is.string(title), is.count(bins))
  assert_that(is.number(conf), conf > 0, conf < 1)

  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  rets$RET <- 100 * rets$RET

  label_var <- paste0("VaR ", conf * 100, "%")
  label_cvar <- paste0("CVaR ", conf * 100, "%")
  lvls <- c("Mean", label_var, label_cvar)

  marks <- do.call(rbind, lapply(split(rets, rets$SYMBOL), function(x) {
    data.frame(
      SYMBOL = x$SYMBOL[1L],
      TYPE = factor(lvls, levels = lvls),
      VALUE = c(
        mean(x$RET),
        -100 * var_hist(x$RET / 100, conf),
        -100 * cvar(x$RET / 100, conf)
      ),
      stringsAsFactors = FALSE
    )
  }))
  rownames(marks) <- NULL
  pal <- setNames(c("grey30", "darkorange", "#d73027"), lvls)

  ggplot(rets, aes(x = RET)) +
    geom_histogram(bins = bins, fill = "steelblue", colour = "white", na.rm = TRUE) +
    geom_vline(aes(xintercept = VALUE, colour = TYPE), data = marks, linetype = "dashed") +
    facet_wrap(~SYMBOL, ncol = 1L) +
    scale_colour_manual(values = pal) +
    labs(title = title, x = "Return (%)", y = "Count", colour = "Marker") +
    theme_minimal()
}
