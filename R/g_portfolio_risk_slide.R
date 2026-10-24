#' Portfolio risk figure
#'
#' A correlation heatmap of the returns of all trading codes side by side with
#' two benchmark-relative panels: rolling Beta against `benchmark` and the
#' information ratio of each code against it. Correlation answers how much of a
#' book is actually diversified, Beta how much of each instrument's move is
#' market, and the information ratio whether the residual move earns its keep.
#'
#' Only dates on which every trading code traded are used, so the correlations
#' are comparable across pairs. `benchmark` must be one of the codes already in
#' `prices`; when it is `NULL` only the heatmap is drawn.
#'
#' @param prices `data.frame` of prices
#' @param symbol `character` Name of the trading code column
#' @param date `character` Name of the date column
#' @param close `character` Name of the price column
#' @param benchmark `character` Trading code already present in `prices` to
#'   compare against, or `NULL` for the heatmap alone
#' @param window `integer` Number of periods in the rolling Beta window
#' @param title `character` Plot title
#'
#' @return A `ggplot` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' g_portfolio_risk_slide(eg_ohlc, benchmark = "ANZ.NZ")
#'
#' # Correlation only
#' g_portfolio_risk_slide(eg_ohlc)
#'
g_portfolio_risk_slide <- function(prices,
                                   symbol = code_col(),
                                   date = "DATE",
                                   close = "CLOSE",
                                   benchmark = NULL,
                                   window = 63L,
                                   title = "Portfolio Risk") {
  assert_that(is.string(title), is.count(window))

  d <- canonical_prices(prices, symbol, date, close)
  bm <- check_benchmark(d, benchmark)
  rets <- returns_long(prices, symbol, date, close)
  assert_that(nrow(rets) > 0L, msg = "Every trading code needs at least two price observations.")

  cm <- cor_matrix(rets)
  pair <- expand.grid(VAR1 = rownames(cm), VAR2 = colnames(cm), stringsAsFactors = FALSE)
  pair$VALUE <- as.numeric(cm[cbind(pair$VAR1, pair$VAR2)])

  p_cor <- ggplot(pair, aes(x = VAR1, y = VAR2, fill = VALUE)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", VALUE)), size = 3) +
    scale_fill_gradient2(
      limits = c(-1, 1), breaks = c(-1, 0, 1),
      low = "#d73027", mid = "white", high = "#1a9850"
    ) +
    coord_equal() +
    labs(
      title = title,
      subtitle = sprintf("Diversification ratio: %.2f", div_ratio(rets)),
      x = NULL, y = NULL, fill = "Correlation"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank())

  if (is.null(bm)) {
    message("No benchmark given; drawing the correlation heatmap only.")
    return(p_cor)
  }

  m <- returns_matrix(rets)
  dates <- as.Date(rownames(m))
  peers <- setdiff(colnames(m), bm)
  assert_that(
    length(peers) > 0L,
    msg = "No trading code left to compare against the benchmark."
  )

  bdf <- do.call(rbind, lapply(peers, function(s) {
    data.frame(
      SYMBOL = s,
      DATE = dates,
      BETA = roll_beta(m[, s], m[, bm], window),
      stringsAsFactors = FALSE
    )
  }))
  rownames(bdf) <- NULL

  idf <- data.frame(
    SYMBOL = peers,
    IR = vapply(peers, function(s) ir(m[, s], m[, bm], 252), numeric(1)),
    stringsAsFactors = FALSE
  )
  rownames(idf) <- NULL

  p_beta <- ggplot(bdf, aes(x = DATE, y = BETA, colour = SYMBOL)) +
    geom_line(na.rm = TRUE) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    labs(x = "Date", y = "Rolling Beta", colour = NULL) +
    theme_minimal()

  p_ir <- ggplot(idf, aes(x = SYMBOL, y = IR, fill = SYMBOL)) +
    geom_col(show.legend = FALSE) +
    geom_hline(yintercept = 0, colour = "grey30") +
    labs(x = NULL, y = "Information Ratio") +
    theme_minimal()

  cowplot::plot_grid(
    p_cor,
    cowplot::plot_grid(p_beta, p_ir, ncol = 1L, rel_heights = c(2, 1), align = "v"),
    ncol = 2L,
    rel_widths = c(2, 3)
  )
}
