#' autoslider.trade Package
#'
#' Trading tables, listings and figures, downstream of `autoslider.core`.
#' Outputs follow the `autoslider` naming convention: `t_*_slide()` for tables,
#' `l_*_slide()` for listings and `g_*_slide()` for figures.
#'
"_PACKAGE"

#' @importFrom assertthat assert_that has_name is.count is.flag is.number is.string
#' @importFrom formatters var_labels
#' @importFrom ggplot2 aes annotate coord_equal element_blank facet_wrap geom_area
#'   geom_col geom_histogram geom_hline geom_line geom_rect geom_ribbon geom_segment
#'   geom_text geom_tile geom_vline ggplot labs position_dodge scale_colour_manual
#'   scale_fill_gradient2 scale_fill_manual scale_linetype_manual theme theme_minimal
#' @importFrom rlistings as_listing
#' @importFrom rtables analyze basic_table build_table keep_split_levels rcell
#'   split_cols_by
#' @importFrom stats ave cor cov dnorm pnorm qnorm quantile sd setNames var
#' @importFrom utils tail
NULL

# `autoslider.trade` is a downstream package of `autoslider.core`: the outputs
# built here are rendered to slides by `generate_slides()`, which is
# re-exported so that attaching this package is enough to render a deck.
#' @importFrom autoslider.core generate_slides
#' @export
autoslider.core::generate_slides

# Column names used inside ggplot2::aes(), which cannot be resolved statically.
utils::globalVariables(c(
  "SYMBOL", "DATE", "VALUE",
  "OPEN", "HIGH", "LOW", "CLOSE", "VOLUME", "DIR",
  "UP", "MID", "DN", "VMA", "RSI", "DIF", "DEA", "HIST", "y", "series",
  "RET", "DD", "REGIME", "VOL", "SHARPE", "VAR", "MAXDD", "BETA", "IR",
  "VAR1", "VAR2", "TYPE",
  "WINDOW", "TENOR", "SERIES", "HIGH", "LOW"
))
