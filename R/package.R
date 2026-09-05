#' autoslider.trade Package
#'
#' Trading tables, listings and figures, downstream of `autoslider.core`.
#' Outputs follow the `autoslider` naming convention: `t_*_slide()` for tables,
#' `l_*_slide()` for listings and `g_*_slide()` for figures.
#'
"_PACKAGE"

#' @importFrom assertthat assert_that has_name is.count is.flag is.string
#' @importFrom formatters var_labels
#' @importFrom ggplot2 aes geom_line ggplot labs theme_minimal
#' @importFrom rlistings as_listing
#' @importFrom rtables analyze basic_table build_table rcell split_cols_by
#' @importFrom stats ave
NULL

# `autoslider.trade` is a downstream package of `autoslider.core`: the outputs
# built here are rendered to slides by `generate_slides()`, which is
# re-exported so that attaching this package is enough to render a deck.
#' @importFrom autoslider.core generate_slides
#' @export
autoslider.core::generate_slides

# Column names used inside ggplot2::aes(), which cannot be resolved statically.
utils::globalVariables(c("SYMBOL", "DATE", "VALUE"))
