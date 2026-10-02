#' Fundamentals or valuation metrics across reporting periods
#'
#' A trend table for whatever the user has: revenue, margins, returns, leverage,
#' or valuation multiples such as forward P/E and EV/EBITDA. The rows are the
#' periods, the columns are the metrics, and a `Change` row shows the move from
#' the first period to the last.
#'
#' This package does not fetch financials, so the numbers are supplied as a tidy
#' `data.frame` of `METRIC` / `PERIOD` / `VALUE`. Nothing is derived here beyond
#' the change and the ordering, which keeps the table honest about where its
#' numbers came from: it renders what it is given, and says so.
#'
#' @param financials `data.frame` with one row per metric and period
#' @param metric `character` Name of the metric name column
#' @param period `character` Name of the period column
#' @param value `character` Name of the value column
#' @param change `logical` Should the first-to-last change be shown as a row?
#' @param relative `logical` Should the change be a percentage rather than a
#'   difference? Ignored for metrics that change sign, where a percentage is
#'   meaningless.
#' @param digits `integer` Number of decimal places in the output
#' @param title `character` Table title
#'
#' @return An `rtables` object, ready to be rendered with `generate_slides()`
#' @export
#'
#' @examples
#' # Five years of two metrics, as they would come from a company's accounts.
#' fin <- data.frame(
#'   METRIC = rep(c("Revenue (m)", "Operating margin (%)"), each = 3L),
#'   PERIOD = rep(c("FY23", "FY24", "FY25"), times = 2L),
#'   VALUE = c(120, 135, 158, 11.2, 12.8, 14.1)
#' )
#' t_fundamentals_slide(fin)
#'
t_fundamentals_slide <- function(financials,
                                 metric = "METRIC",
                                 period = "PERIOD",
                                 value = "VALUE",
                                 change = TRUE,
                                 relative = TRUE,
                                 digits = 2L,
                                 title = "Fundamentals by Period") {
  assert_that(is.data.frame(financials), nrow(financials) > 0L)
  assert_that(has_name(financials, c(metric, period, value)))
  assert_that(is.string(title), is.count(digits))
  assert_that(is.flag(change), is.flag(relative))

  d <- as.data.frame(financials[c(metric, period, value)])
  names(d) <- c("METRIC", "PERIOD", "VALUE")
  d$VALUE <- as.numeric(d$VALUE)
  assert_that(all(!is.na(d$VALUE)), msg = "Every metric needs a numeric value.")

  periods <- sort(unique(d$PERIOD))
  assert_that(
    length(periods) > 0L,
    msg = "At least one period is needed to build a trend table."
  )
  # Metrics stay in the order they were given; sorting them alphabetically would
  # put revenue below operating margin for no reason.
  metrics <- unique(d$METRIC)

  # One row per metric, one column per period: the shape rtables splits on.
  stats <- do.call(rbind, lapply(metrics, function(m) {
    sub <- d[d$METRIC == m, , drop = FALSE]
    row <- as.list(sub$VALUE[match(periods, sub$PERIOD)])
    names(row) <- as.character(periods)
    as.data.frame(row, stringsAsFactors = FALSE)
  }))
  stats$METRIC <- factor(metrics, levels = metrics)
  rownames(stats) <- NULL

  if (change && length(periods) > 1L) {
    first <- stats[[as.character(periods[1L])]]
    last <- stats[[as.character(periods[length(periods)])]]
    stats$Change <- if (relative && all(first > 0)) {
      100 * (last / first - 1)
    } else {
      last - first
    }
  }

  fmt_value <- function(x) rcell(round(mean(x), digits))

  lyt <- basic_table(title = title) |>
    split_cols_by("METRIC", split_fun = keep_split_levels(metrics))

  for (m in setdiff(names(stats), "METRIC")) {
    lyt <- analyze(lyt, m, fmt_value)
  }

  build_table(lyt, stats)
}
