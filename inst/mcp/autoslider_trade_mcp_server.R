#!/usr/bin/env Rscript
# autoslider.trade MCP Server
#
# Exposes the trading-figure / table / listing functions of autoslider.trade as
# MCP tools, so any MCP-compatible client (Claude Desktop, Claude Code, Cursor,
# Open WebUI, etc.) can build trading slide decks conversationally — the same
# outputs autoslider.core produces for clinical data, but for prices and trades.
#
# Usage:
#   Rscript inst/mcp/autoslider_trade_mcp_server.R
#
# Register in your MCP client, e.g. for CodeBuddy add to ~/.codebuddy/.mcp.json:
#   {
#     "mcpServers": {
#       "autoslider_trade": {
#         "command": "Rscript",
#         "args": ["/abs/path/to/autoslider.trade/inst/mcp/autoslider_trade_mcp_server.R"],
#         "type": "stdio"
#       }
#     }
#   }
#
# Requirements:
#   install.packages("mcptools")   # on CRAN; provides mcp_server()/tool()/type_string()
#   # autoslider.trade already installed / loadable
#
# Two tools write to disk: trade_generate_slides and stock_report. Both refuse to
# replace an existing file unless overwrite = TRUE. Approval is a client setting,
# not something a server can enforce, so the recommendation shipped with this
# server is: require approval for those two tools in your client, and point
# unattended runs at a scratch directory. See README.md.

suppressPackageStartupMessages({
  library(mcptools)
  library(ellmer)
  library(autoslider.trade)
})

# ---- session state ----------------------------------------------------------
# State is keyed by session id rather than held in one global slot. A stdio
# server is normally one process per client, so the old single-slot version was
# fine in practice; keying it costs nothing and means a shared or HTTP-hosted
# server can serve concurrent callers without one deck bleeding into another.
# An empty session_id falls back to "default", so existing callers behave
# exactly as before.
.state <- new.env(parent = emptyenv())
.state$sessions <- list()

DEFAULT_SESSION <- "default"

# Fetch (creating on first use) the environment holding one session's outputs.
# An environment, not a list, so writes inside tool functions actually stick.
session <- function(session_id) {
  if (is.null(session_id) || is.na(session_id) || !nzchar(session_id)) {
    session_id <- DEFAULT_SESSION
  }
  s <- .state$sessions[[session_id]]
  if (is.null(s)) {
    s <- new.env(parent = emptyenv())
    s$outputs <- NULL
    all <- .state$sessions
    all[[session_id]] <- s
    .state$sessions <- all
    s <- .state$sessions[[session_id]]
  }
  s
}

# ---- helpers ----------------------------------------------------------------
stop_if <- function(cond, msg) if (cond) stop(msg, call. = FALSE)

optional <- function(x) if (nzchar(x)) x else NULL

# Names of what this session has built so far. Error messages quote them, so a
# model that names a missing output is told what it could have named instead —
# it can correct itself rather than guess again.
built_names <- function(s) {
  if (is.null(s$outputs)) character(0) else names(s$outputs)
}

require_outputs <- function(s) {
  stop_if(
    length(built_names(s)) == 0L,
    paste0(
      "No outputs built yet in this session. Call a render_* tool first, ",
      "e.g. candle_chart() or risk_table()."
    )
  )
}

# Resolve a dataset: "example" pulls a bundled dataset from this package,
# otherwise read an .rds file from disk.
load_df <- function(data_path, example_name) {
  if (data_path == "example") {
    get(example_name, envir = asNamespace("autoslider.trade"))
  } else {
    readRDS(data_path)
  }
}

# The risk outputs need a longer series than `eg_prices` has, so they fall back
# to `eg_ohlc` and use its CLOSE column.
load_prices <- function(data_path, symbol) {
  df <- load_df(data_path, "eg_ohlc")
  if (nzchar(symbol)) {
    # Name what the data actually holds: a model that guesses a code can correct
    # itself from the list instead of trying one name after another.
    available <- sort(unique(df$SYMBOL))
    df <- df[df$SYMBOL == symbol, ]
    stop_if(
      nrow(df) == 0L,
      sprintf(
        "No rows for symbol '%s'. Available in this data: %s",
        symbol, paste(available, collapse = ", ")
      )
    )
  }
  df
}

# ---- shared tool argument ---------------------------------------------------
# Every tool that reads or writes session state takes this.

session_arg <- type_string(
  paste(
    'Session id. Give each concurrent run its own id so decks do not mix.',
    'Empty string ("") uses the default session.'
  )
)

# Refuse to clobber unless explicitly asked.
#
# A person chooses an output path deliberately; an agent picks one as casually as
# any other argument, and nothing stops it landing on a file that already exists.
# Overwriting silently is worse than failing: the reply says "written to: ..." and
# the caller believes it succeeded, while the file it replaced is gone. So the
# default is to refuse, and the error says exactly how to proceed.
guard_outfile <- function(outfile, overwrite) {
  path <- normalizePath(outfile, mustWork = FALSE)
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(
      sprintf(
        paste0(
          "'%s' already exists and overwrite is FALSE, so nothing was written. ",
          "Pass overwrite=TRUE to replace it, or write to a different path."
        ),
        path
      ),
      call. = FALSE
    )
  }
  path
}

# Horizons arrive over MCP as text, so "21,63,252" becomes c(21L, 63L, 252L).
parse_windows <- function(windows) {
  x <- suppressWarnings(as.integer(trimws(strsplit(windows, ",")[[1]])))
  x <- x[!is.na(x)]
  stop_if(
    length(x) == 0L || any(x < 1L),
    sprintf("windows must be comma-separated positive integers, got '%s'.", windows)
  )
  sort(unique(x))
}

# One code or none: both t_horizon_slide() and g_vol_premium_slide() assert on
# more than one, so narrow in the tool and say which was dropped rather than
# letting the assert fire with a message the caller cannot act on.
one_code <- function(df) {
  codes <- sort(unique(df$SYMBOL))
  if (length(codes) > 1L) df[df$SYMBOL == codes[1L], , drop = FALSE] else df
}

# Every builder replies the same way: what it built, what the session now holds,
# and the output_name to hand trade_generate_slides. That last part is the point:
# without it a model has to guess which name it just created, and guessing is how
# it ends up asking for 'risk_table' after building 'candle'.
built_reply <- function(what, name, s) {
  sprintf(
    "%s. Built: %s. Call trade_generate_slides(output_name='%s').",
    what, paste(built_names(s), collapse = ", "), name
  )
}

left_out_note <- function(available, plotted, what) {
  others <- setdiff(available, plotted)
  if (!length(others)) return("")
  sprintf(
    " One %s covers one trading code, so %s %s left out; call again with symbol= for each.",
    what, paste(others, collapse = ", "),
    if (length(others) > 1L) "were" else "was"
  )
}

# ---- tool functions ---------------------------------------------------------

fn_list_outputs <- function(session_id) {
  paste0(
    "Available trade outputs (render_* tools):\n",
    "  candle_chart  - g_candle_slide(): candlestick + Bollinger/MA/RSI/MACD, one code per chart\n",
    "  performance    - t_performance_slide(): per-instrument return / drawdown table\n",
    "  trades         - l_trades_slide(): trade listing\n",
    "  equity_curve   - g_equity_slide(): rebased price curve\n",
    "Risk analytics (need a longer series than the 5-row eg_prices):\n",
    "  risk_table     - t_risk_slide(): VaR / CVaR / drawdown / Sharpe ratios table\n",
    "  drawdown       - g_drawdown_slide(): underwater plot per instrument\n",
    "  ret_dist       - g_return_dist_slide(): return histogram with VaR / CVaR marked\n",
    "  rolling_risk   - g_rolling_risk_slide(): rolling volatility / Sharpe / VaR / drawdown\n",
    "  portfolio      - g_portfolio_risk_slide(): correlation heatmap + beta / info ratio\n",
    "  stock_report   - render_stock_report(): one full analysis deck for one company\n",
    "  range_chart    - g_range_slide(): price inside its 52-week band\n",
    "  vol_term       - g_vol_term_slide(): realized vol over 20/60/90/252 days\n",
    "  option_table   - t_option_slide(): implied vol and Greeks (example data available)\n",
    "  fundamentals   - t_fundamentals_slide(): metrics by period (example data available)\n",
    "  horizon        - t_horizon_slide(): return and risk by horizon, one code\n",
    "  vol_premium    - g_vol_premium_slide(): implied vs realised vol (needs quotes)\n",
    "\n",
    "Then call trade_generate_slides(outfile=..., output_name=...) to render ONE\n",
    "built output per file, and optionally pptx_to_pdf(path=...) to convert it.\n",
    "\n",
    "Already built in this session: ",
    if (length(built_names(session(session_id)))) {
      paste(built_names(session(session_id)), collapse = ", ")
    } else {
      "(none)"
    }
  )
}

fn_candle <- function(symbol, data_path, session_id) {
  df <- load_df(data_path, "eg_ohlc")
  available <- sort(unique(df$SYMBOL))

  # A candlestick chart is one instrument's OHLC bars: several codes cannot share
  # one, so an empty symbol takes the first code rather than silently dropping the
  # rest inside g_candle_slide(). The reply names what was plotted and what was
  # not, so a caller does not walk away believing it charted everything.
  if (nzchar(symbol)) {
    df <- df[df$SYMBOL == symbol, ]
    stop_if(
      nrow(df) == 0L,
      sprintf(
        "No rows for symbol '%s'. Available in this data: %s",
        symbol, paste(available, collapse = ", ")
      )
    )
    plotted <- symbol
  } else {
    plotted <- available[1L]
    df <- df[df$SYMBOL == plotted, ]
  }

  s <- session(session_id)
  s$outputs[["candle"]] <- g_candle_slide(df)

  msg <- paste0(
    built_reply(
      sprintf("Candlestick chart built for '%s' (%d rows)", plotted, nrow(df)),
      "candle", s
    ),
    left_out_note(available, plotted, "candlestick chart")
  )

  others <- setdiff(available, plotted)
  if (length(others)) {
    msg <- paste0(
      msg, sprintf(
        " One candlestick chart covers one trading code, so %s %s left out; call again with symbol= for each.",
        paste(others, collapse = ", "),
        if (length(others) > 1L) "were" else "was"
      )
    )
  }
  msg
}

fn_performance <- function(data_path, session_id) {
  df <- load_df(data_path, "eg_prices")
  s <- session(session_id)
  s$outputs[["performance"]] <- t_performance_slide(df)
  built_reply("Performance table built", "performance", s)
}

fn_trades <- function(data_path, session_id) {
  df <- load_df(data_path, "eg_trades")
  s <- session(session_id)
  s$outputs[["trades"]] <- l_trades_slide(df)
  built_reply("Trade listing built", "trades", s)
}

fn_equity <- function(data_path, session_id) {
  df <- load_df(data_path, "eg_prices")
  s <- session(session_id)
  s$outputs[["equity"]] <- g_equity_slide(df)
  built_reply("Equity curve built", "equity", s)
}

fn_generate_slides <- function(outfile, output_name, overwrite, session_id) {
  s <- session(session_id)
  require_outputs(s)
  stop_if(
    !output_name %in% built_names(s),
    sprintf(
      paste0(
        "No output named '%s'. Built outputs in this session: %s. ",
        "Call the matching render_* tool first, or pass one of the built names."
      ),
      output_name, paste(built_names(s), collapse = ", ")
    )
  )
  outfile <- guard_outfile(outfile, overwrite)
  tryCatch(
    generate_slides(s$outputs[[output_name]], outfile = outfile),
    error = function(e) stop("Slide generation error: ", e$message, call. = FALSE)
  )
  sprintf("Slides for '%s' written to: %s", output_name, outfile)
}

# No session state: this converts a file that already exists, so unlike every
# other tool it takes no session_id. An argument a tool ignores is a lie the
# caller cannot detect.
fn_pptx_to_pdf <- function(path, output_dir) {
  out <- pptx_to_pdf(path, if (nzchar(output_dir)) output_dir else NULL)
  sprintf("PDF(s) written: %s", paste(out, collapse = ", "))
}

# The option and fundamentals outputs need data this package does not fetch, so
# they take it as a path rather than a bundled example.
fn_range <- function(data_path, symbol, session_id) {
  df <- load_prices(data_path, symbol)
  s <- session(session_id)
  s$outputs[["range"]] <- g_range_slide(df)
  built_reply(sprintf("Range figure built (%d rows)", nrow(df)), "range", s)
}

fn_vol_term <- function(data_path, symbol, session_id) {
  df <- load_prices(data_path, symbol)
  s <- session(session_id)
  s$outputs[["vol_term"]] <- g_vol_term_slide(df)
  built_reply(
    sprintf("Volatility term structure built (%d rows)", nrow(df)), "vol_term", s
  )
}

fn_option_table <- function(data_path, spot, session_id) {
  df <- if (data_path == "example") {
    get("eg_options", envir = asNamespace("autoslider.trade"))
  } else {
    readRDS(data_path)
  }
  stop_if(nrow(df) == 0L, sprintf("No option quotes in '%s'.", data_path))

  # bs_iv() needs a real spot, so 0 cannot mean "use one from the quotes".
  # It means: for the bundled example, take the last close of the series the
  # quotes were written against (AIA.NZ); for anything else, insist on a price.
  if (spot > 0) {
    used_spot <- spot
    spot_note <- sprintf(", spot %.2f", spot)
  } else if (data_path == "example") {
    px <- get("eg_ohlc", envir = asNamespace("autoslider.trade"))
    used_spot <- tail(px$CLOSE[px$SYMBOL == "AIA.NZ"], 1)
    spot_note <- sprintf(", spot %.2f taken from eg_ohlc", used_spot)
  } else {
    stop(
      "spot is required: the price of the underlying. The quotes do not carry it ",
      "and it cannot be inferred from STRIKE/TYPE/PRICE/TTE.",
      call. = FALSE
    )
  }

  s <- session(session_id)
  s$outputs[["options"]] <- t_option_slide(df, spot = used_spot)
  built_reply(
    sprintf("Option table built (%d quotes%s)", nrow(df), spot_note), "options", s
  )
}

fn_fundamentals <- function(data_path, session_id) {
  df <- if (data_path == "example") {
    get("eg_financials", envir = asNamespace("autoslider.trade"))
  } else {
    readRDS(data_path)
  }
  stop_if(nrow(df) == 0L, sprintf("No metrics in '%s'.", data_path))
  s <- session(session_id)
  s$outputs[["fundamentals"]] <- t_fundamentals_slide(df)
  built_reply(
    sprintf("Fundamentals table built (%d rows)", nrow(df)), "fundamentals", s
  )
}

# Drop one built output without clearing the rest: without this, removing a
# single output means resetting the whole session and rebuilding everything.
fn_drop_output <- function(output_name, session_id) {
  s <- session(session_id)
  built <- built_names(s)
  stop_if(
    length(built) == 0L,
    "No outputs built yet in this session; nothing to drop."
  )
  stop_if(
    !output_name %in% built,
    sprintf(
      "No output named '%s'. Built outputs in this session: %s",
      output_name, paste(built, collapse = ", ")
    )
  )
  s$outputs[[output_name]] <- NULL
  left <- built_names(s)
  sprintf(
    "Dropped '%s'. Remaining built: %s",
    output_name,
    if (length(left)) paste(left, collapse = ", ") else "(none)"
  )
}

fn_horizon <- function(symbol, data_path, windows, session_id) {
  available <- sort(unique(load_df(data_path, "eg_ohlc")$SYMBOL))
  df <- one_code(load_prices(data_path, symbol))
  plotted <- sort(unique(df$SYMBOL))[1L]
  wins <- parse_windows(windows)

  s <- session(session_id)
  s$outputs[["horizon"]] <- t_horizon_slide(df, windows = wins)

  paste0(
    built_reply(
      sprintf(
        "Horizon table built for '%s' (windows %s)", plotted, paste(wins, collapse = "/")
      ),
      "horizon", s
    ),
    left_out_note(available, plotted, "horizon table")
  )
}

# Implied vol needs option quotes, which this server does not fetch, so it takes
# a path the same way option_table does.
fn_vol_premium <- function(symbol, data_path, options_path, spot, windows, session_id) {
  available <- sort(unique(load_df(data_path, "eg_ohlc")$SYMBOL))
  df <- one_code(load_prices(data_path, symbol))
  plotted <- sort(unique(df$SYMBOL))[1L]

  opts <- readRDS(options_path)
  stop_if(
    !is.data.frame(opts) || nrow(opts) == 0L,
    sprintf(
      "No option quotes in '%s'. Quotes are not fetched: pass an .rds with STRIKE/TYPE/PRICE/TTE.",
      options_path
    )
  )
  wins <- parse_windows(windows)

  s <- session(session_id)
  s$outputs[["vol_premium"]] <- g_vol_premium_slide(
    df,
    options = opts,
    spot = if (spot > 0) spot else NULL,
    windows = wins
  )

  paste0(
    built_reply(
      sprintf(
        "Implied-vs-realised volatility figure built for '%s' (windows %s)",
        plotted, paste(wins, collapse = "/")
      ),
      "vol_premium", s
    ),
    left_out_note(available, plotted, "volatility premium figure")
  )
}

fn_stock_report <- function(symbol, data_path, benchmark, outfile, overwrite, session_id) {
  df <- load_df(data_path, "eg_ohlc")
  stop_if(nrow(df) == 0L, sprintf("No rows in the data for '%s'.", data_path))
  outfile <- guard_outfile(outfile, overwrite)
  res <- tryCatch(
    render_stock_report(
      df,
      ticker = symbol,
      benchmark = optional(benchmark),
      outfile = outfile
    ),
    error = function(e) stop("Report error: ", e$message, call. = FALSE)
  )
  # The report is one opaque call that writes a whole deck, so report back which
  # pages it decided to include: some are dropped when the data is too short for
  # them, and the caller should be able to see that without opening the file.
  titles <- attr(res, "slides", exact = TRUE)
  msg <- sprintf("Report for '%s' written to: %s", symbol, res)
  if (length(titles)) {
    msg <- paste0(
      msg, "\n", length(titles), " pages:\n  - ", paste(titles, collapse = "\n  - ")
    )
  }
  msg
}

fn_reset <- function(session_id) {
  id <- if (is.null(session_id) || is.na(session_id) || !nzchar(session_id)) {
    DEFAULT_SESSION
  } else {
    session_id
  }
  all <- .state$sessions
  all[[id]] <- NULL
  .state$sessions <- all
  sprintf("Session '%s' state cleared.", id)
}

# ---- risk analytics ---------------------------------------------------------

fn_risk_table <- function(data_path, benchmark, symbol, session_id) {
  df <- load_prices(data_path, symbol)
  s <- session(session_id)
  s$outputs[["risk_table"]] <- t_risk_slide(df, benchmark = optional(benchmark))
  built_reply(sprintf("Risk summary built (%d rows)", nrow(df)), "risk_table", s)
}

fn_drawdown <- function(data_path, symbol, session_id) {
  df <- load_prices(data_path, symbol)
  s <- session(session_id)
  s$outputs[["drawdown"]] <- g_drawdown_slide(df)
  built_reply(sprintf("Drawdown figure built (%d rows)", nrow(df)), "drawdown", s)
}

fn_ret_dist <- function(data_path, conf, symbol, session_id) {
  stop_if(!(conf > 0 && conf < 1), sprintf("conf must be in (0, 1), got %s.", conf))
  df <- load_prices(data_path, symbol)
  s <- session(session_id)
  s$outputs[["ret_dist"]] <- g_return_dist_slide(df, conf = conf)
  built_reply(
    sprintf("Return distribution built (%d rows)", nrow(df)), "ret_dist", s
  )
}

fn_rolling_risk <- function(data_path, window, symbol, session_id) {
  df <- load_prices(data_path, symbol)
  s <- session(session_id)
  s$outputs[["rolling_risk"]] <- g_rolling_risk_slide(df, window = window)
  built_reply(
    sprintf("Rolling risk figure built (%d rows, window %d)", nrow(df), as.integer(window)),
    "rolling_risk", s
  )
}

fn_portfolio <- function(data_path, benchmark, window, session_id) {
  df <- load_prices(data_path, "")
  stop_if(
    nrow(df) < 2L || length(unique(df$SYMBOL)) < 2L,
    "Portfolio risk needs prices for at least two trading codes."
  )
  s <- session(session_id)
  s$outputs[["portfolio"]] <- g_portfolio_risk_slide(
    df, benchmark = optional(benchmark), window = window
  )
  built_reply(
    sprintf(
      "Portfolio risk figure built (%d rows, benchmark '%s')",
      nrow(df), if (nzchar(benchmark)) benchmark else "none"
    ),
    "portfolio", s
  )
}

# ---- register tools ---------------------------------------------------------

tools <- list(

  tool(
    fun = fn_list_outputs,
    name = "list_trade_outputs",
    description = paste(
      "List the trading output types this server can build, how to assemble them",
      "into slides, and what this session has already built. Call first to discover",
      "the render_* tools, and again whenever you need to know what is available",
      "to render."
    ),
    arguments = list(session_id = session_arg)
  ),

  tool(
    fun = fn_candle,
    name = "candle_chart",
    description = paste(
      "Build a candlestick chart with Bollinger Bands, moving averages, RSI and",
      "MACD for one trading code (g_candle_slide). Adds it to the deck.",
      'Use data_path = "example" for the bundled OHLCV data, or an .rds path.',
      "A candlestick chart covers ONE code: an empty symbol takes the first code",
      "in the data, not all of them, so call once per code to chart several."
    ),
    arguments = list(
      symbol = type_string(
        paste(
          'Trading code to chart, e.g. "AIA.NZ". One chart covers one code; an empty',
          'string ("") takes the first code in the data and the reply names what was',
          'left out, so call once per code to chart several.'
        )
      ),
      data_path = type_string(
        'Path to an .rds OHLCV data frame, or "example" for bundled eg_ohlc.'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_performance,
    name = "performance_table",
    description = paste(
      "Build the per-instrument performance table (return, drawdown, first/last",
      "price) as a slide-ready rtables object (t_performance_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_prices.'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_trades,
    name = "trades_listing",
    description = paste(
      "Build a trade listing (symbol, date, side, qty, price) as a slide-ready",
      "listing (l_trades_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds trades data frame, or "example" for bundled eg_trades.'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_equity,
    name = "equity_curve",
    description = paste(
      "Build a rebased equity curve figure comparing instruments from a common",
      "starting point (g_equity_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_prices.'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_risk_table,
    name = "risk_table",
    description = paste(
      "Build the per-instrument risk summary table: return, volatility, VaR, CVaR,",
      "drawdown and the Sharpe / Sortino / Calmar / Omega ratios (t_risk_slide).",
      'Name a benchmark (e.g. "ANZ.NZ") to add Beta, tracking error and the',
      "information ratio. Needs a long price series, so it defaults to eg_ohlc."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      benchmark = type_string(
        'Trading code already in the data to compare against, or "" for none.'
      ),
      symbol = type_string('Restrict to one trading code, or "" for all.'),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_drawdown,
    name = "drawdown_chart",
    description = paste(
      "Build the underwater plot of how far below its own running peak each",
      "trading code sits over time (g_drawdown_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      symbol = type_string('Restrict to one trading code, or "" for all.'),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_ret_dist,
    name = "return_distribution",
    description = paste(
      "Build a histogram of periodic returns per trading code with the mean, VaR",
      "and CVaR marked (g_return_dist_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      conf = type_number("Confidence level for VaR and CVaR, e.g. 0.95 or 0.99."),
      symbol = type_string('Restrict to one trading code, or "" for all.'),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_rolling_risk,
    name = "rolling_risk",
    description = paste(
      "Build four stacked rolling panels: volatility, Sharpe ratio, VaR and",
      "maximum drawdown over a trailing window (g_rolling_risk_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      window = type_number("Number of periods per window, e.g. 63 for about a quarter."),
      symbol = type_string('Restrict to one trading code, or "" for all.'),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_portfolio,
    name = "portfolio_risk",
    description = paste(
      "Build the return correlation heatmap of all trading codes (g_portfolio_risk_slide).",
      'Name a benchmark (e.g. "ANZ.NZ") to add rolling Beta and information ratio',
      "panels alongside it."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      benchmark = type_string(
        'Trading code already in the data to compare against, or "" for the heatmap alone.'
      ),
      window = type_number("Number of periods per rolling Beta window, e.g. 63."),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_range,
    name = "range_chart",
    description = paste(
      "Build the figure placing the price inside its own trailing 52-week high-low",
      "band, with the current position in that band (g_range_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      symbol = type_string('Restrict to one trading code, or "" for all.'),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_vol_term,
    name = "vol_term_structure",
    description = paste(
      "Build realized volatility over 20, 60, 90 and 252-day windows side by side,",
      "the shape an implied volatility term structure is quoted in (g_vol_term_slide)."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      symbol = type_string('Restrict to one trading code, or "" for all.'),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_option_table,
    name = "option_table",
    description = paste(
      "Price a sheet of European option quotes and report implied volatility and",
      "Greeks (t_option_slide). Quotes are not fetched. Use data_path = \"example\"",
      "for the bundled eg_options quotes (AIA.NZ, Black-Scholes priced at its own",
      "realised vol) with spot = 0 to take the spot from the example series;",
      "otherwise pass an .rds with STRIKE, TYPE, PRICE and TTE, and the spot."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds of option quotes (STRIKE/TYPE/PRICE/TTE), or "example".'
      ),
      spot = type_number(
        paste(
          "Spot price of the underlying. 0 works only with data_path='example',",
          "where it is taken from eg_ohlc; otherwise it is required."
        )
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_fundamentals,
    name = "fundamentals_table",
    description = paste(
      "Build the fundamentals or valuation trend table (t_fundamentals_slide).",
      'Use data_path = "example" for the bundled eg_financials metrics;',
      "otherwise pass an .rds with METRIC, PERIOD and VALUE. Nothing is fetched."
    ),
    arguments = list(
      data_path = type_string(
        'Path to an .rds of METRIC / PERIOD / VALUE rows, or "example".'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_drop_output,
    name = "drop_output",
    description = paste(
      "Remove ONE built output from this session, leaving the rest in place.",
      "Use it when an output is no longer wanted; trade_reset clears everything.",
      "If the name is not built, the error lists what is."
    ),
    arguments = list(
      output_name = type_string(
        paste(
          'Name of the built output to drop, e.g. "candle". Call list_trade_outputs',
          "to see what this session has built."
        )
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_horizon,
    name = "horizon_table",
    description = paste(
      "Build the return-and-risk-by-horizon table: what the instrument returned",
      "and how volatile it was over several trailing windows at once",
      "(t_horizon_slide). Compares horizons for ONE trading code, so name a",
      "symbol; an empty symbol takes the first code in the data and the reply",
      "names what was left out."
    ),
    arguments = list(
      symbol = type_string(
        paste(
          'Trading code, e.g. "AIA.NZ". One table covers one code; empty string',
          '("") takes the first code in the data.'
        )
      ),
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      windows = type_string(
        'Comma-separated trailing windows in trading days, e.g. "21,63,252".'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_vol_premium,
    name = "vol_premium",
    description = paste(
      "Build the figure putting implied volatility from option quotes against",
      "realised volatility over several windows (g_vol_premium_slide) — the",
      "volatility risk premium. Covers ONE underlying. Needs option quotes, which",
      "are not fetched: pass an .rds of STRIKE/TYPE/PRICE/TTE plus the price series."
    ),
    arguments = list(
      symbol = type_string('Trading code of the underlying, e.g. "AIA.NZ".'),
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      options_path = type_string(
        "Path to an .rds of option quotes (STRIKE/TYPE/PRICE/TTE). Not fetched."
      ),
      spot = type_number(
        "Spot price of the underlying. 0 uses the last close in the series."
      ),
      windows = type_string(
        'Comma-separated realised-vol windows in days, e.g. "20,60,90".'
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_stock_report,
    name = "stock_report",
    description = paste(
      "Build one full analysis deck for a single trading code: a written reading",
      "of the long and short term, the horizon table, price history, drawdown, the",
      "risk summary, the return distribution, the candlestick chart and rolling",
      "risk, plus a benchmark comparison when one is named (render_stock_report).",
      "This is the tool to use when asked to analyse one company. Some pages are",
      "left out when the data is too short for them; the page list is returned.",
      "It refuses to overwrite an existing file unless overwrite is TRUE."
    ),
    arguments = list(
      symbol = type_string('Trading code to report on, e.g. "AIR.NZ".'),
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      benchmark = type_string(
        'Trading code already in the data to compare against, or "" for none.'
      ),
      outfile = type_string('Output .pptx path, e.g. "/tmp/air_nz_report.pptx".'),
      overwrite = type_boolean(
        "TRUE replaces an existing file at outfile. FALSE (the default) refuses."
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_generate_slides,
    name = "trade_generate_slides",
    description = paste(
      "Render one built output to a PowerPoint (.pptx) file and return its",
      "absolute path. Call a render_* tool first, then name the output to",
      'render (e.g. "candle", "performance", "trades", "equity", "risk_table",',
      '"drawdown", "ret_dist", "rolling_risk", "portfolio").',
      "One output per file; call again for each deck you want.",
      "If the name is not built yet, the error lists what is available."
    ),
    arguments = list(
      outfile = type_string('Output .pptx path, e.g. "/tmp/trade_deck.pptx".'),
      output_name = type_string(
        paste(
          'Name of the built output to render: "candle", "performance", "trades",',
          '"equity", "risk_table", "drawdown", "ret_dist", "rolling_risk" or "portfolio".',
          'Call list_trade_outputs to see what this session has already built.'
        )
      ),
      overwrite = type_boolean(
        paste(
          "TRUE replaces an existing file at outfile. FALSE (the default) refuses,",
          "because an agent may pick a path that is already a deck someone needs."
        )
      ),
      session_id = session_arg
    )
  ),

  tool(
    fun = fn_pptx_to_pdf,
    name = "pptx_to_pdf",
    description = paste(
      "Convert a .pptx file to PDF (LibreOffice if available, else PowerPoint via",
      "Windows COM from WSL). Returns the PDF path(s). Needs LibreOffice or",
      "PowerPoint on the machine: if it fails, the cause is the environment,",
      "not the arguments, so do not retry with different paths."
    ),
    arguments = list(
      path = type_string("Path to the .pptx file to convert."),
      output_dir = type_string(
        'Directory for the PDF. Empty string ("") writes next to the .pptx.'
      )
    )
  ),

  tool(
    fun = fn_reset,
    name = "trade_reset",
    description = paste(
      "Clear the built outputs of one session so a fresh deck can be assembled.",
      'Pass the session_id you were using; "" clears the default session.'
    ),
    arguments = list(session_id = session_arg)
  )

)

# ---- start server -----------------------------------------------------------

mcp_server(tools = tools)
