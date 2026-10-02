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

suppressPackageStartupMessages({
  library(mcptools)
  library(ellmer)
  library(autoslider.trade)
})

# ---- session state ----------------------------------------------------------
# Holds renderable outputs across tool calls within one session.
.state <- new.env(parent = emptyenv())
.state$outputs <- NULL

# ---- helpers ----------------------------------------------------------------
stop_if <- function(cond, msg) if (cond) stop(msg, call. = FALSE)
require_outputs <- function() {
  stop_if(
    is.null(.state$outputs) || length(.state$outputs) == 0L,
    "No outputs built yet. Call a render_* tool (e.g. candle_chart) first."
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

# ---- tool functions ---------------------------------------------------------

fn_list_outputs <- function() {
  paste(
    "Available trade outputs (render_* tools):",
    "  candle_chart  - g_candle_slide(): candlestick + Bollinger/MA/RSI/MACD",
    "  performance    - t_performance_slide(): per-instrument return / drawdown table",
    "  trades         - l_trades_slide(): trade listing",
    "  equity_curve   - g_equity_slide(): rebased price curve",
    "Risk analytics (need a longer series than the 5-row eg_prices):",
    "  risk_table     - t_risk_slide(): VaR / CVaR / drawdown / Sharpe ratios table",
    "  drawdown       - g_drawdown_slide(): underwater plot per instrument",
    "  ret_dist       - g_return_dist_slide(): return histogram with VaR / CVaR marked",
    "  rolling_risk   - g_rolling_risk_slide(): rolling volatility / Sharpe / VaR / drawdown",
    "  portfolio      - g_portfolio_risk_slide(): correlation heatmap + beta / info ratio",
    "  stock_report   - render_stock_report(): one full analysis deck for one company",
    "",
    "Then call trade_generate_slides(outfile=...) to assemble a .pptx,",
    "and optionally pptx_to_pdf(path=...) to convert it.",
    sep = "\n"
  )
}

fn_candle <- function(symbol, data_path) {
  df <- load_df(data_path, "eg_ohlc")
  if (nzchar(symbol)) df <- df[df$SYMBOL == symbol, ]
  stop_if(nrow(df) == 0L, sprintf("No rows for symbol '%s' in the data.", symbol))
  .state$outputs[["candle"]] <- g_candle_slide(df)
  sprintf("Candlestick chart built (%d rows). Call trade_generate_slides to render.", nrow(df))
}

fn_performance <- function(data_path) {
  df <- load_df(data_path, "eg_prices")
  .state$outputs[["performance"]] <- t_performance_slide(df)
  sprintf("Performance table built. Call trade_generate_slides to render.")
}

fn_trades <- function(data_path) {
  df <- load_df(data_path, "eg_trades")
  .state$outputs[["trades"]] <- l_trades_slide(df)
  sprintf("Trade listing built. Call trade_generate_slides to render.")
}

fn_equity <- function(data_path) {
  df <- load_df(data_path, "eg_prices")
  .state$outputs[["equity"]] <- g_equity_slide(df)
  sprintf("Equity curve built. Call trade_generate_slides to render.")
}

fn_generate_slides <- function(outfile, output_name) {
  require_outputs()
  stop_if(
    !output_name %in% names(.state$outputs),
    sprintf(
      "No output named '%s'. Built outputs: %s",
      output_name, paste(names(.state$outputs), collapse = ", ")
    )
  )
  outfile <- normalizePath(outfile, mustWork = FALSE)
  tryCatch(
    generate_slides(.state$outputs[[output_name]], outfile = outfile),
    error = function(e) stop("Slide generation error: ", e$message, call. = FALSE)
  )
  sprintf("Slides for '%s' written to: %s", output_name, outfile)
}

fn_pptx_to_pdf <- function(path, output_dir) {
  out <- pptx_to_pdf(path, if (nzchar(output_dir)) output_dir else NULL)
  sprintf("PDF(s) written: %s", paste(out, collapse = ", "))
}

fn_stock_report <- function(symbol, data_path, benchmark, outfile) {
  df <- load_df(data_path, "eg_ohlc")
  stop_if(nrow(df) == 0L, sprintf("No rows in the data for '%s'.", data_path))
  outfile <- normalizePath(outfile, mustWork = FALSE)
  tryCatch(
    render_stock_report(
      df,
      ticker = symbol,
      benchmark = optional(benchmark),
      outfile = outfile
    ),
    error = function(e) stop("Report error: ", e$message, call. = FALSE)
  )
  sprintf("Report for '%s' written to: %s", symbol, outfile)
}

fn_reset <- function() {
  .state$outputs <- NULL
  "Session state cleared."
}

# ---- risk analytics ---------------------------------------------------------
# The risk outputs need a longer series than `eg_prices` has, so they fall back
# to `eg_ohlc` and use its CLOSE column. `benchmark` travels over MCP as a
# string, so an empty string stands for `NULL`.

optional <- function(x) if (nzchar(x)) x else NULL

load_prices <- function(data_path, symbol) {
  df <- load_df(data_path, "eg_ohlc")
  if (nzchar(symbol)) df <- df[df$SYMBOL == symbol, ]
  stop_if(nrow(df) == 0L, sprintf("No rows for symbol '%s' in the data.", symbol))
  df
}

fn_risk_table <- function(data_path, benchmark, symbol) {
  df <- load_prices(data_path, symbol)
  .state$outputs[["risk_table"]] <- t_risk_slide(df, benchmark = optional(benchmark))
  sprintf("Risk summary built (%d rows). Call trade_generate_slides to render.", nrow(df))
}

fn_drawdown <- function(data_path, symbol) {
  df <- load_prices(data_path, symbol)
  .state$outputs[["drawdown"]] <- g_drawdown_slide(df)
  sprintf("Drawdown figure built (%d rows). Call trade_generate_slides to render.", nrow(df))
}

fn_ret_dist <- function(data_path, conf, symbol) {
  stop_if(!(conf > 0 && conf < 1), sprintf("conf must be in (0, 1), got %s.", conf))
  df <- load_prices(data_path, symbol)
  .state$outputs[["ret_dist"]] <- g_return_dist_slide(df, conf = conf)
  sprintf("Return distribution built (%d rows). Call trade_generate_slides to render.", nrow(df))
}

fn_rolling_risk <- function(data_path, window, symbol) {
  df <- load_prices(data_path, symbol)
  .state$outputs[["rolling_risk"]] <- g_rolling_risk_slide(df, window = window)
  sprintf(
    "Rolling risk figure built (%d rows, window %d). Call trade_generate_slides to render.",
    nrow(df), as.integer(window)
  )
}

fn_portfolio <- function(data_path, benchmark, window) {
  df <- load_prices(data_path, "")
  stop_if(
    nrow(df) < 2L || length(unique(df$SYMBOL)) < 2L,
    "Portfolio risk needs prices for at least two trading codes."
  )
  .state$outputs[["portfolio"]] <- g_portfolio_risk_slide(
    df, benchmark = optional(benchmark), window = window
  )
  sprintf(
    "Portfolio risk figure built (%d rows, benchmark '%s'). Call trade_generate_slides to render.",
    nrow(df), if (nzchar(benchmark)) benchmark else "none"
  )
}

# ---- register tools ---------------------------------------------------------

tools <- list(

  tool(
    fun = fn_list_outputs,
    name = "list_trade_outputs",
    description = paste(
      "List the trading output types this server can build and how to assemble",
      "them into slides. Call first to discover the render_* tools."
    ),
    arguments = list()
  ),

  tool(
    fun = fn_candle,
    name = "candle_chart",
    description = paste(
      "Build a candlestick chart with Bollinger Bands, moving averages, RSI and",
      "MACD for one or all trading codes (g_candle_slide). Adds it to the deck.",
      'Use data_path = "example" for the bundled OHLCV data, or an .rds path.'
    ),
    arguments = list(
      symbol = type_string(
        'Trading code to chart, e.g. "AIA.NZ". Empty string ("") charts all codes.'
      ),
      data_path = type_string(
        'Path to an .rds OHLCV data frame, or "example" for bundled eg_ohlc.'
      )
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
      )
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
      )
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
      )
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
      symbol = type_string('Restrict to one trading code, or "" for all.')
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
      symbol = type_string('Restrict to one trading code, or "" for all.')
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
      symbol = type_string('Restrict to one trading code, or "" for all.')
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
      symbol = type_string('Restrict to one trading code, or "" for all.')
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
      window = type_number("Number of periods per rolling Beta window, e.g. 63.")
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
      "This is the tool to use when asked to analyse one company."
    ),
    arguments = list(
      symbol = type_string('Trading code to report on, e.g. "AIR.NZ".'),
      data_path = type_string(
        'Path to an .rds prices data frame, or "example" for bundled eg_ohlc.'
      ),
      benchmark = type_string(
        'Trading code already in the data to compare against, or "" for none.'
      ),
      outfile = type_string('Output .pptx path, e.g. "/tmp/air_nz_report.pptx".')
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
      "One output per file; call again for each deck you want."
    ),
    arguments = list(
      outfile = type_string('Output .pptx path, e.g. "/tmp/trade_deck.pptx".'),
      output_name = type_string(
        paste(
          'Name of the built output to render: "candle", "performance", "trades",',
          '"equity", "risk_table", "drawdown", "ret_dist", "rolling_risk" or "portfolio".'
        )
      )
    )
  ),

  tool(
    fun = fn_pptx_to_pdf,
    name = "pptx_to_pdf",
    description = paste(
      "Convert a .pptx file to PDF (LibreOffice if available, else PowerPoint via",
      "Windows COM from WSL). Returns the PDF path(s)."
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
    description = "Clear the built outputs so a fresh deck can be assembled.",
    arguments = list()
  )

)

# ---- start server -----------------------------------------------------------

mcp_server(tools = tools)
